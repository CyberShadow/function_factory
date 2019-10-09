module function_factory.factory;

import std.algorithm.iteration;
import std.algorithm.mutation;
import std.array;
import std.random;
import std.range;
import std.traits;

import ae.utils.array;
import ae.utils.meta;
import ae.utils.text;
import ae.utils.text.ascii : toDec;

struct DefaultProgramParams
{
	alias T = double;
	enum maxInstructions = 20;
	enum numArgs   = 1; /// number of values specified for each function call
	enum numVars   = 0; /// number of variables preserved across calls
	enum numLocals = 0; /// number of variables scoped to a single call
	enum T[] constants = [0, 1, 2]; /// literals which may appear in the program
}

struct Program(alias params_)
{
	alias params = params_;
	alias T = params.T;
	static immutable T[] constants = params.constants;

	mixin({
		string s = "enum Op {" ~ q{
			hcf,

			// Unary (stack 1 => 1)
			neg,

			// Binary (stack 2 => 1)
			add,
			sub,
			mul,
			div,
			mod,
			pow,
		};

		// Nonary (stack 0 => 1)
		foreach (i; 0 .. constants.length)
			s ~= "pushConstant" ~ toDec(i) ~ ",";
		foreach (i; 0 .. params.numArgs)
			s ~= "pushArg" ~ toDec(i) ~ ",";
		foreach (i; 0 .. params.numVars)
			s ~= "pushVar" ~ toDec(i) ~ ",";
		foreach (i; 0 .. params.numLocals)
			s ~= "pushLocal" ~ toDec(i) ~ ",";

		// Void (stack 1 => 0)
		foreach (i; 0 .. params.numVars)
			s ~= "setVar" ~ toDec(i) ~ ",";
		foreach (i; 0 .. params.numLocals)
			s ~= "setLocal" ~ toDec(i) ~ ",";

		s ~= "}";
		return s;
	}());

	struct OpShape { byte arg, output; byte delta() const { return cast(byte)(output - arg); } }

	static OpShape opShape(Op op)
	{
		final switch (op)
		{
			case Op.hcf: return OpShape(0, 0);
			case Op.neg: return OpShape(1, 1);
			case Op.add:
			case Op.sub:
			case Op.mul:
			case Op.div:
			case Op.mod:
			case Op.pow: return OpShape(2, 1);

			static foreach (i; 0 .. constants.length)
				mixin("case Op.pushConstant" ~ toDec(i) ~ ":");
			static foreach (i; 0 .. params.numArgs)
				mixin("case Op.pushArg" ~ toDec(i) ~ ":");
			static foreach (i; 0 .. params.numVars)
				mixin("case Op.pushVar" ~ toDec(i) ~ ":");
			static foreach (i; 0 .. params.numLocals)
				mixin("case Op.pushLocal" ~ toDec(i) ~ ":");
			return OpShape(0, 1);

			static if (params.numVars || params.numLocals)
			{
				static foreach (i; 0 .. params.numVars)
					mixin("case Op.setVar" ~ toDec(i) ~ ":");
				static foreach (i; 0 .. params.numLocals)
					mixin("case Op.setLocal" ~ toDec(i) ~ ":");
				return OpShape(1, 0);
			}
		}
	}

	static immutable OpShape[enumLength!Op] opShapes = {
		OpShape[enumLength!Op] result;
		foreach (op; EnumMembers!Op)
			result[op] = opShape(op);
		return result;
	}();

	Op[params.maxInstructions] ops;
	size_t numOps;

	struct Instance
	{
		T[params.numVars] vars;
	}
	Instance initInstance;

	enum maxStack = params.maxInstructions;

	T eval(ref Instance instance, T[params.numArgs] args...)
	{
		T[maxStack] stack = void;
		size_t stackPointer;
		T[params.numLocals] locals;

		void push(T val)
		{
			stack[stackPointer++] = val;
		}

		T pop()
		{
			assert(stackPointer, "Stack underflow");
			return stack[--stackPointer];
		}

		void binary(string op)()
		{
			auto left  = pop();
			auto right = pop();
			push(mixin("left " ~ op ~ " right"));
		}

		foreach (op; ops[0 .. numOps])
		{
		opSwitch:
			final switch (op)
			{
				case Op.hcf: assert(false, "On fire");
				case Op.neg: push(-pop()); break;
				case Op.add: binary!"+"(); break;
				case Op.sub: binary!"-"(); break;
				case Op.mul: binary!"*"(); break;
				case Op.div: binary!"/"(); break;
				case Op.mod: binary!"%"(); break;
				case Op.pow: binary!"^^"(); break;

				static foreach (i; 0 .. constants.length)
				{
					mixin("case Op.pushConstant" ~ toDec(i) ~ ":");
					push(constants[i]);
					break opSwitch;
				}
				static foreach (i; 0 .. params.numArgs)
				{
					mixin("case Op.pushArg" ~ toDec(i) ~ ":");
					push(args[i]);
					break opSwitch;
				}
				static foreach (i; 0 .. params.numVars)
				{
					mixin("case Op.pushVar" ~ toDec(i) ~ ":");
					push(instance.vars[i]);
					break opSwitch;
				}
				static foreach (i; 0 .. params.numLocals)
				{
					mixin("case Op.pushLocal" ~ toDec(i) ~ ":");
					push(locals[i]);
					break opSwitch;
				}

				static foreach (i; 0 .. params.numVars)
				{
					mixin("case Op.setVar" ~ toDec(i) ~ ":");
					instance.vars[i] = pop();
					break opSwitch;
				}
				static foreach (i; 0 .. params.numLocals)
				{
					mixin("case Op.setLocal" ~ toDec(i) ~ ":");
					locals[i] = pop();
					break opSwitch;
				}
			}
		}

		T result = pop();
		assert(stackPointer == 0, "Dirty stack");
		return result;
	}

	static if (params.numVars == 0)
		T eval(T[params.numArgs] args...) { Instance instance = void; return eval(instance, args); }

	static if (params.numArgs == 0)
		T eval(ref Instance instance) { T[params.numArgs] args = void; return eval(instance, args); }

	static Program generate(RNG)(ref RNG rng)
	{
	retryProgram:
		Program program;
		program.numOps = uniform!"[]"(1, params.maxInstructions, rng);
		auto order = program.numOps.iota.array;

		void addOp(size_t pos, Op op)
		{
			assert(pos < program.numOps);
			assert(program.ops[pos] == Op.init);
			program.ops[pos] = op;
		}

		bool stackOK()
		{
			byte depth = 0;
			foreach (op; program.ops[0 .. program.numOps])
			{
				depth -= opShapes[op].arg;
				if (depth < 0)
					return false;
				depth += opShapes[op].output;
			}
			return depth == 1;
		}

		uint tries;
		enum maxTries = params.maxInstructions * 10;

		// Start with 1 push instruction
		static immutable Op[] pushers = [EnumMembers!Op].filter!(op => opShapes[op] == OpShape(0, 1)).array;
		static assert(pushers.length > 0, "No push instructions (no constants/args/vars/locals)?");
		addOp(order.pluck, pushers[uniform(0, $, rng)]);

		while (order.length)
		{
			// Check sanity
			assert(stackOK, "Bad stack generated");

			auto o1 = cast(Op)uniform(1, enumLength!Op, rng);
			OpShape s1 = opShapes[o1];
			if (s1.arg != s1.output)
			{
				// Balance it out

				if (order.length < 2)
					goto retryOp;

				static immutable Op[][3] opsWithDelta =
					iota(-1, 2)
					.map!(delta =>
						[EnumMembers!Op]
						.filter!(op => opShapes[op].delta == delta)
						.array
					)
					.array;

				Op o2 = opsWithDelta[1 - s1.delta][uniform(0, $, rng)];
				OpShape s2 = opShapes[o2];
				if (s1.delta < 0)
				{
					swap(o1, o2); // first add the one which grows stack
					swap(s1, s2);
				}

				auto p1 = uniform(0, order.length    , rng);
				auto p2 = uniform(0, order.length - 1, rng);
				if (p2 >= p1)
					p2++;
				addOp(order[p1], o1);
				addOp(order[p2], o2);
				if (stackOK)
				{
					if (p1 > p2)
						swap(p1, p2);
					order = order.remove(p1, p2);
				}
				else
				{
					program.ops[order[p1]] = program.ops[order[p2]] = Op.init;
					goto retryOp;
				}
			}
			else
			{
				// Wherever
				auto p1 = uniform(0, order.length, rng);
				addOp(order[p1], o1);
				if (stackOK)
					order = order.remove(p1);
				else
				{
					program.ops[order[p1]] = Op.init;
					goto retryOp;
				}
			}

			tries = 0; // success
			continue;

		retryOp:
			if (++tries >= maxTries)
				goto retryProgram;
		}

		static if (constants.length)
			foreach (i; 0 .. params.numVars)
				program.initInstance.vars[i] = constants[uniform(0, $)];

		return program;
	}

	string toString() const
	{
		enum char firstArg   = 'x';
		enum char firstVar   = 'a';
		enum char firstLocal = 'i';

		string[maxStack] stack;
		size_t stackPointer;

		string name(char first, size_t index) { return [cast(immutable char)(first + index)]; }

		void push(string val)
		{
			stack[stackPointer++] = val;
		}

		string pop()
		{
			assert(stackPointer, "Stack underflow");
			return stack[--stackPointer];
		}

		void binary(string op)
		{
			auto left  = pop();
			auto right = pop();
			push("(" ~ left ~ " " ~ op ~ " " ~ right ~ ")");
		}

		string s = T.stringof ~ " f(" ~ chain(
			params.numArgs.iota.map!(i =>          T.stringof ~ " " ~ name(firstArg, i)),
			params.numVars.iota.map!(i => "ref " ~ T.stringof ~ " " ~ name(firstVar, i)),
		).join(", ") ~ ")\n" ~ "{\n";
		if (params.numLocals)
			s ~= "\t" ~ T.stringof ~ " " ~
				params.numLocals.iota.map!(i => name(firstLocal, i)).join(", ") ~ ";\n";

		foreach (op; ops[0 .. numOps])
		{
		opSwitch:
			final switch (op)
			{
				case Op.hcf: s ~= "\tassert(false);\n"; break;
				case Op.neg: push("(- " ~ pop()~ ")"); break;
				case Op.add: binary("+"); break;
				case Op.sub: binary("-"); break;
				case Op.mul: binary("*"); break;
				case Op.div: binary("/"); break;
				case Op.mod: binary("%"); break;
				case Op.pow: binary("^^"); break;

				static foreach (i; 0 .. constants.length)
				{
					mixin("case Op.pushConstant" ~ toDec(i) ~ ":");
					push(constants[i].fpToString);
					break opSwitch;
				}
				static foreach (i; 0 .. params.numArgs)
				{
					mixin("case Op.pushArg" ~ toDec(i) ~ ":");
					push(name(firstArg, i));
					break opSwitch;
				}
				static foreach (i; 0 .. params.numVars)
				{
					mixin("case Op.pushVar" ~ toDec(i) ~ ":");
					push(name(firstVar, i));
					break opSwitch;
				}
				static foreach (i; 0 .. params.numLocals)
				{
					mixin("case Op.pushLocal" ~ toDec(i) ~ ":");
					push(name(firstLocal, i));
					break opSwitch;
				}

				static foreach (i; 0 .. params.numVars)
				{
					mixin("case Op.setVar" ~ toDec(i) ~ ":");
					s ~= "\t" ~ name(firstVar, i) ~ " = " ~ pop() ~ ";\n";
					break opSwitch;
				}
				static foreach (i; 0 .. params.numLocals)
				{
					mixin("case Op.setLocal" ~ toDec(i) ~ ":");
					s ~= "\t" ~ name(firstLocal, i) ~ " = " ~ pop() ~ ";\n";
					break opSwitch;
				}
			}
		}

		s ~= "\treturn " ~ pop() ~ ";\n}\n";
		assert(stackPointer == 0, "Dirty stack");

		return s;
	}
}

Program!params generateFunction(alias params, RNG)(bool delegate(ref Program!params) verifier, ref RNG rng)
{
	alias P = Program!params;
	while (true)
	{
		auto p = P.generate(rng);
		if (verifier(p))
			return p;
	}
}

Program!params generateFunction(alias params)(bool delegate(ref Program!params) verifier) { return generateFunction!params(verifier, rndGen); }

///
unittest
{
	struct Params
	{
		static DefaultProgramParams defaults; alias defaults this;
		enum maxInstructions = 5;
	}
	Program!Params p;

	Xorshift rng;

	// Return a constant
	p = generateFunction!Params((ref p) =>
		p.eval(1) == 1 &&
		p.eval(2) == 1 &&
		p.eval(4) == 1, rng);
	assert(p.eval(42) == 1);

	// Return arg (identity)
	p = generateFunction!Params((ref p) =>
		p.eval(1) == 1 &&
		p.eval(2) == 2 &&
		p.eval(4) == 4, rng);
	assert(p.eval(42) == 42);

	// Guess offset
	p = generateFunction!Params((ref p) =>
		p.eval(1) == 2 &&
		p.eval(2) == 3 &&
		p.eval(4) == 5, rng);
	assert(p.eval(42) == 43);

	// Guess multiplication factor
	p = generateFunction!Params((ref p) =>
		p.eval(1) == 2 &&
		p.eval(2) == 4 &&
		p.eval(4) == 8, rng);
	assert(p.eval(42) == 84);

	// Guess modulus
	p = generateFunction!Params((ref p) =>
		p.eval( 4) == 0 &&
		p.eval( 7) == 1 &&
		p.eval(18) == 0, rng);
	assert(p.eval(41) == 1);
}

///
unittest
{
	struct Params
	{
		static DefaultProgramParams defaults; alias defaults this;
		enum numArgs = 0;
		enum numVars = 1;
		enum maxInstructions = 5;
	}
	Program!Params p;
	Program!Params.Instance i;

	Xorshift rng;

	// Series - incrementing value
	p = generateFunction!Params((ref p) {
			auto i = p.Instance(0);
			return
				p.eval(i) == 1 &&
				p.eval(i) == 2 &&
				p.eval(i) == 3;
		}, rng);
	i = p.Instance(42); assert(p.eval(i) == 43);

	// Series - exponential
	p = generateFunction!Params((ref p) {
			auto i = p.Instance(1);
			return
				p.eval(i) == 2 &&
				p.eval(i) == 4 &&
				p.eval(i) == 8;
		}, rng);
	i = p.Instance(64); assert(p.eval(i) == 128);
}

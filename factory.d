module function_factory.factory;

import std.algorithm.comparison;
import std.algorithm.iteration;
import std.algorithm.mutation;
import std.array;
import std.math;
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
	enum T[] constants = [0, 1, 2, PI]; /// literals which may appear in the program
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

			sin,
			cos,
			tan,

			// Binary (stack 2 => 1)
			add,
			sub,
			mul,
			div,
			mod,
			pow,
			min,
			max_,
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

	struct OpShape { byte input, output; byte delta() const { return cast(byte)(output - input); } }

	static OpShape opShape(Op op)
	{
		final switch (op)
		{
			case Op.hcf: return OpShape(0, 0);
			case Op.sin:
			case Op.cos:
			case Op.tan:
			case Op.neg: return OpShape(1, 1);
			case Op.add:
			case Op.sub:
			case Op.mul:
			case Op.div:
			case Op.mod:
			case Op.pow:
			case Op.min:
			case Op.max_: return OpShape(2, 1);

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
				case Op.sin: push(sin(pop())); break;
				case Op.cos: push(cos(pop())); break;
				case Op.tan: push(tan(pop())); break;
				case Op.add: binary!"+"(); break;
				case Op.sub: binary!"-"(); break;
				case Op.mul: binary!"*"(); break;
				case Op.div: binary!"/"(); break;
				case Op.mod: binary!"%"(); break;
				case Op.pow: binary!"^^"(); break;
				case Op.min: push(min(pop(), pop())); break;
				case Op.max_:push(max(pop(), pop())); break;

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
		Program program;
		program.numOps = 1 + uniform!uint(rng) % params.maxInstructions;
		size_t[params.maxInstructions] orderBuf;
		auto order = orderBuf[0 .. program.numOps];
		copy(program.numOps.iota, order);
		order.shift();
		order.randomShuffle(rng);

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
				depth -= opShapes[op].input;
				if (depth < 0)
					return false;
				depth += opShapes[op].output;
			}
			return depth == 1;
		}

		// Start with 1 push instruction
		static immutable Op[] pushers = [EnumMembers!Op].filter!(op => opShapes[op] == OpShape(0, 1)).array;
		static assert(pushers.length > 0, "No push instructions (no constants/args/vars/locals)?");
		addOp(0, pushers[uniform!uint(rng) % $]);

		while (order.length)
		{
			// Check sanity
			assert(stackOK, "Bad stack generated");

			enum minDelta = [EnumMembers!Op].map!(op => opShapes[op].delta).reduce!min;
			enum maxDelta = [EnumMembers!Op].map!(op => opShapes[op].delta).reduce!max;
			static immutable Op[][maxDelta - minDelta + 1] opsWithDelta =
				iota(minDelta, maxDelta + 1)
				.map!(delta =>
					[EnumMembers!Op]
					.filter!(op => op != Op.init && opShapes[op].delta == delta)
					.array
				)
				.array;

			Op o1;
			if (order.length < 2)
				o1 = opsWithDelta[0 - minDelta][uniform!uint(rng) % $];
			else
				o1 = cast(Op)(1 + uniform!uint(rng) % (enumLength!Op - 1));

			OpShape s1 = opShapes[o1];
			if (s1.input != s1.output)
			{
				// Balance it out

				assert(order.length >= 2);

				Op o2 = opsWithDelta[1 - s1.delta][uniform!uint(rng) % $];
				OpShape s2 = opShapes[o2];
				if (s1.delta < 0)
				{
					swap(o1, o2); // first add the one which grows stack
					swap(s1, s2);
				}

				auto p1 = order.shift();
				auto p2 = order.shift();
				if (p1 > p2)
					swap(p1, p2);
				addOp(p1, o1);
				addOp(p2, o2);
			}
			else
			{
				// Wherever
				auto p1 = order.shift();
				addOp(p1, o1);
			}
		}

		static if (constants.length)
			foreach (i; 0 .. params.numVars)
				program.initInstance.vars[i] = constants[uniform!uint(rng) % $];

		return program;
	}

	string toString() const @safe nothrow
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
				case Op.sin: push("sin(" ~ pop() ~ ")"); break;
				case Op.cos: push("cos(" ~ pop() ~ ")"); break;
				case Op.tan: push("tan(" ~ pop() ~ ")"); break;
				case Op.min: push("min(" ~ pop() ~ ", " ~ pop() ~ ")"); break;
				case Op.max_:push("max(" ~ pop() ~ ", " ~ pop() ~ ")"); break;

				static foreach (i; 0 .. constants.length)
				{
					mixin("case Op.pushConstant" ~ toDec(i) ~ ":");
					if (constants[i] == params.T(PI))
						push("PI");
					else
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

Program!params generateFunction(alias params, alias verifier, RNG)(ref RNG rng)
{
	alias P = Program!params;
	while (true)
	{
		auto p = P.generate(rng);
		if (verifier(p))
			return p;
	}
}

Program!params generateFunction(alias params, alias verifier)() { return generateFunction!(params, verifier)(rndGen); }

///
pure @safe nothrow @nogc
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
	p = generateFunction!(Params, (ref p) =>
		p.eval(1) == 1 &&
		p.eval(2) == 1 &&
		p.eval(4) == 1)(rng);
	assert(p.eval(42) == 1);

	// Return arg (identity)
	p = generateFunction!(Params, (ref p) =>
		p.eval(1) == 1 &&
		p.eval(2) == 2 &&
		p.eval(4) == 4)(rng);
	assert(p.eval(42) == 42);

	// Guess offset
	p = generateFunction!(Params, (ref p) =>
		p.eval(1) == 2 &&
		p.eval(2) == 3 &&
		p.eval(4) == 5)(rng);
	assert(p.eval(42) == 43);

	// Guess multiplication factor
	p = generateFunction!(Params, (ref p) =>
		p.eval(1) == 2 &&
		p.eval(2) == 4 &&
		p.eval(4) == 8)(rng);
	assert(p.eval(42) == 84);

	// Guess modulus
	p = generateFunction!(Params, (ref p) =>
		p.eval( 4) == 0 &&
		p.eval( 7) == 1 &&
		p.eval(18) == 0)(rng);
	assert(p.eval(41) == 1);
}

///
pure @safe nothrow @nogc
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
	p = generateFunction!(Params, (ref p) {
			auto i = p.Instance(0);
			return
				p.eval(i) == 1 &&
				p.eval(i) == 2 &&
				p.eval(i) == 3;
		})(rng);
	i = p.Instance(42); assert(p.eval(i) == 43);

	// Series - exponential
	p = generateFunction!(Params, (ref p) {
			auto i = p.Instance(1);
			return
				p.eval(i) == 2 &&
				p.eval(i) == 4 &&
				p.eval(i) == 8;
		})(rng);
	i = p.Instance(64); assert(p.eval(i) == 128);
}

Program!params generateFunctionMT(alias params, alias verifier, alias rndGenGen)()
{
	alias P = Program!params;
	bool found;
	P result;

	import std.parallelism : totalCPUs, parallel;
	foreach (thread; totalCPUs.iota.parallel(1))
	{
		auto rng = rndGenGen();
		while (!found)
		{
			auto p = P.generate(rng);
			if (verifier(p))
			{
				synchronized
					if (!found)
					{
						result = p;
						found = true;
					}
				break;
			}
		}
	}

	return result;
}

Program!params generateFunctionMT(alias params, alias verifier)()
{
	return generateFunctionMT!(params, verifier, () => Random(unpredictableSeed));
}

unittest
{
	generateFunctionMT!(DefaultProgramParams, (ref p) => p.eval(1) == 2);
}

/// Discover a function to generate the Fibonacci sequence.

module examples.gen_fibonacci;

import std.algorithm.searching;
import std.math;
import std.random;
import std.range;
import std.stdio;

import factory;

void main()
{
	struct Params
	{
		static DefaultProgramParams defaults; alias defaults this;
		enum numArgs = 0;
		enum numVars = 2;
		enum maxInstructions = 7;
	}
	auto p = generateFunction!Params((ref p) {
		auto i = p.initInstance;
		return
			p.eval(i) == 0 &&
			p.eval(i) == 1 &&
			p.eval(i) == 1 &&
			p.eval(i) == 2 &&
			p.eval(i) == 3 &&
			p.eval(i) == 5 &&
			p.eval(i) == 8;
		});
	p.writeln;
}

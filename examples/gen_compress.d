/// Compress positive numbers into the [0, 1) range.

module function_factory.examples.gen_compress;

import std.algorithm.searching;
import std.math;
import std.random;
import std.range;
import std.stdio;

import function_factory.factory;

void main()
{
	struct Params
	{
		static DefaultProgramParams defaults; alias defaults this;
		enum maxInstructions = 6;
	}
	auto p = generateFunction!Params((ref p) =>
		p.eval(0) == 0 &&
		p.eval(1) == 0.5 &&
		iota(10).all!(k =>
			p.eval(PI^^k) > 0 &&
			p.eval(PI^^k) < 1 &&
			p.eval(PI^^k) < p.eval(PI^^k + 1)));
	p.writeln;
	foreach (i; 0 .. 10)
		writeln(i, " => ", p.eval(i));
}

Function Factory
================

This module generates random functions which satisfy the given conditions.

```d
struct Params
{
    static DefaultProgramParams defaults; alias defaults this;
    enum maxInstructions = 5;
}
Program!Params p;

// Guess multiplication factor
p = generateFunction!Params((ref p) => p.eval(1) == 2 &&
                                       p.eval(2) == 4 &&
                                       p.eval(4) == 8);
assert(p.eval(42) == 84);

// Guess modulus
p = generateFunction!Params((ref p) => p.eval(4) == 0 &&
                                       p.eval(7) == 1 &&
                                       p.eval(18) == 0);
assert(p.eval(41) == 1);

// Resulting functions can be printed to get D source code:
writeln(p);
// double f(double x)
// {
//     return (x % 2);
// }
```

The factory generates functions with a configurable number of parameters, local variables, and persistent variables (for series).
The constants (numeric literals) which may appear in the function are also configurable.

Example for generating a parameter-less series generator function:

```d
struct Params
{
    static DefaultProgramParams defaults; alias defaults this;
    enum numArgs = 0;
    enum numVars = 1;
    enum maxInstructions = 5;
}
Program!Params p;

// Exponential series
p = generateFunction!Params((ref p) {
        auto i = p.Instance(1);
        return
            p.eval(i) == 2 &&
            p.eval(i) == 4 &&
            p.eval(i) == 8;
    }, rng);
auto i = p.Instance(64);
assert(p.eval(i) == 128);
```

The internal representation and function evaluation is implemented as a simple stack machine
(operations and operands are stored in Reverse Polish Notation).

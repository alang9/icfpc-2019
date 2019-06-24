icfpc-2019
==========

Building
--------

You can build our source code using Haskell's [stack] build system.  Once
stack is installed, issue:

    stack build

[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/

Running
-------

Our solvers (`greedy`, `dfs-clone`, ...) take two command line parameters and
produce the solution on `stdout`:

    stack exec greedy -- problem-001.desc problem-001.buy >problem-001.sol

`buyer` is a small program that writes a folder of buys.  It takes two
parameters, the balance for which it can go buy things and the output directory:

    stack exec buyer -- 222222 buys/

`coin-puzzle` generates a task description from conditions, for lambdacoin
mining:

    stack exec coin-puzzle -- puzzle.cond >puzzle.desc

Approach
--------

Our final strategy was dfs-clone.  It combines a depth first search together
with some heuristics, and places particular importance on cloning.  This is also
why we spend all our lambdacoin budget on buying clones.

Feedback
--------

It was a lot of fun.

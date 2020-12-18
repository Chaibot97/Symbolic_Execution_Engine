Symbolic Execution Engine
====  
Authors: Lizhou Cai, Junrui Liu  


## Prerequisites
* `cabal` v3.2.0.0
* `python` v3.5+
  * `sexpdata` library, which can installed via `python3 -m pip install sexpdata`


## Usage

Build the project by typing `make`.

Run the program with

    bin/see [--dry-run] FILE N

where
- `FILE` is the source program
- `N` is an integer representing the number of times the symbolic execution engine should unroll the loops
- `--dry-run` lets the engine print out the SMT formula(s) without verifying it.


## Caveats

- For every assertion reachable via *some execution path p and symbolic state s*, we query `z3` about the validity of the assertion. That is, a single assertion may results in multiple queries, if the assertion is reachable from multiple paths or symbolic states.

  For programs with one non-nested loop and one assertion at the very end, this is identical to the behavior implied by example in the specification.

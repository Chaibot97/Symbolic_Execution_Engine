Mini-Dafny  
===  
Authors: Lizhou Cai, Junrui Liu  

## Prerequisites
* cabal v3.2.0.0
* z3 v4.8.9

## Building
`cd` to `vcgen/` and run `make vcgen`.

## Usage

In directory `vcgen/`, run

    ./vcgen.sh IMP_FILE

This will print out either 
* `Verified` if the IMP program is valid, i.e. the specified precondition implies the weakest precondition computed by mini-Dafny, or
* `Not verified` if the program is invalid, or there was an error (such as a parser error).

To prevent z3 from freezing, a three-second timeout is set.

We are currently using z3 version 4.8.9 and it has some bugs that cause the program to hang indefinitely for some test case (e.g. `gcd.imp`).
However, the version of z3 used in <https://rise4fun.com/z3/> is working fine.

In this case, run the program with option `--vc`:

    ./vcgen.sh IMP_FILE --vc

This will print out the negation of the verification condition in the `SMT-LIB` (version 2) format. You can paste it into the online z3 solver to get the correct result (`unsat` -> valid, `sat` -> invalid).


---
## Benchmarking
The official benchmarks can be found in `Benchmarks/`, while our original benchmarks are in `Benchmarks_ORI/`.

Test files are further partitioned into `valid` and `invalid` according to the program's validity.

To run all the tests, simply run

    ./test.sh

For every `.imp` test file, it will output
* `ok` if the z3 output is consistent with expectation.
* `failed / timeout` otherwise.

(Test `gcd.imp` will timeout because of z3v4.8.9's bug, but all the other tests should pass.)
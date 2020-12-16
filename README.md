Symbolic Execution Engine
====  
Authors: Lizhou Cai, Junrui Liu  

## Prerequisites
* cabal v3.2.0.0

## Usage

`cd` into `see`. Build the project by typing `make`.

Run the program with

```
./see.sh [--dry-run] FILE N
```

where
- `FILE` is the source program
- `N` is an integer representing the number of times the symbolic execution engine should unroll the loops
- `--dry-run` lets the engine print out the SMT formula without verifying it.
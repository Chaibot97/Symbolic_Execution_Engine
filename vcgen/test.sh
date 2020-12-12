#!/bin/bash

suites=("../Benchmarks" "../Benchmarks_ORI")
cases=(valid invalid)
valid_msg="unsat"
invalid_msg="sat"
for suite in "${suites[@]}"; do
  seq  -f "-" -s "" 50
  echo "$suite"
  for case in "${cases[@]}"; do
    find "$suite"/$case -name "*.imp" | while read t; do
      printf "$t >>> "
      out=$(./vcgen.sh "$t" --vc | z3 -in -t:3000)
      if [[ $case == valid && "$out" == "$valid_msg" ]] || [[ "$case" == invalid && "$out" == "$invalid_msg" ]]; then
        echo ok
      else
        echo failed / timeout
        echo "$out"
      fi
    done
  done
done
#!/bin/bash

for i in {0..5}; do
  for f in test/benchmarks/invalid/*.imp; do
    printf "$f.. " 
    out=$(bin/see "$f" $i | grep "Not verified")
    if [ $? -ne 0 ]; then
      echo "failed"
    else
      echo "ok"
    fi
  done
done

for i in {0..3}; do
  for f in test/benchmarks/valid/*.imp; do
    printf "$f.. "
    out=$(bin/see "$f" $i | grep "Verified")
    if [ $? -ne 0 ]; then
      echo "failed"
    else
      echo "ok"
    fi
  done
done
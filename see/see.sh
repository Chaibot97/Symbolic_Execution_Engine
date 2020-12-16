#!/bin/bash
if ! command -v z3 &> /dev/null; then
    echo "z3 could not be found"
    exit
fi
if ! command -v cabal &> /dev/null; then
    echo "cabal could not be found"
    exit
fi

valid_msg="Verified"
invalid_msg="Not verified"
error_msg="Error"


formula=$(cabal run see "$@")
if [ "$?" -ne "0" ]; then
    echo "$error_msg"
    echo "$formula"
elif [ "$1" == "--dry-run" ]; then # dry run: print smt formula
    echo "$formula"
else
    out=$(echo "$formula" | z3 -in)
    if [ "$out" = unsat ]; then
        echo "$valid_msg"
    elif [ "$out" = sat ]; then
        echo "$invalid_msg"
    else
        echo "$invalid_msg"
    fi
fi
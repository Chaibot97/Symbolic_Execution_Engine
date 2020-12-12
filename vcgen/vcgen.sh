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


vc=$(cabal -v0 new-run vcgen "$1" 2>/dev/null)
if [ "$?" -ne "0" ]; then
    echo "$invalid_msg"
elif [ "$2" == "--vc" ]; then # print verification condition only
    echo "$vc"
else
    out=$(echo "$vc" | z3 -in)
    if [ "$out" = unsat ]; then
        echo "$valid_msg"
    elif [ "$out" = sat ]; then
        echo "$invalid_msg"
    else
        echo "$invalid_msg"
    fi
fi
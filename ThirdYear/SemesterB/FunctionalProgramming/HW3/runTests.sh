#!/bin/bash

echo "--- Compiling HW2.hs ---"
ghc -Wall -Werror HW2.hs

echo "--- Compiling Spec.hs ---"
ghc --make -v Spec.hs -o Spec -package HUnit

echo "--- Running Spec ---"
./Spec

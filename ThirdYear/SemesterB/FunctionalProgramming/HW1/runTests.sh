#!/bin/bash

echo "--- Compiling HW1.hs ---"
ghc -Wall -Werror HW1.hs

echo "--- Compiling Spec.hs ---"
ghc --make -v Spec.hs -o Spec -package HUnit

echo "--- Running Spec ---"
./Spec

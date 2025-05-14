#!/bin/bash

echo "--- Compiling HW1.hs ---"
ghc -Wall -Werror HW1.hs

echo "--- Compiling Spec.hs ---"
# ghc --make -v Spec.hs -o Spec -package HUnit
ghc --make -v HW1_tests.hs -o HW1_tests \
    -package tasty \
    -package tasty-hunit \
    -package tasty-quickcheck

echo "--- Running Spec ---"
# ./Spec
./HW1_tests

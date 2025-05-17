#!/bin/bash

echo "--- Compiling HW3.hs ---"
# ghc -Wall -Werror -package containers HW3.hs
ghc -Wall -Werror HW3.hs

echo "--- Compiling Spec.hs ---"
ghc --make -v Spec.hs -o Spec \
     -package HUnit \
     -package hspec \
     -package containers

echo "--- Running Spec ---"
./Spec

#!/bin/bash

echo "--- Compiling HW6.hs ---"
ghc -Wall -Werror -package containers HW6.hs

echo "--- Compiling Spec.hs ---"
ghc --make -v Spec.hs -o Spec \
     -package HUnit \
     -package hspec \
     -package containers

echo "--- Running Tests ---"
./Spec

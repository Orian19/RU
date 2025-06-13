echo "--- Compiling MultiSet.hs ---"
ghc -Wall -Werror -package containers MultiSet.hs

echo "--- Compiling HW4.hs ---"
ghc -Wall -Werror -package containers HW5.hs

echo "--- Compiling Spec.hs ---"
ghc --make -v Spec.hs -o Spec \
     -package HUnit \
     -package hspec \
     -package containers

echo "--- Running Tests ---"
./Spec

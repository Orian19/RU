{-# LANGUAGE GHC2024 #-}
import Control.Exception (catch, SomeException)
import qualified Data.Ratio as Ratio
import Test.Hspec
import HW4
import MultiSet
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Utility
(@?=) :: (Eq a, Show a) => a -> a -> IO ()
x @?= y = if x == y then pure () else error ("Expected: " ++ show y ++ ", but got: " ++ show x)

-- Sample data for testing
sampleMatrix :: Matrix Int
sampleMatrix = Matrix [[1,2,3], [4,5,6], [7,8,9]]

sampleMatrix2 :: Matrix Int
sampleMatrix2 = Matrix [[9,8,7], [6,5,4], [3,2,1]]

identityMatrix :: Matrix Int
identityMatrix = Matrix [[1,0,0], [0,1,0], [0,0,1]]

zeroMatrix :: Matrix Int
zeroMatrix = Matrix [[0,0,0], [0,0,0], [0,0,0]]

sparseMatrix1 :: SparseMatrix Int
sparseMatrix1 = SparseMatrix 3 3 (Map.fromList [((0,0),1), ((1,1),5), ((2,2),9)])

sparseMatrix2 :: SparseMatrix Int
sparseMatrix2 = SparseMatrix 3 3 (Map.fromList [((0,1),2), ((1,0),4), ((2,1),8)])

emptySparseMatrix :: SparseMatrix Int
emptySparseMatrix = SparseMatrix 3 3 (Map.empty :: Map.Map (Integer, Integer) Int)

adjMatrix :: Matrix Int
adjMatrix = Matrix [[0,1,1,0], [0,0,1,1], [0,0,0,1], [0,0,0,0]]

-- Main spec
main :: IO ()
main = hspec $ do
  
  describe "Section 1: MultiSet Implementation" $ do
    
    describe "empty" $ do
      it "creates an empty multiset (test 1)" $
        count 1 empty @?= 0
      it "creates an empty multiset (test 2)" $
        count "hello" empty @?= 0
      it "creates an empty multiset (test 3)" $
        member 'a' empty @?= False
      it "creates an empty multiset (test 4)" $
        toList (empty :: MultiSet Int) @?= []
      it "creates an empty multiset (test 5)" $
        show (empty :: MultiSet String) @?= "{}"
    
    describe "member" $ do
      it "checks non-existent element (test 1)" $
        member 1 empty @?= False
      it "checks existing element (test 2)" $
        member 1 (insert 1 empty) @?= True
      it "checks after multiple inserts (test 3)" $ do
        let ms = insert 2 (insert 1 (insert 2 empty))
        member 2 ms @?= True
      it "checks non-member in non-empty set (test 4)" $ do
        let ms = insert 1 (insert 2 empty)
        member 3 ms @?= False
      it "checks with characters (test 5)" $
        member 'x' (insert 'x' (insert 'y' empty)) @?= True
    
    describe "count" $ do
      it "counts zero for empty set (test 1)" $
        count 1 empty @?= 0
      it "counts single occurrence (test 2)" $
        count 1 (insert 1 empty) @?= 1
      it "counts multiple occurrences (test 3)" $ do
        let ms = insert 1 (insert 1 (insert 1 empty))
        count 1 ms @?= 3
      it "counts different elements (test 4)" $ do
        let ms = insert 2 (insert 1 (insert 2 empty))
        count 2 ms @?= 2
      it "counts with mixed elements (test 5)" $ do
        let ms = fromList [1,2,1,3,1,2]
        count 2 ms @?= 2
    
    describe "remove" $ do
      it "removes from empty set (test 1)" $
        count 1 (remove 1 empty) @?= 0
      it "removes single occurrence (test 2)" $ do
        let ms = insert 1 empty
        count 1 (remove 1 ms) @?= 0
      it "removes one of multiple occurrences (test 3)" $ do
        let ms = insert 1 (insert 1 empty)
        count 1 (remove 1 ms) @?= 1
      it "removes non-existent element (test 4)" $ do
        let ms = insert 2 empty
        count 2 (remove 1 ms) @?= 1
      it "removes until empty (test 5)" $ do
        let ms = insert 1 (insert 1 empty)
            ms' = remove 1 (remove 1 ms)
        count 1 ms' @?= 0
    
    describe "insert" $ do
      it "inserts single element (test 1)" $
        count 1 (insert 1 empty) @?= 1
      it "inserts multiple same elements (test 2)" $
        count 2 (insert 2 (insert 2 empty)) @?= 2
      it "inserts different elements (test 3)" $ do
        let ms = insert 3 (insert 2 (insert 1 empty))
        count 1 ms @?= 1
      it "inserts and checks membership (test 4)" $
        member 5 (insert 5 empty) @?= True
      it "inserts strings (test 5)" $
        count "test" (insert "test" (insert "test" empty)) @?= 2
    
    describe "fromList" $ do
      it "creates from empty list (test 1)" $
        count 1 (fromList []) @?= 0
      it "creates from single element list (test 2)" $
        count 1 (fromList [1]) @?= 1
      it "creates from list with duplicates (test 3)" $ do
        let ms = fromList [1,2,1,3,1]
        count 1 ms @?= 3
      it "creates from list of strings (test 4)" $ do
        let ms = fromList ["a", "b", "a"]
        count "a" ms @?= 2
      it "creates from large list (test 5)" $ do
        let ms = fromList (replicate 10 'x')
        count 'x' ms @?= 10
    
    describe "toList" $ do
      it "converts empty multiset (test 1)" $
        toList (empty :: MultiSet Int) @?= []
      it "converts single element (test 2)" $
        length (toList (insert 1 empty)) @?= 1
      it "converts with duplicates (test 3)" $ do
        let ms = fromList [1,2,1,3]
        length (toList ms) @?= 4
      it "roundtrip property (test 4)" $ do
        let original = [1,2,1,3,2,1]
            ms = fromList original
        length (toList ms) @?= length original
      it "converts strings (test 5)" $ do
        let ms = fromList ["test", "test"]
        length (toList ms) @?= 2

  describe "Section 4: MatrixSum Semigroup" $ do
    
    describe "MatrixSum operations" $ do
      it "adds two matrices element-wise (test 1)" $ do
        let m1 = MatrixSum (Matrix [[1,2], [3,4]])
            m2 = MatrixSum (Matrix [[5,6], [7,8]])
            result = m1 <> m2
        getMS result @?= Matrix [[6,8], [10,12]]
      it "adds with zero matrix (test 2)" $ do
        let m1 = MatrixSum (Matrix [[1,2], [3,4]])
            m2 = MatrixSum (Matrix [[0,0], [0,0]])
            result = m1 <> m2
        getMS result @?= Matrix [[1,2], [3,4]]
      it "adds identity matrices (test 3)" $ do
        let m1 = MatrixSum identityMatrix
            m2 = MatrixSum identityMatrix
            result = m1 <> m2
        getMS result @?= Matrix [[2,0,0], [0,2,0], [0,0,2]]
      it "adds single element matrices (test 4)" $ do
        let m1 = MatrixSum (Matrix [[5]])
            m2 = MatrixSum (Matrix [[3]])
            result = m1 <> m2
        getMS result @?= Matrix [[8]]
      it "adds larger matrices (test 5)" $ do
        let m1 = MatrixSum sampleMatrix
            m2 = MatrixSum (Matrix [[1,1,1], [1,1,1], [1,1,1]])
            result = m1 <> m2
        getMS result @?= Matrix [[2,3,4], [5,6,7], [8,9,10]]
    
    describe "MatrixSum with Bool" $ do
      it "adds Bool matrices (XOR) (test 1)" $ do
        let m1 = MatrixSum (Matrix [[True, False], [False, True]])
            m2 = MatrixSum (Matrix [[True, True], [False, False]])
            result = m1 <> m2
        getMS result @?= Matrix [[False, True], [False, True]]
      it "adds Bool identity (test 2)" $ do
        let m1 = MatrixSum (Matrix [[True, False], [False, True]])
            m2 = MatrixSum (Matrix [[False, False], [False, False]])
            result = m1 <> m2
        getMS result @?= Matrix [[True, False], [False, True]]
      it "Bool matrix self-addition (test 3)" $ do
        let m1 = MatrixSum (Matrix [[True, True], [True, True]])
            result = m1 <> m1
        getMS result @?= Matrix [[False, False], [False, False]]
      it "Bool mixed addition (test 4)" $ do
        let m1 = MatrixSum (Matrix [[True, False, True]])
            m2 = MatrixSum (Matrix [[False, True, True]])
            result = m1 <> m2
        getMS result @?= Matrix [[True, True, False]]
      it "Bool single element (test 5)" $ do
        let m1 = MatrixSum (Matrix [[True]])
            m2 = MatrixSum (Matrix [[False]])
            result = m1 <> m2
        getMS result @?= Matrix [[True]]

  describe "Section 5: MatrixMult Semigroup" $ do
    
    describe "MatrixMult operations" $ do
      it "multiplies two 2x2 matrices (test 1)" $ do
        let m1 = MatrixMult (Matrix [[1,2], [3,4]])
            m2 = MatrixMult (Matrix [[5,6], [7,8]])
            result = m1 <> m2
        getMM result @?= Matrix [[19,22], [43,50]]
      it "multiplies with identity matrix (test 2)" $ do
        let m1 = MatrixMult sampleMatrix
            m2 = MatrixMult identityMatrix
            result = m1 <> m2
        getMM result @?= sampleMatrix
      it "multiplies rectangular matrices (test 3)" $ do
        let m1 = MatrixMult (Matrix [[1,2,3]])
            m2 = MatrixMult (Matrix [[4], [5], [6]])
            result = m1 <> m2
        getMM result @?= Matrix [[32]]
      it "multiplies with zero elements (test 4)" $ do
        let m1 = MatrixMult identityMatrix
            m2 = MatrixMult sampleMatrix
            result = m1 <> m2
        getMM result @?= sampleMatrix
      it "multiplies 1x1 matrices (test 5)" $ do
        let m1 = MatrixMult (Matrix [[3]])
            m2 = MatrixMult (Matrix [[7]])
            result = m1 <> m2
        getMM result @?= Matrix [[21]]
    
    describe "MatrixMult associativity" $ do
      it "matrix multiplication is associative (test 1)" $ do
        let m1 = MatrixMult (Matrix [[1,2], [3,4]])
            m2 = MatrixMult (Matrix [[5,6], [7,8]])
            m3 = MatrixMult (Matrix [[1,0], [0,1]])
            result1 = (m1 <> m2) <> m3
            result2 = m1 <> (m2 <> m3)
        getMM result1 @?= getMM result2
      it "three 1x1 matrix multiplication (test 2)" $ do
        let m1 = MatrixMult (Matrix [[2]])
            m2 = MatrixMult (Matrix [[3]])
            m3 = MatrixMult (Matrix [[4]])
            result1 = (m1 <> m2) <> m3
            result2 = m1 <> (m2 <> m3)
        getMM result1 @?= getMM result2
      it "identity matrix associativity (test 3)" $ do
        let m1 = MatrixMult identityMatrix
            m2 = MatrixMult sampleMatrix
            m3 = MatrixMult identityMatrix
            result1 = (m1 <> m2) <> m3
            result2 = m1 <> (m2 <> m3)
        getMM result1 @?= getMM result2
      it "chain multiplication (test 4)" $ do
        let m1 = MatrixMult (Matrix [[1,1], [0,1]])
            m2 = MatrixMult (Matrix [[1,0], [1,1]])
            m3 = MatrixMult (Matrix [[2,0], [0,2]])
            result1 = (m1 <> m2) <> m3
            result2 = m1 <> (m2 <> m3)
        getMM result1 @?= getMM result2
      it "square matrix chain (test 5)" $ do
        let m = MatrixMult (Matrix [[1,2], [3,4]])
            result1 = (m <> m) <> m
            result2 = m <> (m <> m)
        getMM result1 @?= getMM result2

  describe "Section 6: Num type class and instances" $ do
    
    describe "Bool Num instance" $ do
      it "True + True = False (test 1)" $
        (True + True) @?= False
      it "True + False = True (test 2)" $
        (True + False) @?= True
      it "False + True = True (test 3)" $
        (False + True) @?= True
      it "False + False = False (test 4)" $
        (False + False) @?= False
      it "True * False = False (test 5)" $
        (True * False) @?= False
    
    describe "Bool fromInteger" $ do
      it "odd numbers to True (test 1)" $
        (9 :: Bool) @?= True
      it "even numbers to False (test 2)" $
        (42 :: Bool) @?= False
      it "zero to False (test 3)" $
        (0 :: Bool) @?= False
      it "one to True (test 4)" $
        (1 :: Bool) @?= True
      it "negative odd to True (test 5)" $
        ((-7) :: Bool) @?= True
    
    describe "Expression Num instance" $ do
      it "Lit + Lit (test 1)" $
        (Lit 3 + Lit 5) @?= Plus (Lit 3) (Lit 5)
      it "Lit - Lit (test 2)" $
        (Lit 10 - Lit 4) @?= Minus (Lit 10) (Lit 4)
      it "Iden + Lit (test 2)" $
        (Iden "x" + Lit 0) @?= Plus (Iden "x") (Lit 0)
      it "multiplication (test 3)" $
        (Lit 2 * Iden "y") @?= Mult (Lit 2) (Iden "y")
      it "signum of Lit (test 5)" $
        (signum (Lit (-3))) @?= Signum (Lit (-3))
      it "absoulute value of Lit (test 6)" $
        (abs (Lit (-5))) @?= Lit 5
      it "fromInteger (test 4)" $
        (5 :: Expression Integer) @?= Lit 5
      it "complex expression (test 5)" $
        (Lit 1 + Lit 2 * Iden "x") @?= Plus (Lit 1) (Mult (Lit 2) (Iden "x"))

  describe "Section 7: General functions" $ do
    
    describe "evalPoly" $ do
      it "evaluates constant polynomial (test 1)" $
        evalPoly [5] 2 @?= 5
      it "evaluates linear polynomial (test 2)" $
        evalPoly [1, 2] 3 @?= 7
      it "evaluates quadratic polynomial (test 3)" $
        evalPoly [1, 2, 3] 2 @?= 17
      it "evaluates empty polynomial (test 4)" $
        evalPoly [] 5 @?= 0
      it "evaluates polynomial with zeros (test 5)" $
        evalPoly [1, 0, 0, 3] 2 @?= 25
    
    describe "evalPoly Bool" $ do
      it "evaluates Bool polynomial (test 1)" $
        evalPoly [True, False] True @?= True
      it "evaluates Bool constant (test 2)" $
        evalPoly [False] True @?= False
      it "evaluates Bool linear (test 3)" $
        evalPoly [True, True] False @?= True
      it "evaluates Bool complex (test 4)" $
        evalPoly [False, True, False] True @?= True
      it "evaluates Bool all false (test 5)" $
        evalPoly [False, False, False] True @?= False
    
    describe "pathsOfLengthK" $ do
      it "path of length 1 (test 0)" $
        pathsOfLengthK 1 1 2 adjMatrix @?= 1
      it "paths of length 1 direct connection (test 1)" $
        pathsOfLengthK 1 0 1 adjMatrix @?= 1
      it "paths of length 1 no connection (test 2)" $
        pathsOfLengthK 1 0 3 adjMatrix @?= 0
      it "paths of length 2 (test 3)" $
        pathsOfLengthK 2 0 2 adjMatrix @?= 1
      it "paths of length 2 multiple (test 4)" $
        pathsOfLengthK 2 0 3 adjMatrix @?= 2
      it "paths from disconnected node (test 5)" $
        pathsOfLengthK 1 3 0 adjMatrix @?= 0
      it "paths of length 2 (test 6)" $
        pathsOfLengthK 2 0 3 adjMatrix @?= 2
    
    describe "hasPath" $ do
      it "has direct path (test 1)" $
        hasPath 0 1 adjMatrix @?= True
      it "has indirect path (test 2)" $
        hasPath 0 3 adjMatrix @?= True
      it "no path exists (test 3)" $
        hasPath 3 0 adjMatrix @?= False
      it "same node (test 4)" $
        hasPath 1 1 adjMatrix @?= False
      it "complex path (test 5)" $
        hasPath 0 2 adjMatrix @?= True

  describe "Section 8: Expression Simplification" $ do
    
    describe "simplify Plus" $ do
      it "e + 0 = e (test 1)" $
        simplify (Plus (Iden "x") (Lit 0)) @?= Iden "x"
      it "0 + e = e (test 2)" $
        simplify (Plus (Lit 0) (Iden "x")) @?= Iden "x"
      it "literal addition (test 3)" $
        simplify (Plus (Lit 3) (Lit 5)) @?= Lit 8
      it "nested simplification (test 4)" $
        simplify (Plus (Plus (Lit 0) (Iden "x")) (Lit 0)) @?= Iden "x"
      it "complex case (test 5)" $
        simplify (Plus (Lit 2) (Plus (Lit 3) (Lit 0))) @?= Lit 5
    
    describe "simplify Expression - pdf tests" $ do
      it "(test 6)" $
        simplify (Plus (Lit 0) (Iden "x")) @?= Iden "x"
      it "(test 7)" $
        simplify (Minus ( Iden "y") ( Lit 0)) @?= Iden "y"
      it "(test 8)" $
        simplify (Mult ( Lit 1) ( Plus ( Lit 3) ( Iden "z"))) @?= Plus (Lit 3) (Iden "z")
      it "(test 9)" $
        simplify ( Div ( Lit 10) ( Lit 5) ) @?= Lit 2
      it "(test 10)" $
        simplify ( Div ( Lit 10) ( Lit 0) ) @?= Div (Lit 10) (Lit 0)
      it "(test 11)" $
        simplify ( Signum ( Lit ( -7) )) @?= Lit (-1)
      it "(test 12)" $
        simplify (Signum ( Mult ( Lit ( -3) ) ( Div ( Lit ( -5) ) ( Iden "w")))) @?=
          Mult ( Lit ( -1)) ( Div (Lit ( -1)) ( Signum ( Iden "w")))
    
    describe "simplify Minus" $ do
      it "e - 0 = e (test 1)" $
        simplify (Minus (Iden "y") (Lit 0)) @?= Iden "y"
      it "literal subtraction (test 2)" $
        simplify (Minus (Lit 10) (Lit 3)) @?= Lit 7
      it "zero minus literal (test 3)" $
        simplify (Minus (Lit 0) (Lit 5)) @?= Lit (-5)
      it "nested minus (test 4)" $
        simplify (Minus (Minus (Iden "x") (Lit 0)) (Lit 0)) @?= Iden "x"
      it "complex case (test 5)" $
        simplify (Minus (Lit 20) (Minus (Lit 5) (Lit 0))) @?= Lit 15
    
    describe "simplify Mult" $ do
      it "e * 1 = e (test 1)" $
        simplify (Mult (Iden "x") (Lit 1)) @?= Iden "x"
      it "1 * e = e (test 2)" $
        simplify (Mult (Lit 1) (Iden "x")) @?= Iden "x"
      it "literal multiplication (test 3)" $
        simplify (Mult (Lit 4) (Lit 6)) @?= Lit 24
      it "nested multiplication (test 4)" $
        simplify (Mult (Mult (Lit 1) (Iden "x")) (Lit 1)) @?= Iden "x"
      it "complex case (test 5)" $
        simplify (Mult (Lit 1) (Plus (Lit 3) (Iden "z"))) @?= Plus (Lit 3) (Iden "z")
    
    describe "simplify Div" $ do
      it "e / 1 = e (test 1)" $
        simplify (Div (Iden "x") (Lit 1)) @?= Iden "x"
      it "literal division (test 2)" $
        simplify (Div (Lit 10) (Lit 5)) @?= Lit 2
      it "division by zero unchanged (test 3)" $
        simplify (Div (Lit 10) (Lit 0)) @?= Div (Lit 10) (Lit 0)
      it "nested division (test 4)" $
        simplify (Div (Div (Iden "x") (Lit 1)) (Lit 1)) @?= Iden "x"
      it "complex case (test 5)" $
        simplify (Div (Mult (Lit 6) (Lit 1)) (Lit 3)) @?= Lit 2
    
    describe "simplify Signum" $ do
      it "signum of positive literal (test 1)" $
        simplify (Signum (Lit 5)) @?= Lit 1
      it "signum of negative literal (test 2)" $
        simplify (Signum (Lit (-7))) @?= Lit (-1)
      it "signum of zero (test 3)" $
        simplify (Signum (Lit 0)) @?= Lit 0
      it "signum of multiplication (test 4)" $ do
        let result = simplify (Signum (Mult (Lit (-3)) (Div (Lit (-5)) (Iden "w"))))
        result @?= Mult (Lit (-1)) (Div (Lit (-1)) (Signum (Iden "w")))
      it "signum of division (test 5)" $ do
        let result = simplify (Signum (Div (Lit 10) (Lit 2)))
        result @?= Lit 1

  describe "Section 9: Instances and Show tests" $ do
    
    describe "MultiSet Show" $ do
      it "shows empty multiset (test 1)" $
        show (empty :: MultiSet Int) @?= "{}"
      it "shows single element (test 2)" $
        show (insert 1 empty) @?= "{1}"
      it "shows multiple elements (test 3)" $ do
        let ms = fromList [1,2,1]
        show ms @?= "{1,1,2}"
      it "shows string elements (test 4)" $ do
        let ms = fromList ["a", "b"]
        show ms @?= "{\"a\",\"b\"}"
      it "shows duplicates (test 5)" $ do
        let ms = fromList [1,1,1]
        show ms @?= "{1,1,1}"
      it "combines overlapping elements (test 4)" $ do
        let ms1 = fromList [1,1,2]
            ms2 = fromList [1,3]
            result = ms1 <> ms2
        count 1 result @?= 3
        count 2 result @?= 1
        count 3 result @?= 1
        show result @?= "{1,1,1,2,3}"
    
    describe "MultiSet Eq" $ do
      it "empty sets equal (test 1)" $
        (empty :: MultiSet Int) @?= (empty :: MultiSet Int)
      it "same elements same counts (test 2)" $
        fromList [1,2,1] @?= fromList [2,1,1]
      it "different counts not equal (test 3)" $ do
        let result = (fromList [1,1] == fromList [1]) :: Bool
        result @?= False
      it "different elements not equal (test 4)" $ do
        let result = (fromList [1,2] == fromList [1,3]) :: Bool
        result @?= False
      it "order doesn't matter (test 5)" $
        fromList [3,1,2,1] @?= fromList [1,2,3,1]
    
    describe "MultiSet Semigroup" $ do
      it "combines empty sets (test 1)" $ do
        let result = (empty :: MultiSet Int) <> empty
        count 1 result @?= 0
      it "combines with empty (test 2)" $ do
        let ms = fromList [1,2]
            result = ms <> empty
        count 1 result @?= 1
      it "combines different elements (test 3)" $ do
        let ms1 = fromList [1,2]
            ms2 = fromList [3,4]
            result = ms1 <> ms2
        count 3 result @?= 1
      it "combines overlapping elements (test 4)" $ do
        let ms1 = fromList [1,1,2]
            ms2 = fromList [1,3]
            result = ms1 <> ms2
        count 1 result @?= 3
      it "combines complex case (test 5)" $ do
        let ms1 = fromList [1,2,1,3]
            ms2 = fromList [2,3,3]
            result = ms1 <> ms2
        count 3 result @?= 3
    
    describe "MultiSet Monoid" $ do
      it "mempty is empty (test 1)" $
        (mempty :: MultiSet Int) @?= empty
      it "left identity (test 2)" $ do
        let ms = fromList [1,2,3]
        (mempty <> ms) @?= ms
      it "right identity (test 3)" $ do
        let ms = fromList [1,2,3]
        (ms <> mempty) @?= ms
      it "associativity (test 4)" $ do
        let ms1 = fromList [1]
            ms2 = fromList [2]
            ms3 = fromList [3]
        ((ms1 <> ms2) <> ms3) @?= (ms1 <> (ms2 <> ms3))
      it "mempty with operations (test 5)" $ do
        let ms = insert 1 mempty
        count 1 ms @?= 1

  -- describe "Section 10: SparseMatrix Semigroup Operations" $ do

  --   describe "SparseMatrixSum operations" $ do
  --     it "adds sparse matrices element-wise (test 1)" $ do
  --       let sm1 = SparseMatrixSum sparseMatrix1
  --           sm2 = SparseMatrixSum sparseMatrix2
  --           result = sm1 <> sm2
  --       -- Check that non-zero entries are combined properly
  --       let SparseMatrix _ _ entries = getSMS result
  --       Map.lookup (0,0) entries @?= Just 1
  --       Map.lookup (0,1) entries @?= Just 2
  --       Map.lookup (1,0) entries @?= Just 4
  --       Map.lookup (1,1) entries @?= Just 5
      
  --     it "adds with empty sparse matrix (test 2)" $ do
  --       let sm1 = SparseMatrixSum sparseMatrix1
  --           sm2 = SparseMatrixSum emptySparseMatrix
  --           result = sm1 <> sm2
  --       getSMS result @?= sparseMatrix1
      
  --     it "adds same position entries (test 3)" $ do
  --       let sparse1 = SparseMatrix 2 2 (Map.fromList [((0,0),3), ((1,1),4)])
  --           sparse2 = SparseMatrix 2 2 (Map.fromList [((0,0),2), ((1,1),1)])
  --           sm1 = SparseMatrixSum sparse1
  --           sm2 = SparseMatrixSum sparse2
  --           result = sm1 <> sm2
  --       let SparseMatrix _ _ entries = getSMS result
  --       Map.lookup (0,0) entries @?= Just 5
  --       Map.lookup (1,1) entries @?= Just 5
      
  --     it "associativity property (test 4)" $ do
  --       let sm1 = SparseMatrixSum sparseMatrix1
  --           sm2 = SparseMatrixSum sparseMatrix2
  --           sm3 = SparseMatrixSum emptySparseMatrix
  --           result1 = (sm1 <> sm2) <> sm3
  --           result2 = sm1 <> (sm2 <> sm3)
  --       getSMS result1 @?= getSMS result2
      
  --     it "single element sparse matrices (test 5)" $ do
  --       let sparse1 = SparseMatrix 3 3 (Map.fromList [((1,1),10)])
  --           sparse2 = SparseMatrix 3 3 (Map.fromList [((1,1),5)])
  --           sm1 = SparseMatrixSum sparse1
  --           sm2 = SparseMatrixSum sparse2
  --           result = sm1 <> sm2
  --       let SparseMatrix _ _ entries = getSMS result
  --       Map.lookup (1,1) entries @?= Just 15
      
  --     it "filters out zero entries after addition (test 6)" $ do
  --       let sparse1 = SparseMatrix 3 3 (Map.fromList [((0,0),5), ((1,1),3), ((2,2),-7)])
  --           sparse2 = SparseMatrix 3 3 (Map.fromList [((0,0),-5), ((1,1),2), ((2,2),7)])
  --           sm1 = SparseMatrixSum sparse1
  --           sm2 = SparseMatrixSum sparse2
  --           result = sm1 <> sm2
  --       let SparseMatrix _ _ entries = getSMS result
  --       Map.lookup (0,0) entries @?= Nothing  -- 5 + (-5) = 0, should be filtered out
  --       Map.lookup (1,1) entries @?= Just 5   -- 3 + 2 = 5
  --       Map.lookup (2,2) entries @?= Nothing  -- (-7) + 7 = 0, should be filtered out
  --       Map.size entries @?= 1  -- Only one non-zero entry should remain
    
  --   describe "SparseMatrixMult operations" $ do
  --     it "multiplies sparse matrices (test 1)" $ do
  --       let diag1 = SparseMatrix 3 3 (Map.fromList [((0,0),2), ((1,1),3), ((2,2),4)])
  --           diag2 = SparseMatrix 3 3 (Map.fromList [((0,0),1), ((1,1),2), ((2,2),1)])
  --           sm1 = SparseMatrixMult diag1
  --           sm2 = SparseMatrixMult diag2
  --           result = sm1 <> sm2
  --       let SparseMatrix _ _ entries = getSMM result
  --       Map.lookup (0,0) entries @?= Just 2
  --       Map.lookup (1,1) entries @?= Just 6
  --       Map.lookup (2,2) entries @?= Just 4
      
  --     it "multiplies with identity-like sparse (test 2)" $ do
  --       let identity = SparseMatrix 3 3 (Map.fromList [((0,0),1), ((1,1),1), ((2,2),1)])
  --           sm1 = SparseMatrixMult sparseMatrix1
  --           sm2 = SparseMatrixMult identity
  --           result = sm1 <> sm2
  --       getSMM result @?= sparseMatrix1
      
  --     it "multiplies with empty sparse matrix (test 3)" $ do
  --       let sm1 = SparseMatrixMult sparseMatrix1
  --           sm2 = SparseMatrixMult emptySparseMatrix
  --           result = sm1 <> sm2
  --       let SparseMatrix _ _ entries = getSMM result
  --       Map.size entries @?= 0
      
  --     it "associativity property (test 4)" $ do
  --       let identity = SparseMatrix 2 2 (Map.fromList [((0,0),1), ((1,1),1)])
  --           sparse = SparseMatrix 2 2 (Map.fromList [((0,0),2), ((0,1),3)])
  --           sm1 = SparseMatrixMult identity
  --           sm2 = SparseMatrixMult sparse
  --           sm3 = SparseMatrixMult identity
  --           result1 = (sm1 <> sm2) <> sm3
  --           result2 = sm1 <> (sm2 <> sm3)
  --       getSMM result1 @?= getSMM result2
      
  --     it "single element multiplication (test 5)" $ do
  --       let sparse1 = SparseMatrix 1 1 (Map.fromList [((0,0),5)])
  --           sparse2 = SparseMatrix 1 1 (Map.fromList [((0,0),3)])
  --           sm1 = SparseMatrixMult sparse1
  --           sm2 = SparseMatrixMult sparse2
  --           result = sm1 <> sm2
  --       let SparseMatrix _ _ entries = getSMM result
  --       Map.lookup (0,0) entries @?= Just 15

  -- describe "Section 11: Jsonable roundtrip tests" $ do
    
  --   describe "Bool Jsonable roundtrip" $ do
  --     it "True roundtrip (test 1)" $
  --       fromJson (toJson True) @?= Just True
  --     it "False roundtrip (test 2)" $
  --       fromJson (toJson False) @?= Just False
    
  --   describe "Integer Jsonable roundtrip" $ do
  --     it "positive integer roundtrip (test 1)" $
  --       (fromJson (toJson (42 :: Integer)) :: Maybe Integer) @?= Just 42
  --     it "zero roundtrip (test 2)" $
  --       (fromJson (toJson (0 :: Integer)) :: Maybe Integer) @?= Just 0
  --     it "negative integer roundtrip (test 3)" $
  --       (fromJson (toJson ((-15) :: Integer)) :: Maybe Integer) @?= Just (-15)
    
  --   describe "List Jsonable roundtrip" $ do
  --     it "empty list roundtrip (test 1)" $
  --       (fromJson (toJson ([] :: [Integer])) :: Maybe [Integer]) @?= Just []
  --     it "single element list roundtrip (test 2)" $
  --       (fromJson (toJson [42 :: Integer]) :: Maybe [Integer]) @?= Just [42]
  --     it "multiple elements roundtrip (test 3)" $
  --       (fromJson (toJson [1,2,3 :: Integer]) :: Maybe [Integer]) @?= Just [1,2,3]
    
  --   describe "Maybe Jsonable roundtrip" $ do
  --     it "Nothing roundtrip (test 1)" $
  --       (fromJson (toJson (Nothing :: Maybe Integer)) :: Maybe (Maybe Integer)) @?= Just Nothing
  --     it "Just value roundtrip (test 2)" $
  --       (fromJson (toJson (Just 42 :: Maybe Integer)) :: Maybe (Maybe Integer)) @?= Just (Just 42)
    
  --   describe "MultiSet Jsonable roundtrip" $ do
  --     it "empty multiset roundtrip (test 1)" $
  --       (fromJson (toJson (empty :: MultiSet Integer)) :: Maybe (MultiSet Integer)) @?= Just empty
  --     it "single element multiset roundtrip (test 2)" $
  --       (fromJson (toJson (insert 1 empty)) :: Maybe (MultiSet Integer)) @?= Just (insert 1 empty)
  --     it "multiset with duplicates roundtrip (test 3)" $
  --       (fromJson (toJson (fromList [1,1,2])) :: Maybe (MultiSet Integer)) @?= Just (fromList [1,1,2])
    
  --   describe "Matrix Jsonable roundtrip" $ do
  --     it "empty matrix roundtrip (test 1)" $
  --       (fromJson (toJson (Matrix ([] :: [[Integer]]))) :: Maybe (Matrix Integer)) @?= Just (Matrix [])
  --     it "single element matrix roundtrip (test 2)" $
  --       (fromJson (toJson (Matrix [[42]])) :: Maybe (Matrix Integer)) @?= Just (Matrix [[42]])
  --     it "2x2 matrix roundtrip (test 3)" $
  --       (fromJson (toJson (Matrix [[1,2], [3,4]])) :: Maybe (Matrix Integer)) @?= Just (Matrix [[1,2], [3,4]])
    
  --   describe "SparseMatrix Jsonable roundtrip" $ do
  --     it "empty sparse matrix roundtrip (test 1)" $ do
  --       let emptySparse = SparseMatrix 3 3 (Map.empty :: Map.Map (Integer, Integer) Integer)
  --       (fromJson (toJson emptySparse) :: Maybe (SparseMatrix Integer)) @?= Just emptySparse
  --     it "single entry sparse matrix roundtrip (test 2)" $ do
  --       let sparse1 = SparseMatrix 3 3 (Map.fromList [((0,0),1), ((1,1),5), ((2,2),9)] :: Map.Map (Integer, Integer) Integer)
  --       (fromJson (toJson sparse1) :: Maybe (SparseMatrix Integer)) @?= Just sparse1
  --     it "multiple entries sparse matrix roundtrip (test 3)" $ do
  --       let sparse2 = SparseMatrix 3 3 (Map.fromList [((0,1),2), ((1,0),4), ((2,1),8)] :: Map.Map (Integer, Integer) Integer)
  --       (fromJson (toJson sparse2) :: Maybe (SparseMatrix Integer)) @?= Just sparse2
    
  --   describe "Tree Jsonable roundtrip" $ do
  --     it "empty tree roundtrip (test 1)" $
  --       (fromJson (toJson (Empty :: Tree Integer)) :: Maybe (Tree Integer)) @?= Just Empty
  --     it "single node tree roundtrip (test 2)" $
  --       (fromJson (toJson (Tree Empty 42 Empty)) :: Maybe (Tree Integer)) @?= Just (Tree Empty 42 Empty)
  --     it "complex tree roundtrip (test 3)" $ do
  --       let tree = Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty)
  --       (fromJson (toJson tree) :: Maybe (Tree Integer)) @?= Just tree
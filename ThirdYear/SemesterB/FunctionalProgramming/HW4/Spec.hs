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

sparseMatrix1 :: SparseMatrix Int
sparseMatrix1 = SparseMatrix 3 3 (Map.fromList [((0,0),1), ((1,1),5), ((2,2),9)])

sparseMatrix2 :: SparseMatrix Int
sparseMatrix2 = SparseMatrix 3 3 (Map.fromList [((0,1),2), ((1,0),4), ((2,1),8)])

adjMatrix :: Matrix Int
adjMatrix = Matrix [[0,1,1,0], [0,0,1,1], [0,0,0,1], [0,0,0,0]]

-- Main spec
main :: IO ()
main = hspec $ do
  
  describe "Section 1: MultiSet Implementation" $ do
    
    describe "empty" $ do
      it "creates an empty multiset (test 1)" $
        count 1 empty `shouldBe` 0
      it "creates an empty multiset (test 2)" $
        count "hello" empty `shouldBe` 0
      it "creates an empty multiset (test 3)" $
        member 'a' empty `shouldBe` False
      it "creates an empty multiset (test 4)" $
        toList (empty :: MultiSet Int) `shouldBe` []
      it "creates an empty multiset (test 5)" $
        show (empty :: MultiSet String) `shouldBe` "{}"
    
    describe "member" $ do
      it "checks non-existent element (test 1)" $
        member 1 empty `shouldBe` False
      it "checks existing element (test 2)" $
        member 1 (insert 1 empty) `shouldBe` True
      it "checks after multiple inserts (test 3)" $
        let ms = insert 2 (insert 1 (insert 2 empty))
        in member 2 ms `shouldBe` True
      it "checks non-member in non-empty set (test 4)" $
        let ms = insert 1 (insert 2 empty)
        in member 3 ms `shouldBe` False
      it "checks with characters (test 5)" $
        member 'x' (insert 'x' (insert 'y' empty)) `shouldBe` True
    
    describe "count" $ do
      it "counts zero for empty set (test 1)" $
        count 1 empty `shouldBe` 0
      it "counts single occurrence (test 2)" $
        count 1 (insert 1 empty) `shouldBe` 1
      it "counts multiple occurrences (test 3)" $
        let ms = insert 1 (insert 1 (insert 1 empty))
        in count 1 ms `shouldBe` 3
      it "counts different elements (test 4)" $
        let ms = insert 2 (insert 1 (insert 2 empty))
        in count 2 ms `shouldBe` 2
      it "counts with mixed elements (test 5)" $
        let ms = fromList [1,2,1,3,1,2]
        in count 1 ms `shouldBe` 3
    
    describe "remove" $ do
      it "removes from empty set (test 1)" $
        count 1 (remove 1 empty) `shouldBe` 0
      it "removes single occurrence (test 2)" $
        let ms = insert 1 empty
        in count 1 (remove 1 ms) `shouldBe` 0
      it "removes one of multiple occurrences (test 3)" $
        let ms = insert 1 (insert 1 empty)
        in count 1 (remove 1 ms) `shouldBe` 1
      it "removes non-existent element (test 4)" $
        let ms = insert 2 empty
        in count 2 (remove 1 ms) `shouldBe` 1
      it "removes until empty (test 5)" $
        let ms = insert 1 (insert 1 empty)
        let ms' = remove 1 (remove 1 ms)
        in count 1 ms' `shouldBe` 0
    
    describe "insert" $ do
      it "inserts single element (test 1)" $
        count 1 (insert 1 empty) `shouldBe` 1
      it "inserts multiple same elements (test 2)" $
        count 2 (insert 2 (insert 2 empty)) `shouldBe` 2
      it "inserts different elements (test 3)" $
        let ms = insert 3 (insert 2 (insert 1 empty))
        in count 1 ms `shouldBe` 1
      it "inserts and checks membership (test 4)" $
        member 5 (insert 5 empty) `shouldBe` True
      it "inserts strings (test 5)" $
        count "test" (insert "test" (insert "test" empty)) `shouldBe` 2
    
    describe "fromList" $ do
      it "creates from empty list (test 1)" $
        count 1 (fromList []) `shouldBe` 0
      it "creates from single element list (test 2)" $
        count 1 (fromList [1]) `shouldBe` 1
      it "creates from list with duplicates (test 3)" $
        let ms = fromList [1,2,1,3,1]
        in count 1 ms `shouldBe` 3
      it "creates from list of strings (test 4)" $
        let ms = fromList ["a", "b", "a"]
        in count "a" ms `shouldBe` 2
      it "creates from large list (test 5)" $
        let ms = fromList (replicate 10 'x')
        in count 'x' ms `shouldBe` 10
    
    describe "toList" $ do
      it "converts empty multiset (test 1)" $
        toList (empty :: MultiSet Int) `shouldBe` []
      it "converts single element (test 2)" $
        length (toList (insert 1 empty)) `shouldBe` 1
      it "converts with duplicates (test 3)" $
        let ms = fromList [1,2,1,3]
        in length (toList ms) `shouldBe` 4
      it "roundtrip property (test 4)" $
        let original = [1,2,1,3,2,1]
        let ms = fromList original
        in length (toList ms) `shouldBe` length original
      it "converts strings (test 5)" $
        let ms = fromList ["test", "test"]
        in length (toList ms) `shouldBe` 2

  describe "Section 2: Jsonable type class and instances" $ do
    
    describe "Bool Jsonable" $ do
      it "toJson/fromJson Bool True (test 1)" $
        fromJson (toJson True) `shouldBe` Just True
      it "toJson/fromJson Bool False (test 2)" $
        fromJson (toJson False) `shouldBe` Just False
      it "fromJson JsonBool True (test 3)" $
        fromJson (JsonBool True) `shouldBe` Just True
      it "fromJson JsonBool False (test 4)" $
        fromJson (JsonBool False) `shouldBe` Just False
      it "fromJson invalid type returns Nothing (test 5)" $
        (fromJson (JsonInt 1) :: Maybe Bool) `shouldBe` Nothing
    
    describe "JString Jsonable" $ do
      it "toJson/fromJson JString (test 1)" $
        fromJson (toJson (JString "hello")) `shouldBe` Just (JString "hello")
      it "toJson/fromJson empty JString (test 2)" $
        fromJson (toJson (JString "")) `shouldBe` Just (JString "")
      it "toJson/fromJson special chars (test 3)" $
        fromJson (toJson (JString "hello\nworld")) `shouldBe` Just (JString "hello\nworld")
      it "fromJson JsonString (test 4)" $
        fromJson (JsonString (JString "test")) `shouldBe` Just (JString "test")
      it "fromJson invalid type returns Nothing (test 5)" $
        (fromJson (JsonInt 1) :: Maybe JString) `shouldBe` Nothing
    
    describe "Int Jsonable" $ do
      it "toJson/fromJson Int positive (test 1)" $
        fromJson (toJson (42 :: Int)) `shouldBe` Just (42 :: Int)
      it "toJson/fromJson Int zero (test 2)" $
        fromJson (toJson (0 :: Int)) `shouldBe` Just (0 :: Int)
      it "toJson/fromJson Int negative (test 3)" $
        fromJson (toJson ((-17) :: Int)) `shouldBe` Just ((-17) :: Int)
      it "fromJson JsonInt (test 4)" $
        fromJson (JsonInt 100) `shouldBe` Just (100 :: Int)
      it "fromJson invalid type returns Nothing (test 5)" $
        (fromJson (JsonBool True) :: Maybe Int) `shouldBe` Nothing
    
    describe "Tuple Jsonable" $ do
      it "toJson/fromJson (Int, Bool) (test 1)" $
        fromJson (toJson (1 :: Int, True)) `shouldBe` Just (1 :: Int, True)
      it "toJson/fromJson (String, String) (test 2)" $
        fromJson (toJson (JString "a", JString "b")) `shouldBe` Just (JString "a", JString "b")
      it "toJson/fromJson nested tuples (test 3)" $
        fromJson (toJson ((1 :: Int, 2 :: Int), (3 :: Int, 4 :: Int))) `shouldBe` Just ((1 :: Int, 2 :: Int), (3 :: Int, 4 :: Int))
      it "fromJson JsonArray of 2 elements (test 4)" $
        fromJson (JsonArray [JsonInt 1, JsonBool True]) `shouldBe` Just (1 :: Int, True)
      it "fromJson invalid array length returns Nothing (test 5)" $
        (fromJson (JsonArray [JsonInt 1]) :: Maybe (Int, Bool)) `shouldBe` Nothing
    
    describe "Maybe Jsonable" $ do
      it "toJson/fromJson Maybe Nothing (test 1)" $
        fromJson (toJson (Nothing :: Maybe Int)) `shouldBe` Just (Nothing :: Maybe Int)
      it "toJson/fromJson Maybe Just (test 2)" $
        fromJson (toJson (Just (42 :: Int))) `shouldBe` Just (Just (42 :: Int))
      it "toJson/fromJson Maybe complex type (test 3)" $
        fromJson (toJson (Just (True, False))) `shouldBe` Just (Just (True, False))
      it "fromJson JsonNull (test 4)" $
        fromJson JsonNull `shouldBe` Just (Nothing :: Maybe Int)
      it "fromJson JsonArray with Just (test 5)" $
        fromJson (JsonArray [JsonInt 5]) `shouldBe` Just (Just (5 :: Int))

  describe "Section 3: Num type class and instances" $ do
    
    describe "Bool Num instance" $ do
      it "True + True = False (test 1)" $
        (True + True) `shouldBe` False
      it "True + False = True (test 2)" $
        (True + False) `shouldBe` True
      it "False + True = True (test 3)" $
        (False + True) `shouldBe` True
      it "False + False = False (test 4)" $
        (False + False) `shouldBe` False
      it "True * False = False (test 5)" $
        (True * False) `shouldBe` False
    
    describe "Bool fromInteger" $ do
      it "odd numbers to True (test 1)" $
        (9 :: Bool) `shouldBe` True
      it "even numbers to False (test 2)" $
        (42 :: Bool) `shouldBe` False
      it "zero to False (test 3)" $
        (0 :: Bool) `shouldBe` False
      it "one to True (test 4)" $
        (1 :: Bool) `shouldBe` True
      it "negative odd to True (test 5)" $
        ((-7) :: Bool) `shouldBe` True
    
    describe "Expression Num instance" $ do
      it "Lit + Lit (test 1)" $
        (Lit 3 + Lit 5) `shouldBe` Plus (Lit 3) (Lit 5)
      it "Iden + Lit (test 2)" $
        (Iden "x" + Lit 0) `shouldBe` Plus (Iden "x") (Lit 0)
      it "multiplication (test 3)" $
        (Lit 2 * Iden "y") `shouldBe` Mult (Lit 2) (Iden "y")
      it "fromInteger (test 4)" $
        (5 :: Expression Integer) `shouldBe` Lit 5
      it "complex expression (test 5)" $
        (Lit 1 + Lit 2 * Iden "x") `shouldBe` Plus (Lit 1) (Mult (Lit 2) (Iden "x"))
    
    describe "MatrixSum Semigroup" $ do
      it "adds two matrices element-wise (test 1)" $
        let m1 = MatrixSum (Matrix [[1,2], [3,4]])
        let m2 = MatrixSum (Matrix [[5,6], [7,8]])
        let result = m1 <> m2
        in getMS result `shouldBe` Matrix [[6,8], [10,12]]
      it "adds with zero matrix (test 2)" $
        let m1 = MatrixSum (Matrix [[1,2], [3,4]])
        let m2 = MatrixSum (Matrix [[0,0], [0,0]])
        let result = m1 <> m2
        in getMS result `shouldBe` Matrix [[1,2], [3,4]]
      it "adds Bool matrices (test 3)" $
        let m1 = MatrixSum (Matrix [[True, False], [False, True]])
        let m2 = MatrixSum (Matrix [[True, True], [False, False]])
        let result = m1 <> m2
        in getMS result `shouldBe` Matrix [[False, True], [False, True]]
      it "adds single element matrices (test 4)" $
        let m1 = MatrixSum (Matrix [[5]])
        let m2 = MatrixSum (Matrix [[3]])
        let result = m1 <> m2
        in getMS result `shouldBe` Matrix [[8]]
      it "adds larger matrices (test 5)" $
        let m1 = MatrixSum (Matrix [[1,2,3], [4,5,6]])
        let m2 = MatrixSum (Matrix [[1,1,1], [1,1,1]])
        let result = m1 <> m2
        in getMS result `shouldBe` Matrix [[2,3,4], [5,6,7]]
    
    describe "MatrixMult Semigroup" $ do
      it "multiplies two 2x2 matrices (test 1)" $
        let m1 = MatrixMult (Matrix [[1,2], [3,4]])
        let m2 = MatrixMult (Matrix [[5,6], [7,8]])
        let result = m1 <> m2
        in getMM result `shouldBe` Matrix [[19,22], [43,50]]
      it "multiplies with identity matrix (test 2)" $
        let m1 = MatrixMult (Matrix [[1,2], [3,4]])
        let m2 = MatrixMult (Matrix [[1,0], [0,1]])
        let result = m1 <> m2
        in getMM result `shouldBe` Matrix [[1,2], [3,4]]
      it "multiplies rectangular matrices (test 3)" $
        let m1 = MatrixMult (Matrix [[1,2,3]])
        let m2 = MatrixMult (Matrix [[4], [5], [6]])
        let result = m1 <> m2
        in getMM result `shouldBe` Matrix [[32]]
      it "multiplies with zero elements (test 4)" $
        let m1 = MatrixMult (Matrix [[1,0], [0,1]])
        let m2 = MatrixMult (Matrix [[2,3], [4,5]])
        let result = m1 <> m2
        in getMM result `shouldBe` Matrix [[2,3], [4,5]]
      it "multiplies 3x3 matrices (test 5)" $
        let m1 = MatrixMult (Matrix [[1,0,0], [0,1,0], [0,0,1]])
        let m2 = MatrixMult (Matrix [[1,2,3], [4,5,6], [7,8,9]])
        let result = m1 <> m2
        in getMM result `shouldBe` Matrix [[1,2,3], [4,5,6], [7,8,9]]

  describe "Section 4: General functions" $ do
    
    describe "evalPoly" $ do
      it "evaluates constant polynomial (test 1)" $
        evalPoly [5] 2 `shouldBe` 5
      it "evaluates linear polynomial (test 2)" $
        evalPoly [1, 2] 3 `shouldBe` 7
      it "evaluates quadratic polynomial (test 3)" $
        evalPoly [1, 2, 3] 2 `shouldBe` 17
      it "evaluates empty polynomial (test 4)" $
        evalPoly [] 5 `shouldBe` 0
      it "evaluates polynomial with zeros (test 5)" $
        evalPoly [1, 0, 0, 3] 2 `shouldBe` 25
    
    describe "evalPoly Bool" $ do
      it "evaluates Bool polynomial (test 1)" $
        evalPoly [True, False] True `shouldBe` True
      it "evaluates Bool constant (test 2)" $
        evalPoly [False] True `shouldBe` False
      it "evaluates Bool linear (test 3)" $
        evalPoly [True, True] False `shouldBe` True
      it "evaluates Bool complex (test 4)" $
        evalPoly [False, True, False] True `shouldBe` True
      it "evaluates Bool all false (test 5)" $
        evalPoly [False, False, False] True `shouldBe` False
    
    describe "pathsOfLengthK" $ do
      it "paths of length 1 direct connection (test 1)" $
        pathsOfLengthK 1 0 1 adjMatrix `shouldBe` 1
      it "paths of length 1 no connection (test 2)" $
        pathsOfLengthK 1 0 3 adjMatrix `shouldBe` 0
      it "paths of length 2 (test 3)" $
        pathsOfLengthK 2 0 2 adjMatrix `shouldBe` 1
      it "paths of length 2 multiple (test 4)" $
        pathsOfLengthK 2 0 3 adjMatrix `shouldBe` 2
      it "paths from disconnected node (test 5)" $
        pathsOfLengthK 1 3 0 adjMatrix `shouldBe` 0
    
    describe "hasPath" $ do
      it "has direct path (test 1)" $
        hasPath 0 1 adjMatrix `shouldBe` True
      it "has indirect path (test 2)" $
        hasPath 0 3 adjMatrix `shouldBe` True
      it "no path exists (test 3)" $
        hasPath 3 0 adjMatrix `shouldBe` False
      it "same node (test 4)" $
        hasPath 1 1 adjMatrix `shouldBe` False
      it "complex path (test 5)" $
        hasPath 0 2 adjMatrix `shouldBe` True

  describe "Section 5: Expression Simplification" $ do
    
    describe "simplify Plus" $ do
      it "e + 0 = e (test 1)" $
        simplify (Plus (Iden "x") (Lit 0)) `shouldBe` Iden "x"
      it "0 + e = e (test 2)" $
        simplify (Plus (Lit 0) (Iden "x")) `shouldBe` Iden "x"
      it "literal addition (test 3)" $
        simplify (Plus (Lit 3) (Lit 5)) `shouldBe` Lit 8
      it "nested simplification (test 4)" $
        simplify (Plus (Plus (Lit 0) (Iden "x")) (Lit 0)) `shouldBe` Iden "x"
      it "complex case (test 5)" $
        simplify (Plus (Lit 2) (Plus (Lit 3) (Lit 0))) `shouldBe` Lit 5
    
    describe "simplify Minus" $ do
      it "e - 0 = e (test 1)" $
        simplify (Minus (Iden "y") (Lit 0)) `shouldBe` Iden "y"
      it "literal subtraction (test 2)" $
        simplify (Minus (Lit 10) (Lit 3)) `shouldBe` Lit 7
      it "zero minus literal (test 3)" $
        simplify (Minus (Lit 0) (Lit 5)) `shouldBe` Lit (-5)
      it "nested minus (test 4)" $
        simplify (Minus (Minus (Iden "x") (Lit 0)) (Lit 0)) `shouldBe` Iden "x"
      it "complex case (test 5)" $
        simplify (Minus (Lit 20) (Minus (Lit 5) (Lit 0))) `shouldBe` Lit 15
    
    describe "simplify Mult" $ do
      it "e * 1 = e (test 1)" $
        simplify (Mult (Iden "x") (Lit 1)) `shouldBe` Iden "x"
      it "1 * e = e (test 2)" $
        simplify (Mult (Lit 1) (Iden "x")) `shouldBe` Iden "x"
      it "literal multiplication (test 3)" $
        simplify (Mult (Lit 4) (Lit 6)) `shouldBe` Lit 24
      it "nested multiplication (test 4)" $
        simplify (Mult (Mult (Lit 1) (Iden "x")) (Lit 1)) `shouldBe` Iden "x"
      it "complex case (test 5)" $
        simplify (Mult (Lit 1) (Plus (Lit 3) (Iden "z"))) `shouldBe` Plus (Lit 3) (Iden "z")
    
    describe "simplify Div" $ do
      it "e / 1 = e (test 1)" $
        simplify (Div (Iden "x") (Lit 1)) `shouldBe` Iden "x"
      it "literal division (test 2)" $
        simplify (Div (Lit 10) (Lit 5)) `shouldBe` Lit 2
      it "division by zero unchanged (test 3)" $
        simplify (Div (Lit 10) (Lit 0)) `shouldBe` Div (Lit 10) (Lit 0)
      it "nested division (test 4)" $
        simplify (Div (Div (Iden "x") (Lit 1)) (Lit 1)) `shouldBe` Iden "x"
      it "complex case (test 5)" $
        simplify (Div (Mult (Lit 6) (Lit 1)) (Lit 3)) `shouldBe` Lit 2
    
    describe "simplify Signum" $ do
      it "signum of positive literal (test 1)" $
        simplify (Signum (Lit 5)) `shouldBe` Lit 1
      it "signum of negative literal (test 2)" $
        simplify (Signum (Lit (-7))) `shouldBe` Lit (-1)
      it "signum of zero (test 3)" $
        simplify (Signum (Lit 0)) `shouldBe` Lit 0
      it "signum of multiplication (test 4)" $
        let result = simplify (Signum (Mult (Lit (-3)) (Div (Lit (-5)) (Iden "w"))))
        in result `shouldBe` Mult (Lit (-1)) (Div (Lit (-1)) (Signum (Iden "w")))
      it "signum of division (test 5)" $
        let result = simplify (Signum (Div (Lit 10) (Lit 2)))
        in result `shouldBe` Lit 1

  describe "Instances and Show tests" $ do
    
    describe "MultiSet Show" $ do
      it "shows empty multiset (test 1)" $
        show (empty :: MultiSet Int) `shouldBe` "{}"
      it "shows single element (test 2)" $
        show (insert 1 empty) `shouldBe` "{1}"
      it "shows multiple elements (test 3)" $
        let ms = fromList [1,2,1]
        in length (show ms) > 0 `shouldBe` True
      it "shows string elements (test 4)" $
        let ms = fromList ["a", "b"]
        in length (show ms) > 0 `shouldBe` True
      it "shows duplicates (test 5)" $
        let ms = fromList [1,1,1]
        in length (show ms) > 0 `shouldBe` True
    
    describe "MultiSet Eq" $ do
      it "empty sets equal (test 1)" $
        (empty :: MultiSet Int) `shouldBe` (empty :: MultiSet Int)
      it "same elements same counts (test 2)" $
        fromList [1,2,1] `shouldBe` fromList [2,1,1]
      it "different counts not equal (test 3)" $
        fromList [1,1] `shouldNotBe` fromList [1]
      it "different elements not equal (test 4)" $
        fromList [1,2] `shouldNotBe` fromList [1,3]
      it "order doesn't matter (test 5)" $
        fromList [3,1,2,1] `shouldBe` fromList [1,2,3,1]
    
    describe "MultiSet Semigroup" $ do
      it "combines empty sets (test 1)" $
        let result = (empty :: MultiSet Int) <> empty
        in count 1 result `shouldBe` 0
      it "combines with empty (test 2)" $
        let ms = fromList [1,2]
        let result = ms <> empty
        in count 1 result `shouldBe` 1
      it "combines different elements (test 3)" $
        let ms1 = fromList [1,2]
        let ms2 = fromList [3,4]
        let result = ms1 <> ms2
        in count 3 result `shouldBe` 1
      it "combines overlapping elements (test 4)" $
        let ms1 = fromList [1,1,2]
        let ms2 = fromList [1,3]
        let result = ms1 <> ms2
        in count 1 result `shouldBe` 3
      it "combines complex case (test 5)" $
        let ms1 = fromList [1,2,1,3]
        let ms2 = fromList [2,3,3]
        let result = ms1 <> ms2
        in count 3 result `shouldBe` 3
    
    describe "MultiSet Monoid" $ do
      it "mempty is empty (test 1)" $
        (mempty :: MultiSet Int) `shouldBe` empty
      it "left identity (test 2)" $
        let ms = fromList [1,2,3]
        in mempty <> ms `shouldBe` ms
      it "right identity (test 3)" $
        let ms = fromList [1,2,3]
        in ms <> mempty `shouldBe` ms
      it "associativity (test 4)" $
        let ms1 = fromList [1]
        let ms2 = fromList [2]
        let ms3 = fromList [3]
        in (ms1 <> ms2) <> ms3 `shouldBe` ms1 <> (ms2 <> ms3)
      it "mempty with operations (test 5)" $
        let ms = insert 1 mempty
        in count 1 ms `shouldBe` 1

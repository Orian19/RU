{-# LANGUAGE GHC2024 #-}
import Control.Exception (catch, SomeException)
import qualified Data.Ratio as Ratio
import Test.Hspec
import HW5
import MultiSet
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Monoid (Sum(..), Product(..), All(..), Any(..), First(..), Last(..))
import Data.Semigroup (Max(..), Min(..))
import Prelude (Eq, Show, IO, Int, Char, String, Foldable, Double, Either(..), Bool(..), Maybe(..), error, take, pure, ($), (++), show, (==), (+), (-), (*), (/), (<), (>), (<=), (>=), abs, negate, even, id, const, map, replicate, (<>), length, isNaN, foldr, max, fromIntegral, mempty)

-- Tree data type for testing
data Tree a = Empty | Tree (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Helper function to create a single-node tree
single :: a -> Tree a
single a = Tree Empty a Empty

-- Foldable instance for Tree (in-order traversal)
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Empty = z
  foldr f z (Tree left x right) = Prelude.foldr f (f x (Prelude.foldr f z right)) left

-- Utility
(@?=) :: (Eq a, Show a) => a -> a -> IO ()
x @?= y = if x == y then pure () else error ("Expected: " ++ show y ++ ", but got: " ++ show x)

-- Sample data for testing
sampleMultiSet :: MultiSet Int
sampleMultiSet = fromList [1, 2, 2, 3, 3, 3]

emptyMultiSet :: MultiSet Int
emptyMultiSet = empty

singleElementMultiSet :: MultiSet Int
singleElementMultiSet = fromList [42]

charMultiSet :: MultiSet Char
charMultiSet = fromList ['a', 'b', 'a', 'c', 'a']

stringMultiSet :: MultiSet String
stringMultiSet = fromList ["foo", "bar", "foo", "baz"]

-- Test data for folds
testList :: [Int]
testList = [1, 2, 3, 4, 5]

emptyList :: [Int]
emptyList = []

singleList :: [Int]
singleList = [42]

negativeList :: [Int]
negativeList = [-1, -2, -3]

mixedList :: [Int]
mixedList = [-2, 0, 1, 3, -1]

-- Main spec
main :: IO ()
main = hspec $ do
  
  describe "Section 1: Foldable Functions" $ do
    
    describe "fold" $ do
      it "folds empty list with Sum monoid (test 1)" $
        getSum (HW5.fold (map Sum emptyList)) @?= 0
      it "folds list with Sum monoid (test 2)" $
        getSum (HW5.fold (map Sum [1, 2, 3])) @?= 6
      it "folds list with Product monoid (test 3)" $
        getProduct (HW5.fold (map Product [2, 3, 4])) @?= 24
      it "folds list with All monoid (test 4)" $
        getAll (HW5.fold (map All [True, True, False])) @?= False
      it "folds list with Any monoid (test 5)" $
        getAny (HW5.fold (map Any [False, False, True])) @?= True

    describe "toList" $ do
      it "converts Nothing to empty list (test 1)" $
        HW5.toList Nothing @?= ([] :: [Int])
      it "converts Just to singleton list (test 2)" $
        HW5.toList (Just 4) @?= [4]
      it "converts regular list (test 3)" $
        HW5.toList [1, 2, 3] @?= [1, 2, 3]
      it "converts empty list (test 4)" $
        HW5.toList ([] :: [Int]) @?= []
      it "converts Either Right (test 5)" $
        HW5.toList (Right 42 :: Either String Int) @?= [42]
      it "converts Tree to list (test 6)" $
        HW5.toList (Tree (single 1) 2 (single 3)) @?= [1, 2, 3]

    describe "elem" $ do
      it "checks element in empty list (test 1)" $
        HW5.elem 1 emptyList @?= False
      it "checks existing element (test 2)" $
        HW5.elem 3 testList @?= True
      it "checks non-existing element (test 3)" $
        HW5.elem 10 testList @?= False
      it "checks element in Maybe Nothing (test 4)" $
        HW5.elem 1 Nothing @?= False
      it "checks element in Maybe Just (test 5)" $
        HW5.elem 42 (Just 42) @?= True

    describe "find" $ do
      it "finds in empty list (test 1)" $
        HW5.find (> 0) emptyList @?= Nothing
      it "finds existing element (test 2)" $
        HW5.find (> 3) testList @?= Just 4
      it "finds non-existing element (test 3)" $
        HW5.find (> 10) testList @?= Nothing
      it "finds in single element list (test 4)" $
        HW5.find (== 42) singleList @?= Just 42
      it "finds first matching element (test 5)" $
        HW5.find even [1, 3, 4, 6, 8] @?= Just 4

    describe "length" $ do
      it "length of empty list (test 1)" $
        HW5.length emptyList @?= 0
      it "length of regular list (test 2)" $
        HW5.length testList @?= 5
      it "length of single element list (test 3)" $
        HW5.length singleList @?= 1
      it "length of Maybe Nothing (test 4)" $
        HW5.length Nothing @?= 0
      it "length of Maybe Just (test 5)" $
        HW5.length (Just 1) @?= 1

    describe "null" $ do
      it "null check on empty list (test 1)" $
        HW5.null emptyList @?= True
      it "null check on non-empty list (test 2)" $
        HW5.null testList @?= False
      it "null check on single element (test 3)" $
        HW5.null singleList @?= False
      it "null check on Maybe Nothing (test 4)" $
        HW5.null Nothing @?= True
      it "null check on Maybe Just (test 5)" $
        HW5.null (Just 1) @?= False

    describe "maximum" $ do
      it "maximum of empty list (test 1)" $
        HW5.maximum emptyList @?= Nothing
      it "maximum of regular list (test 2)" $
        HW5.maximum testList @?= Just 5
      it "maximum of single element (test 3)" $
        HW5.maximum singleList @?= Just 42
      it "maximum of negative numbers (test 4)" $
        HW5.maximum negativeList @?= Just (-1)
      it "maximum of mixed numbers (test 5)" $
        HW5.maximum mixedList @?= Just 3

    describe "maxBy" $ do
      it "maxBy on empty list (test 1)" $
        maxBy Prelude.length ([] :: [String]) @?= Nothing
      it "maxBy length on strings (test 2)" $
        maxBy Prelude.length ["foo", "bar", "bazz"] @?= Just "bazz"
      it "maxBy on single element (test 3)" $
        maxBy Prelude.length ["hello"] @?= Just "hello"
      it "maxBy abs on mixed numbers (test 4)" $
        maxBy abs [-5, 3, -2, 4] @?= Just (-5)
      it "maxBy id on regular numbers (test 5)" $
        maxBy id [1, 3, 2, 5, 4] @?= Just 5

    describe "minimum" $ do
      it "minimum of empty list (test 1)" $
        HW5.minimum emptyList @?= Nothing
      it "minimum of regular list (test 2)" $
        HW5.minimum testList @?= Just 1
      it "minimum of single element (test 3)" $
        HW5.minimum singleList @?= Just 42
      it "minimum of negative numbers (test 4)" $
        HW5.minimum negativeList @?= Just (-3)
      it "minimum of mixed numbers (test 5)" $
        HW5.minimum mixedList @?= Just (-2)

    describe "minBy" $ do
      it "minBy on empty list (test 1)" $
        minBy Prelude.length ([] :: [String]) @?= Nothing
      it "minBy length on strings (test 2)" $
        minBy Prelude.length ["bar", "bazz"] @?= Just "bar"
      it "minBy on single element (test 3)" $
        minBy Prelude.length ["hello"] @?= Just "hello"
      it "minBy abs on mixed numbers (test 4)" $
        minBy abs [-5, 3, -1, 4] @?= Just (-1)
      it "minBy id on regular numbers (test 5)" $
        minBy id [5, 3, 2, 1, 4] @?= Just 1

    describe "sum" $ do
      it "sum of empty list (test 1)" $
        HW5.sum emptyList @?= 0
      it "sum of regular list (test 2)" $
        HW5.sum testList @?= 15
      it "sum of single element (test 3)" $
        HW5.sum singleList @?= 42
      it "sum of negative numbers (test 4)" $
        HW5.sum negativeList @?= (-6)
      it "sum of mixed numbers (test 5)" $
        HW5.sum mixedList @?= 1

    describe "product" $ do
      it "product of empty list (test 1)" $
        HW5.product emptyList @?= 1
      it "product of regular list (test 2)" $
        HW5.product [1, 2, 3] @?= 6
      it "product of single element (test 3)" $
        HW5.product singleList @?= 42
      it "product with zero (test 4)" $
        HW5.product [1, 2, 0, 4] @?= 0
      it "product of negative numbers (test 5)" $
        HW5.product [-1, -2] @?= 2

    describe "concatMap" $ do
      it "concatMap on empty list (test 1)" $
        HW5.concatMap (\x -> [x, x]) emptyList @?= ([] :: [Int])
      it "concatMap duplicate elements (test 2)" $
        HW5.concatMap (\x -> [x, x]) [1, 2] @?= [1, 1, 2, 2]
      it "concatMap with empty result (test 3)" $
        HW5.concatMap (const ([] :: [Int])) testList @?= ([] :: [Int])
      it "concatMap with singleton (test 4)" $
        HW5.concatMap (\x -> [x]) testList @?= testList
      it "concatMap with range (test 5)" $
        HW5.concatMap (\x -> [1..x]) [2, 3] @?= [1, 2, 1, 2, 3]

  describe "Section 2: Composing Folds" $ do
    
    describe "runFold" $ do
      it "runs sumF on empty list (test 1)" $
        runFold sumF emptyList @?= 0
      it "runs sumF on regular list (test 2)" $
        runFold sumF testList @?= 15
      it "runs lengthF on list (test 3)" $
        runFold lengthF testList @?= 5
      it "runs productF on list (test 4)" $
        runFold productF [1, 2, 3, 4] @?= 24
      it "runs nullF on empty list (test 5)" $
        runFold nullF emptyList @?= True

    describe "nullF" $ do
      it "nullF on empty list (test 1)" $
        runFold nullF emptyList @?= True
      it "nullF on non-empty list (test 2)" $
        runFold nullF testList @?= False
      it "nullF on single element (test 3)" $
        runFold nullF singleList @?= False
      it "nullF on string list (test 4)" $
        runFold nullF ["hello"] @?= False
      it "nullF on empty string list (test 5)" $
        runFold nullF ([] :: [String]) @?= True

    describe "findF" $ do
      it "findF on empty list (test 1)" $
        runFold (findF (> 0)) emptyList @?= Nothing
      it "findF finds element (test 2)" $
        runFold (findF (> 3)) testList @?= Just 4
      it "findF no match (test 3)" $
        runFold (findF (> 10)) testList @?= Nothing
      it "findF first match (test 4)" $
        runFold (findF even) [1, 3, 4, 6] @?= Just 4
      it "findF on single element (test 5)" $
        runFold (findF (== 42)) singleList @?= Just 42
    --   it "findF on infinite list (test 6)" $
    --     runFold (findF (> 100)) [1..] @?= Just 101

    describe "topKF" $ do
      it "topKF with k=0 (test 1)" $
        runFold (topKF 0) testList @?= []
      it "topKF with k=3 (test 2)" $
        runFold (topKF 3) [5, 1, 4, 2, 3] @?= [5, 4, 3]
      it "topKF with k larger than list (test 3)" $
        runFold (topKF 10) [3, 1, 2] @?= [3, 2, 1]
      it "topKF with duplicates (test 4)" $
        runFold (topKF 2) [3, 1, 3, 2, 3] @?= [3, 3]
      it "topKF on single element (test 5)" $
        runFold (topKF 1) singleList @?= [42]

    describe "averageF" $ do
      it "averageF on empty list (test 1)" $
        (runFold averageF ([] :: [Double])) @?= 0.0  -- todo isNan?
      it "averageF on regular list (test 2)" $
        runFold averageF [1.0, 2.0, 3.0, 4.0, 5.0] @?= 3.0
      it "averageF on single element (test 3)" $
        runFold averageF [42.0] @?= 42.0
      it "averageF on negative numbers (test 4)" $
        runFold averageF [-1.0, -2.0, -3.0] @?= (-2.0)
      it "averageF on mixed numbers (test 5)" $
        runFold averageF [-2.0, 0.0, 2.0] @?= 0.0

    describe "filterF" $ do
      it "filterF with no matches (test 1)" $
        runFold (filterF (> 10) sumF) testList @?= 0
      it "filterF with some matches (test 2)" $
        runFold (filterF (> 3) sumF) testList @?= 9  -- 4 + 5
      it "filterF with all matches (test 3)" $
        runFold (filterF (> 0) sumF) testList @?= 15
      it "filterF on empty list (test 4)" $
        runFold (filterF (> 0) sumF) emptyList @?= 0
      it "filterF with lengthF (test 5)" $
        runFold (filterF even lengthF) [1, 2, 3, 4, 5, 6] @?= 3

    describe "mapF" $ do
      it "mapF with identity (test 1)" $
        runFold (mapF id sumF) testList @?= 15
      it "mapF with doubling (test 2)" $
        runFold (mapF (* 2) sumF) [1, 2, 3] @?= 12  -- 2 + 4 + 6
      it "mapF with negation (test 3)" $
        runFold (mapF negate sumF) testList @?= (-15)
      it "mapF on empty list (test 4)" $
        runFold (mapF (* 2) sumF) emptyList @?= 0
      it "mapF with abs (test 5)" $
        runFold (mapF abs sumF) [-1, -2, 3] @?= 6

    describe "combine" $ do
      it "combine sumF and lengthF on empty list (test 1)" $
        runFold (combine sumF lengthF) emptyList @?= (0, 0)
      it "combine sumF and lengthF on regular list (test 2)" $
        runFold (combine sumF lengthF) testList @?= (15, 5)
      it "combine sumF and productF (test 3)" $
        runFold (combine sumF productF) [1, 2, 3] @?= (6, 6)
      it "combine lengthF and nullF (test 4)" $
        runFold (combine lengthF nullF) testList @?= (5, False)
      it "combine on single element (test 5)" $
        runFold (combine sumF lengthF) singleList @?= (42, 1)

    describe "combineWith" $ do
      it "combineWith (+) sumF lengthF on empty list (test 1)" $
        runFold (combineWith (+) sumF lengthF) emptyList @?= 0
      it "combineWith (+) sumF lengthF on regular list (test 2)" $
        runFold (combineWith (+) sumF lengthF) testList @?= 20  -- 15 + 5
      it "combineWith (*) sumF productF (test 3)" $
        runFold (combineWith (*) sumF productF) [1, 2, 3] @?= 36  -- 6 * 6
      it "combineWith max sumF lengthF (test 4)" $
        runFold (combineWith max sumF lengthF) [10, 20] @?= 30  -- max(30, 2) = 30
      it "combineWith division for average (test 5)" $
        runFold (combineWith (\s l -> if l == 0 then 0 else s / fromIntegral l) (sumF :: Fold Double Double Double) lengthF) ([2.0, 4.0, 6.0] :: [Double]) @?= (4.0 :: Double)

  describe "Section 3: Functor Functions" $ do
    
    describe "fmapToFst" $ do
      it "fmapToFst on empty list (test 1)" $
        fmapToFst Prelude.length ([] :: [String]) @?= []
      it "fmapToFst length on strings (test 2)" $
        fmapToFst Prelude.length ["foo", "bar"] @?= [(3, "foo"), (3, "bar")]
      it "fmapToFst on Maybe Nothing (test 3)" $
        fmapToFst Prelude.length (Nothing :: Maybe String) @?= (Nothing :: Maybe (Int, String))
      it "fmapToFst on Maybe Just (test 4)" $
        fmapToFst Prelude.length (Just "hello") @?= Just (5, "hello")
      it "fmapToFst with numbers (test 5)" $
        fmapToFst (* 2) [1, 2, 3] @?= [(2, 1), (4, 2), (6, 3)]

    describe "fmapToSnd" $ do
      it "fmapToSnd on empty list (test 1)" $
        fmapToSnd Prelude.length ([] :: [String]) @?= []
      it "fmapToSnd length on Maybe (test 2)" $
        fmapToSnd Prelude.length (Just "foo") @?= Just ("foo", 3)
      it "fmapToSnd on Maybe Nothing (test 3)" $
        fmapToSnd Prelude.length (Nothing :: Maybe String) @?= (Nothing :: Maybe (String, Int))
      it "fmapToSnd on list (test 4)" $
        fmapToSnd (* 2) [1, 2, 3] @?= [(1, 2), (2, 4), (3, 6)]
      it "fmapToSnd on Either (test 5)" $
        fmapToSnd Prelude.length (Right "hello" :: Either String String) @?= (Right ("hello", 5) :: Either String (String, Int))

    describe "strengthenL" $ do
      it "strengthenL on empty list (test 1)" $
        strengthenL 42 ([] :: [Int]) @?= []
      it "strengthenL on Either Right (test 2)" $
        strengthenL 42 (Right "foo" :: Either String String) @?= (Right (42, "foo") :: Either String (Int, String))
      it "strengthenL on Either Left (test 3)" $
        strengthenL 42 (Left "error" :: Either String String) @?= (Left "error" :: Either String (Int, String))
      it "strengthenL on list (test 4)" $
        strengthenL "x" [1, 2, 3] @?= [("x", 1), ("x", 2), ("x", 3)]
      it "strengthenL on Maybe (test 5)" $
        strengthenL 0 (Just 5) @?= Just (0, 5)

    describe "strengthenR" $ do
      it "strengthenR on empty list (test 1)" $
        strengthenR "x" ([] :: [Int]) @?= []
      it "strengthenR on list (test 2)" $
        strengthenR "x" [1, 2, 3] @?= [(1, "x"), (2, "x"), (3, "x")]
      it "strengthenR on Maybe Nothing (test 3)" $
        strengthenR "x" (Nothing :: Maybe Int) @?= (Nothing :: Maybe (Int, String))
      it "strengthenR on Maybe Just (test 4)" $
        strengthenR "x" (Just 42) @?= Just (42, "x")
      it "strengthenR on Either (test 5)" $
        strengthenR 0 (Right "hello" :: Either String String) @?= (Right ("hello", 0) :: Either String (String, Int))

    describe "unzip" $ do
      it "unzip Maybe pair (test 1)" $
        HW5.unzip (Just (1, 2)) @?= (Just 1, Just 2)
      it "unzip Maybe Nothing (test 2)" $
        HW5.unzip (Nothing :: Maybe (Int, Char)) @?= (Nothing :: Maybe Int, Nothing :: Maybe Char)
      it "unzip list of pairs (test 3)" $
        HW5.unzip [(1, 'a'), (2, 'b')] @?= ([1, 2], ['a', 'b'])
      it "unzip empty list (test 4)" $
        HW5.unzip ([] :: [(Int, Char)]) @?= ([], [])
      it "unzip Either pair (test 5)" $
        HW5.unzip (Right (1, "hello") :: Either String (Int, String)) @?= (Right 1 :: Either String Int, Right "hello" :: Either String String)

    describe "coUnZip" $ do
      it "coUnZip Right list (test 1)" $
        coUnzip (Right [1, 2, 3] :: Either [Char] [Int]) @?= ([Right 1, Right 2, Right 3] :: [Either Char Int])
      it "coUnZip Left string (test 2)" $
        coUnzip (Left "foo" :: Either String [Int]) @?= ([Left 'f', Left 'o', Left 'o'] :: [Either Char Int])
      it "coUnZip Right empty list (test 3)" $
        coUnzip (Right ([] :: [Int]) :: Either [Char] [Int]) @?= ([] :: [Either Char Int])
      it "coUnZip Left empty string (test 4)" $ 
        coUnzip (Left "" :: Either String [Int]) @?= ([] :: [Either Char Int])
      it "coUnZip Right single element (test 5)" $
        coUnzip (Right [42] :: Either [Char] [Int]) @?= ([Right 42] :: [Either Char Int])

  describe "Section 4: MultiSet Foldable Instances" $ do
    
    describe "MultiSet Foldable instance" $ do
      it "toList respects multiplicity (test 1)" $ do
        let ms = insert 2 $ insert 1 $ insert 2 empty
        HW5.toList ms @?= [1, 2, 2]  -- 1 appears once, 2 appears twice
      it "toList on empty multiset (test 2)" $
        HW5.toList (empty :: MultiSet Int) @?= []
      it "sum with multiplicity (test 3)" $ do
        let ms = fromList [1, 2, 2, 3]
        HW5.sum ms @?= 8  -- 1 + 2 + 2 + 3
      it "length counts all occurrences (test 4)" $ do
        let ms = fromList [1, 1, 2, 2, 2]
        HW5.length ms @?= 5
      it "elem finds existing element (test 5)" $ do
        let ms = fromList [1, 2, 3]
        HW5.elem 2 ms @?= True

    describe "FoldOccur instance" $ do
      it "toList unique elements (test 1)" $ do
        let ms = FoldOccur $ insert 2 $ insert 1 $ insert 2 empty
        HW5.toList ms @?= [1, 2]  -- Only unique elements
      it "FoldOccur on empty multiset (test 2)" $
        HW5.toList (FoldOccur (empty :: MultiSet Int)) @?= []
      it "FoldOccur preserves uniqueness (test 3)" $ do
        let ms = FoldOccur $ fromList [1, 1, 2, 2, 3]
        HW5.length ms @?= 3  -- Only 3 unique elements
      it "FoldOccur sum counts once (test 4)" $ do
        let ms = FoldOccur $ fromList [1, 1, 2, 2]
        HW5.sum ms @?= 3  -- 1 + 2 (each counted once)
      it "FoldOccur with single element (test 5)" $ do
        let ms = FoldOccur $ fromList [5, 5, 5]
        HW5.toList ms @?= [5]

    -- describe "MinToMax instance" $ do
    --   it "toList ascending order (test 1)" $ do
    --     let ms = MinToMax $ insert 3 $ insert 1 $ insert 2 empty
    --     HW5.toList ms @?= [1, 2, 3]
    --   it "MinToMax with duplicates (test 2)" $ do
    --     let ms = MinToMax $ fromList [3, 1, 2, 1, 3]
    --     HW5.toList ms @?= [1, 1, 2, 3, 3]
    --   it "MinToMax single element (test 3)" $ do
    --     let ms = MinToMax $ fromList [42]
    --     HW5.toList ms @?= [42]
    --   it "MinToMax empty set (test 4)" $
    --     HW5.toList (MinToMax (empty :: MultiSet Int)) @?= []
    --   it "MinToMax with negatives (test 5)" $ do
    --     let ms = MinToMax $ fromList [-1, 3, -2, 0]
    --     HW5.toList ms @?= [-2, -1, 0, 3]

    -- describe "MaxToMin instance" $ do
    --   it "toList descending order (test 1)" $ do
    --     let ms = MaxToMin $ insert 1 $ insert 3 $ insert 2 empty
    --     HW5.toList ms @?= [3, 2, 1]
    --   it "MaxToMin with duplicates (test 2)" $ do
    --     let ms = MaxToMin $ fromList [1, 3, 2, 3, 1]
    --     HW5.toList ms @?= [3, 3, 2, 1, 1]
    --   it "MaxToMin single element (test 3)" $ do
    --     let ms = MaxToMin $ fromList [42]
    --     HW5.toList ms @?= [42]
    --   it "MaxToMin empty set (test 4)" $
    --     HW5.toList (MaxToMin (empty :: MultiSet Int)) @?= []
    --   it "MaxToMin with negatives (test 5)" $ do
    --     let ms = MaxToMin $ fromList [-1, 3, -2, 0]
    --     HW5.toList ms @?= [3, 0, -1, -2]

    describe "foldOccur function" $ do
      it "foldOccur on empty multiset (test 1)" $
        foldOccur (\_ _ acc -> acc + 1) 0 (empty :: MultiSet Int) @?= 0
      it "foldOccur counts occurrences (test 2)" $ do
        let ms = fromList [1, 1, 2, 3, 3, 3]
        foldOccur (\elem count acc -> acc + count) 0 ms @?= 6
      it "foldOccur with element sum (test 3)" $ do
        let ms = fromList [2, 2, 3]
        foldOccur (\elem count acc -> acc + elem * count) 0 ms @?= 7  -- 2*2 + 3*1
      it "foldOccur counts unique elements (test 4)" $ do
        let ms = fromList [1, 1, 2, 2, 3]
        foldOccur (\_ _ acc -> acc + 1) 0 ms @?= 3  -- 3 unique elements
      it "foldOccur single element multiple times (test 5)" $ do
        let ms = fromList [5, 5, 5, 5]
        foldOccur (\elem count acc -> if count == 4 then acc + 1 else acc) 0 ms @?= 1

  describe "Bonus: ZipList instances" $ do
    
    describe "ZipList Semigroup" $ do
      it "empty lists combination (test 1)" $
        getZipList (ZipList [] <> ZipList []) @?= ([] :: [Sum Int])
      it "ziplist semigroup with Sum (test 2)" $ do
        let zl1 = ZipList (map Sum [1, 2, 3])
        let zl2 = ZipList (map Sum [4, 5])
        map getSum (getZipList (zl1 <> zl2)) @?= [5, 7]
      it "ziplist with different lengths (test 3)" $ do
        let zl1 = ZipList (map Sum [1, 2, 3, 4])
        let zl2 = ZipList (map Sum [10, 20])
        map getSum (getZipList (zl1 <> zl2)) @?= [11, 22]
      it "ziplist with Product (test 4)" $ do
        let zl1 = ZipList (map Product [1, 2, 3])
        let zl2 = ZipList (map Product [2, 3, 4])
        map getProduct (getZipList (zl1 <> zl2)) @?= [2, 6, 12]
      it "single element ziplists (test 5)" $ do
        let zl1 = ZipList (map Sum [5])
        let zl2 = ZipList (map Sum [3])
        map getSum (getZipList (zl1 <> zl2)) @?= [8]
      it "ziplist with infinite Product lists (test 6)" $ do
        let result = take 5 $ map getProduct $ getZipList $
                     ZipList (map Product [1..]) <> ZipList (map Product [0..])
        result @?= [0, 2, 6, 12, 20]

    describe "ZipList Monoid" $ do
      it "left identity law: mempty <> a == a (test 1)" $ do
        let zl = ZipList (map Sum [1, 2, 3])
        let result = take 3 $ map getSum $ getZipList (mempty <> zl)
        result @?= [1, 2, 3]
      it "right identity law: a <> mempty == a (test 2)" $ do
        let zl = ZipList (map Sum [1, 2, 3])
        let result = take 3 $ map getSum $ getZipList (zl <> mempty)
        result @?= [1, 2, 3]
      it "mempty with Product monoid (test 3)" $ do
        let zl = ZipList (map Product [2, 3, 4])
        let result1 = take 3 $ map getProduct $ getZipList (mempty <> zl)
        let result2 = take 3 $ map getProduct $ getZipList (zl <> mempty)
        result1 @?= [2, 3, 4]
        result2 @?= [2, 3, 4]
      it "mempty with empty ziplist (test 4)" $ do
        let emptyZl = ZipList ([] :: [Sum Int])
        let result1 = take 0 $ map getSum $ getZipList (mempty <> emptyZl)
        let result2 = take 0 $ map getSum $ getZipList (emptyZl <> mempty)
        result1 @?= []
        result2 @?= []
      it "mempty is infinite list of mempty elements (test 5)" $ do
        let memptyZl = (mempty :: ZipList (Sum Int))
        let result = take 5 $ map getSum $ getZipList memptyZl
        result @?= [0, 0, 0, 0, 0]  -- Sum mempty is 0
      it "mempty with infinite lists preserves identity (test 6)" $ do
        let infiniteZl = ZipList (map Sum [1..])
        let result1 = take 5 $ map getSum $ getZipList (mempty <> infiniteZl)
        let result2 = take 5 $ map getSum $ getZipList (infiniteZl <> mempty)
        result1 @?= [1, 2, 3, 4, 5]
        result2 @?= [1, 2, 3, 4, 5]

  describe "Bonus: varianceF" $ do
    
    describe "varianceF fold" $ do
      it "varianceF on empty list (test 1)" $ 
        runFold varianceF ([] :: [Double]) @?= 0.0
      it "varianceF on single element (test 2)" $
        runFold varianceF [5.0] @?= 0.0
      it "varianceF on identical elements (test 3)" $
        runFold varianceF [3.0, 3.0, 3.0] @?= 0.0
      it "varianceF on simple case (test 4)" $ do
        let result = runFold varianceF [1.0, 2.0, 3.0]
        (abs (result - 0.6666666666666666) < 1e-10) @?= True  -- Variance of [1,2,3]
      it "varianceF on negative numbers (test 5)" $ do
        let result = runFold varianceF [-1.0, 0.0, 1.0]
        (abs (result - 0.6666666666666666) < 1e-10) @?= True
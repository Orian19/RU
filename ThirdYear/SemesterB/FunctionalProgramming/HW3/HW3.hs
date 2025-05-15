{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW3.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, head, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, show, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (/), (^), (||))

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.Enum (Bounded)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Set qualified as Set
import Foreign (toBool)
import Data.Bool (Bool(False))

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)

-- Useful utility
singleTree :: a -> Tree a
singleTree x = Tree Empty x Empty

treeSize :: Tree a -> Int
treeSize = \case
    Empty -> 0
    Tree l _ r -> 1 + treeSize l + treeSize r

treeHeight :: Tree a -> Int
treeHeight = \case
    Empty -> 0
    Tree l _ r -> 1 + max (treeHeight l) (treeHeight r)

log2 :: Int -> Int
log2 = \case
    x | x <= 0 -> undefined
    x -> 1 + log2 $ x `div` 2

-- DLR
preOrderTraversal :: Tree a -> [a]
preOrderTraversal = \case
    Empty -> []
    Tree l x r -> [x] ++ preOrderTraversal l ++ preOrderTraversal r

-- LDR
inOrderTraversal :: Tree a -> [a]
inOrderTraversal = \case
    Empty -> []
    Tree l x r -> inOrderTraversal l ++ [x] ++ inOrderTraversal r

-- LRD
postOrderTraversal :: Tree a -> [a]
postOrderTraversal = \case
    Empty -> []
    Tree l x r -> postOrderTraversal l ++ postOrderTraversal r ++ [x]

data Classification = Full | Complete | FullAndComplete | Perfect | Degenerate | Other deriving (Show, Eq)

classify :: Tree a -> Classification
classify t
  | isPerfect t = Perfect
  | isFull t && isComplete t = FullAndComplete
  | isFull t = Full
  | isComplete t = Complete
  | isDegenerate t = Degenerate
  | otherwise = Other
  where
    isDegenerate :: Tree a -> Bool
    isDegenerate = \case
      Empty -> False
      Tree Empty _ Empty -> True
      Tree Empty _ r -> isDegenerate r
      Tree l _ Empty -> isDegenerate l
      Tree _ _ _ -> False

    isFull :: Tree a -> Bool
    isFull = \case
      Empty -> True
      Tree Empty _ Empty -> True
      Tree Empty _ _ -> False
      Tree _ _ Empty -> False
      Tree l _ r -> isFull l && isFull r

    isPerfect :: Tree a -> Bool
    isPerfect t = (2 ^ treeHeight t) - 1 == treeSize t

    isComplete:: Tree a -> Bool
    isComplete t = levelOrder [t] False
        where
            levelOrder :: [Tree a] -> Bool -> Bool
            levelOrder [] _ = True
            levelOrder (Empty : xs) seenEmpty = levelOrder xs True
            levelOrder (Tree l _ r : xs) seenEmpty
                | seenEmpty = False
                | otherwise = levelOrder (xs ++ [l, r]) seenEmpty

        -- isComplete :: Tree a -> Bool
        -- isComplete tree = checkComplete tree 0 (treeSize tree)
        -- where
        --     checkComplete :: Tree a -> Int -> Int -> Bool
        --     checkComplete Empty _ _ = True
        --     checkComplete (Tree l x r) index size =
        --     index < size && -- Check if the current node's index is valid
        --     checkComplete l (2 * index + 1) size && -- Check left subtree
        --     checkComplete r (2 * index + 2) size    -- Check right subtree

-- todo: change style?
isBalanced :: Tree a -> Bool
isBalanced = fst . testBalanced
  where
    testBalanced :: Tree a -> (Bool, Int)
    testBalanced Empty = (True, 0)
    testBalanced (Tree l _ r) =
      let (bl, hl) = testBalanced l
          (br, hr) = testBalanced r
          balanced = bl && br && abs (hl - hr) <= 1
      in (balanced, 1 + max hl hr)

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

-- As defined in the PDf
sample :: InfiniteList a -> [a]
sample = sampleN 10
smallSample :: InfiniteList a -> [a]
smallSample = sampleN 5
sampleN :: Int -> InfiniteList a -> [a]
sampleN n = take n . itoList

itoList :: InfiniteList a -> [a]
iiterate :: (a -> a) -> a -> InfiniteList a
irepeat :: a -> InfiniteList a
naturals :: InfiniteList Integer
imap :: (a -> b) -> InfiniteList a -> InfiniteList b
iconcat :: InfiniteList [a] -> InfiniteList a
grouped :: Integer -> InfiniteList a -> InfiniteList [a]
reverseN :: Integer -> InfiniteList a -> InfiniteList a
sqrtInf :: Rational -> InfiniteList Rational
type InfiniteString = InfiniteList Char
longDivision :: Rational -> InfiniteString
sqrtStrings :: Rational -> InfiniteList InfiniteString

-- Section 3: Maze
data Cell = Open | Blocked | Treasure deriving (Show, Eq, Ord)
data Maze = Maze {width :: Int, height :: Int, layout :: [[Cell]]} deriving Show
cellAt :: Maze -> CellPosition -> Maybe Cell
data CellPosition = CellPosition {row :: Int, col :: Int} deriving (Show, Eq, Ord)

data Error = OutOfBounds | InvalidCell | NoPath deriving (Show, Eq, Ord)
getAvailableMoves :: Maze -> CellPosition -> Either Error [CellPosition]

-- From the lectures
data Queue a = Queue [a] [a] deriving (Show, Eq)
emptyQueue :: Queue a
emptyQueue = Queue [] []
enqueue :: a -> Queue a -> Queue a
enqueue a (Queue l r) = Queue (a : l) r
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue l []) = dequeue $ Queue [] (reverse l)
dequeue (Queue l (r : rs)) = Just (r, Queue l rs)

shortestPath :: Maze -> CellPosition -> CellPosition -> Either Error [CellPosition]
-- Bonus (15 points)
treasureHunt :: Maze -> CellPosition -> Either Error [CellPosition]

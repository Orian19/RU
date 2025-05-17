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
import GHC.Base (undefined)

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
    1 -> 0
    x -> 1 + log2  (x `div` 2)

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
classify tr
  | isPerfect tr = Perfect
  | isFull tr && isComplete tr = FullAndComplete
  | isFull tr = Full
  | isComplete tr = Complete
  | isDegenerate tr = Degenerate
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
            levelOrder (Empty : xs) _ = levelOrder xs True
            levelOrder (Tree l _ r : xs) seenEmpty
                | seenEmpty = False
                | otherwise = levelOrder (xs ++ [l, r]) seenEmpty

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
itoList (x :> xs) = x : itoList xs

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

irepeat :: a -> InfiniteList a
irepeat = iiterate id

naturals :: InfiniteList Integer
naturals = iiterate (+1) 0

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

iconcat :: InfiniteList [a] -> InfiniteList a
iconcat (xs :> xss) = case xs of
  [] -> iconcat xss
  (y : ys) -> y :> iconcat (ys :> xss)

grouped :: Integer -> InfiniteList a -> InfiniteList [a]
grouped n l = appendN n l
    where
        appendN :: Integer -> InfiniteList a -> InfiniteList [a]
        appendN 0 ys = [] :> appendN n ys
        appendN _ (y :> ys) = case appendN (n - 1) ys of
            zs :> zss -> (y : zs) :> zss

reverseN :: Integer -> InfiniteList a -> InfiniteList a
reverseN n l = iconcat $ imap reverseList $ grouped n l
    where
        reverseList :: [a] -> [a]
        reverseList [] = []
        reverseList (x : xs) = reverseList xs ++ [x]

sqrtInf :: Rational -> InfiniteList Rational
sqrtInf x =  iiterate (\ current -> ( current + x / current ) / 2) x

-- type InfiniteString = InfiniteList Char
-- longDivision :: Rational -> InfiniteString
-- longDivision r
--   | r < 0     = '-' :> longDivision (abs r)
--   | otherwise = let (n, d) = (numerator r, denominator r)
--                     (whole, rem) = n `divMod` d
--                 in  mapDigits (show whole ++ ".") rem d
--   where
--     mapDigits :: String -> Integer -> Integer -> InfiniteString
--     mapDigits (c:cs) rem d = c :> mapDigits cs rem d
--     mapDigits [] rem d =
--       let (digit, newRem) = (rem * 10) `divMod` d
--       in intToDigit (fromInteger digit) :> mapDigits [] newRem d

type InfiniteString = InfiniteList Char
longDivision :: Rational -> InfiniteString
longDivision x | x < 0 = '-' :> (longDivison x)
longDivision x = case (numerator(abs(x)), denominator(abs(x))) of
    (x, 0) -> undefined
    (0, y) -> '0' :> '.' :> irepeat '0'
    (x, y) -> divison 0 (reverseNum x) y
    
    where
        divison :: Double -> Integer -> Integer -> InfiniteString
        divison remainder x y = case x of
            0 -> if remainder > 0 then '.' :> show(remainder `div` y) else '.' :> irepeat '0'
            z -> let msb = remainder + (z `mod` 10); res = msb `div` y in 
                if msb >= y then 
                    show(res) :> divison (10 * (msb - res * y)) (x `div` 10) y  
                else divison (10 * (remainder + msb)) (x `div` 10) y

        reverseDigits :: Integer -> Integer
        reverseDigits n = case n of
            x -> (x `mod` 10) * 10 ^ (countDigits x - 1) + reverseDigits (x `div` 10)
            _ -> 0
        
        countDigits :: Integer -> Integer
        countDigits n = case abs n of
            x | x >= 10 -> 1 + countDigits (x `div` 10)
            _ -> 1

sqrtStrings :: Rational -> InfiniteList InfiniteString
sqrtStrings x = imap longDivision (sqrtInf x)

-- Section 3: Maze
data Cell = Open | Blocked | Treasure deriving (Show, Eq, Ord)
data Maze = Maze {width :: Int, height :: Int, layout :: [[Cell]]} deriving Show

cellAt :: Maze -> CellPosition -> Maybe Cell
cellAt (Maze {width = w, height = h, layout = grid}) (CellPosition r c)
    | (0 > r || r >= h) || (0 > c || c >= w) = Nothing
    | otherwise = Just $ (grid !! r) !! c

data CellPosition = CellPosition {row :: Int, col :: Int} deriving (Show, Eq, Ord)
data Error = OutOfBounds | InvalidCell | NoPath deriving (Show, Eq, Ord)

getAvailableMoves :: Maze -> CellPosition -> Either Error [CellPosition]
getAvailableMoves m pos = case cellAt m pos of
    Nothing -> Left OutOfBounds
    Just Blocked -> Left InvalidCell
    Just _ -> Right $ filter isValid neighbors
  where
    directions = [(1,0), (-1,0), (0,1), (0,-1)]
    neighbors = [CellPosition (row pos + dr) (col pos + dc) | (dr, dc) <- directions]
    isValid p = case cellAt m p of
        Just Open -> True
        Just Treasure -> True
        _ -> False

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
shortestPath m s t = case (cellAt m s, cellAt m t) of
    (Blocked, _) -> Left InvalidCell
    (_, Blocked) -> Left InvalidCell
    (Nothing, _) -> Left OutOfBounds
    (_, Nothing) -> Left OutOfBounds
    (_, _) -> getPath (let moves = getAvailableMoves m s in (prev s moves) ++ bfs (Set . fromList [s]) (flattenQueue (enqueue moves emptyQueue)) m t) s t
        where
            flattenList :: [[a]] -> [a]
            flattenList = foldr (++) ([])

            flattenQueue :: Queue [[a]] [a] -> Queue [a] [a]
            flattenQueue (Queue l r) = Queue (flattenList l) r

            prev :: CellPosition -> Either Error [CellPosition] -> [CellPosition, CellPosition]
            prev parent = \case
                Left _ -> []
                Right [] -> []
                Right (x : xs) -> (x, parent) ++ prev parent (Right xs)

            bfs :: Set CellPosition -> Queue a -> Maze -> CellPosition -> [CellPosition, CellPosition]
            bfs visited (Queue (x : xs) r) m t = case getAvailableMoves m x of
                Left _ -> Left NoPath
                Right [] -> Left NoPath
                Right l_moves -> if x `Set.member` visited then [] else (prev x (Right l_moves)) ++ bfs (x `Set.insert` visited) (flattenQueue (enqueue l_moves (Queue xs r))) m t

            getPath :: [(CellPosition, CellPosition)] -> CellPosition -> CellPosition -> Either Error [CellPosition]
            getPath ((c, p) : xs) s t = case (c, p) of
                (s, _) -> []
                (step, t) -> getPath xs step ++ step

            -- getPath :: [(CellPosition, CellPosition)] -> CellPosition -> CellPosition -> Either Error [CellPosition]
            -- getPath cells s t = case backtrack t of
            --     Nothing -> Left NoPath
            --     Just path -> Right (reverse path)
            --     where
            --         backtrack cur = if cur == s then Just [s]
            --             else case lookup cur cells of
            --                 Nothing -> Nothing
            --                 Just prev -> case backtrack prev of
            --                     Nothing -> Nothing
            --                     Just pth -> Just (cur : pth)


-- handled edge cases but left to handle bfs logic (mostly what we need to return and how to find minimum)
-- need to handle when we reached the target

-- Bonus (15 points)
treasureHunt :: Maze -> CellPosition -> Either Error [CellPosition]

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

treeSize :: Tree a -> Int
treeSize = \case
    Empty -> 0
    Tree l _ r -> 1 + treeSize l + treeSize r

treeHeight :: Tree a -> Int
treeHeight = \case
    Empty -> 0
    Tree l _ r -> 1 + max (treeHeight l) (treeHeight r)

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
      Empty -> True
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
grouped n l
    | n <= 0 = undefined
    | otherwise = makeGroups n l
  where
    makeGroups :: Integer -> InfiniteList a -> InfiniteList [a]
    makeGroups k xs = let (group, rest) = takeGroup k xs in group :> makeGroups k rest
    
    takeGroup :: Integer -> InfiniteList a -> ([a], InfiniteList a)
    takeGroup 0 xs = ([], xs)
    takeGroup k (x :> xs) = let (group, rest) = takeGroup (k - 1) xs in (x : group, rest)

reverseN :: Integer -> InfiniteList a -> InfiniteList a
reverseN n l = iconcat $ imap reverseList $ grouped n l
    where
        reverseList :: [a] -> [a]
        reverseList [] = []
        reverseList (x : xs) = reverseList xs ++ [x]

sqrtInf :: Rational -> InfiniteList Rational
sqrtInf x =  iiterate (\ current -> ( current + x / current ) / 2) x

type InfiniteString = InfiniteList Char
longDivision :: Rational -> InfiniteString
longDivision r
  | r < 0 = '-' :> longDivision (negate r)
  | otherwise = let (n, d) = (numerator r, denominator r)
                    (divRes, remainder) = n `divMod` d
                in iLongDivRes (show divRes ++ ".") remainder d
  where
    iLongDivRes :: String -> Integer -> Integer -> InfiniteString
    iLongDivRes (c : cs) remainder d = c :> iLongDivRes cs remainder d
    iLongDivRes [] remainder d =
      let (digit, newRem) = (remainder * 10) `divMod` d
          digitChar = case show digit of
            (c : _) -> c
            [] -> undefined
      in digitChar :> iLongDivRes [] newRem d

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
    directions = [(0, -1), (0, 1), (1, 0), (-1, 0)]
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
    (Just Blocked, _) -> Left InvalidCell
    (_, Just Blocked) -> Left InvalidCell
    (Nothing, _) -> Left OutOfBounds
    (_, Nothing) -> Left OutOfBounds
    _ -> case getAvailableMoves m s of
         Left err -> Left err
         Right moves -> let initPath = prevMoves s moves 
                            initQueue = enqueueList moves emptyQueue
                            visitedSet = Set.fromList [s]
                        in buildPath (initPath ++ bfs visitedSet initQueue) s t
        where
            prevMoves :: CellPosition -> [CellPosition] -> [(CellPosition, CellPosition)]
            prevMoves _ [] = []
            prevMoves parent (x : xs) = (x, parent) : prevMoves parent xs

            bfs :: Set.Set CellPosition -> Queue CellPosition -> [(CellPosition, CellPosition)]
            bfs _ q | dequeue q == Nothing = []
            bfs visited q =
                case dequeue q of
                  Just (curPos, q')
                    | curPos == t -> []
                    | curPos `Set.member` visited -> bfs visited q'
                    | otherwise ->
                        case getAvailableMoves m curPos of
                          Left _ -> bfs visited q'
                          Right nextMoves -> 
                              let cells = prevMoves curPos nextMoves
                                  newVisited = curPos `Set.insert` visited
                                  newQueue = enqueueList nextMoves q'
                              in cells ++ bfs newVisited newQueue
                  Nothing -> []

            enqueueList :: [a] -> Queue a -> Queue a
            enqueueList xs q = foldr enqueue q xs

            buildPath :: [(CellPosition, CellPosition)] -> CellPosition -> CellPosition -> Either Error [CellPosition]
            buildPath cells start end = case backtrack end of
                Nothing -> Left NoPath
                Just path -> Right (reverse (drop 1 path))
                where
                    backtrack cur = if cur == start then Just []
                        else case lookup cur cells of
                            Nothing -> Nothing
                            Just prevPos -> case backtrack prevPos of
                                Nothing -> Nothing
                                Just pth -> Just (cur : pth)

-- Bonus (15 points)
treasureHunt :: Maze -> CellPosition -> Either Error [CellPosition]
treasureHunt maze start = let 
                    treasures = findTreasures maze
                    in if treasures == []
                       then Left NoPath 
                       else findAllPaths maze start treasures

    where
        findTreasures :: Maze -> [CellPosition]
        findTreasures maze' = [CellPosition r c | (r, row) <- zip [0..] (layout maze'), (c, cell) <- zip [0..] row, isTreasure cell]

        isTreasure :: Cell -> Bool
        isTreasure = \case
            Treasure -> True
            _ -> False

        findAllPaths :: Maze -> CellPosition -> [CellPosition] -> Either Error [CellPosition]
        findAllPaths maze' start' = \case
            [] -> Right []
            (x : xs) -> case shortestPath maze' start' x of
                Left err -> Left err
                Right pathToX -> case findAllPaths maze' x xs of
                    Left err -> Left err
                    Right restOfPath -> Right (pathToX ++ [x] ++ restOfPath)
        
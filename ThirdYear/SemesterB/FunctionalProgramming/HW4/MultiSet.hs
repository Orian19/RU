{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MultiSet (MultiSet, empty, member, count, remove, insert, fromList, toList) where

import Data.Either
import Data.List (find, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe
import Data.Semigroup (Arg (..))
import Data.Set qualified as Set
import Prelude (Bool (..), Char, Double, Either (..), Eq (..), Int, Integer, Integral, Maybe (..), Monoid (..), Num (..), Ord (..), Semigroup (..), Show (..), String, all, const, div, drop, error, filter, foldl', foldr, id, init, iterate, length, lookup, map, mod, not, otherwise, product, replicate, reverse, sum, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (^), (||))

newtype MultiSet a = MultiSet {_getMultiset :: Set.Set (Arg a Int)}

lookupS :: Ord a => a -> Set.Set (Arg a b) -> Maybe b
lookupS x s = case Set.lookupGE (Arg x undefined) s of
  Just (Arg y b) | x == y -> Just b
  _ -> Nothing

empty :: MultiSet a
empty = MultiSet Set.empty

member :: Ord a => a -> MultiSet a -> Bool
member x m = case lookupS x (_getMultiset m) of
    Nothing -> False
    Just _ -> True

-- | Returns the count of an element in the multiset, 0 if not present.
count :: Ord a => a -> MultiSet a -> Int
count x m = case lookupS x (_getMultiset m) of
    Nothing -> 0
    Just y -> y

-- | Insert one occurrence of an element into the multiset.
insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x m = MultiSet $ Set.insert (Arg x (count x m + 1)) (_getMultiset m)

-- | Remove one occurrence of an element from the multiset.
remove :: Ord a => a -> MultiSet a -> MultiSet a
remove x (MultiSet s) = case lookupS x s of
    Nothing -> MultiSet s
    Just 1 -> MultiSet $ Set.delete (Arg x 1) s
    Just n -> MultiSet $ Set.insert (Arg x (n - 1)) s --(Set.delete (Arg x n) s))

-- | Convert a list into a multiset.
fromList :: Ord a => [a] -> MultiSet a
fromList = foldr insert empty

-- | Convert a multiset into a list, including duplicates.
toList :: Ord a => MultiSet a -> [a]
toList m = Set.foldr insertVal [] (_getMultiset m)
    where
        insertVal :: Arg a Int -> [a] -> [a]
        insertVal (Arg a b) l = replicate b a ++ l

instance Ord a => Eq (MultiSet a) where
  (==) :: Ord a => MultiSet a -> MultiSet a -> Bool
  m1 == m2 = toList m1 == toList m2

-- todo: need spaces between ","?
-- instance (Ord a, Show a) => Show (MultiSet a) where
--     show m = "{" ++ intercalate ", " (map show (toList m)) ++ "}"
instance Show a => Show (MultiSet a) where
  show :: Show a => MultiSet a -> String
  show m =
    let elems = Set.foldr insertVal [] (_getMultiset m)
        insertVal (Arg a b) l = replicate b (show a) ++ l
    in "{" ++ intercalate "," elems ++ "}"

instance Ord a => Semigroup (MultiSet a) where
     (<>) :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
     MultiSet s1 <> MultiSet s2 = fromList (toList (MultiSet s1) ++ toList (MultiSet s2))

instance Ord a => Monoid (MultiSet a) where
    mempty :: Ord a => MultiSet a
    mempty = empty

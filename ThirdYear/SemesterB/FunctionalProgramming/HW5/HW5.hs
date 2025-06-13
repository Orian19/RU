{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Data.Either
import Data.List (foldl', sort, uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import Data.Set qualified as S
import MultiSet
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Foldable (foldMap, foldl, foldr), Fractional, Functor (fmap), Int, Maybe (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, error, filter, flip, fst, id, init, map, not, or, replicate, reverse, snd, take, takeWhile, uncurry, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (||), (/), fromIntegral, (<$>))

-- Section 1: Foldable functions
fold :: (Foldable t, Monoid a) => t a -> a
fold = foldMap id

toList :: Foldable t => t a -> [a]
toList = foldMap (:[])

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = any . (==)

find :: (Foldable t, Eq a) => (a -> Bool) -> t a -> Maybe a
-- uses toList
find p ta = case filter p (HW5.toList ta) of
  [] -> Nothing
  (x: _) -> Just x

length :: Foldable t => t a -> Int
length = foldr (\_ n -> n + 1) 0

null :: Foldable t => t a -> Bool
-- null = any . (const False)
null = foldr (\_ _ -> False) True

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr step Nothing
  where
    step x Nothing = Just x
    step x (Just y) = Just (max x y)

maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maxBy f = foldr step Nothing
  where
    step x Nothing  = Just x
    step x (Just y) = Just (if f x `max` f y == f x then x else y)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr step Nothing
  where
    step x Nothing = Just x
    step x (Just y) = Just (min x y)

minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
minBy f = foldr step Nothing
  where
    step x Nothing  = Just x
    step x (Just y) = Just (if f x `min` f y == f x then x else y)

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap = foldMap

-- Section 2: Composing folds
data Fold a b c = Fold (b -> a -> b) b (b -> c)

-- Instances + helpers

instance Functor (Fold a b) where
  fmap :: (c -> d) -> Fold a b c -> Fold a b d
  fmap f (Fold a b c) = Fold a b (f . c)

-- From the lectures
-- class Functor f => Apply f where
--   {-# MINIMAL (<*>) | liftA2 #-}
--   liftA2 :: (a -> b -> c) -> fa -> f b -> f c
--   liftA2 f x = (<*>) (fmap f x)
--   (<*>) :: f (a -> b) -> f a -> f b
--   (<*>) = liftA2 id

-- instance Apply (Fold a b) where
--   liftA2 :: (c -> d -> e) -> Fold a b c -> Fold a b d -> Fold a b e
--   liftA2 f (Fold a b c) (Fold a' b' d) =
--     Fold
--       (\(x, y) z ->
--         let x' = a x z
--             y' = a' y z
--         in (x', y'))
--       (b, b')
--       (\(x, y) -> 
--         let cx = c x
--             dy = d y
--         in f cx dy)

-- Execute a fold operation as a left fold.
runFold :: Foldable t => Fold a b c -> t a -> c
runFold (Fold a b c) ta = c (foldl' a b ta)
-- "F" companions to normal fold utils

-- Will only fold over elements that satisfy a predicate
filterF :: (a -> Bool) -> Fold a b c -> Fold a b c
filterF p (Fold a b c) = Fold (\x y -> if p y then a x y else x) b c

-- Applies a function to each elements before applying the step function
mapF :: (a -> a) -> Fold a b c -> Fold a b c -- Not to be confused with fmap!
mapF f (Fold a b c) = Fold (\x y -> a x (f y)) b c

nullF :: Fold a Bool Bool
nullF = Fold (\_ _ -> False) True id

findF :: (a -> Bool) -> Fold a (Maybe a) (Maybe a)
findF p = Fold (\x y -> if p y then Just y else x) Nothing id

topKF :: Ord a => Int -> Fold a [a] [a]
topKF k = Fold (\x y -> y : x) [] (take k . reverse . sort)

-- Mathematical folds
sumF :: Num a => Fold a a a
sumF = Fold (+) 0 id

productF :: Num a => Fold a a a
productF = Fold (*) 1 id

lengthF :: Fold a Int Int
lengthF = Fold (\n _ -> n + 1) 0 id

combine :: Fold a b c -> Fold a b' c' -> Fold a (b, b') (c, c')
combine (Fold a b c) (Fold a' b' c') = 
  Fold
  (\(x, y) z -> -- a (b -> a -> b), a' = (b' -> a' -> b') => x = b, y=b'
        let x' = a x z
            y' = a' y z
        in (x', y'))
      (b, b')
      (\(x, y) ->
        let cx = c x
            cy = c' y
        in (cx, cy))

combineWith :: (c -> c' -> d) -> Fold a b c -> Fold a b' c' -> Fold a (b, b') d
combineWith f (Fold a b c) (Fold a' b' c') =
    Fold
      (\(x, y) z ->
        let x' = a x z
            y' = a' y z
        in (x', y'))
      (b, b')
      (\(x, y) -> 
        let cx = c x
            cy = c' y
        in f cx cy)

-- averageF :: Fractional a => Fold a (a, b) a
averageF :: Fractional a => Fold a (a, Int) a
-- todo: modify to use combine and combine with
averageF = combineWith (\s c -> if c == 0 then 0 else s / fromIntegral c) sumF lengthF
-- averageF = Fold step (0, 0) extract
--   where
--     step (s, c) x = (s + x, c + 1)
--     extract (s, c) = if c == 0 then 0 else s / fromIntegral c

-- Section 3: Functor functions

fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f fa = fmap (\x -> (f x, x)) fa

fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd f fa = fmap (\x -> (x, f x)) fa

strengthenL :: Functor f => b -> f a -> f (b, a)
strengthenL b fa = fmap (\x -> (b, x)) fa

strengthenR :: Functor f => b -> f a -> f (a, b)
strengthenR b fa = fmap (\x -> (x, b)) fa

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip fab = (fmap fst fab, fmap snd fab)

coUnzip :: Functor f => Either (f a) (f b) -> f (Either a b)
coUnzip = \case
  Left fa -> fmap Left fa
  Right fb -> fmap Right fb

-- Section 4: MultiSet Foldable instances

newtype FoldOccur a = FoldOccur {getFoldOccur :: MultiSet a}
instance Foldable FoldOccur where
  foldr :: (a -> b -> b) -> b -> FoldOccur a -> b
  foldr f b (FoldOccur a) = MultiSet.foldOccur (\x _ z -> f x z) b a

-- newtype MinToMax a = MinToMax {getMinToMax :: MultiSet a}
-- instance Ord a => Foldable (MinToMax a) where
--   foldr :: (a -> b -> b) -> b -> MinToMax a -> b
--   foldr f b (MinToMax a) = foldr f b (sort (MultiSet.toList a))

-- newtype MaxToMin a = MaxToMin {getMaxToMin :: MultiSet a}
-- instance Ord a => Foldable (MinToMax a) where
--   foldr :: (a -> b -> b) -> b -> MaxToMin a -> b
--   foldr f b (MaxToMin a) = foldr f b (reverse (sort (MultiSet.toList a)))

-- Bonus section

newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)

instance Semigroup a => Semigroup (ZipList a) where
  (<>) :: ZipList a -> ZipList a -> ZipList a
  ZipList xs <> ZipList ys = ZipList (zipWith (<>) xs ys)

instance Monoid a => Monoid (ZipList a) where
  mempty :: ZipList a
  mempty = ZipList infMem
    where
      infMem = mempty : infMem

-- Bonus (5 pt.): implement the varianceF Fold. This is slightly harder than average - the formula is
-- E[X^2] - E[x]^2, so you need to keep track of more than two quantities in your tuple. Use `combineWith` as needed.
varianceF :: Fractional a => Fold a ((a, a), Int) a
-- varianceF :: Fractional a => Fold a (a, a, Int) a
-- todo: changed sig
varianceF = combineWith (\(s, ssq) c -> (ssq / fromIntegral c) - (s / fromIntegral c) * (s / fromIntegral c)) (combine sumF sumSq) lengthF
  where
    sumSq :: Num a => Fold a a a
    sumSq = mapF (\x -> x * x) sumF

-- todo: modify according to -> 
  -- use combineWith, map and alike as needed to accumulate multiple quantities at the same time and combining them
-- varianceF = Fold
--   (\(s, sumSq, c) x -> (s + x, sumSq + x * x, c + 1)) 
--   (0, 0, 0)
--   (\(s, sumSq, c) -> 
--     if c == 0
--       then 0
--       else (sumSq / fromIntegral c) - (s / fromIntegral c) ^ 2)
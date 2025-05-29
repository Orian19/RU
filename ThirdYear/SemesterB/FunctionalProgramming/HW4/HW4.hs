{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror MultiSet.hs HW4.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Num Bool is an orphan instance (not defined where Num or Bool are defined), so we need to silence the warning.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Either
import Data.List (find, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe
import MultiSet qualified as MS
import Prelude (Bool (..), Char, Double, Either (..), Eq (..), Int, Integer, Integral, Maybe (..), Monoid (..), Num (..), Ord (..), Semigroup (..), Show (..), String, all, const, div, drop, error, filter, foldl', foldr, id, init, iterate, length, lookup, map, mod, not, otherwise, product, replicate, reverse, sum, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (^), (||))

-- Section 2: JSON data and Jsonable typeclass
newtype JString = JString String deriving (Show, Eq)

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString JString
  | JsonInt Integer
  | JsonDouble Double
  | JsonArray [Json]
  | JsonObject (Map.Map String Json)
  deriving (Show, Eq)

class Jsonable a where
  toJson :: a -> Json
  fromJson :: Json -> Maybe a

instance Jsonable Bool where
  toJson = JsonBool
  fromJson JsonBool b = Just b
  fromJson _ = Nothing

instance Jsonable JString where
  toJson (JString s) = JsonString (JString s)
  fromJson (JsonString (JString s)) = Just (JString s)
  fromJson _ = Nothing

instance Jsonable Integer where
  toJson = JsonInt
  fromJson (JsonInt i) = Just i
  fromJson _ = Nothing

instance Jsonable Double where 
  toJson = JsonDouble
  fromJson (JsonDouble d) = Just d
  fromJson _ = Nothing

instance (Jsonable a, Jsonable b) => Jsonable (a, b) where
  toJson (a, b) = JsonArray [toJson a, toJson b]
  fromJson (JsonArray [a, b]) = Just (fromJson a, fromJson b)
  fromJson _ = Nothing

instance (Jsonable a, Jsonable b, Jsonable c) => Jsonable (a, b, c) where
  toJson (a, b, c) = JsonArray [toJson a, toJson b, toJson c]
  fromJson (JsonArray [a, b, c]) = Just (fromJson a, fromJson b, fromJson c)
  fromJson _ = Nothing

instance Jsonable a => Jsonable (Maybe a) where
  toJson = \case
    Nothing -> JsonNull
    Just a -> toJson a
  fromJson = \case
    JsonNull -> Just Nothing
    j -> case fromJson j of
      Just x -> Just (Just x)
      Nothing -> Nothing

instance (Jsonable l, Jsonable r) => Jsonable (Either l r) where
  toJson = \case
    Left l -> JsonArray [JsonString (JString "Left"), toJson l]
    Right r -> JsonArray [JsonString (JString "Right"), toJson r]
  fromJson (JsonArray [JsonString (JString "Left"), jsonL]) = case fromJson jsonL of
    Just l -> Just (Left l)
    Nothing -> Nothing
  fromJson (JsonArray [JsonString (JString "Right"), jsonR]) = case fromJson jsonR of
    Just r -> Just (Right r)
    Nothing -> Nothing
  fromJson _ = Nothing

removeMaybes :: [Maybe a] -> Maybe [a]
removeMaybes [] = Just []
removeMaybes (Nothing : _) = Nothing
removeMaybes (Just x : xs) = case removeMaybes xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)

instance Jsonable a => Jsonable [a] where
  toJson l = JsonArray (map toJson l)
  fromJson (JsonArray l) = Just $ removeMaybes (map fromJson l)
  fromJson _ = Nothing

instance (Jsonable a, Ord a) => Jsonable (MS.MultiSet a) where
  toJson m = JsonArray (map toJson (MS.toList m))
  fromJson (JsonArray l) = case removeMaybes (map fromJson l) of
    Just xs -> Just (MS.fromList xs)
    Nothing -> Nothing
  
data Matrix a = Matrix [[a]] deriving (Show, Eq)

instance Jsonable a => Jsonable (Matrix a) where
  toJson m = JsonArray (map toJson m)
  fromJson (JsonArray m) = case removeMaybes (map fromJson m) of
      Just ls -> Just (Matrix ls)
      Nothing -> Nothing
  fromJson _ = Nothing

-- A sparse matrix is a more efficient representation of a matrix when most of the entries are zero.
-- Note that zero values should not appear in the map.
data SparseMatrix a
  = SparseMatrix
  { rows :: Integer
  , cols :: Integer
  , entries :: Map.Map (Integer, Integer) a
  }
  deriving (Show, Eq)

instance Jsonable a => Jsonable (SparseMatrix a) where
  toJson (SparseMatrix r c es) = JsonObject $ Map.fromList
    [ ("rows", JsonInt r)
    , ("cols", JsonInt c)
    , ("entries", JsonObject $
        Map.mapKeys (\(i,j) -> show i ++ "," ++ show j) $  -- todo: "," / ", "?
        Map.map toJson es)
    ]

-- todo: filter 0 values

  -- toJson (SparseMatrix r c e) = JsonObject $ Map.fromList
  --   [ ("rows", toJson r)
  --   , ("cols", toJson c) 
  --   , ("entries", toJson (Map.toList e))
  --   ]
  -- fromJson (JsonObject obj) = case (Map.lookup "rows" obj, Map.lookup "cols" obj, Map.lookup "entries" obj) of
  --   (Just rowsJson, Just colsJson, Just entriesJson) -> 
  --     case (fromJson rowsJson, fromJson colsJson, fromJson entriesJson) of
  --       (Just r, Just c, Just entryList) -> Just (SparseMatrix r c (Map.fromList entryList))
  --       _ -> Nothing
  --   _ -> Nothing
  -- fromJson _ = Nothing

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
instance Jsonable a => Jsonable (Tree a) where
  toJson Empty = JsonObject $ Map.fromList [("Empty", JsonNull)]
  toJson (Tree l x r) = JsonObject $ Map.fromList [("Tree", JsonArray (toJson l, toJson x, toJson r))]
  fromJson (JsonObject o) = case Map.lookup "Empty" o of
    Just JsonNull -> Just Empty
    _ ->
      case Map.lookup "Tree" o of
        Just (JsonArray [jl, jx, jr]) ->
          case (fromJson jl, fromJson jx, fromJson jr) of
            (Just l, Just x, Just r) -> Just (Tree l x r)
            _ -> Nothing
        _ -> Nothing
  fromJson _ = Nothing

-- Section 3: Num
-- Subsection: Num instances
instance Num Bool where
  b1 + b2 = b1 /= b2
  b1 - b2 = b1 /= b2
  b1 * b2 = b1 && b2
  abs = id
  signum = id
  -- signum b = if b then 1 else 0
  fromInteger = odd
  -- fromInteger n = (n `mod` 2) == 1

data Expression a
  = Iden String
  | Lit a
  | Plus (Expression a) (Expression a)
  | Minus (Expression a) (Expression a)
  | Mult (Expression a) (Expression a)
  | Div (Expression a) (Expression a)
  | Signum (Expression a)
  deriving (Eq, Show)
  
instance Num a => Num (Expression a) where
  (+) = Plus
  (-) = Minus
  (*) = Mult
  abs (Lit a) = Lit (abs a) 
  abs _ = undefined
  signum = Signum
  fromInteger = Lit . fromInteger

newtype MatrixSum a = MatrixSum {getMS :: Matrix a} deriving (Show, Eq)
newtype MatrixMult a = MatrixMult {getMM :: Matrix a} deriving (Show, Eq)

instance Num a => Semigroup (MatrixSum a) where
  (<>) :: MatrixSum a -> MatrixSum a -> MatrixSum a
  (MatrixSum (Matrix a)) <> (MatrixSum (Matrix b)) = MatrixSum $ Matrix $ zipWith (zipWith (+)) a b

instance Num a => Semigroup (MatrixMult a) where
  (<>) :: MatrixMult a -> MatrixMult a -> MatrixMult a
  (MatrixMult (Matrix a)) <> (MatrixMult (Matrix b)) = MatrixMult $ Matrix $ 
    map (\row -> map (dotProduct row) (transpose b)) a
    where
      dotProduct :: [a] -> [a] -> a
      dotProduct xs ys = sum (zipWith (*) xs ys)

newtype SparseMatrixSum a = SparseMatrixSum {getSMS :: SparseMatrix a} deriving (Show, Eq)
newtype SparseMatrixMult a = SparseMatrixMult {getSMM :: SparseMatrix a} deriving (Show, Eq)

-- These have Eq constraint so you can filter out zero values, which should not appear in sparse matrices.
instance (Num a, Eq a) => Semigroup (SparseMatrixSum a) where
  (<>) :: SparseMatrixSum a -> SparseMatrixSum a -> SparseMatrixSum a
  (SparseMatrixSum (SparseMatrix r1, c1, e1)) <> (SparseMatrixSum (SparseMatrix r2, c2, e2))
    | r1 /= r2 || c1 /= c2 = error "dims not compatibale"
    | otherwise = 
      SparseMatrixSum $ SparseMatrix r1 c1 $ Map.filter (/= 0) $ Map.unionWith (+) e1 e2

instance (Num a, Eq a) => Semigroup (SparseMatrixMult a) where
  (<>) :: SparseMatrixMult a -> SparseMatrixMult a -> SparseMatrixMult a
  (SparseMatrixMult (SparseMatrix r1 c1 e1)) <> (SparseMatrixMult (SparseMatrix r2 c2 e2))
    | c1 /= r2 = error "dims not compatibale"
    | otherwise = 
      SparseMatrixMult $ SparseMatrix r1 c2 $ Map.filter (/= 0) $ Map.fromList $
        [ ((i, j), sum [Map.findWithDefault 0 (i, k) e1 * Map.findWithDefault 0 (k, j) e2 | k <- [0..c1-1]])
        | i <- [0..r1-1], j <- [0..c2-1]
        , let val = sum [Map.findWithDefault 0 (i, k) e1 * Map.findWithDefault 0 (k, j) e2 | k <- [0..c1-1]]
        , val /= 0
        ]

-- Subsection: General functions
evalPoly :: Num a => [a] -> a -> a

type Length = Int
type I = Int
type J = Int
pathsOfLengthK :: Length -> I -> J -> Matrix Int -> Int
hasPath :: I -> J -> Matrix Int -> Bool

-- Section 4: Simplify expressions
-- We constrain the type to Integral so we can use integer division
simplify :: (Expression Integer) -> (Expression Integer)
inlineExpressions :: [(Expression Integer, String)] -> [(Expression Integer, String)]
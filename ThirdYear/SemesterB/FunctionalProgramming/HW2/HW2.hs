{-# LANGUAGE GHC2024 #-}
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, elem, error, even, filter, flip, foldl, foldr, fromIntegral, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------
-- Utilities from the lecture
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f = \case
  Nothing -> Nothing
  Just x -> Just $ f x

eitherMap :: (a -> b) -> Either e a -> Either e b
eitherMap f = \case
  Left x -> Left x
  Right y -> Right $ f y

-- Section 1.1: Basic Maybes
fromMaybe :: a -> Maybe a -> a
fromMaybe a = \case
  Nothing -> a
  Just x -> x

concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap f = \case
  Nothing -> Nothing
  Just x -> f x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe a f = \case
  Nothing -> a
  Just x -> f x

maybeHead :: [a] -> Maybe a
maybeHead = \case
  [] -> Nothing
  (x : _) -> Just x

maybeLast :: [a] -> Maybe a
maybeLast = \case
  [] -> Nothing
  [x] -> Just x
  (_ : xs) -> maybeLast xs

maybeMaximum :: [Int] -> Maybe Int
maybeMaximum = \case
  [] -> Nothing
  (x : xs) -> Just $ foldl' max x xs

maybeMinimum :: [Int] -> Maybe Int
maybeMinimum = \case
  [] -> Nothing
  (x : xs) -> Just $ foldl' min x xs

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f = \case
  Nothing -> Nothing
  Just x -> if f x then Just x else Nothing

sumMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumMaybe = liftMaybe2 (+)

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f a b = case a of
  Nothing -> Nothing
  Just x -> case b of
    Nothing -> Nothing
    Just y -> Just $ f x y

catMaybes :: [Maybe a] -> [a]
catMaybes = \case
  [] -> []
  (Just x : xs) -> x : catMaybes xs
  (Nothing : xs) -> catMaybes xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

-- Section 1.2 Basic Eithers
fromEither :: b -> Either a b -> b
fromEither a = \case
  Left _ -> a
  Right x -> x

concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap f = \case
  Left x -> Left x
  Right y -> f y

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g = \case
 Left x -> f x
 Right y -> g y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = \case
  Left x -> Left $ f x
  Right y -> Right y

catEithers :: [Either e a] -> Either e [a]
catEithers = \case
  [] -> Right []
  (Left x : _) -> Left x
  (Right y : ys) -> case catEithers ys of
    Left x -> Left x
    Right zs -> Right $ y : zs

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f = catEithers . map f

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = \case
  [] -> ([], [])
  (Left x : xs) -> case partitionEithers xs of
    (ys, zs) -> (x : ys, zs)
  (Right y : xs) -> case partitionEithers xs of
    (ys, zs) -> (ys, y : zs)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = \case
  Left _ -> Nothing
  Right x -> Just x

productEither :: Either a Int -> Either a Int -> Either a Int
productEither = liftEither2 (*)

liftEither2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
liftEither2 f a b = case a of
  Left x -> Left x
  Right y -> case b of
    Left x -> Left x
    Right z -> Right $ f y z

-- Section 2: Expressions
data Expr = Iden String | Lit Int | Plus Expr Expr | Minus Expr Expr | Mul Expr Expr | Div Expr Expr deriving (Show, Eq)

-- Adds parentheses around sub-expressions (the top level expression never has parentheses).
-- exprToString :: Expr -> String
-- exprToString e = toStr e
--   where
--     toStr :: Expr -> String
--     toStr (Lit n) = show n
--     toStr (Iden s) = s
--     toStr (Plus e1 e2) = toStr e1 ++ " + " ++ wrapPar e2
--     toStr (Minus e1 e2) = toStr e1 ++ " - " ++ wrapPar e2
--     toStr (Mul e1 e2) = toStr e1 ++ " * " ++ wrapPar e2
--     toStr (Div e1 e2) = toStr e1 ++ " / " ++ wrapPar e2
    
--     wrapPar :: Expr -> String
--     wrapPar ex = case ex of
--       Lit x -> show x
--       Iden y -> y
--       _ -> "(" ++ toStr ex ++ ")"

exprToString :: Expr -> String
exprToString = \case
  Iden x -> x
  Lit x -> show x
  Plus l r -> aux '+' l r
  Mul l r -> aux '*' l r
  Minus l r -> aux '-' l r
  Div l r -> aux '/' l r
 where
  aux op l r = withParens l ++ " " ++ [op] ++ " " ++ withParens r
  withParens l = case l of
    Iden _ -> exprToString l
    Lit _ -> exprToString l
    e -> "(" ++ exprToString e ++ ")"

-- Bonus (25 points): Same as the above, but without unnecessary parentheses
exprToString' :: Expr -> String
exprToString' expr = fst (toStrBonus expr)

-- determines if parentheses are needed for the expression by checking the precedence of the operator
toStrBonus :: Expr -> (String, Int)
-- Order of precedence: 
-- 1: Plus, Minus
-- 2: Mul, Div
-- 3: Lit, Iden (never parentheses)

toStrBonus (Lit n) = (show n, 3)
toStrBonus (Iden s) = (s, 3)
toStrBonus (Plus e1 e2) =
  let (s1, _) = toStrBonus e1
      (s2, p2) = toStrBonus e2
      leftExp = s1
      rightExp = if p2 == 1 then "(" ++ s2 ++ ")" else s2
  in (leftExp ++ " + " ++ rightExp, 1)

toStrBonus (Minus e1 e2) =
  let (s1, _) = toStrBonus e1
      (s2, p2) = toStrBonus e2
      leftExp = s1
      rightExp = if p2 == 1 then "(" ++ s2 ++ ")" else s2
  in (leftExp ++ " - " ++ rightExp, 1)

toStrBonus (Mul e1 e2) =
  let (s1, p1) = toStrBonus e1
      (s2, p2) = toStrBonus e2
      leftExp  = if p1 == 1 then "(" ++ s1 ++ ")" else s1
      rightExp = if p2 <= 2 then "(" ++ s2 ++ ")" else s2
  in (leftExp ++ " * " ++ rightExp, 2)

toStrBonus (Div e1 e2) =
  let (s1, p1) = toStrBonus e1
      (s2, p2) = toStrBonus e2
      leftExp  = if p1 == 1 then "(" ++ s1 ++ ")" else s1
      rightExp = if p2 <= 2 then "(" ++ s2 ++ ")" else s2
  in (leftExp ++ " / " ++ rightExp, 2)

-- Returns Nothing on division by zero.
partialEvaluate :: [(String, Int)] -> Expr -> Maybe Expr
partialEvaluate ids = \case
  Iden x -> case lookup x ids of
    Nothing -> Just $ Iden x
    Just y -> Just $ Lit y
    
  Lit x -> Just $ Lit x

  Plus x y -> case partialEvaluate ids x of
    Nothing -> Nothing
    Just a -> case partialEvaluate ids y of
      Nothing -> Nothing
      Just b -> case (a, b) of
        (Lit c, Lit d) -> Just $ Lit (c + d)
        _ -> Just $ Plus a b

  Minus x y -> case partialEvaluate ids x of
    Nothing -> Nothing
    Just a -> case partialEvaluate ids y of
      Nothing -> Nothing
      Just b -> case (a, b) of
        (Lit c, Lit d) -> Just $ Lit (c - d)
        _ -> Just $ Minus a b

  Mul x y -> case partialEvaluate ids x of
    Nothing -> Nothing
    Just a -> case partialEvaluate ids y of
      Nothing -> Nothing
      Just b -> case (a, b) of
        (Lit c, Lit d) -> Just $ Lit (c * d)
        _ -> Just $ Mul a b

  Div x y -> case partialEvaluate ids x of
    Nothing -> Nothing
    Just a -> case partialEvaluate ids y of
      Nothing -> Nothing
      Just b -> case (a, b) of
        (_, Lit 0) -> Nothing
        (Lit c, Lit d) -> Just $ Lit (c `div` d)
        _ -> Just $ Div a b

negateExpr :: Expr -> Expr
negateExpr = Minus (Lit 0)

-- if the exponent is smaller than 0, this should return 0.
powerExpr :: Expr -> Int -> Expr
powerExpr (Lit 0) 0 = Lit 1
powerExpr e n = case n of
  x | x < 0 -> e `Mul` (Lit 0)
  0 -> (e `Div` e)
  1 -> e
  _ -> Mul e (powerExpr e (n - 1))

modExpr :: Expr -> Expr -> Expr
modExpr x y = Minus x (Mul y (Div x y))

-- Section 3.1: zips and products
zip :: [a] -> [b] -> [(a, b)]
zip a b = case a of
  [] -> []
  (x : xs) -> case b of
    [] -> []
    (y : ys) -> (x, y) : zip xs ys

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex a = zip [0..] a

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f a b = case a of
  [] -> []
  (x : xs) -> case b of
    [] -> []
    (y : ys) -> f x y : zipWith f xs ys

zipWithDefault :: a -> b -> [a] -> [b] -> [(a, b)] -- Zips two lists, filling missing elements with defaults
zipWithDefault a b = \case
  [] -> \case
    [] -> []
    (y : ys) -> (a, y) : zipWithDefault a b [] ys
  (x : xs) -> \case
    [] -> (x, b) : zipWithDefault a b xs []
    (y : ys) -> (x, y) : zipWithDefault a b xs ys

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)

zipEither :: [a] -> [b] -> Either ZipFail [(a, b)]
zipEither = \case
  [] -> \case
    [] -> Right []
    (_ : _) -> Left ErrorFirst
  (x : xs) -> \case
    [] -> Left ErrorSecond
    (y : ys) -> case zipEither xs ys of
      Left err -> Left err
      Right zs -> Right ((x, y) : zs)

-- unzip :: [(a, b)] -> ([a], [b])
-- unzip = \case
--   [] -> ([], [])
--   ((a, b) : xs) -> case unzip xs of
--     (as, bs) -> (a : as, b : bs)

-- unzipFirst :: [(a, b)] -> [a]
-- unzipFirst = fst . unzip

-- unzipSecond :: [(a, b)] -> [b]
-- unzipSecond = snd . unzip

unzip :: [(a, b)] -> ([a], [b])
unzip l = (unzipFirst l, unzipSecond l)

unzipFirst :: [(a, b)] -> [a]
unzipFirst = map fst

unzipSecond :: [(a, b)] -> [b]
unzipSecond = map snd

cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith f a b = [f x y | x <- a, y <- b]

-- Section 3.2: list functions
snoc :: [a] -> a -> [a] -- The opposite of cons!
snoc a b = case a of
  [] -> [b]
  (x : xs) -> x : snoc xs b

take :: Int -> [a] -> [a]
take n = \case
  _ | n <= 0 -> []
  [] -> []
  (x : xs) -> x : take (n - 1) xs

-- The last element of the result (if non-empty) is the last element which satisfies the predicate
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = \case
  [] -> []
  (x : xs) -> if f x then x : takeWhile f xs else []

drop :: Int -> [a] -> [a]
drop n = \case
  [] -> []
  (x : xs) -> if n <= 0
            then x : xs
            else drop (n - 1) xs

-- The first element of the result (if non-empty) is the first element which doesn't satisfy the predicate
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f = \case
  [] -> []
  (x : xs) -> if f x then dropWhile f xs else x : xs

slice :: Int -> Int -> [a] -> [a]
slice a b _ | a >= b = []
slice a b ls | a < 0 = take b (drop a ls)
slice a b ls = take (b - a) (drop a ls)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs
  | n <= 0 = id xs
  | otherwise = case drop (n - 1) xs of
      [] -> []
      (y : ys) -> y : takeEvery n ys

dropEvery :: Int -> [a] -> [a]
dropEvery n xs
  | n <= 1 = []
  | otherwise = case xs of
      [] -> []
      _  -> take (n-1) xs ++ dropEvery n (drop n xs)

-- Removes *consecutive* repeats
nub :: [Int] -> [Int]
nub = \case
  [] -> []
  [x] -> [x]
  (x : y : xs) -> if x == y then nub (y : xs) else x : nub (y : xs)

-- Removes *all* repeats, consecutive or not.
uniq :: [Int] -> [Int]
uniq = \case
  [] -> []
  (x : xs) ->
    if elem x xs
    then x : uniq (filter (/= x) xs)
    else x : uniq xs      

-- Section 3.3: base64
base64Chars :: String -- As defined in the PDF
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

toBase64 :: Integer -> String
toBase64 n
  | n == 0 = "A"
  | n < 0 = '-' : toBase64 (-n)
  | otherwise = toPosBase64 n
  where
    toPosBase64 :: Integer -> String
    toPosBase64 = \case
      0 -> ""
      x -> let (d, m) = x `divMod` 64 in 
        toPosBase64 d ++ [base64Chars !! fromIntegral m]

fromBase64 :: String -> Maybe Integer
fromBase64 s = case s of
  "" -> Just 0
  ('-' : xs) -> maybeMap negate $ fromBase64 xs
  _ -> fromPosBase64 0 (reverse s)
  where    
    fromPosBase64 :: Integer -> String -> Maybe Integer
    fromPosBase64 _ [] = Just 0
    fromPosBase64 n (x : xs) =
      if elem x base64Chars
        then case findIdx x base64Chars of
               Nothing -> Nothing
               Just idx -> case fromPosBase64 (n + 1) xs of
                             Nothing -> Nothing
                             Just rest -> Just (rest + fromIntegral idx * (64 ^ n))
        else Nothing

    findIdx :: Char -> String -> Maybe Int
    findIdx ch = f 0
      where
        f _ [] = Nothing
        f i (y : ys) = if ch == y then Just i else f (i + 1) ys

    reverse :: [a] -> [a]
    reverse = rev []
      where
        rev ls [] = ls
        rev ls (x : xs) = rev (x : ls) xs

    (^) :: Integer -> Integer -> Integer
    (^) _ 0 = 1
    (^) b e = b * (^) b (e - 1)

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

-- todo: ?
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
sumMaybe x y = case x of
  Nothing -> Nothing
  Just a -> case y of
    Nothing -> Nothing
    Just b -> Just $ a + b

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
mapMaybe f = \case
  [] -> []
  (x : xs) -> case f x of
    Nothing -> mapMaybe f xs
    Just y -> y : mapMaybe f xs

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
mapEither f = \case
  [] -> Right []
  (x : xs) -> case f x of
    Left y -> Left y
    Right z -> case mapEither f xs of
      Left y -> Left y
      Right ys -> Right $ z : ys --(f z) : ys

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
productEither x y = case x of
  Left e -> Left e
  Right a -> case y of
    Left e -> Left e
    Right b -> Right $ a * b

liftEither2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
liftEither2 f a b = case a of
  Left x -> Left x
  Right y -> case b of
    Left x -> Left x
    Right z -> Right $ f y z

-- Section 2: Expressions
data Expr = Iden String | Lit Int | Plus Expr Expr | Minus Expr Expr | Mul Expr Expr | Div Expr Expr deriving (Show, Eq)

-- Adds parentheses around sub-expressions (the top level expression never has parentheses).
exprToString :: Expr -> String
exprToString e = toStr e
  where
    toStr (Lit n) = show n
    toStr (Iden s) = s
    toStr (Plus e1 e2) = "(" ++ toStr e1 ++ " + " ++ toStr e2 ++ ")"
    toStr (Minus e1 e2) = "(" ++ toStr e1 ++ " - " ++ toStr e2 ++ ")"
    toStr (Mul e1 e2) = "(" ++ toStr e1 ++ " * " ++ toStr e2 ++ ")"
    toStr (Div e1 e2) = "(" ++ toStr e1 ++ " / " ++ toStr e2 ++ ")"

-- Bonus (25 points): Same as the above, but without unnecessary parentheses
-- exprToString' :: Expr -> String
-- exprToString' e = toStr e
--   where
--     toStr (Lit n) = show n
--     toStr (Iden s) = s
--     toStr (Plus e1 e2) = case e1 of
--       toStr (Lit n) = show n
--       toStr (Iden s) = s
--       toStr (Plus e1 e2) = "(" ++ toStr e1 ++ " + " ++ toStr e2 ++ ")"
--       toStr (Minus e1 e2) = "(" ++ toStr e1 ++ " - " ++ toStr e2 ++ ")"
--       toStr (Mul e1 e2) = "(" ++ toStr e1 ++ " * " ++ toStr e2 ++ ")"
--       toStr (Div e1 e2) = "(" ++ toStr e1 ++ " / " ++ toStr e2 ++ ")"
--         "(" ++ toStr e1 ++ " + " ++ toStr e2 ++ ")"
--       toStr (Minus e1 e2) = "(" ++ toStr e1 ++ " - " ++ toStr e2 ++ ")"
--     toStr (Mul e1 e2) = toStr e1 ++ " * " ++ toStr e2
--     toStr (Div e1 e2) = toStr e1 ++ " / " ++ toStr e2

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
        (Lit _, Lit 0) -> Nothing
        (Lit c, Lit d) -> Just $ Lit (c `div` d)
        _ -> Just $ Div a b

negateExpr :: Expr -> Expr
negateExpr e = Minus (Lit 0) e
-- negateExpr = \case
--   Lit x -> Lit -x
--   Iden x -> Mul (Lit (-1)) (Iden x)
--   Plus x y -> Plus (negateExpr x) (negateExpr y)
--   Minus x y -> Plus (negateExpr x) y
--   Mul x y -> Mul (negateExpr x) y
--   Div x y -> Div (negateExpr x) y

-- if the exponent is smaller than 0, this should return 0.
powerExpr :: Expr -> Int -> Expr
powerExpr e n = case n of
  x | x < 0 -> Lit 0
  0 -> Lit 1
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

-- may fail cause length will check an inf list
-- zipEither [] [] = Right []
-- zipEither a b = if length a > length b then Left ErrorSecond 
--   else if length a < length b then Left ErrorFirst 
--   else Right $ zip a b

unzip :: [(a, b)] -> ([a], [b])
unzip = \case
  [] -> ([], [])
  ((a, b) : xs) -> case unzip xs of
    (as, bs) -> (a : as, b : bs)

unzipFirst :: [(a, b)] -> [a]
unzipFirst = fst . unzip

unzipSecond :: [(a, b)] -> [b]
unzipSecond = snd . unzip

cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith f a b = [f x y | x <- a, y <- b]

-- Section 3.2: list functions
snoc :: [a] -> a -> [a] -- The opposite of cons!
snoc a b = case a of
  [] -> [b]
  (x : xs) -> x : snoc xs b

take :: Int -> [a] -> [a]
take n = \case
  [] -> []
  _ | n <= 0 -> []
  (x : xs) -> x : take (n - 1) xs

-- The last element of the result (if non-empty) is the last element which satisfies the predicate
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = \case
  [] -> []
  (x : xs) -> if f x then x : takeWhile f xs else []

drop :: Int -> [a] -> [a]
-- drop n = \case
--   [] -> []
--   (x : xs) -> case n of
--     0 -> x : xs
--     n' -> drop (n' - 1) xs
drop n = \case
  [] -> []
  (x:xs) -> if n <= 0
            then x : xs
            else drop (n - 1) xs

-- The first element of the result (if non-empty) is the first element which doesn't satisfy the predicate
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f = \case
  [] -> []
  (x : xs) -> if f x then dropWhile f xs else x : xs

slice :: Int -> Int -> [a] -> [a]
slice a b ls = take (b - a) (drop a ls)

takeEvery :: Int -> [a] -> [a]
takeEvery n = \case
  [] -> []
  (x : xs) ->
    if n <= 0 then [] 
    else x : takeEvery n (drop (n - 1) xs)

dropEvery :: Int -> [a] -> [a]
-- todo: remove otherwise style?
dropEvery n xs
  | n <= 0 = xs
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

-- uniq :: [Int] -> [Int]
-- uniq = \case
--   [] -> []
--   (x : xs) ->
--     let res = lookupIdxAndRemove x xs
--     in if fst res
--        then x : uniq (x : snd res)
--        else x : uniq xs
--   where
--     lookupIdxAndRemove :: Int -> [Int] -> (Bool, [Int])
--     lookupIdxAndRemove x = \case
--       [] -> (False, [])
--       (y : ys) ->
--         if x == y
--         then (True, ys)   -- remove the duplicate
--         else let (found, zs) = lookupIdxAndRemove x ys
--              in (found, y : zs)

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
-- fromBase64 n
--   | n == "" = Just 0
--   | "-" : xs = fmap negate (fromBase64 xs)
--   | otherwise = fromPosBase64 0 (reverse n)
--     where
--       fromPosBase64 ::  Integer -> String -> Maybe Integer
--       fromPosBase64 n = \case
--         [] -> Just 0
--         (x : xs) -> if elem x base64Chars 
--                     then fromPosBase64 (n + 1) xs + ((findIdx x base64Chars) * (64 ^ n))
--                     else Nothing
--           where
--             findIdx :: Char -> String -> Integer
--             findIdx x (y : ys) = if x == y then 0 else 1 + findIdx x ys

fromBase64 s = case s of
  "" -> Just 0
  ('-' : xs) -> case fromBase64 xs of
                  Nothing -> Nothing
                  Just x -> Just (0 - x)
  _ -> fromPosBase64 0 (myReverse s)
  where    
    fromPosBase64 :: Integer -> String -> Maybe Integer
    fromPosBase64 _ [] = Just 0
    fromPosBase64 n (x:xs) =
      if elem x base64Chars
        then case findIdx x base64Chars of
               Nothing -> Nothing
               Just idx -> case fromPosBase64 (n + 1) xs of
                             Nothing -> Nothing
                             Just rest -> Just (rest + fromIntegral idx * pow 64 n)
        else Nothing

    findIdx :: Char -> String -> Maybe Int
    findIdx c = go 0
      where
        go _ [] = Nothing
        go i (y : ys) = if c == y then Just i else go (i + 1) ys

    myReverse :: [a] -> [a]
    myReverse = rev []
      where
        rev acc [] = acc
        rev acc (x : xs) = rev (x : acc) xs

    pow :: Integer -> Integer -> Integer
    pow _ 0 = 1
    pow b e = b * pow b (e - 1)

    -- fromPosBase64 n (x:xs) =
    --   if x `elem` base64Chars
    --     then do
    --       rest <- fromPosBase64 (n + 1) xs
    --       let Just i = findIdx x base64Chars
    --       return $ i * (64 ^ n) + rest
    --     else Nothing

    -- findIdx :: Char -> String -> Maybe Integer
    -- findIdx x = go 0
    --   where
    --     go _ [] = Nothing
    --     go i (y:ys)
    --       | x == y = Just i
    --       | otherwise = go (i + 1) ys

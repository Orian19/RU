{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-unused-imports #-}

module Main where

import HW2 qualified as HW2
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

tests :: Test
tests = TestList [maybeTests, eitherTests, exprTests, listTests, listEdgeTests, base64Tests]

maybeTests, eitherTests, exprTests, listTests, listEdgeTests, base64Tests :: Test
maybeTests =
  TestList
    [ "fromMaybe Just" ~: HW2.fromMaybe 1 (Just 2) ~?= (2 :: Int),
      "fromMaybe Nothing" ~: HW2.fromMaybe 1 Nothing ~?= (1 :: Int),
      "maybe with Nothing" ~: HW2.maybe 1 (+ 1) Nothing ~?= (1 :: Int),
      "maybe with Just" ~: HW2.maybe 0 length (Just "foo") ~?= (3 :: Int),
      "maybeHead non-empty" ~: HW2.maybeHead [1, 2, 3] ~?= (Just 1 :: Maybe Int),
      "maybeHead empty" ~: HW2.maybeHead [] ~?= (Nothing :: Maybe Int),
      "maybeLast non-empty" ~: HW2.maybeLast [1, 2, 3] ~?= (Just 3 :: Maybe Int),
      "maybeLast empty" ~: HW2.maybeLast [] ~?= (Nothing :: Maybe Int),
      "maybeMaximum non-empty" ~: HW2.maybeMaximum [1, 2, 3] ~?= (Just 3 :: Maybe Int),
      "maybeMaximum empty" ~: HW2.maybeMaximum [] ~?= (Nothing :: Maybe Int),
      "maybeMinimum non-empty" ~: HW2.maybeMinimum [1, 2, 3] ~?= (Just 1 :: Maybe Int),
      "maybeMinimum empty" ~: HW2.maybeMinimum [] ~?= (Nothing :: Maybe Int),
      "filterMaybe passes" ~: HW2.filterMaybe even (Just 2) ~?= (Just 2 :: Maybe Int),
      "filterMaybe fails" ~: HW2.filterMaybe even (Just 3) ~?= (Nothing :: Maybe Int),
      "sumMaybe both Just" ~: HW2.sumMaybe (Just 1) (Just 2) ~?= (Just 3 :: Maybe Int),
      "sumMaybe with Nothing" ~: HW2.sumMaybe (Just 1) Nothing ~?= (Nothing :: Maybe Int),
      "liftMaybe2 both Just" ~: HW2.liftMaybe2 (*) (Just 2) (Just 3) ~?= (Just 6 :: Maybe Int),
      "liftMaybe2 with Nothing" ~: HW2.liftMaybe2 (*) Nothing (Just 3) ~?= (Nothing :: Maybe Int),
      "mapMaybe example" ~: HW2.mapMaybe (\x -> if even x then Just (x * 2) else Nothing) [1, 2, 3] ~?= ([4] :: [Int]),
      "catMaybes example" ~: HW2.catMaybes [Just 1, Nothing, Just 2, Just 3] ~?= ([1, 2, 3] :: [Int]),
      "concatMaybeMap simple" ~: HW2.concatMaybeMap (\x -> if x > 0 then Just (x * 2) else Nothing) (Just 5) ~?= (Just 10 :: Maybe Int)
    ]
eitherTests =
  TestList
    [ "fromEither Right" ~: HW2.fromEither (42 :: Int) (Right (10 :: Int)) ~?= (10 :: Int),
      "fromEither Left" ~: HW2.fromEither (42 :: Int) (Left "error") ~?= (42 :: Int),
      "catEithers all Right" ~: HW2.catEithers [Right (1 :: Int), Right 2] ~?= (Right [1, 2] :: Either String [Int]),
      "catEithers with Left" ~: HW2.catEithers [Right (1 :: Int), Left "err", Right 2] ~?= (Left "err" :: Either String [Int]),
      "mapEither all Right" ~: HW2.mapEither (\x -> if x > 0 then Right (x * 10) else Left x) [1, 2, 3] ~?= (Right [10, 20, 30] :: Either Int [Int]),
      "mapEither with Left" ~: HW2.mapEither (\x -> if x > 0 then Right (x * 10) else Left (x + 5)) [1, -1, 2, -2] ~?= (Left 4 :: Either Int [Int]),
      "partitionEithers" ~: HW2.partitionEithers [Right "foo", Left (42 :: Int), Left 54, Right "bar"] ~?= ([42, 54], ["foo", "bar"]),
      "liftEither2 both Right" ~: HW2.liftEither2 (*) (Right (2 :: Int)) (Right 3) ~?= (Right 6 :: Either String Int),
      "liftEither2 with Left" ~: HW2.liftEither2 (*) (Right (2 :: Int)) (Left "error") ~?= (Left "error" :: Either String Int),
      "concatEitherMap simple" ~: HW2.concatEitherMap (\x -> if x > 0 then Right (x * 2) else Left "error") (Right (5 :: Int)) ~?= (Right 10 :: Either String Int),
      "either simple" ~: HW2.either (+ 1) (* 2) (Left (3 :: Int)) ~?= (4 :: Int),
      "eitherToMaybe Right" ~: HW2.eitherToMaybe (Right (42 :: Int)) ~?= (Just 42 :: Maybe Int),
      "eitherToMaybe Left" ~: HW2.eitherToMaybe (Left "error" :: Either String Int) ~?= (Nothing :: Maybe Int),
      "mapLeft simple" ~: HW2.mapLeft (+ 1) (Left (3 :: Int)) ~?= (Left 4 :: Either Int Int),
      "productEither simple" ~: HW2.productEither (Right (3 :: Int)) (Right (4 :: Int)) ~?= (Right 12 :: Either String Int),
      "productEither with Left" ~: HW2.productEither (Right (3 :: Int)) (Left "error") ~?= (Left "error" :: Either String Int)
    ]
exprTests =
  TestList
    [ "Bonus - exprToString' precedence and associativity 1"
        ~: HW2.exprToString' ((HW2.Iden "x" `HW2.Plus` HW2.Lit 42 `HW2.Plus` HW2.Iden "y") `HW2.Mul` (HW2.Lit 2) `HW2.Plus` (HW2.Lit 3))
        ~?= "(x + 42 + y) * 2 + 3",
      "Bonus - exprToString' precedence and associativity 2"
        ~: HW2.exprToString' (HW2.Lit 1 `HW2.Minus` (HW2.Lit 2 `HW2.Minus` HW2.Lit 3))
        ~?= "1 - (2 - 3)",
      "Bonus - exprToString' precedence and associativity 3"
        ~: HW2.exprToString' ((HW2.Lit 1 `HW2.Minus` HW2.Lit 2) `HW2.Minus` HW2.Lit 3)
        ~?= "1 - 2 - 3",
      "exprToString basic addition"
        ~: HW2.exprToString (HW2.Plus (HW2.Lit 1) (HW2.Lit 2))
        ~?= "1 + 2",
      "exprToString with division and parentheses"
        ~: HW2.exprToString (HW2.Plus (HW2.Iden "x") (HW2.Div (HW2.Lit 2) (HW2.Lit 3)))
        ~?= "x + (2 / 3)",
      "partialEvaluate with one substitution"
        ~: HW2.partialEvaluate
          [("x", 42)]
          (HW2.Mul (HW2.Plus (HW2.Lit 1) (HW2.Iden "x")) (HW2.Minus (HW2.Iden "y") (HW2.Lit 2)))
        ~?= Just (HW2.Mul (HW2.Lit 43) (HW2.Minus (HW2.Iden "y") (HW2.Lit 2))),
      "partialEvaluate division by zero"
        ~: HW2.partialEvaluate
          [("x", 42)]
          (HW2.Mul (HW2.Plus (HW2.Lit 1) (HW2.Iden "x")) (HW2.Div (HW2.Iden "y") (HW2.Lit 0)))
        ~?= Nothing,
      "partialEvaluate with two substitutions"
        ~: HW2.partialEvaluate
          [("x", 42), ("y", 3)]
          (HW2.Mul (HW2.Plus (HW2.Lit 1) (HW2.Iden "x")) (HW2.Minus (HW2.Iden "y") (HW2.Lit 2)))
        ~?= Just (HW2.Lit 43),
      "partialEvaluate with no substitutions"
        ~: HW2.partialEvaluate
          []
          (HW2.Mul (HW2.Plus (HW2.Lit 1) (HW2.Iden "x")) (HW2.Minus (HW2.Iden "y") (HW2.Lit 2)))
        ~?= Just (HW2.Mul (HW2.Plus (HW2.Lit 1) (HW2.Iden "x")) (HW2.Minus (HW2.Iden "y") (HW2.Lit 2))),
      "partialEvaluate with empty expression"
        ~: HW2.partialEvaluate [] (HW2.Lit 42)
        ~?= Just (HW2.Lit 42),
      "partialEvaluate with empty expression and substitutions"
        ~: HW2.partialEvaluate [("x", 42)] (HW2.Lit 42)
        ~?= Just (HW2.Lit 42),
      "partialEvaluate with empty expression and empty substitutions"
        ~: HW2.partialEvaluate [] (HW2.Lit 42)
        ~?= Just (HW2.Lit 42),
      "negateExpr test"
        ~: HW2.partialEvaluate [] (HW2.negateExpr (HW2.Minus (HW2.Lit 20) (HW2.Lit 62)))
        ~?= Just (HW2.Lit 42),
      "powerExpr with 0 exponent"
        ~: HW2.partialEvaluate [] (HW2.powerExpr (HW2.Lit 42) 0)
        ~?= Just (HW2.Lit 1),
      "powerExpr 0^0"
        ~: HW2.partialEvaluate [] (HW2.powerExpr (HW2.Lit 0) 0)
        ~?= Just (HW2.Lit 1),
      "powerExpr with variable and exponent"
        ~: HW2.partialEvaluate
          [("x", 2)]
          (HW2.powerExpr (HW2.Plus (HW2.Iden "x") (HW2.Lit 4)) 3)
        ~?= Just (HW2.Lit 216),
      "powerExpr with negative exponent"
        ~: HW2.partialEvaluate
          []
          (HW2.powerExpr (HW2.Plus (HW2.Lit 2) (HW2.Lit 3)) (-1))
        ~?= Just (HW2.Lit 0),
      "powerExpr with division by zero and negative exponent"
        ~: HW2.partialEvaluate
          []
          (HW2.powerExpr (HW2.Div (HW2.Lit 2) (HW2.Lit 0)) (-1))
        ~?= Nothing,
      "powerExpr with division by zero and zero exponent"
        ~: HW2.partialEvaluate
          []
          (HW2.powerExpr (HW2.Div (HW2.Lit 2) (HW2.Lit 0)) 0)
        ~?= Nothing,
      "modExpr with evaluation"
        ~: HW2.partialEvaluate
          [("x", 2), ("y", 3)]
          (HW2.modExpr (HW2.Lit 42) (HW2.Plus (HW2.Iden "x") (HW2.Iden "y")))
        ~?= Just (HW2.Lit 2),
      "modExpr division by zero"
        ~: HW2.partialEvaluate
          []
          (HW2.modExpr (HW2.Lit 42) (HW2.Lit 0))
        ~?= Nothing
    ]

infiniteRepeats :: [Int]
infiniteRepeats = [tripleX | x <- [1 ..], tripleX <- [x, x, x]]

listTests =
  TestList
    [ "take simple" ~: HW2.take 2 [1, 2, 3] ~?= ([1, 2] :: [Int]),
      "take infinite" ~: HW2.take 4 [1 ..] ~?= ([1, 2, 3, 4] :: [Int]),
      "take empty" ~: HW2.take 0 [1, 2, 3] ~?= ([] :: [Int]),
      "snoc simple" ~: HW2.snoc [1, 2, 3] 4 ~?= ([1, 2, 3, 4] :: [Int]),
      "snoc infinite" ~: (HW2.take 5 $ HW2.snoc [1 ..] 0) ~?= ([1, 2, 3, 4, 5] :: [Int]),
      "drop simple" ~: HW2.drop 2 [1, 2, 3] ~?= ([3] :: [Int]),
      "takeWhile simple" ~: HW2.takeWhile (< 5) [1 ..] ~?= ([1, 2, 3, 4] :: [Int]),
      "dropWhile simple" ~: HW2.take 5 (HW2.dropWhile (< 5) [1 ..]) ~?= ([5, 6, 7, 8, 9] :: [Int]),
      "slice in range" ~: HW2.slice 2 5 [1 ..] ~?= ([3, 4, 5] :: [Int]),
      "takeEvery 3" ~: (HW2.take 5 $ HW2.takeEvery 3 [1 ..]) ~?= ([3, 6, 9, 12, 15] :: [Int]),
      "dropEvery 3" ~: (HW2.take 5 $ HW2.dropEvery 3 [1 ..]) ~?= ([1, 2, 4, 5, 7] :: [Int]),
      "nub consecutive" ~: HW2.nub [1, 1, 2, 2, 3, 3, 4, 4, 4, 5] ~?= ([1, 2, 3, 4, 5] :: [Int]),
      "nub infinite" ~: (HW2.take 5 $ HW2.nub [1 ..]) ~?= ([1, 2, 3, 4, 5] :: [Int]),
      "nub empty" ~: HW2.nub [] ~?= ([] :: [Int]),
      "take infinite repeats" ~: HW2.take 15 infiniteRepeats ~?= ([1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5] :: [Int]),
      "nub infinite repeats" ~: (HW2.take 15 $ HW2.nub infiniteRepeats) ~?= ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] :: [Int]),
      "uniq all duplicates" ~: HW2.uniq [1, 2, 1, 3, 2, 4] ~?= ([1, 2, 3, 4] :: [Int]),
      "zip simple" ~: HW2.zip [1, 2, 3] ['a', 'b', 'c'] ~?= ([(1, 'a'), (2, 'b'), (3, 'c')] :: [(Int, Char)]),
      "zipWith simple" ~: HW2.zipWith (+) [1, 2, 3] [4, 5, 6] ~?= ([5, 7, 9] :: [Int]),
      "zipWith infinite" ~: HW2.zipWith (+) [1, 2] [4, 5 ..] ~?= ([5, 7] :: [Int]),
      "zipWithIndex simple" ~: HW2.zipWithIndex ["a", "b", "c"] ~?= ([(0, "a"), (1, "b"), (2, "c")] :: [(Int, String)]),
      "zipWithDefault simple" ~: HW2.zipWithDefault 0 'a' [1, 2, 3] "foobar" ~?= ([(1, 'f'), (2, 'o'), (3, 'o'), (0, 'b'), (0, 'a'), (0, 'r')] :: [(Int, Char)]),
      "zipWithDefault empty" ~: HW2.zipWithDefault 0 'a' [] [] ~?= ([] :: [(Int, Char)]),
      "zipWithDefault unbalanced lists" ~: HW2.zipWithDefault 0 'a' [1 .. 6] "foo" ~?= ([(1, 'f'), (2, 'o'), (3, 'o'), (4, 'a'), (5, 'a'), (6, 'a')] :: [(Int, Char)]),
      "zipWithDefault unbalanced lists 2" ~: HW2.zipWithDefault 0 'a' [] "foo" ~?= ([(0, 'f'), (0, 'o'), (0, 'o')] :: [(Int, Char)]),
      "zipWithDefault infinite lists" ~: (HW2.take 10 $ HW2.zipWithDefault 1 'a' [1 ..] "foo") ~?= ([(1, 'f'), (2, 'o'), (3, 'o'), (4, 'a'), (5, 'a'), (6, 'a'), (7, 'a'), (8, 'a'), (9, 'a'), (10, 'a')] :: [(Int, Char)]),
      "zipEither simple" ~: HW2.zipEither [1, 2, 3] "foo" ~?= (Right [(1, 'f'), (2, 'o'), (3, 'o')] :: Either HW2.ZipFail [(Int, Char)]),
      "zipEither first error" ~: HW2.zipEither [1, 2] "foobar" ~?= (Left HW2.ErrorFirst :: Either HW2.ZipFail [(Int, Char)]),
      "zipEither second error" ~: HW2.zipEither [1 ..] "foobar" ~?= (Left HW2.ErrorSecond :: Either HW2.ZipFail [(Int, Char)]),
      "unzip simple" ~: HW2.unzip [(1, 'a'), (2, 'b')] ~?= (([1, 2], ['a', 'b']) :: ([Int], [Char])),
      "unzipFirst simple" ~: HW2.unzipFirst [(1, 'a'), (2, 'b')] ~?= ([1, 2] :: [Int]),
      "unzipSecond simple" ~: HW2.unzipSecond ([(1, 'a'), (2, 'b')] :: [(Int, Char)]) ~?= (['a', 'b'] :: [Char]),
      "cartesianWith multiplication" ~: HW2.cartesianWith (*) [10, 20] [3, 4, 5, 6] ~?= ([30, 40, 50, 60, 60, 80, 100, 120] :: [Int])
    ]

listEdgeTests =
  TestList
    [ "take full" ~: HW2.take 4 [1, 2, 3] ~?= ([1, 2, 3] :: [Int]),
      "take 0 undefined" ~: HW2.take 0 undefined ~?= ([] :: [Int]),
      "take negative" ~: HW2.take (-1) undefined ~?= ([] :: [Int]),
      "takeWhile fails" ~: HW2.takeWhile (< 1) [1 ..] ~?= ([] :: [Int]),
      "drop beyond" ~: HW2.drop 4 [1, 2, 3] ~?= ([] :: [Int]),
      "drop 0" ~: HW2.drop 0 [1, 2, 3] ~?= ([1, 2, 3] :: [Int]),
      "drop negative" ~: HW2.drop (-1) [1, 2, 3] ~?= ([1, 2, 3] :: [Int]),
      "slice same index undefined" ~: HW2.slice 3 3 undefined ~?= ([] :: [Int]),
      "slice out of order undefined" ~: HW2.slice 5 2 undefined ~?= ([] :: [Int]),
      "slice with negative index" ~: HW2.slice (-2) 5 [1 ..] ~?= ([1, 2, 3, 4, 5] :: [Int]),
      "takeEvery 4 on short list" ~: HW2.takeEvery 4 [1, 2, 3] ~?= ([] :: [Int]),
      "takeEvery 1" ~: HW2.takeEvery 1 [1, 2, 3] ~?= ([1, 2, 3] :: [Int]),
      "takeEvery 0" ~: HW2.takeEvery 0 [1, 2, 3] ~?= ([1, 2, 3] :: [Int]),
      "takeEvery -1" ~: HW2.takeEvery (-1) [1, 2, 3] ~?= ([1, 2, 3] :: [Int]),
      "dropEvery 4 on short list" ~: HW2.dropEvery 4 [1, 2, 3] ~?= ([1, 2, 3] :: [Int]),
      "dropEvery 1" ~: HW2.dropEvery 1 undefined ~?= ([] :: [Int]),
      "dropEvery 0" ~: HW2.dropEvery 0 undefined ~?= ([] :: [Int]),
      "dropEvery -1" ~: HW2.dropEvery (-1) undefined ~?= ([] :: [Int]),
      "take on infinite repeated list" ~: HW2.take 10 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1] ~?= ([1, 1, 1, 1, 1, 1, 1, 1, 1, 1] :: [Int])
    ]

toBase64Check, fromBase64Check :: Integer -> String -> Test
toBase64Check n s = TestLabel ("toBase64 " ++ show n) (HW2.toBase64 n ~?= s)
fromBase64Check n s = TestLabel ("fromBase64 " ++ show s) (HW2.fromBase64 s ~?= Just n)

base64Tests =
  TestList
    [ toBase64Check 0 "A",
      fromBase64Check 0 "A",
      toBase64Check 509701384549 "Haskell",
      fromBase64Check 509701384549 "Haskell",
      toBase64Check (-509701384549) "-Haskell",
      fromBase64Check (-509701384549) "-Haskell",
      "fromBase64 invalid" ~: HW2.fromBase64 "!" ~?= Nothing,
      "fromBase64 double dash" ~: HW2.fromBase64 "--Haskell" ~?= Just 509701384549,
      "fromBase64 invalid end" ~: HW2.fromBase64 "Haskell?" ~?= Nothing
    ]
{-# LANGUAGE GHC2024 #-}

import GHC.Float (floorDouble)
import HW1 as HW1
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

(~:) :: String -> Assertion -> TestTree
(~:) = testCase

infixr 0 ~:

main :: IO ()
main =
  defaultMain $ do
    localOption (mkTimeout 1_000_000) $
      testGroup
        "HW1 Automatic Grading Tests"
        [ section1Tests
        , section2Tests
        , section3Tests
        , section4Tests
        , bonus
        ]

section1Tests :: TestTree
section1Tests =
  testGroup
    "Section 1 Tests"
    [ "1 curry3" ~: (curry3 (\(a, b, c) -> a + b + c) 1 2 3) @?= (6 :: Int)
    , "1 uncurry3" ~: (uncurry3 ((+) .: (+)) (1, 2, 3)) @?= (6 :: Int)
    , "1 fst3" ~: (fst3 (1, 'x', "foo")) @?= 1
    , "1 snd3" ~: (snd3 (1, 'x', "foo")) @?= 'x'
    , "1 thd3" ~: (thd3 (1, 'x', "foo")) @?= "foo"
    , "1 dropFst" ~: (dropFst (1, 'x', "foo")) @?= ('x', "foo")
    , "1 dropSnd" ~: (dropSnd (1, 'x', "foo")) @?= (1, "foo")
    , "1 dropThd" ~: (dropThd (1, 'x', "foo")) @?= (1, 'x')
    , "0.5 mapPair int" ~: (mapPair (+ 1) (1, 2)) @?= (2, 3)
    , "0.5 mapPair str" ~: (mapPair (++ "x") ("foo", "bar")) @?= ("foox", "barx")
    , "1 pairApply" ~: (pairApply (+ 1) show 3) @?= (4, "3")
    , "1 const" ~: (HW1.const 1 "foo") @?= 1
    , "1 constSecond" ~: (constSecond "bar" 2) @?= 2
    , "1 (.:)" ~: ((aux2 .: aux1 1 "foo" [1, 2, 3]) 'x' 3.14) @?= 1
    , "1 (.:.)" ~: ((aux2 .:. aux1 1 "foo") [1, 2, 3] 'x' 3.14) @?= 1
    , "1 (.::)" ~: ((aux2 .:: aux1 1) "foo" [1, 2, 3] 'x' 3.14) @?= 1
    , "1 (.::.)" ~: ((aux2 .::. aux1) 1 "foo" [1, 2, 3] 'x' 3.14) @?= 1
    , -- Just verifies compilation, since an infinite loop is also a correct implementation
      "1 impossible" ~: True @?= (True || impossible 0)
    ]
 where
  aux1 :: Int -> String -> [Int] -> Char -> Double -> (Int, String, [Int], Char, Double)
  aux1 = (,,,,)
  aux2 (a, _, _, _, _) = a

-- ----------------------------------------------------------------
-- -- Section 2: Number Properties & Manipulation Tests
-- ----------------------------------------------------------------

section2Tests :: TestTree
section2Tests =
  testGroup
    "Section 2 Tests"
    [ testGroup
        "countDigits"
        [ "1 0" ~: (countDigits 0) @?= 1
        , "1 positive" ~: (countDigits 12345) @?= 5
        , "1 negative" ~: (countDigits (-12345)) @?= 5
        ]
    , testGroup
        "sumDigits"
        [ "3 positive" ~: (sumDigits 1023981) @?= 24
        , "1 negative" ~: (sumDigits $ -120123141239800) @?= 37
        ]
    , testGroup
        "reverseDigits"
        [ "1 positive" ~: (reverseDigits 1023981) @?= 1893201
        , "3 positive with zero" ~: (reverseDigits 213489700) @?= 7984312
        , "1 negative" ~: (reverseDigits $ -12381089) @?= -98018321
        , "1 negative with zero" ~: (reverseDigits $ -120123141239800) @?= -8932141321021
        ]
    , testGroup
        "collatzLength"
        [ "1 1" ~: (collatzLength 1) @?= 0
        , "5 12345" ~: (collatzLength 12345) @?= 50
        ]
    ]

section3Tests :: TestTree
section3Tests =
  testGroup
    "Section 3 Tests"
    [ testGroup
        "basic"
        [ testGroup
            "nthGen"
            [ "1 0" ~: (nthGen 0 nats) @?= 0
            , "1 too big" ~: (nthGen (floorDouble 1e50) oneToFive) @?= 5
            , "1 42" ~: (nthGen 42 nats) @?= 42
            ]
        , testGroup
            "nextGen"
            [ "1 nats" ~: (my_currentGen $ nextGen nats) @?= 0
            , "1 empty" ~: (my_currentGen $ nextGen ((+ 1), (== 0), 42)) @?= 43
            ]
        , testGroup
            "hasNext"
            [ "1 false" ~: (hasNext ((+ 1), (>= 5), 0)) @?= False
            , "1 true" ~: (hasNext ((+ 1), (<= 5), 0)) @?= True
            ]
        , testGroup "lengthGen" $
            [ "1 empty" ~: (lengthGen emptyGen) @?= 0
            , "1 finite" ~: (lengthGen oneToFive) @?= 5
            ]
        , testGroup "hasLengthOfAtLeast" $
            [ "1 empty true" ~: (hasLengthOfAtLeast 0 emptyNats) @?= True
            , "1 empty false" ~: (hasLengthOfAtLeast 1 emptyGen) @?= False
            , "2 finite true" ~: (hasLengthOfAtLeast 5 oneToFive) @?= True
            , "1 finite false" ~: (hasLengthOfAtLeast 6 oneToFive) @?= False
            , "2 infinite" ~: (hasLengthOfAtLeast 42 nats) @?= True
            ]
        ]
    , testGroup
        "generators"
        [ "1 constGen" ~: (nthGen 10 $ constGen "foobar") @?= "foobar"
        , "1 forever" ~: (nthGen 9 $ foreverGen (++ "x") "") @?= Prelude.replicate 10 'x'
        , "2 lengthGen" ~: (lengthGen emptyGen) @?= 0
        ]
    , testGroup
        "interactions"
        [ testGroup
            "sumGen"
            [ "1 empty" ~: (sumGen emptyGen) @?= 0
            , "1 empty sum" ~: (sumGen emptyGen) @?= 0
            , "1 finite sum" ~: (sumGen oneToFive) @?= 15
            ]
        , testGroup
            "anyGen"
            [ "1 empty" ~: (anyGen odd emptyGen) @?= False
            , "1 finite true" ~: (anyGen odd oneToFive) @?= True
            , "1 finite false" ~: (anyGen (> 5) oneToFive) @?= False
            , "1 infinite" ~: (anyGen (> 1024) nats) @?= True
            ]
        , testGroup
            "andAlso"
            [ "1 Fails on original condition" ~: lengthGen (andAlso (< 10) oneToFive) @?= 5
            , "1 Fails on new condition" ~: lengthGen (andAlso (< 9) nats) @?= 10
            , "1 Neither condition fails" ~: hasLengthOfAtLeast 42 (andAlso (>= -1) nats) @?= True
            ]
        , testGroup
            "integers"
            [ "3 positives" ~: 42 `elem` my_toList integers @?= True
            , "3 negatives" ~: (-192) `elem` my_toList integers @?= True
            ]
        ]
    ]
 where
  nats :: Generator Integer
  nats = ((+ 1), Prelude.const True, (-1))
  oneToFive = ((+ 1), (< 5), 0)
  emptyNats = ((+ 1), (< 0), 0)

-- Avoid ambiguities
my_currentGen :: Generator a -> a
my_currentGen = thd3
my_toList :: Generator a -> [a]
my_toList gen = if hasNext gen then go $ nextGen gen else []
 where
  go g = my_currentGen g : if hasNext g then go (nextGen g) else []

section4Tests :: TestTree
section4Tests =
  testGroup
    "Section 4 Tests"
    [ "5 isPrime" ~: filter isPrime [-10 .. 200] @?= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199]
    , testGroup
        "nextPrime"
        [ "0.5 Negative" ~: (nextPrime (-12831)) @?= 2
        , "0.5 Positive" ~: (nextPrime 12831) @?= 12841
        ]
    , "5 primes" ~: take 42 (my_toList primes) @?= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181]
    , testGroup
        "isHappy"
        [ "6 isHappy" ~: filter isHappy [0 .. 200] @?= [1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100, 103, 109, 129, 130, 133, 139, 167, 176, 188, 190, 192, 193]
        , "1 negative" ~: filter isHappy [-200 .. 200] @?= [-193, -192, -190, -188, -176, -167, -139, -133, -130, -129, -109, -103, -100, -97, -94, -91, -86, -82, -79, -70, -68, -49, -44, -32, -31, -28, -23, -19, -13, -10, -7, -1, 1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100, 103, 109, 129, 130, 133, 139, 167, 176, 188, 190, 192, 193]
        ]
    , "6 isArmstrong" ~: filter isArmstrong [-100 .. 10000] @?= [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474]
    , "5 isPalindromicPrime" ~: filter isPalindromicPrime [-10 .. 20000] @?= [2, 3, 5, 7, 11, 101, 131, 151, 181, 191, 313, 353, 373, 383, 727, 757, 787, 797, 919, 929, 10301, 10501, 10601, 11311, 11411, 12421, 12721, 12821, 13331, 13831, 13931, 14341, 14741, 15451, 15551, 16061, 16361, 16561, 16661, 17471, 17971, 18181, 18481, 19391, 19891, 19991]
    ]
bonus =
  testGroup
    "Bonus Tests"
    [ localOption (mkTimeout 10_000_000) $ "14 divisors (by way of isPerfect)" ~: filter isPerfect [1 .. 1000] @?= [6, 28, 496]
    , localOption (mkTimeout 10_000_000) $ "1 negative divisors" ~: my_toList (divisors $ -12345) @?= [1, 3, 5, 15, 823, 2469, 4115]
    ]
 where
  isPerfect :: Integer -> Bool
  isPerfect n = sumGen (divisors n) == n

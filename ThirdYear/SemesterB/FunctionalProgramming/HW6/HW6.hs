{-# LANGUAGE GHC2024 #-}
-- When you're done, ghc -Wall -Werror HW6.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW6 where

import qualified Data.Set as S

--
-- Section 1: Binary Tree as Traversable
--

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Tree l d r) = Tree (fmap f l) (f d) (fmap f r)

-- In-order traversal
instance Foldable Tree where
  foldr _ acc Empty = acc
  foldr f acc (Tree l d r) = foldr f (f d (foldr f acc r)) l

-- In-order traversal
instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Tree l d r) = Tree <$> traverse f l <*> f d <*> traverse f r

-- State as defined in the lectures
data State s a = State {runState :: s -> (a, s)}
instance Functor (State s) where
  fmap f sa = State $ \s -> let (a, s1) = runState sa s in (f a, s1)
instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  liftA2 f sa sb = State $ \s ->
    let (a, s1) = runState sa s
        (b, s2) = runState sb s1
     in (f a b, s2)
instance Monad (State s) where
  sa >>= f = State $ \s ->
    let (a, s1) = runState sa s
        sb = f a
     in runState sb s1

-- Labeling: Start from 1, use in-order.
label :: Tree a -> Tree (Int, a)
label tree = fst $ runState (traverse labelValue tree) 1
  where
    labelValue :: a -> State Int (Int, a)
    labelValue value = State $ \counter -> ((counter, value), counter + 1)


-- Section 2: Destroy Iran's Nuclear sites
--
-- Implement a simple "Battleship" style game, where the battleship are single points.
--
-- play the suggested solution


{-
ghci> config = GameConfig 5 5 (S.fromList [Coordinate 0 0]) 5
ghci> play config
\ 01234
0 *****
1 *****
2 *****
3 *****
4 *****

Guess: 0 0

\ 01234
0 X****
1 *****
2 *****
3 *****
4 *****

Guess: 3 4

\ 01234
0 X****
1 *****
2 *****
3 ****!
4 *****

Guess: 4 4

\ 01234
0 X****
1 *****
2 *****
3 ****!
4 ****!

Guess: 2 3

You win!!!!!!
-}

data Coordinate = Coordinate {row :: Int, col :: Int} deriving (Eq, Ord, Show)

data GameConfig = GameConfig
  { numRows :: Int
  , numColumns :: Int
  , sites :: S.Set Coordinate
  , maxGuesses :: Int
  }
  deriving (Eq, Ord, Show)

data GameState = GameState
  { config :: GameConfig
  , hits :: S.Set Coordinate
  , misses :: S.Set Coordinate
  , guessesSoFar :: Int
  }
  deriving (Eq, Ord, Show)

data GameStatus = Running | Win | Lose

data Cell = Hidden | Hit | Miss

-- Entry point

play :: GameConfig -> IO ()

-- Printing / rendering

renderGameState :: GameState -> [[Cell]]

showCell :: Cell -> Char

showCells :: Int -> Int -> [[Cell]] -> String

showBoard :: GameState -> String

-- Initiaoization

initGameState :: GameConfig -> GameState

-- Game flow

gameStatus :: GameState -> GameStatus

nextGameState :: Coordinate -> GameState -> GameState

withGameState :: GameState -> IO ()

-- win / lose functions, pre-defined.

win :: IO ()
win = putStrLn "You win!!!"

lose :: IO ()
lose = putStrLn "Game over :((("

 -- Input parsing, pre-defined.

readCoordinate :: IO (Either String Coordinate)
readCoordinate = parseCoordinate <$> getLine

-- | Parses a string into two integers.
--   Returns Left an error message if parsing fails, Right (a, b) otherwise.
parseCoordinate :: String -> Either String Coordinate
parseCoordinate s =
  case words s of
    [s1, s2] -> do
      n1 <- readEither s1
      n2 <- readEither s2
      pure $ Coordinate n1 n2
    _ -> Left "Input must contain exactly two space-separated integers."

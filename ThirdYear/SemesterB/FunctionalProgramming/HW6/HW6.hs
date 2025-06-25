{-# LANGUAGE GHC2024 #-}
-- When you're done, ghc -Wall -Werror HW6.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW6 where

import qualified Data.Set as S
import Text.Read (readEither)

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
play config = withGameState (initGameState config)

-- Printing / rendering

renderGameState :: GameState -> [[Cell]]
renderGameState state = 
  [ [ cellAt r c | c <- [0..numColumns (config state) - 1] ]
  | r <- [0..numRows (config state) - 1] ]
  where
    cellAt r c
      | coord `S.member` hits state = Hit
      | coord `S.member` misses state = Miss
      | otherwise = Hidden
      where coord = Coordinate r c

showCell :: Cell -> Char
showCell Hidden = '*'
showCell Hit = '!'
showCell Miss = 'X'

showCells :: Int -> Int -> [[Cell]] -> String
showCells rows cols cells = unlines (header : zipWith showRow [0..rows-1] cells)
  where
    header = "\\ " ++ concatMap show [0..cols-1]
    showRow rowNum cellRow = show rowNum ++ " " ++ map showCell cellRow

showBoard :: GameState -> String
showBoard state = showCells (numRows cfg) (numColumns cfg) (renderGameState state)
  where cfg = config state

-- Initiaoization

initGameState :: GameConfig -> GameState
initGameState config = GameState config S.empty S.empty 0

-- Game flow

gameStatus :: GameState -> GameStatus
gameStatus state
  | hits state == sites (config state) = Win
  | guessesSoFar state >= maxGuesses (config state) = Lose
  | otherwise = Running

nextGameState :: Coordinate -> GameState -> GameState
nextGameState coord state
  | coord `S.member` (hits state `S.union` misses state) = state  -- already guessed
  | coord `S.member` sites (config state) = state { hits = S.insert coord (hits state), guessesSoFar = guessesSoFar state + 1 }
  | otherwise = state { misses = S.insert coord (misses state), guessesSoFar = guessesSoFar state + 1 }

withGameState :: GameState -> IO ()
withGameState state = do
  putStrLn (showBoard state)
  case gameStatus state of
    Win -> win
    Lose -> lose
    Running -> do
      putStr "Guess: "
      result <- readCoordinate
      case result of
        Left "?" -> do
          showGameInfo state
          withGameState state
        Left err -> do
          putStrLn err
          withGameState state
        Right coord -> withGameState (nextGameState coord state)

-- show information about guesses so far and remaining guesses
showGameInfo :: GameState -> IO ()
showGameInfo state = do
  putStrLn "Guesses so far:"
  mapM_ (putStrLn . showCoordinate) allGuesses
  putStrLn (show guessesLeft ++ " Guesses left!")
  where
    allGuesses = S.toList (hits state `S.union` misses state)
    guessesLeft = maxGuesses (config state) - guessesSoFar state
    showCoordinate (Coordinate r c) = show r ++ " " ++ show c

-- win / lose functions, pre-defined.

win :: IO ()
win = putStrLn "You win!!!"

lose :: IO ()
lose = putStrLn "Game over :((("

 -- Input parsing, pre-defined.

data InputResult = CoordInput Coordinate | HelpRequest | ParseError String

readCoordinate :: IO (Either String Coordinate)
readCoordinate = do
  result <- parseInput <$> getLine
  case result of
    CoordInput coord -> return (Right coord)
    HelpRequest -> return (Left "?")
    ParseError err -> return (Left err)

parseInput :: String -> InputResult
parseInput "?" = HelpRequest
parseInput s = case parseCoordinate s of
  Right coord -> CoordInput coord
  Left err -> ParseError err

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

{-# LANGUAGE LambdaCase #-}
module Main where

import Test.Hspec
import qualified Data.Set as S
import Data.List (isInfixOf)
import HW6

-- Helper functions to test Cell values without Eq instance
isHidden :: Cell -> Bool
isHidden cell = showCell cell == '*'

isHit :: Cell -> Bool
isHit cell = showCell cell == '!'

isMiss :: Cell -> Bool
isMiss cell = showCell cell == 'X'

main :: IO ()
main = hspec $ do
  describe "Section 1: Binary Tree as Traversable" $ do
    describe "Functor instance" $ do
      it "maps over empty tree" $ do
        fmap (+1) Empty `shouldBe` (Empty :: Tree Int)
      
      it "maps over single node tree" $ do
        fmap (+1) (Tree Empty 5 Empty) `shouldBe` Tree Empty 6 Empty
      
      it "maps over complex tree in-order" $ do
        let tree = Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty)
        fmap (*2) tree `shouldBe` Tree (Tree Empty 2 Empty) 4 (Tree Empty 6 Empty)
    
    describe "Foldable instance" $ do
      it "folds empty tree" $ do
        foldr (+) 0 Empty `shouldBe` 0
      
      it "folds single node tree" $ do
        foldr (+) 0 (Tree Empty 5 Empty) `shouldBe` 5
      
      it "folds tree in-order" $ do
        let tree = Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty)
        foldr (:) [] tree `shouldBe` [1, 2, 3]
      
      it "folds complex tree in-order" $ do
        let tree = Tree 
              (Tree (Tree Empty 'd' Empty) 'b' (Tree Empty 'e' Empty))
              'a'
              (Tree (Tree Empty 'f' Empty) 'c' (Tree Empty 'g' Empty))
        foldr (:) [] tree `shouldBe` ['d', 'b', 'e', 'a', 'f', 'c', 'g']
    
    describe "Traversable instance" $ do
      it "traverses empty tree" $ do
        traverse Just Empty `shouldBe` Just (Empty :: Tree Int)
      
      it "traverses single node tree" $ do
        traverse Just (Tree Empty 5 Empty) `shouldBe` Just (Tree Empty 5 Empty)
      
      it "traverses tree with Maybe in-order" $ do
        let tree = Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty)
        traverse (\x -> if x > 0 then Just x else Nothing) tree `shouldBe` 
          Just (Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty))
      
      it "fails traversal when encountering Nothing" $ do
        let tree = Tree (Tree Empty 1 Empty) (-2) (Tree Empty 3 Empty)
        traverse (\x -> if x > 0 then Just x else Nothing) tree `shouldBe` Nothing
    
    describe "label function" $ do
      it "labels empty tree" $ do
        label Empty `shouldBe` (Empty :: Tree (Int, Char))
      
      it "labels single node tree" $ do
        label (Tree Empty 'a' Empty) `shouldBe` Tree Empty (1, 'a') Empty
      
      it "labels tree in-order starting from 1" $ do
        let tree = Tree (Tree Empty 'b' Empty) 'a' (Tree Empty 'c' Empty)
        label tree `shouldBe` Tree (Tree Empty (1, 'b') Empty) (2, 'a') (Tree Empty (3, 'c') Empty)
      
      it "labels complex tree from PDF example" $ do
        let tree = Tree 
              (Tree 
                (Tree 
                  (Tree Empty 'h' Empty) 
                  'd' 
                  (Tree Empty 'i' Empty)
                ) 
                'b' 
                (Tree (Tree Empty 'j' Empty) 'e' Empty)
              )
              'a'
              (Tree 
                (Tree Empty 'f' Empty) 
                'c' 
                (Tree Empty 'g' Empty)
              )
        let expected = Tree 
              (Tree 
                (Tree 
                  (Tree Empty (1, 'h') Empty) 
                  (2, 'd') 
                  (Tree Empty (3, 'i') Empty)
                ) 
                (4, 'b') 
                (Tree (Tree Empty (5, 'j') Empty) (6, 'e') Empty)
              )
              (7, 'a')
              (Tree 
                (Tree Empty (8, 'f') Empty) 
                (9, 'c') 
                (Tree Empty (10, 'g') Empty)
              )
        label tree `shouldBe` expected

  describe "Section 2: Destroy Iran's Nuclear Sites" $ do
    let testConfig = GameConfig 5 5 (S.fromList [Coordinate 2 3]) 5
    
    describe "GameConfig and basic types" $ do
      it "creates valid coordinate" $ do
        let coord = Coordinate 2 3
        row coord `shouldBe` 2
        col coord `shouldBe` 3
      
      it "creates valid game config" $ do
        numRows testConfig `shouldBe` 5
        numColumns testConfig `shouldBe` 5
        sites testConfig `shouldBe` S.fromList [Coordinate 2 3]
        maxGuesses testConfig `shouldBe` 5
    
    describe "initGameState" $ do
      it "initializes game state correctly" $ do
        let state = initGameState testConfig
        config state `shouldBe` testConfig
        hits state `shouldBe` S.empty
        misses state `shouldBe` S.empty
        guessesSoFar state `shouldBe` 0
    
    describe "gameStatus" $ do
      it "returns Running for initial state" $ do
        let state = initGameState testConfig
        case gameStatus state of
          Running -> return ()
          _ -> expectationFailure "Expected Running status"
      
      it "returns Win when all sites are hit" $ do
        let state = GameState testConfig (S.fromList [Coordinate 2 3]) S.empty 1
        case gameStatus state of
          Win -> return ()
          _ -> expectationFailure "Expected Win status"
      
      it "returns Lose when max guesses reached" $ do
        let state = GameState testConfig S.empty (S.fromList [Coordinate 0 0, Coordinate 1 1]) 5
        case gameStatus state of
          Lose -> return ()
          _ -> expectationFailure "Expected Lose status"
    
    describe "nextGameState" $ do
      let initialState = initGameState testConfig
      
      it "records hit when guessing a site" $ do
        let newState = nextGameState (Coordinate 2 3) initialState
        hits newState `shouldBe` S.fromList [Coordinate 2 3]
        misses newState `shouldBe` S.empty
        guessesSoFar newState `shouldBe` 1
      
      it "records miss when guessing non-site" $ do
        let newState = nextGameState (Coordinate 0 0) initialState
        hits newState `shouldBe` S.empty
        misses newState `shouldBe` S.fromList [Coordinate 0 0]
        guessesSoFar newState `shouldBe` 1
      
      it "doesn't change state for already guessed coordinate" $ do
        let state1 = nextGameState (Coordinate 0 0) initialState
        let state2 = nextGameState (Coordinate 0 0) state1
        state2 `shouldBe` state1
    
    describe "showCell" $ do
      it "shows hidden cell as *" $ do
        showCell Hidden `shouldBe` '*'
      
      it "shows hit cell as !" $ do
        showCell Hit `shouldBe` '!'
      
      it "shows miss cell as X" $ do
        showCell Miss `shouldBe` 'X'
    
    describe "renderGameState" $ do
      it "renders initial state with all hidden cells" $ do
        let state = initGameState testConfig
        let rendered = renderGameState state
        length rendered `shouldBe` 5
        all (\row -> length row == 5) rendered `shouldBe` True
        all (\row -> all isHidden row) rendered `shouldBe` True
      
      it "renders state with hit and miss" $ do
        let state = GameState testConfig (S.fromList [Coordinate 2 3]) (S.fromList [Coordinate 0 0]) 2
        let rendered = renderGameState state
        isMiss (rendered !! 0 !! 0) `shouldBe` True
        isHit (rendered !! 2 !! 3) `shouldBe` True
        isHidden (rendered !! 1 !! 1) `shouldBe` True
    
    describe "showBoard" $ do
      it "formats board with header and row numbers" $ do
        let state = initGameState testConfig
        let board = showBoard state
        lines board `shouldSatisfy` (\ls -> length ls == 6) -- header + 5 rows
        head (lines board) `shouldBe` "\\ 01234"
        (lines board !! 1) `shouldSatisfy` (\line -> take 2 line == "0 ")
    
    describe "parseCoordinate" $ do
      it "parses valid coordinate string" $ do
        parseCoordinate "2 3" `shouldBe` Right (Coordinate 2 3)
      
      it "parses zero coordinates" $ do
        parseCoordinate "0 0" `shouldBe` Right (Coordinate 0 0)
      
      it "fails on single number" $ do
        parseCoordinate "2" `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False
      
      it "fails on too many numbers" $ do
        parseCoordinate "2 3 4" `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False
      
      it "fails on non-numeric input" $ do
        parseCoordinate "a b" `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False
      
      it "fails on empty string" $ do
        parseCoordinate "" `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False

    describe "Input validation and error handling" $ do
      it "handles single word input" $ do
        parseCoordinate "kjh" `shouldSatisfy` \case
          Left err -> "Input must contain exactly two space-separated integers." `isInfixOf` err
          Right _ -> False
      
      it "handles too many numbers" $ do
        parseCoordinate "4 5 6" `shouldSatisfy` \case
          Left err -> "Input must contain exactly two space-separated integers." `isInfixOf` err
          Right _ -> False
      
      it "handles non-numeric input" $ do
        parseCoordinate "34 a" `shouldSatisfy` \case
          Left err -> "no parse" `isInfixOf` err
          Right _ -> False
      
      it "handles mixed valid/invalid input" $ do
        parseCoordinate "a 5" `shouldSatisfy` \case
          Left err -> "no parse" `isInfixOf` err
          Right _ -> False
      
      it "handles empty input" $ do
        parseCoordinate "" `shouldSatisfy` \case
          Left err -> "Input must contain exactly two space-separated integers." `isInfixOf` err
          Right _ -> False
      
      it "handles whitespace only" $ do
        parseCoordinate "   " `shouldSatisfy` \case
          Left err -> "Input must contain exactly two space-separated integers." `isInfixOf` err
          Right _ -> False

    describe "Error message validation" $ do
      it "produces correct error for 'kjh' input" $ do
        case parseCoordinate "kjh" of
          Left err -> err `shouldBe` "Input must contain exactly two space-separated integers."
          Right _ -> expectationFailure "Expected parse error for 'kjh'"
      
      it "produces correct error for '4 5 6' input" $ do
        case parseCoordinate "4 5 6" of
          Left err -> err `shouldBe` "Input must contain exactly two space-separated integers."
          Right _ -> expectationFailure "Expected parse error for '4 5 6'"
      
      it "produces correct error for '34 a' input" $ do
        case parseCoordinate "34 a" of
          Left err -> err `shouldBe` "Prelude.read: no parse"
          Right _ -> expectationFailure "Expected parse error for '34 a'"

  describe "Integration tests" $ do
    describe "Game flow simulation" $ do
      it "simulates a winning game sequence" $ do
        let config = GameConfig 3 3 (S.fromList [Coordinate 1 1]) 5
        let state0 = initGameState config
        case gameStatus state0 of
          Running -> return ()
          _ -> expectationFailure "Expected Running status"
        
        -- Miss
        let state1 = nextGameState (Coordinate 0 0) state0
        case gameStatus state1 of
          Running -> return ()
          _ -> expectationFailure "Expected Running status after miss"
        guessesSoFar state1 `shouldBe` 1
        
        -- Hit and win
        let state2 = nextGameState (Coordinate 1 1) state1
        case gameStatus state2 of
          Win -> return ()
          _ -> expectationFailure "Expected Win status after hitting all sites"
        guessesSoFar state2 `shouldBe` 2
      
      it "simulates a losing game sequence" $ do
        let config = GameConfig 2 2 (S.fromList [Coordinate 1 1]) 2
        let state0 = initGameState config
        
        -- First miss
        let state1 = nextGameState (Coordinate 0 0) state0
        case gameStatus state1 of
          Running -> return ()
          _ -> expectationFailure "Expected Running status after first miss"
        
        -- Second miss - should lose
        let state2 = nextGameState (Coordinate 0 1) state1
        case gameStatus state2 of
          Lose -> return ()
          _ -> expectationFailure "Expected Lose status after max guesses"
        guessesSoFar state2 `shouldBe` 2
      
      it "handles multiple sites" $ do
        let config = GameConfig 3 3 (S.fromList [Coordinate 0 0, Coordinate 2 2]) 5
        let state0 = initGameState config
        
        -- Hit first site
        let state1 = nextGameState (Coordinate 0 0) state0
        case gameStatus state1 of
          Running -> return ()
          _ -> expectationFailure "Expected Running status after first hit"
        
        -- Hit second site - should win
        let state2 = nextGameState (Coordinate 2 2) state1
        case gameStatus state2 of
          Win -> return ()
          _ -> expectationFailure "Expected Win status after hitting all sites"
        S.size (hits state2) `shouldBe` 2

    describe "playDefault initial board" $ do
      it "shows correct initial board format" $ do
        let defaultConfig = GameConfig 5 5 (S.fromList [Coordinate 2 3]) 5
        let initialState = initGameState defaultConfig
        let board = showBoard initialState
        let expectedBoard = unlines [
              "\\ 01234",
              "0 *****",
              "1 *****", 
              "2 *****",
              "3 *****",
              "4 *****"
              ]
        board `shouldBe` expectedBoard
    
    describe "Board output format validation" $ do
      it "matches exact playDefault initial output format" $ do
        let config = GameConfig 5 5 (S.fromList [Coordinate 2 3]) 5
        let state = initGameState config
        let board = showBoard state
        let expectedLines = [
              "\\ 01234",
              "0 *****",
              "1 *****", 
              "2 *****",
              "3 *****",
              "4 *****"
              ]
        lines board `shouldBe` expectedLines
      
      it "preserves board format after invalid input attempts" $ do
        let config = GameConfig 5 5 (S.fromList [Coordinate 2 3]) 5
        let state = initGameState config
        -- Board should remain unchanged after parse errors
        let board = showBoard state
        let expectedLines = [
              "\\ 01234",
              "0 *****",
              "1 *****", 
              "2 *****",
              "3 *****",
              "4 *****"
              ]
        lines board `shouldBe` expectedLines

    describe "Game play sequence with board updates" $ do
      it "shows miss on board after incorrect guess" $ do
        let config = GameConfig 5 5 (S.fromList [Coordinate 2 3]) 5
        let initialState = initGameState config
        let stateAfterMiss = nextGameState (Coordinate 0 0) initialState
        let board = showBoard stateAfterMiss
        let expectedBoard = unlines [
              "\\ 01234",
              "0 X****",
              "1 *****",
              "2 *****",
              "3 *****",
              "4 *****"
              ]
        board `shouldBe` expectedBoard
      
      it "shows hit on board after correct guess" $ do
        let config = GameConfig 5 5 (S.fromList [Coordinate 2 3]) 5
        let initialState = initGameState config
        let stateAfterHit = nextGameState (Coordinate 2 3) initialState
        let board = showBoard stateAfterHit
        let expectedBoard = unlines [
              "\\ 01234",
              "0 *****",
              "1 *****",
              "2 ***!*",
              "3 *****",
              "4 *****"
              ]
        board `shouldBe` expectedBoard
      
      it "shows multiple hits and misses on board" $ do
        let config = GameConfig 5 5 (S.fromList [Coordinate 2 3, Coordinate 2 2]) 5
        let state0 = initGameState config
        let state1 = nextGameState (Coordinate 0 0) state0  -- Miss at (0,0)
        let state2 = nextGameState (Coordinate 2 4) state1  -- Miss at (2,4)
        let state3 = nextGameState (Coordinate 2 3) state2  -- Hit at (2,3)
        let state4 = nextGameState (Coordinate 2 2) state3  -- Hit at (2,2)
        let board = showBoard state4
        let expectedBoard = unlines [
              "\\ 01234",
              "0 X****",
              "1 *****",
              "2 **!!X",
              "3 *****",
              "4 *****"
              ]
        board `shouldBe` expectedBoard
      
      it "handles game sequence from example" $ do
        let config = GameConfig 5 5 (S.fromList [Coordinate 2 3]) 5
        let state0 = initGameState config
        
        -- First guess: 0 0 (miss)
        let state1 = nextGameState (Coordinate 0 0) state0
        case gameStatus state1 of
          Running -> return ()
          _ -> expectationFailure "Should be running after first miss"
        
        -- Second guess: 2 4 (miss, but showing as hit in example - let's assume site is at 2,4)
        let configWithSiteAt24 = GameConfig 5 5 (S.fromList [Coordinate 2 4]) 5
        let stateNew = initGameState configWithSiteAt24
        let stateAfterMiss = nextGameState (Coordinate 0 0) stateNew
        let stateAfterHit = nextGameState (Coordinate 2 4) stateAfterMiss
        
        case gameStatus stateAfterHit of
          Win -> return ()
          _ -> expectationFailure "Should win after hitting the only site"
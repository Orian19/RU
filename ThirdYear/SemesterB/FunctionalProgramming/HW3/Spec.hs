{-# LANGUAGE GHC2024 #-}
import Control.Exception (catch, SomeException)
import qualified Data.Ratio as Ratio
import Test.Hspec
import HW3

-- Utility
(@?=) :: (Eq a, Show a) => a -> a -> IO ()
x @?= y = if x == y then pure () else error ("Expected: " ++ show y ++ ", but got: " ++ show x)

-- Helper functions for creating trees
singleTree :: a -> Tree a
singleTree x = Tree Empty x Empty

-- Sample trees for testing
emptyTree :: Tree a
emptyTree = Empty

fullTree :: Tree Int
fullTree = Tree 
            (Tree 
              (Tree (singleTree 8) 4 (singleTree 9)) 
              2 
              (singleTree 5)) 
            1 
            (singleTree 3)

completeTree :: Tree Int
completeTree = Tree
                (Tree
                  (Tree (singleTree 8) 4 (singleTree 9))
                  2
                  (Tree (singleTree 10) 5 Empty))
                1
                (Tree
                  (singleTree 6)
                  3
                  (singleTree 7)
                )

fullAndCompleteTree :: Tree Int
fullAndCompleteTree = Tree
                  (Tree
                    (Tree (singleTree 8) 4 (singleTree 9))
                    2
                    (singleTree 5))
                  1
                  (Tree
                    (singleTree 6)
                    3
                    (singleTree 7)
                  )

perfectTree :: Tree Int
perfectTree = Tree
              (Tree
                  (Tree (singleTree 8) 4 (singleTree 9))
                  2
                  (Tree (singleTree 10) 5 (singleTree 11))
              )
                1
                (Tree
                  (Tree (singleTree 12) 6 (singleTree 13))
                  3
                  (Tree (singleTree 14) 7 (singleTree 15))
                )

degenerateTree :: Tree Int
degenerateTree = Tree 
                  (Tree 
                    (singleTree 3) 
                    2 
                    Empty) 
                  1 
                  Empty

largeTree :: Tree Int
largeTree = Tree
             (Tree
               (Tree
                 (Tree
                   (singleTree 16)
                   8
                   (singleTree 17))
                 4
                 (Tree
                   (singleTree 18)
                   9
                   (singleTree 19)))
               2
               (Tree
                 (Tree
                   (singleTree 20)
                   10
                   (singleTree 21))
                 5
                 (singleTree 11)))
             1
             (Tree
               (Tree
                 (singleTree 12)
                 6
                 (Tree
                   (singleTree 22)
                   13
                   (singleTree 23)))
               3
               (Tree
                 (singleTree 14)
                 7
                 (singleTree 15)))

unbalancedTree :: Tree Int
unbalancedTree = Tree
                  (Tree
                   (Tree
                     (Tree
                       (singleTree 16)
                       8
                       Empty)
                     4
                     Empty)
                   2
                   Empty)
                  1
                  (singleTree 3)

-- Sample mazes for testing
sampleMaze :: Maze
sampleMaze = Maze 
  { width = 3
  , height = 3
  , layout = 
    [ [Open, Open, Open]
    , [Open, Blocked, Open]
    , [Treasure, Open, Treasure]
    ]
  }

blockedMaze :: Maze
blockedMaze = Maze
  { width = 3
  , height = 3
  , layout =
    [ [Open, Open, Open]
    , [Blocked, Blocked, Blocked]
    , [Open, Open, Open]
    ]
  }

treasureMaze :: Maze
treasureMaze = Maze
  { width = 5
  , height = 5
  , layout =
    [ [Open,     Open,    Open,    Open,     Open]
    , [Open,     Blocked, Blocked, Treasure, Open]
    , [Open,     Open,    Open,    Blocked,  Open]
    , [Blocked,  Blocked, Open,    Open,     Open]
    , [Treasure, Open,    Open,    Blocked,  Treasure]
    ]
  }

emptyMaze :: Maze
emptyMaze = Maze
  { width = 3
  , height = 3
  , layout =
    [ [Open, Open, Open]
    , [Open, Open, Open]
    , [Open, Open, Open]
    ]
  }

-- Main spec
main :: IO ()
main = hspec $ do
  describe "Section 1: Trees" $ do
    describe "treeSize" $ do
      it "returns 0 for an empty tree" $
        treeSize emptyTree `shouldBe` 0
      
      it "returns the correct size for a non-empty tree" $
        treeSize fullTree `shouldBe` 7
        
      it "returns the correct size for a large tree" $
        treeSize largeTree `shouldBe` 23
    
    describe "treeHeight" $ do
      it "returns 0 for an empty tree" $
        treeHeight emptyTree `shouldBe` 0
        
      it "returns 1 for a single node tree" $
        treeHeight (singleTree 1) `shouldBe` 1
        
      it "returns the correct height for a complex tree" $
        treeHeight fullAndCompleteTree `shouldBe` 4
        
      it "returns the correct height for a degenerate tree" $
        treeHeight degenerateTree `shouldBe` 3
    
    describe "preOrderTraversal" $ do
      it "returns an empty list for an empty tree" $
        preOrderTraversal emptyTree `shouldBe` ([] :: [Int])
        
      it "returns the correct traversal for a small tree" $
        preOrderTraversal fullTree `shouldBe` [1, 2, 4, 8, 9, 5, 3]
          
      it "returns the correct traversal for a prefect tree" $
        preOrderTraversal perfectTree `shouldBe` [1, 2, 4, 8, 9, 5, 10, 11, 3, 6, 12, 13, 7, 14, 15]

      -- it "returns the correct traversal for a large tree" $
      --   preOrderTraversal perfectTree `shouldBe` [1, 2, 4, 5, 3, 6, 7]
    
    describe "inOrderTraversal" $ do
      it "returns an empty list for an empty tree" $
        inOrderTraversal emptyTree `shouldBe` ([] :: [Int])
        
      it "returns the correct traversal for a small tree" $
        inOrderTraversal fullTree `shouldBe` [8, 4, 9, 2, 5, 1, 3]
        
      it "returns the correct traversal for a perfect tree" $
        inOrderTraversal perfectTree `shouldBe` [8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15]
      -- it "returns the correct traversal for a large tree" $
      --   inOrderTraversal perfectTree `shouldBe` [4, 2, 5, 1, 6, 3, 7]
    
    describe "postOrderTraversal" $ do
      it "returns an empty list for an empty tree" $
        postOrderTraversal emptyTree `shouldBe` ([] :: [Int])
        
      it "returns the correct traversal for a small tree" $
        postOrderTraversal fullTree `shouldBe` [8, 9, 4, 5, 2, 3, 1]
        
      it "returns the correct traversal for a perfect tree" $
        postOrderTraversal perfectTree `shouldBe` [8, 9, 4, 10, 11, 5, 2, 12, 13, 6, 14, 15, 7, 3, 1]

      -- it "returns the correct traversal for a large tree" $
      --   postOrderTraversal perfectTree `shouldBe` [4, 5, 2, 6, 7, 3, 1]
    
    -- todo: add similar tests for all other classes
    describe "isComplete" $ do
        it "returns True for an empty tree" $
            (classify emptyTree == Complete || classify emptyTree == FullAndComplete || classify emptyTree == Perfect) `shouldBe` True
            
        it "returns True for a single node tree" $
            (classify (singleTree 1) == Complete || classify (singleTree 1) == FullAndComplete || classify (singleTree 1) == Perfect) `shouldBe` True
            
        it "returns True for a complete tree" $
            (classify completeTree == Complete || classify completeTree == FullAndComplete || classify completeTree == Perfect) `shouldBe` True
            
        it "returns False for a non-complete tree" $
            (classify (Tree (Tree Empty 2 Empty) 1 (Tree (Tree Empty 4 Empty) 3 Empty)) == Complete || 
            classify (Tree (Tree Empty 2 Empty) 1 (Tree (Tree Empty 4 Empty) 3 Empty)) == FullAndComplete || 
            classify (Tree (Tree Empty 2 Empty) 1 (Tree (Tree Empty 4 Empty) 3 Empty)) == Perfect) `shouldBe` False

        -- Replace the "isBalanced" test block with this:
    describe "isBalanced" $ do
        it "returns True for an empty tree" $
            (classify emptyTree /= Degenerate && classify emptyTree /= Other) `shouldBe` True
            
        it "returns True for a single node tree" $
            (classify (singleTree 1) /= Degenerate && classify (singleTree 1) /= Other) `shouldBe` True
            
        it "returns True for a balanced tree" $
            (classify perfectTree /= Degenerate && classify perfectTree /= Other) `shouldBe` True
            
        it "returns False for an unbalanced tree" $
            (classify unbalancedTree == Degenerate || classify unbalancedTree == Other) `shouldBe` True
    
    describe "classify" $ do
      it "classifies an empty tree as Perfect" $
        classify emptyTree `shouldBe` Perfect
        
      it "classifies a single node tree as Perfect" $
        classify (singleTree 1) `shouldBe` Perfect
        
      it "classifies a full tree correctly" $
        classify fullTree `shouldBe` Full
        
      it "classifies a complete tree correctly" $
        classify completeTree `shouldBe` Complete
        
      it "classifies a full and complete tree correctly" $
        classify fullAndCompleteTree `shouldBe` FullAndComplete
        
      it "classifies a perfect tree correctly" $
        classify perfectTree `shouldBe` Perfect
        
      it "classifies a degenerate tree correctly" $
        classify degenerateTree `shouldBe` Degenerate
        
      it "classifies an irregular tree as Other" $
        classify (Tree (Tree (singleTree 3) 2 (singleTree 5)) 1 Empty) `shouldBe` Other

  describe "Section 2: Infinite Lists" $ do
    describe "itoList" $ do
      it "converts an infinite list to a regular list" $
        take 5 (itoList (1 :> 2 :> 3 :> 4 :> 5 :> irepeat 6)) `shouldBe` [1, 2, 3, 4, 5]
    
    describe "iiterate" $ do
      it "iterates a function over a seed value" $
        take 5 (itoList (iiterate (*2) 1)) `shouldBe` [1, 2, 4, 8, 16]
        
      it "works with complex functions" $
        take 5 (itoList (iiterate (\x -> x * x + x) 1)) `shouldBe` [1, 2, 6, 42, 1806]
    
    describe "irepeat" $ do
      it "repeats a value infinitely" $
        take 5 (itoList (irepeat 42)) `shouldBe` [42, 42, 42, 42, 42]
    
    describe "naturals" $ do
      it "generates the sequence of natural numbers" $
        take 10 (itoList naturals) `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    
    describe "imap" $ do
      it "maps a function over an infinite list" $
        take 5 (itoList (imap (*2) naturals)) `shouldBe` [0, 2, 4, 6, 8]
    
    describe "iconcat" $ do
      it "concatenates a list of lists" $
        take 10 (itoList (iconcat (irepeat [1, 2, 3]))) `shouldBe` [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
        
      it "handles empty lists properly" $
        take 5 (itoList (iconcat ([1] :> [2] :> [3] :> [4] :> [5] :> irepeat []))) `shouldBe` [1, 2, 3, 4, 5]
    
    describe "grouped" $ do
      it "groups elements into sublists of specified size" $
        take 3 (itoList (grouped 2 naturals)) `shouldBe` [[0, 1], [2, 3], [4, 5]]
        
      it "handles large group sizes" $
        take 2 (itoList (grouped 5 naturals)) `shouldBe` [[0, 1, 2, 3, 4], [5, 6, 7, 8, 9]]
    
    describe "reverseN" $ do
      it "reverses elements in groups of specified size" $
        take 10 (itoList (reverseN 3 naturals)) `shouldBe` [2, 1, 0, 5, 4, 3, 8, 7, 6, 11]
        
      it "handles groups of size 1 (no change)" $
        take 5 (itoList (reverseN 1 naturals)) `shouldBe` [0, 1, 2, 3, 4]
    
    describe "sqrtInf" $ do
      it "generates square root approximations" $
        take 4 (map show (itoList (sqrtInf 2))) `shouldBe` ["2 % 1", "3 % 2", "17 % 12", "577 % 408"]
    
    describe "longDivision" $ do
      it "converts rational numbers to decimal strings" $
        take 5 (itoList (longDivision (1 Ratio.% 3))) `shouldBe` "0.333"
        
      it "handles fractions properly" $
        take 5 (itoList (longDivision (1 Ratio.% 2))) `shouldBe` "0.500"
        
      it "handles mixed numbers" $
        take 5 (itoList (longDivision (3 Ratio.% 2))) `shouldBe` "1.500"
        
      it "handles negative numbers" $
        take 5 (itoList (longDivision ((-3) Ratio.% 2))) `shouldBe` "-1.50"
    
    describe "sqrtStrings" $ do
      it "generates string representations of square root approximations" $
        take 3 (map (take 5 . itoList) (itoList (sqrtStrings 2))) `shouldBe` ["2.000", "1.500", "1.416"]

  describe "Section 3: Maze" $ do
    describe "cellAt" $ do
      it "returns the correct cell" $
        cellAt sampleMaze (CellPosition 0 0) `shouldBe` Just Open
        
      it "returns Blocked for blocked cells" $
        cellAt sampleMaze (CellPosition 1 1) `shouldBe` Just Blocked
        
      it "returns Treasure for treasure cells" $
        cellAt sampleMaze (CellPosition 2 0) `shouldBe` Just Treasure
        
      it "returns Nothing for out-of-bounds positions" $
        cellAt sampleMaze (CellPosition 3 0) `shouldBe` Nothing
        
      it "returns Nothing for negative positions" $
        cellAt sampleMaze (CellPosition 0 (-1)) `shouldBe` Nothing
    
    describe "getAvailableMoves" $ do
      it "returns available moves from an open cell" $
        getAvailableMoves sampleMaze (CellPosition 0 0) `shouldBe` Right [CellPosition 0 1, CellPosition 1 0]
        
      it "returns InvalidCell for blocked cells" $
        getAvailableMoves sampleMaze (CellPosition 1 1) `shouldBe` Left InvalidCell
        
      it "returns OutOfBounds for out-of-bounds positions" $
        getAvailableMoves sampleMaze (CellPosition (-1) 42) `shouldBe` Left OutOfBounds
        
      it "handles edge cases correctly" $
        getAvailableMoves sampleMaze (CellPosition 0 1) `shouldBe` Right [CellPosition 0 0, CellPosition 0 2]
    
    describe "shortestPath" $ do
      it "finds the shortest path between two points" $
        shortestPath sampleMaze (CellPosition 0 1) (CellPosition 2 2) `shouldBe` Right [CellPosition 0 2, CellPosition 1 2]
        
      it "returns empty list for adjacent cells" $
        shortestPath sampleMaze (CellPosition 0 0) (CellPosition 0 1) `shouldBe` Right []
        
      it "returns InvalidCell when target is blocked" $
        shortestPath sampleMaze (CellPosition 0 1) (CellPosition 1 1) `shouldBe` Left InvalidCell
        
      it "returns OutOfBounds when target is out of bounds" $
        shortestPath sampleMaze (CellPosition 0 1) (CellPosition 3 2) `shouldBe` Left OutOfBounds
        
      it "returns NoPath when there is no path" $
        shortestPath blockedMaze (CellPosition 0 0) (CellPosition 2 2) `shouldBe` Left NoPath
      
      it "returns same cell for identical start and end" $
        shortestPath sampleMaze (CellPosition 0 0) (CellPosition 0 0) `shouldBe` Right []
      
      it "returns same cell for identical start and end" $
        shortestPath treasureMaze (CellPosition 0 4) (CellPosition 4 0) `shouldBe` Right [CellPosition 1 4, CellPosition 2 4, CellPosition 3 4, CellPosition 3 3, CellPosition 3 2, CellPosition 4 2, CellPosition 4 1]


    -- Bonus section
    -- describe "treasureHunt" $ do
    --   it "finds a path that visits all treasures" $ do
    --     let result = treasureHunt sampleMaze (CellPosition 0 0)
    --     case result of
    --       Left err -> fail $ "Expected Right, but got Left " ++ show err
    --       Right path -> do
    --         -- Check that all treasures are visited
    --         let treasurePositions = [CellPosition 2 0, CellPosition 2 2]
    --         let reachable = all (\pos -> pos `elem` path || pos == CellPosition 0 0) treasurePositions
    --         reachable `shouldBe` True
            
    --   it "returns InvalidCell for blocked starting positions" $
    --     treasureHunt sampleMaze (CellPosition 1 1) `shouldBe` Left InvalidCell
        
    --   it "returns OutOfBounds for out-of-bounds starting positions" $
    --     treasureHunt sampleMaze (CellPosition 3 3) `shouldBe` Left OutOfBounds
        
    --   it "handles complex mazes" $ do
    --     let result = treasureHunt treasureMaze (CellPosition 0 0)
    --     case result of
    --       Left err -> fail $ "Expected Right, but got Left " ++ show err
    --       Right path -> do
    --         -- Check that all treasures are visited
    --         let treasurePositions = [CellPosition 1 3, CellPosition 4 0, CellPosition 4 4]
    --         let reachable = all (\pos -> pos `elem` path || pos == CellPosition 0 0) treasurePositions
    --         reachable `shouldBe` True
            
    --   it "returns NoPath when treasures are unreachable" $
    --     treasureHunt blockedMaze (CellPosition 0 0) `shouldBe` Left NoPath
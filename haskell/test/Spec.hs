import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = putStrLn "Our proof of concept is a server socket to connect to, and so is not tested through this file/Tasty. See the instructions in the README for how to test our proof of concept."
    
    {-
defaultMain $ testGroup "Tests" $
  [ testCase "Addition works" $ do
      2 + 3 @?= 5  -- actual @?= expected
  , testCase "Multiplication works" $ do
      6 @=? 2 * 3  -- expected @=? actual
  -- NOTE: Uncomment this to see what a failing assertion looks like:
  -- , testCase "Bad assertion" $ do
  --     1 @?= 2
  -- NOTE: This is how to explicitly assert failures:
  -- , testCase "Explicit failure" $ do
  --     assertFailure "BOOM!"
  ]
-}



-- testBoardView :: BoardView
-- testBoardView = BV testBoard testBoard

-- testBoard :: [[String]]
-- testBoard = [["S", "~", "S", "S", "S", "~", "~", "~", "~", "~"],
--              ["S", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
--              ["S", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
--              ["S", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
--              ["~", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
--              ["~", "~", "~", "~", "X", "X", "X", "~", "~", "~"],
--              ["~", "~", "~", "~", "~", "~", "~", "~", "~", "~"],
--              ["~", "~", "~", "~", "~", "~", "~", "~", "~", "~"],
--              ["~", "~", "~", "~", "~", "~", "~", "S", "S", "~"],
--              ["~", "~", "~", "~", "~", "~", "~", "~", "~", "~"]]

-- -- note board view is a stub and is not accurate of current state
-- -- this state has hits on all the first ship's squares except Square B One
-- testAlmostSunkState :: GameState
-- testAlmostSunkState = GameState (BSS (PlayerState [Ship [Square B One, Square C One, Square D One, Square E One, Square F One],
--                                  Ship [Square I One, Square I Two, Square I Three, Square I Four],
--                                  Ship [Square C Three, Square D Three, Square E Three],
--                                  Ship [Square A Six, Square A Seven, Square A Eight],
--                                  Ship [Square H Nine, Square I Nine]] [Square C One, Square D One, Square E One, Square F One] ) []) (serverBoardView)

-- -- note board view is a stub and is not accurate of current state
-- -- this state has hits on all ship squares except Square H Nine
-- testAlmostLostState :: GameState
-- testAlmostLostState = GameState (BSS (PlayerState [Ship [Square B One, Square C One, Square D One, Square E One, Square F One],
--                                  Ship [Square I One, Square I Two, Square I Three, Square I Four],
--                                  Ship [Square C Three, Square D Three, Square E Three],
--                                  Ship [Square A Six, Square A Seven, Square A Eight],
--                                  Ship [Square H Nine, Square I Nine]] 
--                                  [Square B One, Square C One, Square D One, Square E One, Square F One, Square I One, Square I Two, Square I Three, Square I Four,
--                                  Square C Three, Square D Three, Square E Three, Square A Six, Square A Seven, Square A Eight, Square I Nine] ) []) (serverBoardView)

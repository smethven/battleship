import Lib
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

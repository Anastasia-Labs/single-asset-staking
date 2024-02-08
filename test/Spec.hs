module Main (main) where

import Spec.StakingSpec (propertyTest, unitTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Single Asset Staking Tests"
      [ unitTest
      , propertyTest
      ]

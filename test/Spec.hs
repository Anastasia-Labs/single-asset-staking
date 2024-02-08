module Main (main) where

import Spec.StakingSpec (unitTest, propertyTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Single Asset Staking Tests"
      [ unitTest
      , propertyTest
      ]

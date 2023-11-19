module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List

import MyLib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testCase " Test 1" $ zipLong [1, 2, 3] "abc" @?= [(1, 'a'), (2, 'b'), (3, 'c')]
  , testCase " Test 2" $ zipLong [1, 2] "abcd" @?= [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')]
  , testCase " Test 3" $ zipLong [] "sfd" @?= ([] :: [(Int, Char)])
  ]

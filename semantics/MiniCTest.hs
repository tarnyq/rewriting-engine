module MiniCTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Map (fromList)

import MiniC
import MiniC_Program

tests :: TestTree
tests = testGroup "MiniC Tests"
  [ testGroup "Basic" [
        testCase "SumToN 10" $
              (eval_miniC $ sum_n 10)
          @?= (State [] (fromList [("n",0),("sum",55)]))
    ]
  ]

module SBVTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import SBV
import Tarnyq (evalWithDepth)

import Data.SBV
import Data.Map (findWithDefault)

import System.IO.Unsafe

tests :: TestTree
tests = testGroup "SBV Tests"
  [ testCase "SumToN 10" $
          (unsafePerformIO $ isTheorem (findWithDefault undefined "sum" (store (eval_imp $ sum_imp 10)) .== 55))
      @?= True
  , testCase "SumToN n Approx One iteration" $
          (unsafePerformIO $ isTheorem (findWithDefault undefined "sum" (store (evalWithDepth imp 28 $ impInitState (sum_imp $ sym "n"))) .== (sym "n")))
      @?= True
  ]

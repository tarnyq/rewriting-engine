module KTutImpTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import KTutImp
import Data.Map (fromList)

tests :: TestTree
tests = testGroup "KTutImp Tests"
  [ testCase "SumToN 10" $
          (eval_imp $ sum_imp 10)
      @?= (State [] (fromList [("n",0),("sum",55)]), [])
  , testCase "div" $
          (eval_imp divide_imp)
      @?= (State [] (fromList [("a",100),("b",3),("r",33)]), [])
  , testCase "div0" $
          (eval_imp div0_imp)
      @?= (State -- Program is stuck since we cannot divide by zero.
                 [KI_AExp (Int 42 :/ Int 0),KI_Stmts ("r" := AHole)]
                 (fromList [("r",0)]), [])
  ]

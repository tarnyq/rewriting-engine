module KTutImpTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Map (fromList)

import KTutImp
import KTutImp_Program
import Rewrite.IO

tests :: TestTree
tests = testGroup "KTutImp Tests"
  [ testGroup "Basic" [
        testCase "SumToN 10" $
              (eval_imp $ sum_imp 10)
          @?= (State [] (fromList [("n",0),("sum",55)]))
      , testCase "SumToN 10 (AllPaths)" $
              (evalAllPaths_imp $ sum_imp 10)
          @?= [State [] (fromList [("n",0),("sum",55)])]
      , testCase "div" $
              (eval_imp divide_imp)
          @?= (State [] (fromList [("a",100),("b",3),("r",33)]))
      , testCase "div0" $
              (eval_imp div0_imp)
          @?= (State -- Program is stuck since we cannot divide by zero.
                     [KI_AExp (Int 42 :/ Int 0),KI_Stmts ("r" := AHole)]
                     (fromList [("r",0)]))
    ]
  , testGroup "Summarized" [
        testCase "SumToN 10" $
              (eval_sum_summary $ 10)
          @?= (State [] (fromList [("n",0),("sum",55)]))
    ]
  , testGroup "IO" [
        testCase "SumToN 10" $
              (eval_imp_io_pure (sum_imp 10) [])
          @?= (State [] (fromList [("n",0),("sum",55)]), ProgramIOState [] [])
      , testCase "SumToN IO" $
              (eval_imp_io_pure sum_imp_io [10])
          @?= (State [] (fromList [("n",0),("sum",55)]), ProgramIOState [] [55])
      , testCase "SumToN Fail" $
              (eval_imp_io_pure sum_imp_io [])
          @?= (State stuck_read (fromList [("n",0),("sum",0)]), ProgramIOState [] [])
    ]
  ]
  where stuck_read = [KI_AExp Read
                     , KI_Stmts ("n" := AHole)
                     , KI_Stmts ("sum" := Int 0)
                     , KI_Stmts (While (Not (Var "n" :<= Int 0))
                                    (StmtsBlock (StPair
                                        ("sum" := (Var "sum" :+ Var "n"))
                                        ("n" := (Var "n" :+ Negate 1)))))
                     , KI_Stmts (Print (Var "sum"))
                     ]

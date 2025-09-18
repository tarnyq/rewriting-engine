module KTutImpTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Map (fromList)

import KTutImp
import Rewrite.IO
import Rewrite.DomainValue

tests :: TestTree
tests = testGroup "KTutImp Tests"
  [ testGroup "Basic" [
        testCase "SumToN 10" $
              (eval_imp $ sum_imp $ dInteger 10)
          @?= (State [] (fromList [("n",dInteger 0),("sum",dInteger 55)]))
      , testCase "SumToN 10 (AllPaths)" $
              (evalAllPaths_imp $ sum_imp $ dInteger 10)
          @?= [State [] (fromList [("n",dInteger 0),("sum",dInteger 55)])]
      , testCase "div" $
              (eval_imp divide_imp)
          @?= (State [] (fromList [("a",dInteger 100),("b",dInteger 3),("r",dInteger 33)]))
      , testCase "div0" $
              (eval_imp div0_imp)
          @?= (State -- Program is stuck since we cannot divide by zero.
                     [ KI_AExp (Int (dInteger 42) :/ Int (dInteger 0))
                     , KI_Stmts ("r" := AHole)]
                     (fromList [("r",dInteger 0)]))
    ]
  , testGroup "Summarized" [
        testCase "SumToN 10" $
              (eval_sum_summary $ 10)
          @?= (State [] (fromList [("n",dInteger 0),("sum", dInteger 55)]))
    ]
  , testGroup "IO" [
        testCase "SumToN 10" $
              (eval_imp_io_pure (sum_imp $ dInteger 10) [])
          @?= (State [] (fromList [("n",dInteger 0),("sum",dInteger 55)]), ProgramIOState [] [])
      , testCase "SumToN IO" $
              (eval_imp_io_pure sum_imp_io [10])
          @?= (State [] (fromList [("n",dInteger 0),("sum",dInteger 55)]), ProgramIOState [] [dInteger 55])
      , testCase "SumToN Fail" $
              (eval_imp_io_pure sum_imp_io [])
          @?= (State stuck_read (fromList [("n",dInteger 0),("sum",dInteger 0)]), ProgramIOState [] [])
    ]
  ]
  where stuck_read = [KI_AExp Read
                     , KI_Stmts ("n" := AHole)
                     , KI_Stmts ("sum" := Int (dInteger 0))
                     , KI_Stmts (While (Not (Var "n" :<= Int (dInteger 0)))
                                    (StmtsBlock (StPair
                                        ("sum" := (Var "sum" :+ Var "n"))
                                        ("n" := (Var "n" :+ Negate (dInteger 1))))))
                     , KI_Stmts (Print (Var "sum"))
                     ]

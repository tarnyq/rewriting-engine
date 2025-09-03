module NonDetTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import NonDet

tests :: TestTree
tests = testGroup "NonDet Tests"
  [ testCase "eval"     $ (eval_nondet      A) @?= (B, [])
  , testCase "evalrev"  $ (evalRev_nondet   A) @?= (C, [])
  , testCase "evalAP"   $ (evalAP_nondet    A) @?= [(B, []), (C, [])]
  ]

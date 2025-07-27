module Main (main) where

import KTutImp

main :: IO ()
--  We must evaluate `benchmark` result to full normal form to make sure it
--  runs! Printing the result is an easy way to force that lazy Haskell to
--  actually do the work.
main = print benchmark

benchmark :: State
benchmark = eval_imp $ sum_imp (10 * 1000 * 1000)

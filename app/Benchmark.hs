module Main (main) where

import KTutImp

main :: IO ()
--  We must evaluate `benchmark` result to full normal form to make sure it
--  runs! Printing the result is an easy way to force that lazy Haskell to
--  actually do the work.
main = print benchmark

benchmark :: State
benchmark = go where
 go = eval_imp $ sum_imp (10 * 1000 * 1000)                 -- Imp with rewriting abstractions.
 -- go = eval_hand_opt_imp $ sum_imp (10 * 1000 * 1000)     -- "Hand optimized" imp.
 -- go = eval_sum $ (10 * 1000 * 1000)                      -- summarized sum
 -- Imp with mocked IO
 -- go = eval_imp_io_pure (sum_imp_io) [show $ 10 * 1000 * 1000]

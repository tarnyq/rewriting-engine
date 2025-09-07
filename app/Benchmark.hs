module Main (main) where

import KTutImp
import Rewrite.IO

main :: IO ()
--  We must evaluate `benchmark` result to full normal form to make sure it
--  runs! Printing the result is an easy way to force that lazy Haskell to
--  actually do the work.
main = do print benchmark

benchmark :: State
benchmark = go where
    go = eval_imp $ sum_imp (10 * 1000 * 1000)              -- sum
 -- go = eval_sum $ (10 * 1000 * 1000)                      -- summarized sum
 -- go = eval_imp_io_pure (sum_imp_io) [show $ 10 * 1000 * 1000]

 --      eval_imp_io_io             -- Needs stdin/stout set up/redirected.
 --      evalAllPaths_imp
 --      evalAllPaths_sum_summary
 --      evalAllPaths_imp_io

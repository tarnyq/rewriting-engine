module Main (main) where

import KTutImp
import HandOptImp

import Data.SBV
import Data.SBV.Control
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["imp",     "sum", n]    -> print $ eval_imp    (sum_imp $ read n)
       ["imp_io",  "sum_io"]    -> do state <- eval_imp_io sum_imp_io
                                      print state
       ["imppure","sum_io", n]  -> print $ eval_imp_io_pure sum_imp_io [n]
       ["optimp",  "sum", n]    -> print $ eval_hand_opt_imp (sum_imp $ read n)
       ["summarized",     n]    -> print $ eval_sum_summary $ read n

       -- No args; Run the standard benchmark
       []                       -> print $ eval_imp $ sum_imp $ 10*1000*1000

       _ -> putStrLn "Bad usage." -- TODO should go to stderr.

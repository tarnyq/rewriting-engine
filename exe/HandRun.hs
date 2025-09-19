{-  hand-run - execute a hand-coded function call

    This is essentially a quick hack to let us run various
    semantics/evaluator/program combinations. We pattern match against the
    command-line arguments to match constants and bind values and, based on
    that, execute the matching hand-coded function call.

    This should be replaced with something (probably an `eval` program)
    that has a consistent interface and can call arbitary specifications
    of evaluator, semantics and program.
-}
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import KTutImp
import HandOptImp

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["imp",     "sum", n]   -> print $ eval_imp    (sum_imp $ read n)
        ["imp_io",  "sum_io"]   -> do state <- eval_imp_io sum_imp_io
                                      print state
        ["imppure","sum_io", n] -> print $ eval_imp_io_pure sum_imp_io [n]
        ["optimp",  "sum", n]   -> print $ eval_hand_opt_imp (sum_imp $ read n)
        ["summarized",     n]   -> print $ eval_sum_summary $ read n
        -- No args; Run the standard benchmark
        []                      -> print $ eval_imp $ sum_imp $ 10*1000*1000
        -- Bad arguments
        _                       -> do
            hPutStrLn stderr $ "Bad args: " ++ show args
            exitFailure

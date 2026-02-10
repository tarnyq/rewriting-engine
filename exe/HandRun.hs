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
import KTutImp_Program
import HandOptImp

-- XXX Move the symbolic stuff to the language semantics so we don't need
-- to import all this.
import SymbolicImp
import Rewrite.Symbolic
import Domain.Term (Term(..))
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["imp",     "sum", n]   -> print $ eval_imp    (sum_imp $ read n)
        ["imp_io",  "sum_io"]   -> do state <- eval_imp_io sum_imp_io
                                      print state
        ["imppure","sum_io", n] -> print $ eval_imp_io_pure sum_imp_io [read n]
        ["optimp",  "sum", n]   -> print $ eval_hand_opt_imp (sum_imp $ read n)
        ["summarized",     n]   -> print $ eval_sum_summary $ read n

        -- Run sum on onepath, using the "symbolic" semantics with ConcreteValue
        ["symbolic", "crunsum", n] ->
             print $ evalOnePath_imp_symbolic
                        $ sum_imp_symbolic (dInteger (read n))
        -- Run sum on all-paths, using the "symbolic" semantics with SymbolicExpr
        ["symbolic", "srunsum", n] ->
             do cstate <- evalAllPaths_imp_symbolic 1000 $
                            Constrained (SymbolicImp.impInitState $ sum_imp_symbolic $ dInteger (read n))
                                        (dBool True)
                liftIO $ print cstate

        -- Return terminal states for sum-to-n, where N < 10
        ["symbolic", "lt10"]  ->
            do cstate <- evalAllPaths_imp_symbolic 10000 $
                    (Constrained (SymbolicImp.impInitState $ sum_imp_symbolic (IntVar "n"))
                                 (dLt (IntVar "n") (IntLit $ 10)))
               print cstate
        ["symbolic", "summarize"]  ->
            do _cstate <- summarize_imp $ sum_imp_symbolic (IntVar "n")
               pure ()
        _ {- Bad Arguments -}   -> do
            hPutStrLn stderr $ "Bad args: " ++ show args
            exitFailure


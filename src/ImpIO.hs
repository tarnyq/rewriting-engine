module ImpIO (imp_io) where

import Tarnyq
import RewritePureIO
import KTutImp

----------------------------------------------------------------------
-- Imp IO extends the semantics of Imp with IO operations

imp_io :: (Console r, MonadRewrite r State) => [r ()]
imp_io = imp ++ [printHeat, printCool, print_]

printHeat :: MonadRewrite r State => r ()
printHeat = do ((KI_Stmts (Print exp)):rest) <- getK
               guard $ not (isInt exp)
               putK $ (KI_AExp exp):(KI_Stmts (Print AHole)):rest

printCool :: MonadRewrite r State => r ()
printCool = do ((KI_AExp (Int i)):(KI_Stmts (Print AHole)):rest) <- getK
               putK $ (KI_Stmts (Print (Int i))):rest

print_ :: (Console r, MonadRewrite r State) => r ()
print_ = do (KI_Stmts (Print (Int i))):rest <- getK
            printConsole (show i)
            putK rest

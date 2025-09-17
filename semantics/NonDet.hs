{-  Toy language for testing non-determinism.
-}

module NonDet (State(..), eval_nondet, evalRev_nondet, evalAP_nondet) where

import Rewrite.Class
import Rewrite.Basic

data State = A | B | C
deriving instance Show State
deriving instance Eq   State

nondet :: MonadRewrite m State => [m ()]
nondet = [aToB, aToC] where
    aToB = do A <- get
              put B
    aToC = do A <- get
              put C

eval_nondet :: State -> State
eval_nondet = evalOnePath nondet

evalRev_nondet :: State -> State
evalRev_nondet = evalOnePath $ reverse nondet

evalAP_nondet :: State -> [State]
evalAP_nondet = evalAllPaths nondet

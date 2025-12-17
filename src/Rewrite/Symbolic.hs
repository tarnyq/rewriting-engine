module Rewrite.Symbolic
    ( DomainValue(..)
    , Constrained(..)
    , RewriteSymbolic(..)
    , SymbolicExpr(..)
    , evalAllPathsSymbolic
    )
  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Extra (concatMapM)
import           Data.Maybe
import           Data.SBV
import           Data.SBV.Trans.Control

import           Domain
import           Domain.Class
import           Domain.SymbolicExpr (fromTerm)
import           Rewrite.Class

data Constrained s dv =
        Constrained { state :: (s dv), constraint :: (dv Bool) }
instance DomainFunctor s => DomainFunctor (Constrained s)   where
    dmapM f s = Constrained <$>  (dmapM f $ state s) <*> (f $ constraint s)
deriving instance (Show (s dv), Show (dv Bool)) => Show (Constrained s dv)
deriving instance (Eq (s dv), Eq (dv Bool)) => Eq (Constrained s dv)
deriving instance (Ord (s dv), Ord (dv Bool)) => Ord (Constrained s dv)


newtype RewriteSymbolic s a =
    RewriteSymbolic {
        rewriterSymbolic :: (Constrained s SymbolicExpr) -> Maybe (a, Constrained s SymbolicExpr)
    } deriving Functor

instance Applicative (RewriteSymbolic s) where
    pure x = RewriteSymbolic $ \s -> pure (x, s)
    (<*>) = ap

instance Monad (RewriteSymbolic s) where
    {-# INLINE (>>=) #-}
    p >>= q = RewriteSymbolic $
        \s -> do (a', s') <- ((rewriterSymbolic p) s)
                 ((rewriterSymbolic $ q a') s')

-- MonadFail allows us to have binding patterns that fail in do notation.
instance MonadFail (RewriteSymbolic s) where
    fail _ = RewriteSymbolic $ \_ -> Nothing

instance Alternative (RewriteSymbolic s) where
    empty = RewriteSymbolic $ \_ -> Nothing
    {-# INLINE (<|>) #-}
    r1 <|> r2 = RewriteSymbolic
        $ \s -> ((rewriterSymbolic r1) s) <|> ((rewriterSymbolic r2) s)

instance MonadRewrite (RewriteSymbolic s) SymbolicExpr (s SymbolicExpr) where
    get       = RewriteSymbolic $ \(Constrained s cond) -> Just (s, (Constrained s cond))
    put s'    = RewriteSymbolic $ \(Constrained _ cond) -> Just ((), (Constrained s' cond))
    matchFail = RewriteSymbolic $ \_                  -> Nothing
    sguard sbool = RewriteSymbolic $ \(Constrained s cond) -> Just ((), (Constrained s (dAnd cond sbool)))

------------------------------------------------------------------------
--  All path evaluation

-- Return all terminal states (leaves of an execution tree) using
-- depth-first evaluation.
evalAllPathsSymbolic :: forall s. DomainFunctor s =>
    [RewriteSymbolic s ()] -> (Constrained s Term) -> IO [Constrained s Term]
evalAllPathsSymbolic rewrites cstate
    = runSMT $ query $ do
            symState <- dmapM fromTerm cstate
            result <- eval' symState
            pure $ map (dmap term) result
  where
    eval' :: Constrained s SymbolicExpr -> Query [Constrained s SymbolicExpr]
    -- We split off the expr from cond so that we do not need to repeatedly
    -- assert the entire path condition, rather, only the incremental addition.
    eval' (Constrained s (SymbolicExpr cond expr))
          -- case analysis to avoid calling into the sat solver
          -- when unnessesary.
        = case unliteral cond of Just True -> satCase s expr
                                 _         -> checkWithSolver s cond expr
    checkWithSolver s cond expr = inNewAssertionStack $
        do constrain cond
           res <- checkSat
           case res of
               Sat -> satCase s expr
               Unsat -> pure []
               Unk -> error "Solver returned unknown!"
               DSat _ -> error "Solver returned DSat?!"

    satCase s expr = inNewAssertionStack $
        do -- Since we already asserted the condition in this Query context,
           -- we do not need that as part of the Constrained for continued
           -- execution.
           let ns = nexts (Constrained s (SymbolicExpr sTrue expr))
           terminals <- concatMapM eval' ns

           -- If the branch conditions of all next states is not total,
           -- we also need to consider the residue, i.e. the negation
           -- of the disjunction of all the path conditions returned
           -- by next.
           let residue = mkResidue ns s
           constrain $ (sbv . constraint) residue
           let expr' = dAnd ((term.constraint) residue) expr
           let constr' = (SymbolicExpr
                        ((sbv.constraint) residue)
                        (expr'))
           let residue' = residue { constraint=constr' }
           res <- checkSat

           case res of Sat -> pure $ residue':terminals
                       Unsat -> pure terminals
                       Unk -> error "Solver returned unknown!"
                       DSat _ -> error "Solver returned DSat?!"

    --  At each step next gives all new states derived from a single input
    --  state.
    nexts :: Constrained s SymbolicExpr -> [Constrained s SymbolicExpr]
    nexts s = (nexts' rewrites s)

    mkResidue :: [Constrained s SymbolicExpr] -> (s SymbolicExpr) -> Constrained s SymbolicExpr
    mkResidue ss s =
        Constrained s (dNot $ foldl' (dOr) (dBool False) (fmap constraint ss))

    nexts' :: [RewriteSymbolic s ()]
           -> (Constrained s SymbolicExpr)
           -> [Constrained s SymbolicExpr]
    nexts' []       _ = []
    nexts' (rw:rws) s
        = let thisrw = maybeToList (fmap snd ((rewriterSymbolic rw) s))
          in thisrw ++ (nexts' rws s)


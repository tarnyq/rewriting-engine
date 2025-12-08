module Rewrite.Symbolic
    ( DomainValue(..)
    , Constrained(..), SymBool, SymInteger
    , DomainTerm(IntVar)
    , SymbolicExpr(..)
    , evalAllPathsSymbolic
    , IsSameNode
    , MakeConvex
    , summarize
    )
  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Extra (concatMapM)
import           Data.Maybe
import           Data.SBV
import           Data.SBV.Control

import           Rewrite.Class
import           Rewrite.Domain

import Debug.Trace


type SymInteger = SymbolicExpr Integer
type SymBool = SymbolicExpr Bool
data Constrained s =
        Constrained { state :: s, constraint :: (SymbolicExpr Bool) }
deriving instance (Show a) => Show (Constrained a)


newtype RewriteSymbolic s a =
    RewriteSymbolic {
        rewriterSymbolic :: (Constrained s) -> Maybe (a, Constrained s)
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

instance MonadRewrite (RewriteSymbolic s) SymbolicExpr s where
    get       = RewriteSymbolic $ \(Constrained s cond) -> Just (s, (Constrained s cond))
    put s'    = RewriteSymbolic $ \(Constrained _ cond) -> Just ((), (Constrained s' cond))
    matchFail = RewriteSymbolic $ \_                  -> Nothing
    sguard sbool = RewriteSymbolic $ \(Constrained s cond) -> Just ((), (Constrained s (dAnd cond sbool)))

------------------------------------------------------------------------
--  All path evaluation

-- Return all terminal states (leaves of an execution tree) using
-- depth-first evaluation.
evalAllPathsSymbolic ::
    forall s. (Show s) => [RewriteSymbolic s ()] -> s -> Symbolic [Constrained s]
evalAllPathsSymbolic rewrites state
    = query $ evalAllPathsSymbolic' rewrites (Constrained state (dBool True))

evalAllPathsSymbolic' ::
    forall s. (Show s) => [RewriteSymbolic s ()] -> (Constrained s) -> Query [Constrained s]
evalAllPathsSymbolic' rewrites state
    = do -- io $ traceIO $ show ("init", state)
         eval' state
  where
    eval' :: Constrained s -> Query [Constrained s]

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
           -- io $ traceIO $ "-----------------------------------------------"
           let ns = nexts (Constrained s (SymbolicExpr sTrue expr))
           -- io $ traceIO $ show ("ns: ", length ns, ns)
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
           -- io $ traceIO $ show ("Residue: ", res, length ns, residue ns s)

           case res of Sat -> pure $ residue':terminals
                       Unsat -> pure terminals
                       Unk -> error "Solver returned unknown!"
                       DSat _ -> error "Solver returned DSat?!"

    --  At each step next gives all new states derived from a single input
    --  state.
    nexts :: Constrained s -> [Constrained s]
    nexts s = (nexts' rewrites s)

    mkResidue :: [Constrained s] -> s -> Constrained s
    mkResidue ss s =
        Constrained s (dNot $ foldl' (dOr) (dBool False) (fmap constraint ss))

    nexts' :: [RewriteSymbolic s ()] -> (Constrained s) -> [Constrained s]
    nexts' []       _ = []
    nexts' (rw:rws) s
        = let thisrw = maybeToList (fmap snd ((rewriterSymbolic rw) s))
          in thisrw ++ (nexts' rws s)


type IsSameNode s = s -> s -> Bool
type MakeConvex s = s -> s -> Query (Maybe s)

--- data Summary s = Terminal s
---                | Incomplete s
---                | Branch [(Constrained s, Summary s)]
---                | Root (Constrained s, Summary s)

summarize :: forall s. (Show s) =>
       [RewriteSymbolic s ()] -- ordinary rules
    -> [RewriteSymbolic s ()]
    -> IsSameNode s
    -> MakeConvex s
    -> s
    -> Symbolic [Constrained s]
summarize rules cutRules isSameNode makeConvex init
  = do let initNodes = [Constrained init (dBool True)]
       query $ summarize' rules cutRules isSameNode makeConvex initNodes

summarize' :: forall s. (Show s) =>
       [RewriteSymbolic s ()] -- ordinary rules
    -> [RewriteSymbolic s ()]
    -> IsSameNode s
    -> MakeConvex s
    -> [Constrained s]
    -> Query [Constrained s]
summarize' rules cutRules isSameNode makeConvex frontier
    = do n1 <- concatMapM (evalAllPathsSymbolic' rules) frontier
         io $ traceIO $ "==== Reached while"
         n2 <- concatMapM (evalAllPathsSymbolic' cutRules) n1
         io $ traceIO $ "==== Reached while again"
         n3 <- concatMapM (evalAllPathsSymbolic' rules) n2
         convex <- makeConvex ((state.head) n1) ((state.head) n3)
         let n4 = [((\s -> (Constrained s (dBool True))).(fromMaybe undefined)) convex]
         n5 <- concatMapM (evalAllPathsSymbolic' cutRules) n4
         n6 <- concatMapM (evalAllPathsSymbolic' rules) n5
         pure $ n6

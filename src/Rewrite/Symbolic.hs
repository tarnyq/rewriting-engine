module Rewrite.Symbolic
    ( DomainValue(..)
    , Constrained(..)
    , RewriteSymbolic(..)
    , SymbolicExpr(..)
    , ExecResult(..), ExecBranch(..)
    , evalAllPathsSymbolic
    )
  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Extra (concatMapM)
import qualified Control.Monad.Trans.State as T
import           Data.Maybe
import qualified Data.Map as M
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

-- All path symbolic execution
-- ---------------------------

-- TODO: Implements (rewrite+) for a rewriting system 'rewrite'

-- All path symbolic execution is an algorithm that allows us to compute
-- the all states reachable from an initial symbolic state, perhaps conditionally.
-- Thus the Monad must encode: (a) non-application of all rules,
-- (b) conditional application of a rule (c) non-deterministic application of rules.
--
-- The result of execution is a list of branches. Each branch has an associated
-- constraint, and a potential set (represented as a list) of reachable
-- states constrained by the path condition. The constraint is the incremental
-- addition to the path condition.
--
-- If no rule applies, then 'states' is Nothing. If some set of rules apply,
-- then 'states' is a list of non-deterministically reachable rules.

newtype RewriteSymbolic s a =
    RewriteSymbolic {
        rewriterSymbolic :: (Constrained s SymbolicExpr) -> ExecResult s a
    } deriving Functor

type ExecResult s a = [ExecBranch s a]
data ExecBranch s a =
    ExecBranch { constr :: SymbolicExpr Bool
               , states :: Maybe [(a, Constrained s SymbolicExpr)]
               }
  deriving Functor
deriving instance (Show (s SymbolicExpr), Show a) => Show (ExecBranch s a)


-- Together, the 'constr's form a partition--they are disjoint and their
-- disjunction is valid:
--      c₁ ∨ c₂ ∨ ... ∨ cₙ → ⊤
-- and
--      cᵢ ∧ cⱼ → ⊥       when i ≠ j

-- Let us look at some examples:
--
-- When no rule matches we get:
--
--      [ ExecBranch (dTrue, Nothing) ]
--
-- When only one matches unconditionally we get:
--
--      [ ExecBranch (dTrue, Just [s])]
--
-- When two states are non-deterministically and unconditionally reachable from
-- a state, we get a result:
--      [ ExecBranch (dTrue, [s₁, s₂]) ]
--
-- When two complementary rules matches (i.e. their branch constraints
-- are mutually exclusive), we get:
--
--      [ ExecBranch (constr,      Just [s₁])
--      , ExecBranch (dNot constr, Just [s₂])
--      ]

-- For non-complementary rules, cases (b) an    d (c) may overlap--we may have
-- an application that is both non-deterministic and conditional.
-- Our representation requires that we decompose these explicitly.
-- That is, the application of the following two rules:
--
--      N::Int => Fizz   requires n % 3 == 0
--      N::Int => Buzz   requires n % 5 == 0
--
-- has at least three branches, explicitly representing the case where both
-- conditions apply:
--
--      [ ( n % 3 == 0 ∧ ¬n % 5 == 0, Just [Fizz])
--      , (¬n % 3 == 0 ∧  n % 5 == 0, Just [Buzz])
--      , ( n % 3 == 0 ∧ ¬n % 5 == 0, Just [Fizz, Buzz])
--      ]
--
-- Note that this may also be represented as below:
--
--      [ ( n % 3 == 0 ∧ ¬n % 5 == 0, Just [Fizz])
--      , (¬n % 3 == 0 ∧  n % 5 == 0, Just [Buzz])
--      , ( n % 3 == 0 ∧ ¬n % 5 == 0, Just [Fizz, Buzz])
--      , (¬n % 3 == 0 ∧ ¬n % 5 == 0, Nothing)
--      ]
--
-- when the final vacuous branch contraint may have not been checked for
-- satisfiability.

-- In general a branch of the form:
--
--    ExecBranch (constr, Just [])
--
-- represents a vacuous case. Thus the following result is also semantically
-- valid, explicitly representing the vacuous case:
--
--      [ ( n % 3 == 0 ∧ ¬n % 5 == 0, Just [Fizz])
--      , (¬n % 3 == 0 ∧  n % 5 == 0, Just [Buzz])
--      , ( n % 3 == 0 ∧ ¬n % 5 == 0, Just [Fizz, Buzz])
--      , (¬n % 3 == 0 ∧ ¬n % 5 == 0, Just [])
--      ]
--
--   Currently, we consider it invalid to use `Just []` when the *path* condition
--   for the state is satisfiable. If supported, it would represent a semantics
--   similar to Boogie/Dafny's `assume false;`.

instance Applicative (RewriteSymbolic s) where
    pure x = RewriteSymbolic $ \cs -> [ExecBranch dTrue $ Just [(x, cs)]]
    (<*>) = ap

instance Alternative (RewriteSymbolic s) where
    empty = matchFail
    {-# INLINE (<|>) #-}
    r1 <|> r2 = RewriteSymbolic $ \cs ->
        do -- List monad easily lets us compute the cartesian product.
           -- Other exec branches will take care of the negations of
           -- constr1, constr2, since we maintain that the disjunction of the
           -- constraints is total.
           (ExecBranch constr1 states1) <- (rewriterSymbolic r1) cs
           (ExecBranch constr2 states2) <- (rewriterSymbolic r2) cs
           pure (ExecBranch (dAnd constr1 constr2) (sum states1 states2))
      where
        sum Nothing s2 = s2
        sum s1 Nothing = s1
        sum (Just s1) (Just s2) = Just (s1 ++ s2)

instance Monad (RewriteSymbolic s) where
    {-# INLINE (>>=) #-}
    r1 >>= r2 = RewriteSymbolic $ \cs ->
        do br <- (rewriterSymbolic r1) cs
           sequenceDeterministicBr br
      where
        sequenceDeterministicBr (ExecBranch c1 Nothing) = [ExecBranch c1 Nothing]
        sequenceDeterministicBr (ExecBranch c1 (Just [(a1, s1)])) =
            do ExecBranch c2 s2s <- (rewriterSymbolic (r2 a1)) s1
               pure $ ExecBranch (dAnd c1 c2) s2s
        sequenceDeterministicBr (ExecBranch c1 (Just (_:_:_))) = error "Non-deterministic branch of r1!"

instance MonadRewrite (RewriteSymbolic s) SymbolicExpr (s SymbolicExpr) where
    get = RewriteSymbolic $
        \cs -> ([ExecBranch dTrue $ Just [(state cs,  cs)]])
    put s' = RewriteSymbolic $
        \cs -> ([ExecBranch dTrue $ Just [((), Constrained s' (constraint cs))]])
    matchFail = RewriteSymbolic $
        \_ -> ([ExecBranch dTrue   Nothing])
    sguard sbool = RewriteSymbolic $
        \(Constrained s constr) -> [ ExecBranch sbool $ Just [((), Constrained s (dAnd constr sbool))]
                                   , ExecBranch (dNot sbool) Nothing
                                   ]

-- MonadFail allows us to have binding patterns that fail in do notation.
instance MonadFail (RewriteSymbolic s) where
    fail _ = matchFail

------------------------------------------------------------------------
--  All path evaluation: Returns the result of applying (rewrites+), that is
--  one or more rewrites. Using the SMT solver to prune branches.
--  Evaluation uses a depth-first strategy
--  to take maximal advantage of the SMT solver's (push) and (pop) directives.
--  This is a somewhat limited tool, giving little control over execution.
--  It should mainly be used when termination is guaranteed relatively quickly.

termToSymbolic :: forall s. (DomainFunctor s) =>
    (Constrained s Term) -> Query (Constrained s SymbolicExpr)
termToSymbolic t = T.evalStateT (dmapM fromTerm t) M.empty

evalAllPathsSymbolic :: forall s. (DomainFunctor s, Show (s SymbolicExpr)) =>
    [RewriteSymbolic s ()] -> Integer -> (Constrained s Term) -> IO (ExecResult s ())
evalAllPathsSymbolic rewrites depth cstate
    = runSMT $ query $ do
            init <- termToSymbolic cstate
            eval' depth Nothing $ ExecBranch (constraint init)
                    (Just [((), init)])
  where
    eval' :: Integer -> Maybe (Constrained s SymbolicExpr) -> ExecBranch s () -> Query (ExecResult s ())
    --       depth   -> curr: State we arrived here from   -> next-state      -> final states

    -- We split off the expr from sbvcond so that we do not need to repeatedly
    -- assert the entire path condition, rather, only the incremental addition.
    eval' depth cs br@(ExecBranch (SymbolicExpr sbvcond _) _) | depth > 0
          -- case analysis to avoid calling into the sat solver
          -- when unnessesary.
        = case unliteral sbvcond of Just True -> satCase         depth cs br
                                    _         -> checkWithSolver depth cs br
    eval' _ _ br = pure [br]

    checkWithSolver depth cs br@(ExecBranch condexpr@(SymbolicExpr sbvcond _) _)
      = inNewAssertionStack $
        do constrain sbvcond
           res <- checkSat
           case res of
               Sat -> satCase depth cs br
               Unsat -> pure [] -- vacuous
               Unk -> error "Solver returned unknown!"
               DSat _ -> error "Solver returned DSat?!"

    satCase :: Integer -> Maybe (Constrained s SymbolicExpr) -> ExecBranch s () -> Query (ExecResult s ())

    -- If no rule matches, then (rewrite*) return just the last reachable state.
    -- If no steps were taken, we return Nothing.
    satCase depth cs (ExecBranch condexpr Nothing)
     = pure [ExecBranch condexpr $ fmap (\x -> [((), x)]) cs]

    satCase depth cs br@(ExecBranch condexpr (Just [])) = pure [br]

    satCase depth cs (ExecBranch exprcond@(SymbolicExpr sbvcond termcond) (Just [((), next)])) =
        do -- Since we already asserted the condition in this Query context,
           -- we do not need that as part of the Constrained for continued
           -- execution.
           let (nextbrs :: [ExecBranch s ()]) = nexts next
           let (combine :: ExecBranch s () -> ExecBranch s ()) = \(ExecBranch n_exprcond@(SymbolicExpr n_sbvcond n_termcond) n_n) -> ExecBranch
                     (SymbolicExpr
                         -- We have already asserted the condition in
                         -- this query context. We don't need it anymore.
                         n_sbvcond

                         -- However, the full term representation
                         -- f the condition is needed for the return value.
                         (dAnd termcond n_termcond)
                     )
                     n_n
           concatMapM ((eval' (depth-1) (Just next)) . combine) nextbrs

    satCase depth cs (ExecBranch (SymbolicExpr _ expr) (Just ss@(_:_:_))) =
        error $ "Non-determinism not implemented.\n\n" ++ (show expr) ++ "\n\n" ++ (show ss)

    --  At each step next gives all new states derived from a single input --  state.
    nexts :: Constrained s SymbolicExpr -> ExecResult s ()
    nexts s = rewriterSymbolic (foldl (<|>) empty rewrites) s


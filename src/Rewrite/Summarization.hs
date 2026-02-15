{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Rewrite.Summarization
    ( Subst
    , SomeDomainTerm(..)
    , Summarizable(..)
    , SummaryState(..)
    , MonadSummary, freshName
    , MonadSummaryMaybe
    , summarize
    )
  where

import           Control.Applicative
import qualified Control.Monad.State.Class as State
import           Control.Monad
import           Control.Monad.Extra (concatMapM)
import           Control.Monad.State hiding (state)
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.SBV
import           Data.SBV.Trans.Control

import           Domain
import           Domain.Class
import           Domain.SymbolicExpr (fromTerm)
import           Rewrite.Symbolic

------------------------------------------------------------------------------
-- Summarization uses symbolic execution, and abstract interpretation
-- to reduce a program to its control flow graph while simultanuously
-- capturing the state transitions possible.
--
-- To do this, the language needed to support some operations:


class (DomainFunctor s, Ord (s Term), Eq (s Term)) => Summarizable s   where
    -- For summarizing a program, we need to partition the language's rules
    -- into two sets:

    -- Basic rules are rules that make progress in a programs execution
    -- but are, on their own guaranteed to terminate.
    -- We, however, allow non-determinism in these rules, distinguishing
    -- them from equational rules in Maude.
    basicRules :: [RewriteSymbolic s ()]

    -- Cut rules are the rules at which basic blocks are "cut".
    -- They break the program execution into finite or infinite series of
    -- block. These rules are needed to enable Turing complete behaviour such
    -- as iteration and recursion.
    cutRules :: [RewriteSymbolic s ()]

    -- Given two states, compute a state that is more general that both.
    -- This may, for example, climb an abstraction lattice.
    -- Different abstraction lattices may be used for different purposes.
    -- `MonadSummary s` instantiates `MonadError` and can be used to indicate
    -- failure. Failure indicates that the terms are of different
    -- control flow nodes.
    --
    -- In terms of matching logic, we need to find a single pattern, such that
    -- $s₁ ∨ s₂ → (merge s₁ s₂)$.

    merge :: s Term -> s Term -> MonadSummary s (Maybe (s Term))

    -- TODO: This should be automatically definable.
    -- Syntactically checks that $s₁ → s₂$, and
    -- returns the corresponding substitution.
    isCovered :: s Term -> s Term -> Maybe Subst

    {-# MINIMAL basicRules, cutRules, merge, isCovered #-}

-- The result of summarzation is a multi-graph, we call a CFG.
-- The nodes in the CFG are labeled by symbolic states with constraints.
-- These constraints represent invariants at each control-flow node.
--
-- The edges may be labeled in three different ways:
--
data Successor s
-- BasicBlocks are when the successor state is reachable *unconditionally*
-- (i.e. without additional constraints) via application of 'basic' rules,
-- optionally preceeded by a single application of a 'cutpoint' rule.
--
-- 'BasicBlock []' represents the case where there are no such successors
-- i.e. a terminal state. When the argument is a list of length greater than
-- one, it represents non-determinism.
                 = BasicBlock [Constrained s Term]
-- 'Split' edges represent breaking up a symbolic state into a
-- set of more restricted ones e.g. by adding side conditions or narrowing.
-- The disjunction of successor nodes must equal the original.
                 | Split [Constrained s Term]
-- 'Cover' represents replacing a state with a more general one.
-- e.g. by abstracting concrete parts of the state into symbolic ones,
-- or removing side conditions.
                 | Cover (Constrained s Term)
-- In addition, 'Unexplored' represents an incomplete CFG, where further
-- iterations of the algorithm may make further progress.
                 | Unexplored

deriving instance Show (s Term) => Show (Successor s)
deriving instance Eq (s Term) => Eq (Successor s)
deriving instance Ord (s Term) => Ord (Successor s)

targets :: Successor s -> [Constrained s Term]
targets (BasicBlock ns) = ns
targets (Cover n) = [n]
targets (Split ns) = ns
targets Unexplored = []


data SummaryState s = SummaryState {
    freshNames :: [String],

    -- TODO: Consider implementing as directed (possibly cyclic) graph,
    -- via "tying the knot".
    -- We maintain the invariant that every value in the map is also a key.
    nodes :: Map (Constrained s Term) (Successor s),
    root :: Constrained s Term
}

-- Monad for managing state during summarization
type MonadSummary s = StateT (SummaryState s) IO
execSummary :: MonadSummary s a -> SummaryState s -> IO (SummaryState s)
execSummary = execStateT

type MonadSummaryMaybe s = MaybeT (StateT (SummaryState s) IO)

freshName :: MonadSummary s String
freshName = do state <- State.get
               case freshNames state of
                 [] -> error "Not enough freshNames" -- generally we expect infinite fresh names.
                 n:ns -> do State.put (state{freshNames=ns})
                            pure $ n

unexplored :: MonadSummary s [Constrained s Term]
unexplored = do ns <- fmap nodes State.get
                pure  $ ((map fst).(filter isUnexplored).(M.toList)) ns
  where
     isUnexplored (_,Unexplored) = True
     isUnexplored (_,_) = False


-- | Find the canonical representitive for each node.
-- This is done by following covers to the most general node.
representatives :: forall s. Ord (s Term) => MonadSummary s [Constrained s Term]
representatives = do ns <- fmap nodes State.get
                     mapM representitive (M.keys ns)
  where
    representitive :: Constrained s Term -> MonadSummary s (Constrained s Term)
    representitive node = do nodes <- fmap nodes State.get
                             case (M.lookup node nodes) of
                                (Just (Cover cov)) -> representitive cov
                                _ -> pure node

prune :: Ord (s Term) =>
    MonadSummary s ()
prune =
  do summ <- State.get
     State.put $ summ{nodes=M.fromList $ prune' (nodes summ) [root summ] []}

prune' :: Ord (s Term) =>
    Map (Constrained s Term) (Successor s)
    -> [Constrained s Term] -> [(Constrained s Term, Successor s)] -> [(Constrained s Term, Successor s)]
prune' map (n:ns) visited =
        case lookup n visited of
            Just _  -> prune' map ns visited
            Nothing -> let succ = fromMaybe undefined $ M.lookup n map in
                         prune' map (ns ++ targets succ) ((n, succ):visited)
prune' _map [] visited = visited

printState :: Show (s Term) => MonadSummary s ()
printState = do
    st <- State.get
    void $ liftIO $ mapM printNode (M.toAscList $ nodes st)
  where
    printNode (s, succ) = do
        putStrLn "Node:"
        putStr "     "; putStrLn (show s)
        putStr "     "; putStrLn (show succ)
        putStrLn ""

data SomeDomainTerm = forall a. SomeDomainTerm (Term a)
deriving instance Show SomeDomainTerm

type Subst = Map String SomeDomainTerm


summarize :: forall s. Summarizable s
    => Show (s Term)
    => s Term -> IO (SummaryState s)
summarize init = execSummary
    (summarize' >> printState)
    (SummaryState freshVars (M.fromAscList [(initNode, Unexplored)]) initNode)
  where
    initNode = Constrained init (dBool True)
    freshVars = map (\i -> (show @Integer i)) [1..]

summarize' :: forall s. Summarizable s
    => Show (s Term)
    => MonadSummary s (Maybe ())
summarize'
    = runMaybeT $     (MaybeT extend >> MaybeT summarize')
                  <|> pure ()

extend :: forall s. Summarizable s => MonadSummary s (Maybe ())
extend = runMaybeT $ do
    unex:_ <- lift unexplored

           -- We are in the MaybeT monad,
           -- so matching failure results in Nothing.
    rs <- lift representatives
    let distinct = filter (/= unex) rs

    asum -- alternative sum
    -- * First, look for an existing cover.
      ( (fmap (void . MaybeT . tryCoverNode unex) distinct)
    -- * Next, try merging
     ++ (fmap (void . MaybeT . tryMergeNodes unex) distinct)
    -- * Finally, extend via symbolic execution.
     ++ [void $ lift $ doBasicBlock unex]
      )
    -- In each case, we don't care about the return value, so we throw
    -- it away using void.

tryCoverNode :: forall s. Summarizable s =>
    Constrained s Term -> Constrained s Term -> MonadSummary s (Maybe (Constrained s Term))
tryCoverNode covering covered = runMaybeT $ do
     st <- State.get
     Just _ <- pure $ isCovered (state covering) (state covered)
     State.put $ st { nodes = M.unionWith updateNode (nodes st) $
                      M.fromList $ [(covered, Cover covering)]
                    }
     pure $ covered

tryMergeNodes :: forall s. Summarizable s =>
    Constrained s Term -> Constrained s Term -> MonadSummary s (Maybe (Constrained s Term))
tryMergeNodes f1 f2 = runMaybeT $ do
     st <- State.get
     merged <- MaybeT $ merge (state f1) (state f2)
     let merged' = Constrained merged (dBool True)
     State.put $ st { nodes = M.unionWith updateNode (nodes st) $
             M.fromList $ [ (f1, Cover merged')
                          , (f2, Cover merged')
                          , (merged', Unexplored)
                          ] }
     lift prune
     pure $ merged'

-- Helper for resolving conflicts during map updates/merges
updateNode :: Successor s -> Successor s -> Successor s
-- Reject unexplored
updateNode Unexplored n2    = n2
updateNode n1 Unexplored    = n1
-- Prefer covers
updateNode n1@(Cover _) _   = n1
updateNode _ n2@(Cover _)   = n2
updateNode n1 _             = n1


------------------------------------------------------------------------
--  All path evaluation

-- Return all terminal states (leaves of an execution tree) using
-- depth-first evaluation.
doBasicBlock :: forall s. Summarizable s =>
    Constrained s Term -> MonadSummary s [Constrained s Term]
doBasicBlock n = do
    -- atMostOneCut may apply multiple rules internally, before checking
    -- with the SMT solver. So we only need to check one path condition.
    result <- liftIO $ evalAllPathsSymbolic [atMostOneCut] 1 n
    let split_targets = fmap (\br -> (Constrained (dAnd (constraint n) (constr br)) (state n))) result
    let split_edge = [n, Split split_targets]
    let bb_edges = fmap (\(br,split) -> (split, BasicBlock (states br))) (zip result split_targets)
    let reached_nodes = concatMap (maybe [] (fmap snd).states) result
    let unexplored_edges = zip reached (repeat Unexplored)
    st <- State.get

    State.put $ st { nodes = M.unionWith updateNode (nodes st) $
       M.fromList $  [split_edge] ++ bb_edges ++ unexplored_edges }
    pure reached_nodes
  where
    basic = (foldl (<|>) empty basicRules)
    cuts = (foldl (<|>) empty cutRules)
    basicStar = many basic

    atMostOneCut :: RewriteSymbolic s ()
    atMostOneCut = (cuts <|> pure ()) >> basicStar

{-  Testing of GHC pattern matching optimization.

    Given a single function with many cases, each of which has complex and
    partially overlapping patterns, GHC will optimize these to a decision
    tree that minimizes the number of matches and decisions it needs to
    make to find the matching pattern, always using only the first match.

    However, it's nicer for us to define sets of rules as sets of functions
    (a separate function for each rule) and then combine them later; this
    also allows us also to e.g. execute _all_ matches so that we trace
    through all execution paths of a non-deterministic semantics. (We may
    want to choose different execution strategies at build or even runtime
    to achieve different goals, e.g. model checking vs. concrete execution.)

    But when we do this, GCC is no longer able to apply the optimizations
    it uses for matching the pattern cases of a single function. Other
    systems build their own decision tree to do this; we're wondering if
    there's a way to convince GCC to do some of this optimization for us.

-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

----------------------------------------------------------------------
-- A "deep state."

data State  = A A           | AStuff        deriving (Show)
data A      = B B           | BStuff        deriving (Show)
data B      = C C           | CStuff        deriving (Show)
data C      = CI Int
            | CB Bool                       deriving (Show)

-- Accessor for some data deep in the state.
liftC :: (C -> Maybe C) -> State -> Maybe State
liftC f (A (B (C tval))) = case (f tval) of
                                Just x  -> Just (A (B (C x)))
                                Nothing -> Nothing
liftC _f _state          = Nothing

----------------------------------------------------------------------
-- Rules that work on the 'C' structure within the state.

decrement :: C -> Maybe C
decrement (CI n)  | n > 0  = Just (CI $ n-1)
decrement _                = Nothing

toggle :: C -> Maybe C
toggle (CB b) = Just (CB $ not b)
toggle _      = Nothing

----------------------------------------------------------------------
-- Produce the next state in the program's transition system.

-- 'liftC' distributed across the list of rules.
next_dist :: State -> Maybe State
next_dist = (liftC toggle) `orElse` (liftC decrement)

-- 'liftC' optimized based on the distributive property.
-- (This is another example of an optimization we'd like, but that the
-- compiler does not have enough information to do.)
next_undist :: State -> Maybe State
next_undist = liftC (toggle `orElse` decrement)

-- Combine all rules into a single function.
-- (Basically, a big hand-optimization.)
next_combined :: State -> Maybe State
next_combined (A (B (C (CB b))))            = Just (A (B (C (CB $ not b))))
next_combined (A (B (C (CI n)))) | n > 0    = Just (A (B (C (CI $ n-1))))
next_combined _                             = Nothing

----------------------------------------------------------------------
--  The Rewrite Rule System

--  A single rewrite rule applied to something can either match, in which
--  case it produces a new something, or not match, in which case that's
--  a signal to try another rewrite rule or just give up.
type Rewrite a      = a -> Maybe a

--  A Semantics is just a set of rewrite rules that someone will apply
--  using whatever strategy they like (one by one stopping on first match,
--  all at once producing an execution "tree," or whatever).
type Semantics a    = Rewrite a

--  Strategy for combining rewrite rules:
--  try second rule if first one doesn't match.
orElse :: Rewrite a -> Rewrite a -> Rewrite a
orElse r1 r2 = \state -> case (r1 state) of
                              Nothing  -> r2 state
                              Just st' -> Just st'

--  Evalulate a Semantics by repeatedly applying the first-matched rule
--  until no rules match.
eval :: Semantics a -> a -> a
eval rewrites state = eval' state (rewrites state)  where
    eval' s Nothing   = s
    eval' _ (Just s') = eval' s' (rewrites s')

----------------------------------------------------------------------
-- Benchmarking

main :: IO ()
main = do  print init
           print $ eval next init
       where
           init    = (A (B (C (CI count))))
           count   = 1_000_000_000
       --  next    = next_dist      -- ~6 seconds
       --  next    = next_undist    -- ~4 seconds
           next    = next_combined  -- ~4 seconds (same because few rules)

{-  Testing of GHC pattern matching optimization. -}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main, next_dist, next_undist, next_combined) where
-- We export all functions for which we want to see the GHC core output so
-- that they don't get optimized away.

----------------------------------------------------------------------
-- A "deep state." For the purposes of this example we are using
-- functions that access only stuff in the 'C' object deep down in
-- the state, but in a a real system all of the levels would have
-- lots of other things in them as well, represented by the (unused
-- here) '*OtherStuff' constructors.

data State  = A A           | AOtherStuff   deriving (Show)
data A      = B B           | BOtherStuff   deriving (Show)
data B      = C C           | COtherStuff   deriving (Show)
data C      = CI Int
            | CB Bool                       deriving (Show)

-- "Accessor" for some data deep in the state.
liftC :: (C -> Maybe C) -> State -> Maybe State
liftC f (A (B (C tval))) = case (f tval) of
                                Just x  -> Just (A (B (C x)))
                                Nothing -> Nothing
liftC _f _state          = Nothing

----------------------------------------------------------------------
-- Rewriting logic-style rules that work on the 'C' structure within the
-- state. If they match particular bits of structure they apply and update
-- the state, otherwise they give 'Nothing' and presumably the system will
-- attempt to apply other rules.

decrement :: C -> Maybe C
decrement (CI n)  | n > 0  = Just (CI $ n-1)
decrement _                = Nothing

toggle :: C -> Maybe C
toggle (CB b) = Just (CB $ not b)
toggle _      = Nothing

----------------------------------------------------------------------
-- A 'next' function produces the the next state in the ruleset's
-- transition system. Here we use a simple strategy of "apply each avalable
-- rule in turn, returning the new state from the first match, or nothing
-- if no rules matched."
--   We have three different implementations here, all of which encode
-- the same ruleset but at different levels of "optimization."

-- Combine all rules into a single function. GHC optimizes this extremely
-- well, creating a decision tree that does minimal work to check all the
-- cases. In particular, it will not repeat the '(A (B (C …)))' match,
-- seeing that it needs to do that only once.
next_combined :: State -> Maybe State
next_combined (A (B (C (CB b))))            = Just (A (B (C (CB $ not b))))
next_combined (A (B (C (CI n)))) | n > 0    = Just (A (B (C (CI $ n-1))))
next_combined _                             = Nothing

-- Here we use separate functions but have hand-optimized it to bring
-- 'liftC' up to the top. (This works because 'liftC' is distributive here,
-- though GHC does not know that. This produces the same GHC core as above
-- because GHC seems to be able to identify that, though we now have
-- separate functions with the toggle and decrement patterns, it can see
-- that 'liftC' applies a common pattern prefix to both.
next_undist :: State -> Maybe State
next_undist = liftC (toggle `orElse` decrement)

-- When we distribute 'liftC' across the list of rules, we have a
-- semantically equivalant version to the two versions above, but GHC
-- is no longer able to optimize this so well; it produces core that
-- matches down through A, B, C… for 'toggle', and if that fails, does
-- the whole A, B, C… match again for 'decrement'. This particular case
-- is easily hand-opimized to the above, but if we had more rules some
-- of which did not use 'liftC', that becomes much harder to do and
-- produces much messier source code.
next_dist :: State -> Maybe State
next_dist = (liftC toggle) `orElse` (liftC decrement)

----------------------------------------------------------------------
--  The Rewrite Rule System

--  A single rewrite rule applied to something can either match, in which
--  case it produces a new something, or not match, in which case that's
--  a signal to try another rewrite rule or just give up. Multiple rewrite
--  rules can be combined as below.
type Rewrite a      = a -> Maybe a

--  Strategy for combining rewrite rules:
--  try second rule if first one doesn't match.
orElse :: Rewrite a -> Rewrite a -> Rewrite a
orElse r1 r2 = \state -> case (r1 state) of
                              Nothing  -> r2 state
                              Just st' -> Just st'

--  Evalulate a Rewrite rule by repeatedly applying the first-matched rule
--  until no rules match.
eval :: Rewrite a -> a -> a
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

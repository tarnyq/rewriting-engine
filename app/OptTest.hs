{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module OptTest (main) where

-- A "deep state."
data State  = A A           | AStuff        deriving (Show)
data A      = B B           | BStuff        deriving (Show)
data B      = C C           | CStuff        deriving (Show)
data C      = CI Int
            | CB Bool                       deriving (Show)

liftC :: (C -> Maybe C) -> State -> Maybe State
liftC f (A (B (C tval))) = case (f tval) of
                                Just x  -> Just (A (B (C x)))
                                Nothing -> Nothing
liftC _f _state          = Nothing

----------------------------------------------------------------------
-- Rules that work on the 'C' bit within the state.

decrement :: C -> Maybe C
decrement (CI n)  | n > 0  = Just (CI $ n-1)
decrement _                = Nothing

mynot :: C -> Maybe C
mynot (CB b) = Just (CB $ not b)
mynot _      = Nothing

----------------------------------------------------------------------

next_dist :: State -> Maybe State
next_dist = (liftC decrement) `orElse` (liftC mynot)

next_undist :: State -> Maybe State
next_undist = liftC (decrement `orElse` mynot)

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

--  Combining rewrite rules: try next one if first one doesn't match.
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
           count   = 1_000_000_000                -- ~3-4 seconds
           next    = next_dist

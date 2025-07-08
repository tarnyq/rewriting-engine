Redundant pattern matching slowing down our Haskell program
===========================================================

<https://stackoverflow.com/q/79692341/107294>

We are working on some programs that do rewriting logic, where there is a
state (often complex), a set of rules that match certain patterns in this
state, and strategy for applying these rules to produce new states. (A
typical strategy is to find any rule that matches, apply it to produce a new
state, and carry on doing that until no rules match.)

We're at a point where we have a performance problem that seems as if
it should be fixable, but we also don't know what we don't know about how
to better write these things in Haskell. We're posting this to explain
where we are and get advice either on a particular fix for this problem or
on how we should be changing direction to do things better.

The issue is that GHC is not
identifying areas where it can remove redundant pattern matches (i.e.,
re-use pattern matching work that it's already done) because of the way we
separate the rules. (Per above, we welcome better ways of writing this that
increase clarity as well as possibly speeding things up.)

Following is some sample code we came up with that demonstrates the
problem. The state is considerably less complex than what we might be using
in a real application, but having various branches and quite a bit of
depth represents the real situation reasonably well when looking at how the
GHC-generated code does pattern matching. (Again, per above, we're aware
that there may be better ways of "reaching in" to deep data structures like
this and are also hoping for advice on that; our `liftC` function is a sad
little attempt at this.)

The comments in the code provide most of the details; we give some GHC core
output showing the issue after this.

```haskell
{-# LANGUAGE NumericUnderscores #-}
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
```

The GHC core output for `next_combined`, which is essentially a
"hand-optimized" version of our set of rules, does only one set of matches
for A, B etc., and runs quickly:

```
next_combined
  = \ (ds_d1fB :: State) ->
      case ds_d1fB of {
        A ds1_d1fV ->
          case ds1_d1fV of {
            B ds2_d1fW ->
              case ds2_d1fW of {
                C ds3_d1fX ->
                  case ds3_d1fX of {
                    CI n_aSw ->
                      case n_aSw of { I# x_a2bc ->
                      case ># x_a2bc 0# of {
                        __DEFAULT -> Nothing @State;
                        1# ->
                          Just
                            @State
                            (A
                               (B (C (CI (I# (-# x_a2bc 1#))))))
                      }
                      };
                    CB b_aSv ->
                      Just
                        @State
                        (A
                           (B
                              (C
                                 (CB
                                    (case b_aSv of {
                                       False -> True;
                                       True -> False
                                     })))))
                  };
                COtherStuff -> Nothing @State
              };
            BOtherStuff -> Nothing @State
          };
        AOtherStuff -> Nothing @State
      }
```

`next_undist` produces exactly the same code, no doubt because `liftC` is
applied to this entire set of rules after combining them at the "data C"
level. It would not normally be the case that we'd have a set of rules that
operates on just one small part of the state, though, and if we distribute
the `liftC` down on to each individual rule, bringing them separately up to
the "data A" level as in `next_dist` we see the problem:

```
next_dist
  = \ (state_aSz :: State) ->
      join {
        $j_s2aS [Dmd=ML] :: Maybe State
        [LclId[JoinId(0)(Nothing)]]
        $j_s2aS
          = case state_aSz of {
              A ds_d1gJ ->
                case ds_d1gJ of {
                  B ds1_d1gK ->
                    case ds1_d1gK of {
                      C tval_aE1 ->
                        case tval_aE1 of {
                          CI n_aE5 ->
                            case n_aE5 of { I# x_a2bc ->
                            case ># x_a2bc 0# of {
                              __DEFAULT -> Nothing @State;
                              1# ->
                                Just
                                  @State
                                  (A
                                     (B
                                        (C (CI (I# (-# x_a2bc 1#))))))
                            }
                            };
                          CB ipv_s28R -> Nothing @State
                        };
                      COtherStuff -> Nothing @State
                    };
                  BOtherStuff -> Nothing @State
                };
              AOtherStuff -> Nothing @State
            } } in
      case state_aSz of {
        A ds_d1gJ ->
          case ds_d1gJ of {
            B ds1_d1gK ->
              case ds1_d1gK of {
                C tval_aE1 ->
                  case tval_aE1 of {
                    CI ipv_s1hO -> jump $j_s2aS;
                    CB b_aSu ->
                      Just
                        @State
                        (A
                           (B
                              (C
                                 (CB
                                    (case b_aSu of {
                                       False -> True;
                                       True -> False
                                     })))))
                  };
                COtherStuff -> jump $j_s2aS
              };
            BOtherStuff -> jump $j_s2aS
          };
        AOtherStuff -> jump $j_s2aS
      }
```

The initial pattern matching work goes down through A and B and, if it
matches `CI` (meaning that `toggle` has not matched) it jumps to the
matching code for `decrement` (`$j_s2aS`) which here is repeating the entire
matching sequence for A and B to get to CI.

We don't see a huge performance penalty in this particular code
because there are so few rules, but when we have dozens of rules there the
program can easily take two or three times as long to run.

Note that, for simplicity, this code does ordered matching, but that's not
usually important to us; we're perfectly happy in most cases to have
matches occur in any order, though we're not clear on how to compose a set
of rules or build a strategy to do this.

We're looking for thoughts on what we should be doing both in terms of
general design of the Haskell code for this kind of application and
specifically for making this type of thing run fast.

module Rewrite.Basic
    (RewriteBasic(..), evalOnePath, evalAllPaths) where

import Data.Maybe
import Control.Applicative
import Control.Monad

import Rewrite.Class

----------------------------------------------------------------------
--  This is the simplest instace for MonadRewrite.
--  It represents a *possible* transition over a state.
--  It is the minimal generalization of a rewrite rule that enables
--  a monadic interface through the additional `a` parameter.
--  allowing returning a value, besides updating the state.
--
--  It is also a generalization of Haskell's State Monad in that it
--  is possible for the action to not "match", returning a Nothing.
--  This lets us try multiple rewrites in parallel until one succeeds.

newtype RewriteBasic s a = RewriteBasic { rewriter :: s -> Maybe (a, s) }
    deriving Functor

instance Applicative (RewriteBasic s) where
    pure x = RewriteBasic (\s -> Just (x, s))
    (<*>) = ap

-- Alternative allows *parallel* composition of Rewrites--i.e.
-- if one fails, we fallback to the other. This is the 'Applicative'
-- version of 'MonadPlus'; 'MonadPlus' reuses these definitions.
instance Alternative (RewriteBasic s) where
    empty = matchFail
    {-# INLINE (<|>) #-}
    r1 <|> r2 = RewriteBasic $ \s -> ((rewriter r1) s) <|> ((rewriter r2) s)

instance Monad (RewriteBasic s) where
    {-# INLINE (>>=) #-}
    --          -> RewriteBasic { rewriter :: s -> Maybe (a, s) }
    p >>= q = RewriteBasic $
        \s -> do (a', s') <- ((rewriter p) s)
                 ((rewriter $ q a') s')

-- MonadFail allows us to have binding patterns that fail in do notation.
instance MonadFail (RewriteBasic s) where
    fail _ = RewriteBasic $ \_ -> Nothing

instance MonadRewrite (RewriteBasic s) s where
    get    = RewriteBasic $ \s -> Just (s, s)
    put s' = RewriteBasic $ \_ -> Just ((), s')

{-  The functions below are (nearly) forced always to be inlined because
    we use INLINE instead of INLINABLE; GHC is not eager enough to inline
    them otherwise producing a 2-3× slower running result. For situations
    such as `orElse` which is trivially small this isn't a big deal, but
    it might be for larger functions such as `eval`. What we probably
    want to do here is use INLINABLE and supply rewrite rules[1] that
    help GHC do proper inlining; this is to be investigated in the future.
    [1]: https://wiki.haskell.org/Inlining_and_Specialisation#What_is_specialisation
-}

------------------------------------------------------------------------
--  One path evaluation

{-# INLINE evalOnePath #-}
-- Return the terminal state of one path through the execution tree using
-- first-match evaluation.
evalOnePath :: forall s. [RewriteBasic s ()] -> s -> s
evalOnePath rewrites state = unwrap $ applyRewrite eval' state
  where
    -- If all rewrites fail, return the existing state instead of failing.
    -- Thus, when no rules match, we get the terminal state instead of
    -- a failure. This tail recurses (we think) on eval'.
    eval' :: RewriteBasic s ()
    eval' =     (next >> eval') -- If next succeeds, recurse
            <|> pure ()         -- otherwise return the previous state

    next :: RewriteBasic s ()
    next = asum rewrites -- Choose first rewrite that applies

    -- asum :: (Foldable t, Alternative f) => t (f a) -> f a

    unwrap :: Maybe a -> a
    unwrap = fromMaybe undefined -- always returns a Just.

    applyRewrite :: RewriteBasic s () -> s -> Maybe s
    applyRewrite r = (fmap snd) . (rewriter r)


------------------------------------------------------------------------
--  All path evaluation

-- Return all terminal states (leaves of an execution tree) using
-- depth-first evaluation.
evalAllPaths :: forall s. [RewriteBasic s ()] -> s -> [s]
evalAllPaths rewrites s = eval' [s] (next s) where

    -- We process the list of current states (cs) in depth-first order,
    -- producing all successor states for the first before moving on
    -- to the next. Arguments:
    -- * The stack of current states (cs).
    -- * The list successor states for just the current state at the
    --   top of the stack (ns).
    eval' :: [s] -> [s] -> [s]

    -- If there are no current states left we are done.
    eval' [] _  = []

    -- If the list of successor states is empty, this is a terminal state.
    -- Add it to the output states list and continue evaluation with the
    -- next input state.
    eval' (c:cs) [] = c:(eval' cs (nextHead cs))

    -- If we have successor states, the current state is non-terminal.
    -- We push its successors to the stack, and process the next
    -- state in the stack. Since we are using a stack we get a depth-first
    -- traversal. Replacing (ns++cs) with (cs++ns) we would instead give us
    -- a queue and a breadth first traversal.
    eval' (_:cs) ns = eval' (ns++cs) (nextHead ns)

    -- Given a list of states, return the successors of the first.
    -- If the list is empty return nothing.
    nextHead :: [s] -> [s]
    nextHead []     = []
    nextHead (s:_)  = (next s)

    --  At each step next gives all new states derived from a single input
    --  state, but also drops any terminal states from the previous step.
    next :: s -> [s]
    next = foldr parRewrite (\_ -> []) (map rewriteListResult rewrites)

    -- Given a rewrite rule, convert the result from a Maybe to a List.
    rewriteListResult :: RewriteBasic s () -> (s -> [s])
    rewriteListResult rw = \s -> case ((rewriter rw) s) of
                                  Nothing -> []
                                  Just((), s') -> [s']

    -- Combine two rewrites-to-list into s single rewrite-to-list
    -- by applying them in parallel.
    parRewrite :: (s -> [s]) -> (s -> [s]) -> (s -> [s])
    parRewrite r1 r2 = \state -> (r1 state) ++ (r2 state)

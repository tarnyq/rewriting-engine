module Tarnyq
    (   RewriteM(..), Rewrite,
        get, put, guard, fail',
        mkLift,
        evalOnePath, evalAllPaths
    )
  where

import Control.Monad (ap)
import Control.Applicative

----------------------------------------------------------------------
--  RewriteM represents a *possible* transition over a state.
--  It is the minimal generalization of a rewrite rule that enables
--  a monadic interface through the additional `a` parameter.
--  allowing returning a value, besides updating the state.
--
--  It is also a generalization of Haskell's State Monad in that it
--  is possible for the action to not "match", returning a Nothing.
--  This lets us try multiple rewrites in parallel until one succeeds.

newtype RewriteM m s a = RewriteM { getFun :: s -> m (a, s) }
    deriving Functor

instance Monad m => Applicative (RewriteM m s) where
    pure x = RewriteM (\s -> pure (x, s))
    (<*>) = ap

instance Monad m => Monad (RewriteM m s) where
    p >>= q = RewriteM $
        \s -> do (a', s') <- ((getFun p) s)
                 (getFun $ q a') s'


-- MonadFail allows us to have binding patterns that fail in do notation.
instance MonadFail m => MonadFail (RewriteM m s) where
    fail msg = RewriteM $ \_ -> fail msg

-- Since we're throwing out the value anyway, lets not force the caller to
-- think of a value each time.

fail' :: MonadFail m => RewriteM m s a
fail' = fail "dummy"

instance (MonadFail m, Alternative m) => Alternative (RewriteM m s) where
    empty = RewriteM $ \_ -> fail "empty."
    r1 <|> r2 = RewriteM $ \s -> ((getFun r1) s) <|> ((getFun r2) s)


-- Rewrite rules may only update the State
type Rewrite m s = RewriteM m s ()


guard :: MonadFail m => Bool -> Rewrite m s
guard True  = pure ()
guard False = fail'

-- Similar to the State Monad, we can get and put the state.
get :: MonadFail m => RewriteM m s s
get = RewriteM $ \s -> pure (s, s)

put :: MonadFail m => s -> RewriteM m s ()
put s = RewriteM $ \_ -> pure ((), s)

-- "Contexts" may be defined using two functions: "unplug", that pulls a
-- subterm (called the plug (noun)) out of a larger term, and
-- "plug", that puts it back in.
-- The "unplug" function is more general than a projection function
-- (e.g. fst, snd) that similarly pull subterms out of terms, in that it may
-- fail e.g. due to pattern matching failing.

-- For any context, we may lift rewrites on the subterm's type
-- to the context's type.

{-# INLINE mkLift #-}
mkLift :: MonadFail m =>
    (s -> Maybe (p, c)) -> (p -> c -> s) -> (RewriteM m p a -> RewriteM m s a)
mkLift unplug plug rw
  = RewriteM $ \s ->
       case unplug s of
            Just (p, ctx) -> do (a, p') <- (getFun rw) p
                                pure (a, plug p' ctx)
            Nothing       -> fail "no match"

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
evalOnePath :: forall s. [Rewrite Maybe s] -> s -> s
evalOnePath rewrites state = eval' state (next state)  where
    -- Given the current state and the next state/no-state:
    eval' :: s -> Maybe s -> s
    eval' s Nothing   = s                   -- terminal state: done
    eval' _ (Just s') = eval' s' (next s')  -- non-terminal, continue stepping

    -- The next state is from the first rule in [Rewrite s] that matches,
    -- or Nothing if no rules match.
    next :: s -> Maybe s
    next = unwrapRewrite $ foldr (<|>) fail' rewrites

    unwrapRewrite :: Rewrite Maybe a -> (a -> Maybe a)
    unwrapRewrite rw = (fmap snd) . (getFun rw)

    --  Mystery! foldr1 is 1/3 the speed of foldr above.
    --next = unwrapRewrite $ foldr1 orElse rewrites

------------------------------------------------------------------------
--  All path evaluation

-- Return all terminal states (leaves of an execution tree) using
-- depth-first evaluation.
evalAllPaths :: forall a. [Rewrite Maybe a] -> a -> [a]
evalAllPaths rewrites s = eval' [s] (next s) where

    -- We process the list of current states (cs) in depth-first order,
    -- producing all successor states for the first before moving on
    -- to the next. Arguments:
    -- * The stack of current states (cs).
    -- * The list successor states for just the current state at the
    --   top of the stack (ns).
    eval' :: [a] -> [a] -> [a]

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
    nextHead :: [a] -> [a]
    nextHead []     = []
    nextHead (s:_)  = (next s)

    --  At each step next gives all new states derived from a single input
    --  state, but also drops any terminal states from the previous step.
    next :: a -> [a]
    next = foldr parRewrite (\_ -> []) (map rewriteListResult rewrites)

    -- Given a rewrite rule, convert the result from a Maybe to a List.
    rewriteListResult :: Rewrite Maybe a -> (a -> [a])
    rewriteListResult rw = \s -> case ((getFun rw) s) of
                                  Nothing -> []
                                  Just((), s') -> [s']

    -- Combine two rewrites-to-list into a single rewrite-to-list
    -- by applying them in parallel.
    parRewrite :: (a -> [a]) -> (a -> [a]) -> (a -> [a])
    parRewrite r1 r2 = \state -> (r1 state) ++ (r2 state)

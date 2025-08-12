module Tarnyq (Rewrite, evalOnePath, evalAllPaths) where

type Rewrite a = a -> Maybe a

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

{-# INLINE orElse #-}
orElse :: Rewrite a -> Rewrite a -> Rewrite a
orElse r1 r2 = \state -> case (r1 state) of
                              Nothing  -> r2 state
                              Just st' -> Just st'

{-# INLINE evalOnePath #-}
-- Return the terminal state of one path through the execution tree using
-- first-match evaluation.
evalOnePath :: forall a. [Rewrite a] -> a -> a
evalOnePath rewrites state = eval' state (next state)  where
    -- Given the current state and the next state/no-state:
    eval' :: a -> Maybe a -> a
    eval' s Nothing   = s                   -- terminal state: done
    eval' _ (Just s') = eval' s' (next s')  -- non-terminal, continue stepping

    -- The next state is from the first rule in [Rewrite a] that matches,
    -- or Nothing if no rules match.
    next :: Rewrite a
    next = foldr orElse (\_ -> Nothing) rewrites

    --  Mystery! foldr1 is 1/3 the speed of foldr above.
    --next = foldr1 orElse rewrites

------------------------------------------------------------------------
--  All path evaluation

-- Return all terminal states (leaves of an execution tree) using
-- depth-first evaluation.
evalAllPaths :: forall a. [Rewrite a] -> a -> [a]
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
    rewriteListResult :: (a -> Maybe a) -> (a -> [a])
    rewriteListResult rw = \s -> case (rw s) of
                                  Nothing -> []
                                  Just s' -> [s']

    -- Combine two rewrites-to-list into a single rewrite-to-list
    -- by applying them in parallel.
    parRewrite :: (a -> [a]) -> (a -> [a]) -> (a -> [a])
    parRewrite r1 r2 = \state -> (r1 state) ++ (r2 state)

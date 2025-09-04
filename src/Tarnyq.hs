{-# LANGUAGE FunctionalDependencies #-}

module Tarnyq
    (   RewriteM(..),
        MonadRewrite, Rewrite,
        get, put, guard, matchFail,
        mkLift,
        evalOnePath, evalAllPaths,
        Console, printConsole
    )
  where

import Control.Monad (ap)
import Control.Applicative
import Data.Maybe


-- TODO Require MonadPlus so we can use its guard.
class MonadFail (r s) => MonadRewrite r s where
  get   :: r s s
  matchFail :: r s a
  put   :: s -> r s ()

  -- "Contexts" may be defined using two functions: "unplug", that pulls a
  -- subterm (called the plug (noun)) out of a larger term, and
  -- "plug", that puts it back in.
  -- The "unplug" function is more general than a projection function
  -- (e.g. fst, snd) that similarly pull subterms out of terms, in that it may
  -- fail e.g. due to pattern matching failing.
  -- For any context, we may lift rewrites on the subterm's type
  -- to the context's type.
  mkLift :: (s -> Maybe (p, c)) -> (p -> c -> s) -> (r p a -> r s a)

  guard :: Bool -> r s ()
  guard True  = pure ()
  guard False = matchFail

  {-# MINIMAL get, put, matchFail, mkLift #-}

class Monad m => Console m where
  printConsole :: String -> m ()


----------------------------------------------------------------------
--  RewriteM represents a *possible* transition over a state.
--  It is the minimal generalization of a rewrite rule that enables
--  a monadic interface through the additional `a` parameter.
--  allowing returning a value, besides updating the state.
--
--  It is also a generalization of Haskell's State Monad in that it
--  is possible for the action to not "match", returning a Nothing.
--  This lets us try multiple rewrites in parallel until one succeeds.

newtype RewriteM s a = RewriteM { getFun :: s -> Maybe (a, s, [String]) }
    deriving Functor

instance Applicative (RewriteM s) where
    pure x = RewriteM (\s -> Just (x, s, []))
    (<*>) = ap

-- Alternative allows *parallel* composition of Rewrites--i.e.
-- if one fails, we fallback to the other
instance Alternative (RewriteM s) where
    empty = RewriteM $ \_ -> Nothing
    {-# INLINE (<|>) #-}
    r1 <|> r2 = RewriteM $ \s -> ((getFun r1) s) <|> ((getFun r2) s)

instance Monad (RewriteM s) where
    p >>= q = RewriteM $
        \s -> do (aP, sP, outputP) <- ((getFun p) s)
                 (aQ, sQ, outputQ) <- ((getFun $ q aP) sP)
                 pure (aQ, sQ, outputQ ++ outputP)



-- MonadFail allows us to have binding patterns that fail in do notation.
instance MonadFail (RewriteM s) where
    fail _ = RewriteM $ \_ -> Nothing

instance MonadRewrite RewriteM s where
    get   = RewriteM $ \s -> Just (s, s, [])
    put s = RewriteM $ \_ -> Just ((), s, [])
    matchFail = RewriteM $ \_ -> Nothing

    {-# INLINE mkLift #-}
    mkLift unplug plug rw
      = do Just (p, ctx) <- fmap unplug get
           case (getFun rw) p of
             Just (a, p', output) -> RewriteM $ \_ -> Just (a, plug p' ctx, output)
             Nothing              -> matchFail


--- Rewrite rules may only update the State
type Rewrite s = RewriteM s ()

applyRewrite :: Rewrite s -> s -> Maybe (s, [String])
applyRewrite r = (fmap dropFirst) . (getFun r)  where
    dropFirst (_, b, c) = (b, c)

------------------------------------------------------------------------

instance Console (RewriteM s) where
    printConsole str = RewriteM $ \s -> Just ((), s, [str])

putOutput :: [String] -> RewriteM s ()
putOutput output = RewriteM $ \s -> Just ((), s, output)

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
evalOnePath :: forall s. [Rewrite s] -> s -> (s, [String])
evalOnePath rewrites state = unwrap $ applyRewrite eval' state
  where
    eval' :: Rewrite s
    eval' =     (next >> eval') -- If next succeeds, recurse
            <|> pure ()         -- otherwise return the previous state

    next :: Rewrite s
    next = asum rewrites -- Choose first rewrite that applies

    unwrap :: Maybe a -> a
    unwrap = fromMaybe undefined -- always returns a Just.


------------------------------------------------------------------------
--  All path evaluation

-- Return all terminal states (leaves of an execution tree) using
-- depth-first evaluation.
evalAllPaths :: forall s. [Rewrite s] -> s -> [(s, [String])]
evalAllPaths rws s = [evalOnePath rws s]
-- evalAllPaths rewrites s = eval' [s] (next s) where
-- 
--     -- We process the list of current states (cs) in depth-first order,
--     -- producing all successor states for the first before moving on
--     -- to the next. Arguments:
--     -- * The stack of current states (cs).
--     -- * The list successor states for just the current state at the
--     --   top of the stack (ns).
--     eval' :: [s] -> [s] -> [s]
-- 
--     -- If there are no current states left we are done.
--     eval' [] _  = []
-- 
--     -- If the list of successor states is empty, this is a terminal state.
--     -- Add it to the output states list and continue evaluation with the
--     -- next input state.
--     eval' (c:cs) [] = c:(eval' cs (nextHead cs))
-- 
--     -- If we have successor states, the current state is non-terminal.
--     -- We push its successors to the stack, and process the next
--     -- state in the stack. Since we are using a stack we get a depth-first
--     -- traversal. Replacing (ns++cs) with (cs++ns) we would instead give us
--     -- a queue and a breadth first traversal.
--     eval' (_:cs) ns = eval' (ns++cs) (nextHead ns)
-- 
--     -- Given a list of states, return the successors of the first.
--     -- If the list is empty return nothing.
--     nextHead :: [s] -> [s]
--     nextHead []     = []
--     nextHead (s:_)  = (next s)
-- 
--     --  At each step next gives all new states derived from a single input
--     --  state, but also drops any terminal states from the previous step.
--     next :: s -> [s]
--     next = foldr parRewrite (\_ -> []) (map rewriteListResult rewrites)
-- 
--     -- Given a rewrite rule, convert the result from a Maybe to a List.
--     rewriteListResult :: Rewrite s -> (s -> [s])
--     rewriteListResult rw = \s -> case ((getFun rw) s) of
--                                   Nothing -> []
--                                   Just((), s') -> [s']
-- 
--     -- Combine two rewrites-to-list into s single rewrite-to-list
--     -- by applying them in parallel.
--     parRewrite :: (s -> [s]) -> (s -> [s]) -> (s -> [s])
--     parRewrite r1 r2 = \state -> (r1 state) ++ (r2 state)

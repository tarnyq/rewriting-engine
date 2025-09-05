module Rewrite.Class
    (MonadRewrite(..))
  where

-- |MonadRewrite defines an abstract interface for defining rewriting rules.
--  Language semantics are defined using this interface, and instances
--  such as `Rewrite.Basic.Rewrite`, `Rewrite.IO.RewritePure`, or
--  `Rewrite.IO.RewritePure` may be chose to suit the particular task at hand.

class MonadFail m => MonadRewrite m s | m -> s where

  -- |`get` allows us to bind the left-hand side of a rewrite, and, togther
  --  with the monadic bind operator, match on in.
  get   :: m s

  -- |`matchFail` and its wrapper `guard`  allow us to define conditional
  -- rewrites (i.e. rewrites with requires clauses),
  matchFail :: m a

  -- |`guard` is a convenience wrapper around `matchFail`. This is analogous to
  --  K's "requires" claus.
  guard :: Bool -> m ()
  guard True  = pure ()
  guard False = matchFail

  -- |`put` allows us to build the right-hand side of the rule.
  put   :: s -> m ()

  {-# MINIMAL get, put, matchFail #-}

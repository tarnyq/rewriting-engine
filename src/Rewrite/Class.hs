module Rewrite.Class
    (MonadRewrite(..))
  where

import           Rewrite.Domain (DomainValue)

--  | Each rewriting rule (or combinator used to help build a rewriting
--  rule) is a 'MonadRewrite' of 'm' and 's', where 's' is the state that
--  is updated as rewrite rules are applied and 'm' is a context that
--  provides e.g. different types of I/O, ability to do all-paths
--  execution, limits to execution depth, etc.
--
--  Typically a language designer will define her own state class 's' but
--  leave 'm' as a parameter to allow use of Tarnyq-supplied rewrite
--  modules, such as 'Rewrite.Basic.Rewrite', 'Rewrite.IO.RewritePure',
--  'Rewrite.IO.RewriteIO', etc. to suit the particular task at hand.
--
class (DomainValue dv, MonadFail m)
    => MonadRewrite m dv s | m -> s, m -> dv where

  --  | Get the state 's' of the MonadRewrite. The result is typically
  --  bound to a pattern; if the pattern match fails, the rule immediately
  --  fails (see 'matchFail' below). (I.e., this expresses the left-hand
  --  side of a rewrite rule.)
  get   :: m s

  --  | Set the state 's' of the MonadRewrite. (This typically expresses
  --  the right-hand side of a rewrite rule.)
  put :: s -> m ()

  --  | 'matchFail' indicates that the rule does not match (and thus cannot
  --  be applied). This is MonadFail's 'fail' without a String argument.
  --  The default definition works only for MonadFail instances that ignore
  --  the String argument; this must be redefined if you are making use of
  --  the String.
  matchFail :: m a
  matchFail = fail "matchFail failed"

  --  | 'guard' takes a concrete predicate and continues if it is true
  --  or fails if it is false. (This is a convenience wrapper around
  --  'matchFail'.)
  --
  --  Use this for parts of the state that are guaranteed to be concrete.
  guard :: Bool -> m ()
  guard True  = pure ()
  guard False = matchFail

  -- | 'sguard' a generalization of 'guard' to arbritrary domain
  -- representations. It maybe used for potentially symplic parts of the state.
  sguard :: dv Bool -> m ()

  {-# MINIMAL get, put, sguard #-}

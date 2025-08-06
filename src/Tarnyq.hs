module Tarnyq (Rewrite, Semantics, eval) where

type Rewrite a = a -> Maybe a
type Semantics a = [Rewrite a]

{-  The functions below are (nearly) forced always to be inlined because
    we use INLINE instead of INLINABLE; GHC is not eager enough to inline
    them otherwise producing a 2-3× slower running result. For situations
    such as `orElse` which is trivially small this isn't a big deal, but
    it might be for larger functions such as `eval`. What we probably
    want to do here is use INLINABLE and supply rewrite rules[1] that
    help GHC do proper inlining; this is to be investigated in the future.
    [1]: https://wiki.haskell.org/Inlining_and_Specialisation#What_is_specialisation
-}

{-# INLINE orElse #-}
orElse :: Rewrite a -> Rewrite a -> Rewrite a
orElse r1 r2 = \state -> case (r1 state) of
                              Nothing  -> r2 state
                              Just st' -> Just st'

{-# INLINE eval #-}
eval :: Semantics a -> a -> a
eval rewrites state = eval' state (next state)  where
    eval' s Nothing   = s
    eval' _ (Just s') = eval' s' (next s')
    next = foldr orElse (\_ -> Nothing) rewrites
    --  Mystery! foldr1 is 1/3 the speed of foldr above.
    --next = foldr1 orElse rewrites

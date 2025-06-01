{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- XXX Add this to package.yaml, when we figure out the syntax there.
{-# LANGUAGE UnicodeSyntax #-}

module BExp () where

{-  Abstract Syntax -}
data B          = T | F                 deriving (Show)
data BExp       = Const B | Not BExp    deriving (Show)
type Program    = BExp

{-  State -}
data State = State { program ∷ Program } deriving (Show)

{-  Semantics as a recursive function

    Since we have only one "cell" in the state, we need simply apply
    the evaulator for that one cell to the contents of it. In a more
    complex case, the evaluator might look at state other than just the
    current program itself.
-}
eval ∷ State → State
eval State { program = prog } = State { program = peval prog }

peval ∷ Program → Program
peval x@(Const _) = x
peval (Not b) = case (peval b) of
                     Const x → Const $ notB x
                     x       → Not x

-- Operations on types in our language.
notB ∷ B → B
notB F = T
notB T = F

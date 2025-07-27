{-  One try (but wrong) at abstract syntax and semantics for a binary
    expression langauge.

    Because we're embedding the build of our AST into Haskell, we don't
    have or need a concrete syntax or associated parser for it; we simply
    build the AST directly.

    We introduce a State, the equivalent of the K "configuration", that is
    a record containing fields for what K would call "cells." This is not
    actually necessary at this point (it's fine to use a State that isn't
    a record), but reminds us that this is the way K works and we want to
    support separate names for sub-states as an option.

    The top-level evaluator must take and produce a State; in our case
    since we have that one field ("cell"), `program`, in it; we simply
    apply the appropriate evaluator for that field, `peval`. A more
    sophisticated evaluator would look at other parts of the state as
    well, to express state external to the program.

    `peval` is a standard recursive evaluator for expressions.

    There are a load of problems with this super-simple approach ,
    including a lack of modularity, extensibility, inability to handle
    non-determinism and interleavings, and so on, all of which generalise
    to a lack of abstraction. We will later be introducing techniques to
    handle all this, that probably eventually lead to giving a list of
    rewrite rules to a function that produces an evaluator, or something
    like that.
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module BExp () where

{-  Abstract Syntax -}
data B          = T | F                 deriving (Show)
data BExp       = Const B | Not BExp    deriving (Show)
type Program    = BExp

{-  State -}
data State = State { program :: Program } deriving (Show)

{-  Semantics as a recursive function

    Since we have only one "cell" in the state, we need simply apply
    the evaulator for that one cell to the contents of it. In a more
    complex case, the evaluator might look at state other than just the
    current program itself.
-}
eval :: State -> State
eval State { program = prog } = State { program = peval prog }

peval :: Program -> Program
peval x@(Const _) = x
peval (Not b) = case (peval b) of
                     Const x -> Const $ notB x
                     x       -> Not x

-- Operations on types in our language.
notB :: B -> B
notB F = T
notB T = F

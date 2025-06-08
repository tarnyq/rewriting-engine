{-  Abstract syntax and semantics for the K tutorial 'Imp' language.

    This would normally be in a separate repo that uses this library,
    but it's here while we do development on the framework.
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Imp () where

{----------------------------------------------------------------------
    Abstract Syntax

    This is almost exactly parallel (including the textual ordering and
    format) to 'module IMP-SYNTAX' in K except that, since it directly
    builds an AST, we leave out things such as parens that are relevant
    only to parsing text to an AST and strictness annotations that are
    actually part of the semantics. (We do however, for convenience,
    keep the precedence settings, both to document them and because it's
    handy when building an AST in Haskell code.)
-}

type Id     = String
data AExp   = Int Integer
            | Var Id
            | Negate Integer    -- Cannot negate AExps for some reason.
            | AExp :/ AExp
         -- | Parens AExp       -- Needed for concrete syntax only.
            | AExp :+ AExp

infixl 7 :/                     -- Same precedences as Prelude.Num
infix  6 :+                     -- Left-assoc in K for more efficient parsing.

-- Sample program 'sum.int': this essentially serves as our test.
-- (XXX we don't have enough "syntax" for this yet.)

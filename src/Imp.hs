{-  Abstract syntax and semantics for the K tutorial 'Imp' language.

    (This would normally be in a separate repo that uses this library,
    but it's here while we do development on the framework.)

    This is a translation to KMonad of the K syntax and semantics for the
    Imp programming language and associated examples from the K
    Tutorial[1]. There is a separate implementation of Imp in the
    imp-semantics repo[2] which is not the same.

    [1]: https://github.com/runtimeverification/pl-tutorial/blob/master/1_k/2_imp/lesson_5/imp.md
    [2]: https://github.com/runtimeverification/imp-semantics
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

    Below we refer to the Imp module as written in K as 'KImp'.
-}

type Id     = String
data AExp   = Int Integer
            | Var Id
            | Negate Integer    -- Cannot negate AExps for some reason.
            | AExp :/ AExp
         -- | Parens AExp       -- Needed for concrete syntax only.
            | AExp :+ AExp
data BExp   = Bool Bool
            | AExp :<= AExp
            | Not BExp
         -- | Parens BExp       -- Needed for concrete syntax only.
            | BExp :&& BExp
data Block  = StmtBlock Stmt    -- KImp 'Stmt' really should be 'Stmts'. :-/
            | EmptyBlock
data Stmt   = Block Block
            | Id := AExp
            | If BExp Block Block
            | While BExp Block
            | Sequence Stmt Stmt
data Pgm    = Pgm Ids Stmt
type Ids    = [Id]

                                -- Same precedence values as the prelude.
infixl 7  :/
infix  6  :+                    -- KImp: Left-assoc for more efficient parsing.
infix  4  :<=
infixl 3  :&&                   -- Should be RA! But broken this way in KImp.

--  The KImp parser doesn't produce a list of statements, but instead, via
--  right-associative parsing, produces a left-skewing nearly degenerate
--  binary tree. (Yes, this is weird, but a consequence of 'Stmt' rather
--  than 'Block' being the root of the AST; a 'Stmt' must be able to act
--  like 'Block'.)
--  We provide this helper function to do the same thing as the KImp parser.
mkStmt :: [Stmt] -> Stmt
mkStmt stmtList = program (reverse stmtList) where
    program []     = error "programs must have at least one statement"
    program [s]    = s
    program (s:ss) = Sequence (program ss) s

--  Make all this showable just for convenience and debugging.
deriving instance Show AExp
deriving instance Show BExp
deriving instance Show Block
deriving instance Show Stmt     -- Not a list, so 'showList' override pointless.
deriving instance Show Pgm

{----------------------------------------------------------------------
    Sample program 'sum.imp': this essentially serves as our test.

        int n, sum;
        n = 100;
        sum = 0;
        while (!(n <= 0)) {
          sum = sum + n;
          n = n + -1;
        }
-}
sum_imp :: Pgm
sum_imp = Pgm ids stmt where
    ids  = ["n", "sum"]
    stmt = mkStmt
         [ "n" := Int 100
         , "sum" := Int 0
         , While (Not (Var "n" :<= (Int 0)))
              (StmtBlock (mkStmt
                [ "sum" := (Var "sum" :+ Var "n")
                , "n" := (Var "n" :+ Negate 1)
                ]))
         ]

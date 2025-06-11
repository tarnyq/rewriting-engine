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

import Data.Map (Map, insert, fromList)

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
data Block  = StmtsBlock Stmts  -- Renamed from 'Stmt' in KImp
            | EmptyBlock
data Stmts  = Block Block
            | Id := AExp
            | If BExp Block Block
            | While BExp Block
            | StPair Stmts Stmts
data Pgm    = Pgm Ids Stmts
type Ids    = [Id]

                                -- Same precedence values as the prelude.
infixl 7  :/
infix  6  :+                    -- KImp: Left-assoc for more efficient parsing.
infix  4  :<=
infixl 3  :&&                   -- Should be RA! But broken this way in KImp.

--  The KImp parser doesn't produce a list of statements, but instead, via
--  right-associative parsing, produces a left-skewing nearly degenerate
--  binary tree. (Yes, this is weird, but a consequence of 'Stmts' rather
--  than 'Block' being the root of the AST; a 'Stmts' must be able to act
--  like 'Block'.)
--  We provide this helper function to do the same thing as the KImp parser.
mkStmts :: [Stmts] -> Stmts
mkStmts stmtList = statements (reverse stmtList)  where
    statements []     = error "programs must have at least one statement"
    statements [s]    = s
    statements (s:ss) = StPair (statements ss) s

--  Make all this showable just for convenience and debugging.
deriving instance Show AExp
deriving instance Show BExp
deriving instance Show Block
deriving instance Show Stmts    -- Not a list, so 'showList' override pointless.
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
sum_imp = Pgm ids stmts  where
    ids   = ["n", "sum"]
    stmts = mkStmts
          [ "n" := Int 100
          , "sum" := Int 0
          , While (Not (Var "n" :<= (Int 0)))
               (StmtsBlock (mkStmts
                 [ "sum" := (Var "sum" :+ Var "n")
                 , "n" := (Var "n" :+ Negate 1)
                 ]))
          ]

----------------------------------------------------------------------
-- Semantics

-- Generic parts
type Rewrite a = a -> Maybe a
type Semantics a = [Rewrite a]

-- Evaluate a program using Imp semantics.
eval_imp :: Pgm -> State
eval_imp state = eval imp $ impInitState state

----------------------------------------
-- User provided Language definition

data State = State { k :: [Stmts], store :: Map Id Integer }  deriving Show

impInitState :: Pgm -> State
impInitState (Pgm ids pgm) = State [pgm] (initStore ids)  where
    initStore ids = fromList $ zip ids (repeat 0)

imp :: Semantics State
imp =   [ assign,
          liftK seqStmt
        ]
    where
        seqStmt :: Rewrite [Stmts]
        seqStmt ((StPair s1 s2):rest)
              = Just $ s1:s2:rest
        seqStmt _ = Nothing

        assign :: Rewrite State
        assign (State ((id := Int i):rest) store)
             = Just $ State rest (insert id i store)
        assign _ = Nothing


--  XXX KMonad should generate this.
liftK :: (Rewrite [Stmts]) -> Rewrite State
liftK f state =
    case f (k state) of
           Just k' -> Just $ state { k = k' }
           Nothing    -> Nothing

--  Sample: evaluate sample program.
eval_sum_imp :: State
eval_sum_imp = eval_imp sum_imp

----------------------------------------------------------------------
-- Library Function (Not part of Imp)

eval :: Semantics a -> a -> a
eval rewrites state = eval' rewrites where
    eval' []     = state
    eval' (r:rs) = case (r state) of
                        Nothing     -> eval' rs
                        Just state' -> eval rewrites state'

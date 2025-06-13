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

import Data.Map (Map, findWithDefault, fromList, insert, member)

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

            | AHole             -- XXX Needed to define strictness.
                                -- In K, these are automatically generated
                                -- by the strict/seqstrict attributes
data BExp   = Bool Bool
            | AExp :<= AExp
            | Not BExp
         -- | Parens BExp       -- Needed for concrete syntax only.
            | BExp :&& BExp

            | BHole             -- XXX Needed to define strictness
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

data State = State { k :: K, store :: Store }  deriving Show
impInitState :: Pgm -> State
impInitState (Pgm ids pgm) = State [KI_Stmts pgm] (impInitStore ids)

type Store = Map Id Integer
impInitStore :: [Id] -> Store
impInitStore ids = fromList $ zip ids (repeat 0)

type K = [KItem]
data KItem = KI_Stmts Stmts
           | KI_AExp AExp
           | KI_BExp BExp
           deriving Show

imp :: Semantics State
imp =   [ liftK     assignHeat
        , liftK     assignCool
        ,           assign
        ,           lookupVar
        , liftK     seqStmt
        , liftStmts while
        , liftK     ifHeat
        , liftK     ifCool
        , liftStmts ifT
        , liftStmts ifF
        , liftK     notHeat
        , liftK     notCool
        , liftBExp  notBExp
        , liftK     leHeat
        , liftK     leCool
        , liftBExp  le
        , liftK     addHeatL
        , liftK     addCoolL
        , liftK     addHeatR
        , liftK     addCoolR
        , liftAExp  add
        , liftAExp  negate
        , liftStmts block
        ]
    where
        seqStmt :: Rewrite K
        seqStmt ((KI_Stmts (StPair s1 s2)):rest)
              = Just $ (KI_Stmts s1):(KI_Stmts s2):rest
        seqStmt _ = Nothing

        assign :: Rewrite State
        assign (State ((KI_Stmts (id := Int i)):rest) store)
             = Just $ State rest (insert id i store)
        assign _ = Nothing

        assignHeat :: Rewrite K
        assignHeat ((KI_Stmts (_ := Int _)):_) = Nothing
        assignHeat ((KI_Stmts (id := rhs)):rest)
               = Just $ (KI_AExp rhs):(KI_Stmts (id := AHole)):rest
        assignHeat _ = Nothing

        assignCool :: Rewrite K
        assignCool ((KI_AExp (Int i)):(KI_Stmts (id := AHole)):rest)
               = Just $ (KI_Stmts (id := (Int i))):rest
        assignCool _ = Nothing


        while :: Rewrite Stmts
        while (While cond body)
            = Just $ (If cond
                         (StmtsBlock $ StPair (Block body)
                                              (While cond body))
                         EmptyBlock)
        while _ = Nothing

        ifT :: Rewrite Stmts
        ifT (If (Bool True) stmtsTrue _) = Just $ (Block stmtsTrue)
        ifT _ = Nothing

        ifF :: Rewrite Stmts
        ifF (If (Bool False) _ stmtsFalse) = Just $ (Block stmtsFalse)
        ifF _ = Nothing

        ifHeat :: Rewrite K
        ifHeat ((KI_Stmts (If (Bool _) _ _)):_) = Nothing
        ifHeat ((KI_Stmts (If cond stmtsTrue stmtsFalse)):rest)
             = Just $ (KI_BExp cond):stmts:rest
             where stmts = (KI_Stmts (If BHole stmtsTrue stmtsFalse))
        ifHeat _ = Nothing

        ifCool :: Rewrite K
        ifCool ((KI_BExp (Bool b)):(KI_Stmts (If BHole stmtsTrue stmtsFalse)):rest)
               = Just $ ((KI_Stmts (If (Bool b) stmtsTrue stmtsFalse)):rest)
        ifCool _ = Nothing

        notHeat :: Rewrite K
        notHeat ((KI_BExp (Not (Bool _))):_) = Nothing
        notHeat ((KI_BExp (Not bexp)):rest)
               = Just $ (KI_BExp bexp):(KI_BExp (Not BHole)):rest
        notHeat _ = Nothing

        notCool :: Rewrite K
        notCool ((KI_BExp (Bool b)):(KI_BExp (Not BHole)):rest)
               = Just $ ((KI_BExp (Not (Bool b))):rest)
        notCool _ = Nothing

        notBExp :: Rewrite BExp
        notBExp (Not (Bool b)) = Just $ (Bool (not b))
        notBExp _ = Nothing

        leHeat :: Rewrite K
        leHeat ((KI_BExp (Int _ :<= _)):_) = Nothing
        leHeat ((KI_BExp (aexp :<= rhs)):rest)
               = Just $ (KI_AExp aexp):(KI_BExp (AHole :<= rhs)):rest
        leHeat _ = Nothing

        leCool :: Rewrite K
        leCool ((KI_AExp (Int i)):(KI_BExp (AHole :<= rhs)):rest)
               = Just $ (KI_BExp (Int i :<= rhs)):rest
        leCool _ = Nothing

        le :: Rewrite BExp
        le (Int i :<= Int j) = Just $ (Bool (i <= j))
        le _ = Nothing

        addHeatL :: Rewrite K
        addHeatL ((KI_AExp (Int _ :+ _)):_) = Nothing
        addHeatL ((KI_AExp (lhs :+ rhs)):rest)
               = Just $ (KI_AExp lhs):(KI_AExp (AHole :+ rhs)):rest
        addHeatL _ = Nothing

        addCoolL :: Rewrite K
        addCoolL ((KI_AExp (Int i)):(KI_AExp (AHole :+ rhs)):rest)
               = Just $ (KI_AExp (Int i :+ rhs)):rest
        addCoolL _ = Nothing

        addHeatR :: Rewrite K
        addHeatR ((KI_AExp (Int _ :+ Int _)):_) = Nothing
        addHeatR ((KI_AExp (Int lhs :+ rhs)):rest)
               = Just $ (KI_AExp rhs):(KI_AExp (Int lhs :+ AHole)):rest
        addHeatR _ = Nothing

        addCoolR :: Rewrite K
        addCoolR ((KI_AExp (Int i)):(KI_AExp (Int lhs :+ AHole)):rest)
               = Just $ (KI_AExp (Int lhs :+ Int i)):rest
        addCoolR _ = Nothing

        add :: Rewrite AExp
        add (Int i :+ Int j) = Just $ (Int (i + j))
        add _ = Nothing

        negate :: Rewrite AExp
        negate (Negate i) = Just $ (Int (-1 * i))
        negate _ = Nothing


        lookupVar :: Rewrite State
        lookupVar (State ((KI_AExp (Var x)):rest) store)
                  | member x store
                = Just $ State (exp:rest) store
                where exp = KI_AExp $ Int $ findWithDefault undefined x store
        lookupVar _ = Nothing

        block :: Rewrite Stmts
        block (Block (StmtsBlock s)) = Just s
        block _ = Nothing


--  XXX KMonad should generate this.
liftK :: Rewrite K -> Rewrite State
liftK f state =
    case f (k state) of
           Just k' -> Just $ state { k = k' }
           Nothing -> Nothing

liftStmts :: Rewrite Stmts -> Rewrite State
liftStmts f state =
    case state of (State ((KI_Stmts s):rest) _) ->
                    case f s of
                        Just s' -> Just $ state { k = ((KI_Stmts s'):rest) }
                        Nothing -> Nothing
                  _ -> Nothing

liftBExp :: Rewrite BExp -> Rewrite State
liftBExp f state =
    case state of (State ((KI_BExp s):rest) _) ->
                    case f s of
                        Just s' -> Just $ state { k = ((KI_BExp s'):rest) }
                        Nothing -> Nothing
                  _ -> Nothing

liftAExp :: Rewrite AExp -> Rewrite State
liftAExp f state =
    case state of (State ((KI_AExp s):rest) _) ->
                    case f s of
                        Just s' -> Just $ state { k = ((KI_AExp s'):rest) }
                        Nothing -> Nothing
                  _ -> Nothing

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

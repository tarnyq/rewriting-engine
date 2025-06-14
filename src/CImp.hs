{-  Abstract syntax and semantics for 'CImp', an imperative language.

    (This would normally be in a separate repo that uses this library,
    but it's here while we do development on the framework.)
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module CImp () where

import qualified Data.Map.Lazy as Map

----------------------------------------------------------------------
-- Abstract Syntax
--

type Id         = Char

type Prog       = Block
newtype Block   = Seq [Cmd]
data Cmd        = Id := IExp
data IExp       = Const Integer
                | Var   Id

emptyBlock :: Block
emptyBlock = Seq []

deriving instance Show Block
deriving instance Show Cmd
deriving instance Show IExp

----------------------------------------------------------------------
-- Semantics

data State      = State { store :: Map.Map Id Integer }  deriving Show

emptyState     :: State
emptyState      = State Map.empty

----------------------------------------------------------------------
-- Examples and Tests

one_imp :: Prog
one_imp = Seq [ 'i' := Const 42, 'j' := Var 'i' ]

run :: Prog -> State
run prog = evalProg prog emptyState  where
    evalProg :: Prog -> State -> State
    evalProg (Seq [])         state = state
    evalProg (Seq (cmd:cmds)) state = evalProg (Seq cmds) (evalCmd cmd state)

evalCmd :: Cmd -> State -> State
evalCmd (v := Const c) (State store) = State $ Map.insert v c store
evalCmd (v₁ := Var v₂) (State store) = State $ Map.insert v₁ (lookup v₂) store
    where lookup v = Map.findWithDefault 0 v store

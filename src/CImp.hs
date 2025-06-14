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
--  Semantics

--  A 'CImpStore' contains *all* possible variables each initialised to the
--  default value '0'. (For efficiency, we do not actually allocate storage
--  for these until they are assigned.) This is only one way of doing it
--  and was chosen because it's the simplest to implement. It also happens
--  to easily give us, at the end of execution, the set of all variables
--  that were assigned. Different implementations might make it e.g. more
--  difficult to get that but easier to get the set of all variables that
--  were referenced.
type CImpStore   = Map.Map Id Integer
iniCImpStore     :: CImpStore
iniCImpStore     = Map.empty
lookupStore     :: Id -> CImpStore -> Integer
lookupStore     = Map.findWithDefault 0

--  The CImpState includes the program; our "program counter" is just removing
--  each line as it's executed (which is fine when we have no loops).
--  Whether we use a mutible program here that we rewrite is really just a
--  decision on the part of the implementer (though this may become less
--  true as we better define the framework).
data CImpState  = CImpState
            {  prog :: Prog
            , store :: CImpStore
            }  deriving Show

initCImpState :: Prog -> CImpState
initCImpState prog  = CImpState prog iniCImpStore

----------------------------------------------------------------------
-- Examples and Tests

one_imp :: Prog
one_imp = Seq [ 'i' := Const 42, 'j' := Var 'i' ]

run :: Prog -> CImpState
run prog = evalProg (initCImpState prog)  where
    evalProg :: CImpState -> CImpState
    evalProg state@(CImpState (Seq []) _) = state
    evalProg state@(CImpState (Seq (cmd:cmds)) σ)
        = evalProg state { prog = Seq cmds, store = evalCmd cmd σ }

-- This cannot rewrite the program in the state, so is used only for
-- simple Cmds where the rewriting of the program is just the caller
-- removing the executed command.
evalCmd :: Cmd -> CImpStore -> CImpStore
evalCmd (v₁ := Const c₁) store = Map.insert v₁ c₁ store
evalCmd (v₁ := Var v₂)   store = Map.insert v₁ (lookupStore v₂ store) store

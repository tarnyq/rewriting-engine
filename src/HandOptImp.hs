{-  This is a hand-optimized version of Imp. It sheds all Rewriting Logic
    nicities, and smooshes eveything into one function.

    This allows GHC's optimizer to go brrrr.

    In particular, since it knows exactly what all the patterns we match on
    are all at once, it can build a smart decision tree. Even though GHC
    cannot assume that it is OK to commute rewrites, it is still able to
    beat K's concrete backend.

    My belief is that we can reach close to this speed even with
    abstractions at a similar level to those in MonadRewrite.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module HandOptImp
    ( eval_hand_opt_imp
    ) where

import Prelude hiding (negate, div)
import qualified Prelude (div)
import Data.Map (Map, findWithDefault, fromList, insert, member)
import Text.Read (readMaybe)

import Rewrite.Class
import Rewrite.Basic
import KTutImp hiding (imp)

----------------------------------------------------------------------
-- Semantics

imp :: State -> Maybe State
imp (State ((KI_Stmts (StPair s1 s2)):rest) store)
      = Just $ State ((KI_Stmts s1):(KI_Stmts s2):rest) store

imp  (State ((KI_Stmts (id := Int i)):rest) store)
     = Just $ State rest (insert id i store)

imp (State ((KI_Stmts (id := aexp)):rest) store) | not (isInt aexp)
       = Just $ State ((KI_AExp aexp):(KI_Stmts (id := AHole)):rest) store

imp (State ((KI_AExp (Int i)):(KI_Stmts (id := AHole)):rest) store)
       = Just $ State ((KI_Stmts (id := (Int i))):rest) store

imp (State ((KI_Stmts (While cond body)):rest) store)
    = Just $ (State ((KI_Stmts (If cond
                                  (StmtsBlock $ StPair (Block body)
                                                       (While cond body))
                                  EmptyBlock)
                     ):rest)
                    store)

imp (State (KI_Stmts (If (Bool True) stmtsTrue _):rest) store)
      = Just $ State (KI_Stmts (Block stmtsTrue):rest) store

imp (State (KI_Stmts (If (Bool False) _ stmtsFalse):rest) store)
      = Just $ State ((KI_Stmts (Block stmtsFalse)):rest) store

imp (State ((KI_Stmts (If cond stmtsTrue stmtsFalse)):rest) store) | not (isBool cond)
     = Just $ State ((KI_BExp cond):stmts:rest) store
     where stmts = (KI_Stmts (If BHole stmtsTrue stmtsFalse))

imp (State ((KI_BExp (Bool b)):(KI_Stmts (If BHole stmtsTrue stmtsFalse)):rest) store)
       = Just $ State ((KI_Stmts (If (Bool b) stmtsTrue stmtsFalse)):rest) store

imp (State ((KI_BExp (Not bexp)):rest) store) | not (isBool bexp)
       = Just $ State ((KI_BExp bexp):(KI_BExp (Not BHole)):rest) store

imp (State ((KI_BExp (Bool b)):(KI_BExp (Not BHole)):rest) store)
       = Just $ State ((KI_BExp (Not (Bool b))):rest) store


imp (State ((KI_BExp (Not (Bool b))):rest) store) = Just $ (State ((KI_BExp (Bool (not b))):rest) store)

imp (State ((KI_BExp (lhs :<= rhs)):rest) store) | not (isInt lhs)
      = Just $ State ((KI_AExp lhs):(KI_BExp (AHole :<= rhs)):rest) store

imp (State ((KI_AExp (Int i)):(KI_BExp (AHole :<= rhs)):rest) store)
      = Just $ State ((KI_BExp (Int i :<= rhs)):rest) store

imp (State ((KI_BExp (lhs :<= rhs)):rest) store) | not $ (isInt lhs) && (isInt rhs)
      = Just $ State ((KI_AExp rhs):(KI_BExp (lhs :<= AHole)):rest) store

imp (State ((KI_AExp (Int i)):(KI_BExp (lhs :<= AHole)):rest) store)
      = Just $ State ((KI_BExp (lhs :<= Int i)):rest) store

imp (State ((KI_BExp (Int i :<= Int j)):rest) store)
     = Just $ State ((KI_BExp (Bool (i <= j))):rest) store

imp (State ((KI_AExp (lhs :+ rhs)):rest) store) | not (isInt lhs)
       = Just $ State ((KI_AExp lhs):(KI_AExp (AHole :+ rhs)):rest) store

imp (State ((KI_AExp (Int i)):(KI_AExp (AHole :+ rhs)):rest) store)
       = Just $ State ((KI_AExp (Int i :+ rhs)):rest) store

imp (State ((KI_AExp (Int lhs :+ rhs)):rest) store) | not (isInt rhs)
       = Just $ State ((KI_AExp rhs):(KI_AExp (Int lhs :+ AHole)):rest) store

imp (State ((KI_AExp (Int i)):(KI_AExp (Int lhs :+ AHole)):rest) store)
       = Just $ State ((KI_AExp (Int lhs :+ Int i)):rest) store

imp (State ((KI_AExp (Int i :+ Int j)):rest) store)
      = Just $ State ((KI_AExp (Int (i + j))):rest) store

imp (State ((KI_AExp (Negate i)):rest) store)
         = Just $ State ((KI_AExp (Int (-1 * i))):rest) store
imp (State ((KI_AExp (lhs :/ rhs)):rest) store) | not (isInt lhs)
       = Just $ State ((KI_AExp lhs):(KI_AExp (AHole :/ rhs)):rest) store

imp (State ((KI_AExp (Int i)):(KI_AExp (AHole :/ rhs)):rest) store)
           = Just $ State ((KI_AExp (Int i :/ rhs)):rest) store

imp (State ((KI_AExp (Int lhs :/ rhs)):rest) store) | not (isInt rhs)
       = Just $ State ((KI_AExp rhs):(KI_AExp (Int lhs :/ AHole)):rest) store

imp (State ((KI_AExp (Int i)):(KI_AExp (Int lhs :/ AHole)):rest) store)
       = Just $ State ((KI_AExp (Int lhs :/ Int i)):rest) store

imp (State ((KI_AExp (Int i :/ Int j)):rest) store) | j /= 0
      = Just $ State ((KI_AExp (Int (i `Prelude.div` j))):rest) store

imp (State ((KI_AExp (Var x)):rest) store)
          | member x store
        = Just $ State (exp:rest) store
        where exp = KI_AExp $ Int $ findWithDefault undefined x store

imp (State ((KI_Stmts (Block (StmtsBlock s))):rest) store)
        = Just (State ((KI_Stmts s):rest) store)
imp _ = Nothing

isInt :: AExp -> Bool
isInt (Int _) = True
isInt _       = False

isBool :: BExp -> Bool
isBool (Bool _) = True
isBool _       = False

----------------------------------------------------------------------
-- Lift the function to a rewrite

hand_opt_imp :: MonadRewrite m State => [m ()]
hand_opt_imp = [  do s <- get
                     case (imp s) of
                        Nothing -> matchFail
                        Just s' -> put s'
               ]

eval_hand_opt_imp :: Pgm -> State
eval_hand_opt_imp pgm = evalOnePath hand_opt_imp $ impInitState pgm


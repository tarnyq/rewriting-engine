module Rewrite.Domain
    ( ConcreteValue(..)
    , DomainValue(..)
    , DomainTerm(IntVar, IntLit)
    , SymbolicExpr(..)
    )
  where

import Data.SBV

-- | We need a concrete and symbolic representations for domain values.
--  If a semantics expects symbolic exection over a particular values in the
--  state it may use the DomainValue class to represent those values. This
--  allows us to chose between concrete implementations for fast excution, and
--  representations that work with SMT solvers for symbolic execution.
class DomainValue repr where
    dInteger  :: Integer -> repr Integer
    dAdd      :: repr Integer -> repr Integer -> repr Integer
    dMul      :: repr Integer -> repr Integer -> repr Integer

    -- TODO: Should this be defined  as
    --        :: repr Integer -> repr Integer -> repr Maybe Integer
    -- or
    --        :: repr NonZero -> repr Integer -> repr Maybe Integer
    dDiv      :: repr Integer -> repr Integer -> repr Integer

    dBool     :: Bool -> repr Bool
    dNEq      :: repr Integer -> repr Integer -> repr Bool
    lt        :: repr Integer -> repr Integer -> repr Bool
    dAnd      :: repr Bool -> repr Bool -> repr Bool
    dOr       :: repr Bool -> repr Bool -> repr Bool
    dNot      :: repr Bool -> repr Bool

    {-# MINIMAL dInteger, dAdd, dMul, dDiv, dBool, dNEq, lt, dAnd, dOr, dNot #-}

newtype ConcreteValue a = CV { unwrap :: a }
instance DomainValue ConcreteValue where
    dInteger = CV
    dAdd a b = CV $ (unwrap a) + (unwrap b)
    dMul a b = CV $ (unwrap a) * (unwrap b)
    dDiv a b = CV $ (unwrap a) `div` (unwrap b)

    dBool    = CV
    dNEq a b = CV $ (unwrap a) /=  (unwrap b)
    lt   a b = CV $ (unwrap a) <  (unwrap b)
    dAnd a b = CV $ (unwrap a) && (unwrap b)
    dOr  a b = CV $ (unwrap a) || (unwrap b)
    dNot a   = CV $ not (unwrap a)

deriving instance (Show a) => Show (ConcreteValue a)

-- | We use SBV to represent symbolic values that maybe sent to the SMT solver.
instance DomainValue SBV where
    dInteger = literal
    dAdd     = (+)
    dMul     = (*)

    -- TODO: Figure out how to handle 0 case.
    dDiv     = error "Partial function div not implemented"

    dBool    = literal
    dNEq     = (./=)
    lt       = (.<)
    dAnd     = (.&&)
    dOr      = (.||)
    dNot     = sNot

-- | Since SBV cannot print values, we also need a term representation
--  of the expression to allow us to define a Show instance. Besides allowing
--  us to serialize symbolic states it also allows resumption of symbolic
--  execution at some later point by deserializing the term representation.
--
--  After a small amount of symbolic rewriting these expressions
--  can get quite unwieldy. To counter these, we add some simplifications
--  keep all the constant portions summed.
data DomainTerm a where
    IntLit  :: Integer -> DomainTerm Integer
    IntVar  :: String -> DomainTerm Integer
    Add     :: [DomainTerm Integer] -> DomainTerm Integer
    Mul     :: DomainTerm Integer -> DomainTerm Integer -> DomainTerm Integer
    Div     :: DomainTerm Integer -> DomainTerm Integer -> DomainTerm Integer

    BoolLit :: Bool -> DomainTerm Bool
    BoolVar :: String -> DomainTerm Bool
    NEq     :: DomainTerm Integer -> DomainTerm Integer -> DomainTerm Bool
    LT      :: DomainTerm Integer -> DomainTerm Integer -> DomainTerm Bool
    And     :: DomainTerm Bool -> DomainTerm Bool -> DomainTerm Bool
    Or      :: DomainTerm Bool -> DomainTerm Bool -> DomainTerm Bool
    Not     :: DomainTerm Bool -> DomainTerm Bool

deriving instance Show (DomainTerm a)
deriving instance Eq (DomainTerm a)

instance DomainValue DomainTerm where
    -- TODO: For now, we hand-write some simplifications.
    -- Note that these simplifications are not passed on to the SMT
    -- solver.
    dInteger  = IntLit
    dAdd (IntLit 0) n = n
    dAdd (IntLit n) (IntLit m) = (IntLit $ m+n)
    dAdd (Add ((IntLit m):rest)) (IntLit n) = Add ((IntLit $ m+n):rest)
    dAdd (Add ((IntLit m):restm)) (Add ((IntLit n):restn)) = Add ((IntLit $ m+n):restm ++ restn)
    dAdd a          (IntLit n) = Add [(IntLit n), a]
    dAdd a b    = Add [a, b]

    dMul (IntLit n) (IntLit m) = (IntLit $ m * n)
    dMul a b    = Mul a b
    dDiv a b    = Div a b

    dBool       = BoolLit
    dNEq        = NEq
    lt          = Rewrite.Domain.LT

    dAnd (BoolLit True) b = b
    dAnd a (BoolLit True) = a
    dAnd a b    = Rewrite.Domain.And a b

    dOr         = Rewrite.Domain.Or

    dNot (BoolLit False) = (BoolLit True)
    dNot (Not a) = a
    dNot a      = Rewrite.Domain.Not a

-- | We use couple the SBV represntation with the term represntation for
--  symolic execution. In theory, we could get away without using the term
--  expression if we do not need a record of the path condition, for an
--  improved performance? This is the likely situation when debugging
--  is unnessesary.
data SymbolicExpr a = SymbolicExpr { sbv :: SBV a, term :: DomainTerm a }

instance Show (SymbolicExpr a) where
  show exp = show $ term exp
  -- show exp = show $ sbv exp  -- use to elide large symbolic expressions

-- We need SBV based symbolic values in conjunction with the printable Exprs
instance DomainValue SymbolicExpr where
    dInteger l = SymbolicExpr (dInteger l) (dInteger l)
    dAdd (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dAdd a1 a2) (dAdd b1 b2)
    dMul (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dMul a1 a2) (dMul b1 b2)
    dDiv (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dDiv a1 a2) (dDiv b1 b2)

    dBool l = SymbolicExpr (dBool l) (dBool l)
    dNEq  (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dNEq a1 a2) (dNEq b1 b2)
    dNot  (SymbolicExpr a b) = SymbolicExpr (dNot a) (dNot b)
    lt    (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (lt a1 a2) (lt b1 b2)
    dOr   (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dOr a1 a2) (dOr b1 b2)
    dAnd  (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dAnd a1 a2) (dAnd b1 b2)


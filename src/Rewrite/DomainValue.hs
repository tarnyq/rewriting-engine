module Rewrite.DomainValue
    ( DomainValue(..)
    , ConcreteValue(..)
    )
  where

import Data.Tuple.Extra (first)

-- | We need a concrete and symbolic representations for domain values.
--  If a semantics expects symbolic exection over a particular values in the
--  state it may use the DomainValue class to represent those values. This
--  allows us to chose between concrete implementations for fast excution, and
--  representations that work with SMT solvers for symbolic execution.

-- TODO: Look into implementing the Num class.
-- This is not trivial as we need to work around issues with
-- conflicting definition for SBV.
class DomainValue repr where
    dInteger  :: Integer -> repr Integer
    dNEq      :: repr Integer -> repr Integer -> repr Bool
    dAdd      :: repr Integer -> repr Integer -> repr Integer
    dMul      :: repr Integer -> repr Integer -> repr Integer
    dDiv      :: repr Integer -> repr Integer -> repr Integer

    dBool     :: Bool -> repr Bool
    dLte      :: repr Integer -> repr Integer -> repr Bool
    dAnd      :: repr Bool -> repr Bool -> repr Bool
    dOr       :: repr Bool -> repr Bool -> repr Bool
    dNot      :: repr Bool -> repr Bool

    {-# MINIMAL dInteger, dNEq, dAdd, dMul, dDiv, dBool, dLte, dAnd, dOr, dNot #-}

newtype ConcreteValue a = CV { unwrap :: a }
instance DomainValue ConcreteValue where
    dInteger = CV
    dNEq a b = CV $ (unwrap a) /=    (unwrap b)
    dAdd a b = CV $ (unwrap a) +     (unwrap b)
    dMul a b = CV $ (unwrap a) *     (unwrap b)
    dDiv a b = CV $ (unwrap a) `div` (unwrap b)

    dBool    = CV
    dLte a b = CV $ (unwrap a) <= (unwrap b)
    dAnd a b = CV $ (unwrap a) && (unwrap b)
    dOr  a b = CV $ (unwrap a) || (unwrap b)
    dNot a   = CV $ not (unwrap a)

instance Show a => Show (ConcreteValue a) where
    show a = show (unwrap a)
instance Read (ConcreteValue Integer) where
    readsPrec i s = fmap (first CV) (readsPrec i s)

deriving instance Eq a => Eq (ConcreteValue a)


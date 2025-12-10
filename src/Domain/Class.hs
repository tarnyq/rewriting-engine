module Domain.Class (DomainValue(..)) where

import Data.SBV

-- | We need a concrete, abstract and symbolic representations for domain values.
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


-- | We use SBV to represent symbolic values that maybe sent to the SMT solver.

-- This needs to be here, to prevent orphan instance warning.
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



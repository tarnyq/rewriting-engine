module Domain.SymbolicExpr (SymbolicExpr(..)) where

import Data.SBV

import Domain.Class
import Domain.Term

-- | We couple the SBV representation with the term representation for
-- symolic execution. In theory, we could get away without using the term
-- expression if we do not need a record of the path condition, for an
-- improved performance.
--
-- Without this, however, very simple expectations become impossible, such
-- as printing the result after symbolic execution.

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


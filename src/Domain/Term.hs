module Domain.Term (DomainTerm(..)) where

import Domain.Class

-- | It is useful to have a term representation of domain values, so
-- that we may serialize and deserialize them, as well as easily manipulate
-- them. For example, we may want to compute an abstraction or simplification.
--
-- Since symbolic SBV values are the internal representation of the solver,
-- they are not helpful in this aspect.
--
-- After a small amount of symbolic rewriting these expressions
-- can get quite unwieldy. To counter this, we add some simplifications
-- to evaluate concrete subexpressions. Eventually, we may want to split this
-- functionality into a separate utility.

data DomainTerm a where
    IntLit  :: Integer -> DomainTerm Integer
    IntVar  :: String -> DomainTerm Integer
    Add     :: [DomainTerm Integer] -> DomainTerm Integer

    -- TODO: All commutative operators should use Lists
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


instance DomainValue DomainTerm where

    dInteger  = IntLit

    -- TODO: For now, we hand-write some simplifications. Note that these
    -- simplifications are not passed on to the SMT solver, and are only
    -- useful for debugging, and when the expression makes a round-trip
    -- via de/serialization.
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
    lt          = Domain.Term.LT

    dAnd (BoolLit True) b = b
    dAnd a (BoolLit True) = a
    dAnd a b    = And a b

    dOr         = Or

    dNot (BoolLit False) = (BoolLit True)
    dNot (Not a) = a
    dNot a      = Not a

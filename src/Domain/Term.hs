module Domain.Term (Term(..)) where

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

data Term a where
    IntLit  :: Integer -> Term Integer
    IntVar  :: String -> Term Integer
    Add     :: [Term Integer] -> Term Integer

    -- TODO: All commutative operators should use Lists
    Mul     :: Term Integer -> Term Integer -> Term Integer
    Div     :: Term Integer -> Term Integer -> Term Integer

    BoolLit :: Bool -> Term Bool
    BoolVar :: String -> Term Bool
    NEq     :: Term Integer -> Term Integer -> Term Bool
    LT      :: Term Integer -> Term Integer -> Term Bool
    And     :: Term Bool -> Term Bool -> Term Bool
    Or      :: Term Bool -> Term Bool -> Term Bool
    Not     :: Term Bool -> Term Bool

deriving instance Show (Term a)
deriving instance Eq (Term a)
deriving instance Ord (Term a)


instance DomainValue Term where

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
    dLt          = Domain.Term.LT

    dAnd (BoolLit True) b = b
    dAnd a (BoolLit True) = a
    dAnd a b    = And a b

    dOr         = Or

    dNot (BoolLit False) = (BoolLit True)
    dNot (Not a) = a
    dNot a      = Not a

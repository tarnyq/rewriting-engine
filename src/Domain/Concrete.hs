module Domain.Concrete
    (ConcreteValue(..))
  where

import Domain.Class

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



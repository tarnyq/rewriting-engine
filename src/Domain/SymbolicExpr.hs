{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Domain.SymbolicExpr (SymbolicExpr(..), fromTerm, DomainFunctor(..)) where


import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import qualified Data.Map as M
import           Data.SBV (SBV, freshVar)
import           Data.SBV.Control (Query)
import           Data.SBV.Trans.Control (QueryT)
import           Domain.Class
import           Domain.Term
import qualified Domain.Term as T

-- | We couple the SBV representation with the term representation for
-- symolic execution. In theory, we could get away without using the term
-- expression if we do not need a record of the path condition, for an
-- improved performance.
--
-- Without this, however, very simple expectations become impossible, such
-- as printing the result after symbolic execution.

data SymbolicExpr a = SymbolicExpr { sbv :: SBV a, term :: Term a }

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
    dLt    (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dLt a1 a2) (dLt b1 b2)
    dOr   (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dOr a1 a2) (dOr b1 b2)
    dAnd  (SymbolicExpr a1 b1) (SymbolicExpr a2 b2) = SymbolicExpr (dAnd a1 a2) (dAnd b1 b2)


type VarCtx = M.Map String (SBV Integer)
type QueryVar = (StateT VarCtx Query)

fromTerm    :: Term a -> QueryVar (SymbolicExpr a)


fromTerm    t@(IntVar n)
    = do ctx <- get
         v <- case (M.lookup n ctx) of
                Nothing -> do v <- freshVar n
                              liftIO $ putStrLn $ "Create: " ++ n
                              put $ M.insert n v ctx
                              pure v
                Just v  -> do liftIO $ putStrLn $ "Use: " ++ n
                              pure v
         st <-  get
         liftIO $ print st
         pure (SymbolicExpr v t)


fromTerm    (BoolVar _)
    = error $ "Fixme: Bool variables not supported. " ++
              "Use existentials in Ctx to support arbitrary types."

fromTerm    (IntLit i) = pure (dInteger i)
fromTerm    (Add a)
    = do args <- mapM fromTerm    a
         pure (foldl' dAdd (dInteger 0) args)
fromTerm    (Mul a b) = fromTermBin dMul a b
fromTerm    (Div a b) = fromTermBin dDiv a b

fromTerm    (BoolLit i) = pure (dBool i)
fromTerm    (And a b) = fromTermBin dAnd a b
fromTerm    (Or a b) = fromTermBin dOr a b
fromTerm    (NEq a b) = fromTermBin dNEq a b
fromTerm    (Not a)
    = do a' <- fromTerm    a
         pure (dNot a')

fromTerm    (T.LT a b) = fromTermBin dLt a b

fromTermBin :: (SymbolicExpr a -> SymbolicExpr b -> SymbolicExpr c)
               -> Term a -> Term b
               -> QueryVar (SymbolicExpr c)
fromTermBin f a b = do a' <- fromTerm    a
                       b' <- fromTerm    b
                       pure (f a' b')

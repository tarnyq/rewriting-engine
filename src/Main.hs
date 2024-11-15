module Main where

import Control.Monad
import Control.Monad.State

-------------------------------------------------------------------------------
-- Configuration

type Env = (Int, Int)
type Imp = State Env


-------------------------------------------------------------------------------
-- Arithmetic Expressions

data AExp = Int Int
          | X | Y
          | Neg AExp
          | Add AExp AExp
    deriving Show

evalA :: AExp -> Imp Int
evalA (Int n) = return n
evalA (Neg e) = do v <- evalA e
                   return $ -1 * v
evalA (Add l r)  = do vl <- (evalA l)
                      vr <- (evalA r)
                      return $ vl + vr
evalA X = do env <- get
             return $ fst env
evalA Y = do env <- get
             return $ snd env


-------------------------------------------------------------------------------
-- Boolean Expressions

data BExp = Bool Bool
          | LessThan AExp AExp

evalB :: BExp -> Imp Bool
evalB (Bool b)       = return b
evalB (LessThan l r) = do vl <- (evalA l)
                          vr <- (evalA r)
                          return $ vl < vr

-------------------------------------------------------------------------------
-- Testing

runA :: AExp -> Env -> Int
runA exp env = fst (runState (evalA exp) env)

runB :: BExp -> Env -> Bool
runB exp env = fst (runState (evalB exp) env)


test_arith =    (runA (Int 42)        (2,  2) == 42)
             && (runA Y               (2, 42) == 42)
             && (runA (Add (Int 2) Y) (2, 40) == 42)
             && (runA (Add X       Y) (2, 40) == 42)
             && (runA (Add (Neg X) Y) (2, 44) == 42)

test_bool =     (runB (Bool True)    (2, 2) == True)
             && (runB (LessThan X Y) (2, 2) == False)
             && (runB (LessThan X Y) (2, 3) == True)
             && (runB (LessThan X Y) (3, 2) == False)

main :: IO ()
main = do putStrLn $ "test_arith: " ++ (show test_arith)
          putStrLn $ "test_bool: " ++ (show test_bool)


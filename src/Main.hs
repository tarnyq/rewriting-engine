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
          | Add AExp AExp
    deriving Show

evalA :: AExp -> Imp Int
evalA (Int n) = return n
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

runB :: BExp -> Env -> Bool
runB exp env = fst (runState (evalB exp) env)

runA :: AExp -> Env -> Int
runA exp env = fst (runState (evalA exp) env)


test_evalA =    (runA (Int 42)        (2,  2) == 42)
             && (runA Y               (2, 42) == 42)
             && (runA (Add (Int 2) Y) (2, 40) == 42)
             && (runA (Add X       Y) (2, 40) == 42)

test_evalB =    (runB (Bool True)    (2, 2) == True)
             && (runB (LessThan X Y) (2, 2) == False)
             && (runB (LessThan X Y) (2, 3) == True)
             && (runB (LessThan X Y) (3, 2) == False)

main :: IO ()
main = do putStrLn $ "test_evalA: " ++ (show test_evalA)
          putStrLn $ "test_evalB: " ++ (show test_evalB)


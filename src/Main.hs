module Main where

import Control.Monad
import Control.Monad.State

-------------------------------------------------------------------------------
-- Configuration

type Env = (Int, Int)
type Imp = StateT Env []


-------------------------------------------------------------------------------
-- Arithmetic Expressions

data AExp = Int Int
          | X | Y
          | Neg AExp
          | Add AExp AExp
    deriving Show

runA :: AExp -> Imp Int
runA (Int n) = return n
runA (Neg e) = do v <- runA e
                  return $ -1 * v
runA (Add l r)  = do vl <- (runA l)
                     vr <- (runA r)
                     return $ vl + vr
runA X = do env <- get
            return $ fst env
runA Y = do env <- get
            return $ snd env


-------------------------------------------------------------------------------
-- Boolean Expressions

data BExp = Bool Bool
          | LessThan AExp AExp

runB :: BExp -> Imp Bool
runB (Bool b)       = return b
runB (LessThan l r) = do vl <- (runA l)
                         vr <- (runA r)
                         return $ vl < vr


-------------------------------------------------------------------------------
-- Statements

data Stmt = While BExp Stmt
          | Block [Stmt]
          | AssignX AExp
          | AssignY AExp

runStmt :: Stmt -> Imp ()
runStmt (Block [])     = return ()
runStmt (Block (s:ss)) = do runStmt s
                            runStmt $ Block ss
                            return ()
runStmt (AssignX e) = do v <- runA e
                         (x, y) <- get
                         put (v, y)
                         return ()
runStmt (AssignY e) = do v <- runA e
                         (x, y) <- get
                         put (x, v)
                         return ()
runStmt (While cond stmts) = do c <- runB cond
                                if c then runStmt $ Block [stmts, (While cond stmts)]
                                     else return ()


-------------------------------------------------------------------------------
-- Testing

evalA :: AExp -> Env -> [Int]
evalA exp env = evalStateT (runA exp) env

evalB :: BExp -> Env -> [Bool]
evalB exp env = evalStateT (runB exp) env

execStmt :: Stmt -> [Env]
execStmt exp = execStateT (runStmt exp) env
    where env = (0, 0)

test_arith =    (evalA (Int 42)        (2,  2) == [42])
             && (evalA Y               (2, 42) == [42])
             && (evalA (Add (Int 2) Y) (2, 40) == [42])
             && (evalA (Add X       Y) (2, 40) == [42])
             && (evalA (Add (Neg X) Y) (2, 44) == [42])

test_bool =     (evalB (Bool True)    (2, 2) == [True])
             && (evalB (LessThan X Y) (2, 2) == [False])
             && (evalB (LessThan X Y) (2, 3) == [True])
             && (evalB (LessThan X Y) (3, 2) == [False])

coundToN n = (While (LessThan X $ Int n) $
                    Block [(AssignX (Add X $ Int 1))])

test_stmt =     (execStmt (Block [])         == [(0, 0)])
             && (execStmt (AssignX (Int 42)) == [(42, 0)])
             && (execStmt (AssignY (Int 42)) == [(0, 42)])
             && (execStmt (Block [
                              (AssignY (Int 43)),
                              (AssignY (Add Y (Neg $ Int 1)))
                          ])                 == [(0, 42)])
             && (execStmt (coundToN 42)      == [(42, 0)])

main :: IO ()
main = do putStrLn $ "test_arith: " ++ (show test_arith)
          putStrLn $ "test_bool: " ++ (show test_bool)
          putStrLn $ "test_stmt: " ++ (show test_stmt)


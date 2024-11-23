module Main where

import Control.Monad
import Control.Monad.State

-------------------------------------------------------------------------------
-- Syntax

data AExp = Int Int
          | X | Y
          | Neg AExp
          | Add AExp AExp
    deriving Show

data BExp = Bool Bool
          | LessThan AExp AExp
          | Flip

data Stmt = While BExp Stmt
          | Block Stmts
          | AssignX AExp
          | AssignY AExp

type Stmts = [Stmt]
type Pgm = Stmts

-------------------------------------------------------------------------------
-- Configuration

data Config = Config { pgm::Pgm, x::Int, y::Int }
type Imp = StateT Config []


-------------------------------------------------------------------------------
-- Arithmetic Expressions

runA :: AExp -> Imp Int
runA (Int n) = return n
runA (Neg e) = do v <- runA e
                  return $ -1 * v
runA (Add l r)  = do vl <- (runA l)
                     vr <- (runA r)
                     return $ vl + vr
runA X = do config <- get
            return $ x config
runA Y = do config <- get
            return $ y config


-------------------------------------------------------------------------------
-- Boolean Expressions

runB :: BExp -> Imp Bool
runB (Bool b)       = return b
runB (LessThan l r) = do vl <- (runA l)
                         vr <- (runA r)
                         return $ vl < vr
runB (Flip)         = lift $ do ret <- [True, False]
                                return ret


-------------------------------------------------------------------------------
-- Statements

runStmt :: Stmt -> Imp ()
runStmt (Block [])     = return ()
runStmt (Block (s:ss)) = do runStmt s
                            runStmt $ Block ss
                            return ()
runStmt (AssignX e) = do v <- runA e
                         config <- get
                         put config{x = v}
                         return ()
runStmt (AssignY e) = do v <- runA e
                         config <- get
                         put config{y = v}
                         return ()
runStmt (While cond stmts) = do c <- runB cond
                                if c then runStmt $ Block [stmts, (While cond stmts)]
                                     else return ()


-------------------------------------------------------------------------------
-- Testing

evalA :: AExp -> (Int, Int) -> [Int]
evalA exp xy = evalStateT (runA exp) Config{pgm = [], x = fst xy, y = snd xy}

evalB :: BExp -> (Int, Int) -> [Bool]
evalB exp xy = evalStateT (runB exp) Config{pgm = [], x = fst xy, y = snd xy}

execStmt :: Stmt -> [(Int, Int)]
execStmt exp = fmap (\r -> (x r, y r)) results
    where config = Config{pgm = [], x = 0, y = 0}
          results = execStateT (runStmt exp) config

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

         ------ The following fails because runStmt is a recursive function
         ------ that must execute to completion.
         --- && ((take 5 (execStmt (While (Flip) $
         ---                        Block [(AssignX (Add X $ Int 1))])))
         ---                                 == [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0),])


main :: IO ()
main = do putStrLn $ "test_arith: " ++ (show test_arith)
          putStrLn $ "test_bool: " ++ (show test_bool)
          putStrLn $ "test_stmt: " ++ (show test_stmt)


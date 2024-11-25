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

data Stmt = While BExp Stmts
          | AssignX AExp
          | AssignY AExp

type Stmts = [Stmt]
type Pgm = Stmts

-------------------------------------------------------------------------------
-- Configuration

data Config = Config { pgm::Pgm, x::Int, y::Int }

-- Rewriting is a transition system over this configuration.
-- Further we use the List monad to represent non-determinism.
-- Each item in the resulting list is semantically thought of as a disjunct
-- of configurations.
type Imp = StateT Config []

-- TODO: We use rewriting to [] as an encoding of "stuck" states.
-- The more semantically correct way of interpreting this is rewriting
-- to "bottom".
stuck :: Imp a
stuck = mempty


-------------------------------------------------------------------------------
-- Statements

next :: Imp ()
next =      nextAssignX
        <|> nextAssignY
        <|> nextWhileTrue
        <|> nextWhileFalse

nextAssignX
        = do Config{pgm=pgm} <- get
             case pgm of
                  (AssignX (Int e)):ss -> do put cfg{pgm=ss, x=e}
                  _                    -> stuck
nextAssignY
        = do Config{pgm=pgm} <- get
             case pgm of
                  (AssignY (Int e)):ss -> do put cfg{pgm=ss, y=e}
                  _                    -> stuck

nextWhileTrue
        = do Config{pgm=pgm} <- get
             case pgm of
                  (While (Bool False) stmts):ss -> do put cfg{pgm=ss}
                  _                             -> stuck
runStmt (While cond stmts) = do c <- runB cond
                                if c then runStmt $ [stmts, (While cond stmts)]
                                     else return []


-------------------------------------------------------------------------------
-- Arithmetic Expressions

runA :: AExp -> Imp AExp
runA (Int n)        = stuck
runA (Neg (Int i))  = return $ Int $ -1 * i
runA (Add (Int l) (Int r))
                    = return $ Int $ l + r
runA X              = do config <- get
                         return $ Int $ x config
runA Y              = do config <- get
                         return $ Int $ y config


-------------------------------------------------------------------------------
-- Boolean Expressions

runB :: BExp -> Imp BExp
runB (Bool b)       = stuck
runB (LessThan (Int l) (Int r))
                    = return $ Bool $ l < r
runB (Flip)         = lift $ do ret <- [(Bool True), (Bool False)]
                                return ret


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


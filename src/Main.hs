module Main where

import Control.Monad
import Control.Monad.State
import Control.Applicative

-------------------------------------------------------------------------------
-- Syntax

data AExp = Int Int
          | X | Y
          | Neg AExp
          | Add AExp AExp
    deriving (Eq, Show)

data BExp = Bool Bool
          | LessThan AExp AExp
          | Flip
    deriving (Eq, Show)

data Stmt = While BExp Stmts
          | If BExp Stmts
          | AssignX AExp
          | AssignY AExp
    deriving (Eq, Show)

type Stmts = [Stmt]
type Pgm = Stmts

-------------------------------------------------------------------------------
-- Configuration

-- The IMP language is defined over a state consisting of two variables, x and
-- y, and a program.

data Config = Config { pgm::Pgm, x::Int, y::Int }
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Rewriting Infrastructure

-- Rewriting is a transition system over this configuration.
-- Further we use the List monad to represent non-determinism.
-- Each item in the resulting list is semantically thought of as a disjunct
-- of configurations.

type Imp = StateT Config []

-- init gives us the initial configuration of the program
initConfig :: Pgm -> Config
initConfig pgm = Config {pgm=pgm, x=0, y=0}


-- TODO: We use rewriting to [] as an encoding of "stuck" states.
-- The more semantically correct way of interpreting this is rewriting
-- to "bottom", as in the evaluation of `assume false`.
stuck :: Imp a
stuck = StateT $ \cfg -> []

execConfig :: Imp ()
execConfig = StateT f
     where f :: Config -> [((), Config)]
           f config = case nexts of
                              []  -> [((), config)]
                              _:_ -> concatMap f nexts
              where nexts = execStateT step config


execPgm :: Pgm -> [Config]
execPgm pgm = execStateT execConfig $ initConfig pgm

-------------------------------------------------------------------------------
-- Top-level rewrites: Rewriting individual statements

-- Each rewrite is represented as a transition over the StateT Monad, that
-- are then composed in parallel.

step :: Imp ()
step = do cfg <- get
          case pgm cfg of
               (AssignX (Int e)):ss      -> do put cfg{pgm=ss, x=e}
               (AssignX v)      :ss      -> do v' <- nextA v
                                               put cfg{pgm=(AssignX v'):ss}
               (AssignY (Int e)):ss      -> do put cfg{pgm=ss, y=e}
               (AssignY v)      :ss      -> do v' <- nextA v
                                               put cfg{pgm=(AssignY v'):ss}
               (While cond body):ss      -> do put cfg{pgm=(If cond $ body ++ [While cond body]):ss}
               (If (Bool True) body):ss  -> do put cfg{pgm=body ++ ss}
               (If (Bool False) body):ss -> do put cfg{pgm=ss}
               (If cond body):ss         -> do c' <- nextB cond
                                               put cfg{pgm=(If c' body):ss}
               _                         -> stuck


-------------------------------------------------------------------------------
-- Arithmetic Expressions

nextA :: AExp -> Imp AExp
nextA (Int n)        = stuck
nextA (Neg (Int i))  = return $ Int $ -1 * i
nextA (Neg e)        = do e' <- nextA e
                          return $ Neg e'
nextA (Add (Int l) (Int r))
                     = return $ Int $ l + r
nextA (Add (Int l) r)
                     = do r' <- nextA r
                          return $ Add (Int l) r'
nextA (Add l r)      = do l' <- nextA l
                          return $ Add l' r
nextA X              = do config <- get
                          return $ Int $ x config
nextA Y              = do config <- get
                          return $ Int $ y config


-------------------------------------------------------------------------------
-- Boolean Expressions

nextB :: BExp -> Imp BExp
nextB (Bool b)       = stuck
nextB (LessThan (Int l) (Int r))
                     = return $ Bool $ l < r
nextB (LessThan (Int l) r)
                     = do r' <- nextA r
                          return $ LessThan (Int l) r'
nextB (LessThan l r) = do l' <- nextA l
                          return $ LessThan l' r
nextB (Flip)         = lift $ do ret <- [(Bool True), (Bool False)]
                                 return ret


-------------------------------------------------------------------------------
-- Testing

--- Execute a program to termination and
execxy p = map (\c -> (x c, y c)) $ execPgm p

execA :: AExp -> (Int, Int) -> [Int]
execA a (xv, yv) = map x $ execPgm [AssignX (Int xv), AssignY (Int yv), AssignX a]

execB :: BExp -> (Int, Int) -> [Int]
execB b (xv, yv) = map x $ execPgm [ AssignX (Int xv)
                                   , AssignY (Int yv)
                                   , If b [AssignX (Int 99)]
                                   ]

test_arith =    (execA (Int 42)        (2,  2) == [42])
             && (execA Y               (2, 42) == [42])
             && (execA (Add (Int 2) Y) (2, 40) == [42])
             && (execA (Add X       Y) (2, 40) == [42])
             && (execA (Add (Neg X) Y) (2, 44) == [42])

test_bool =     (execB (Bool True)    (2, 2) == [99])
             && (execB (LessThan X Y) (2, 2) == [2])
             && (execB (LessThan X Y) (2, 3) == [99])
             && (execB (LessThan X Y) (3, 2) == [3])
             && (execB (Flip)         (0, 66) == [99, 0])

coundToN n = [While (LessThan X $ Int n) $
                    [(AssignX (Add X $ Int 1))]]

test_stmt =    (execxy ([])                 == [(0, 0)])
            && (execxy ([AssignX (Int 42)]) == [(42, 0)])
            && (execxy ([AssignY (Int 42)]) == [(0, 42)])
            && (execxy ([AssignX (Int 42),
                         AssignY (Int 42)]) == [(42, 42)])
            && (execxy ([AssignX (Int 42),
                         AssignY X])        == [(42, 42)] )
            && (execxy ([ (AssignY (Int 43)),
                          (AssignY (Add Y (Neg $ Int 1)))
                        ])                 == [(0, 42)])
            && (execxy (coundToN 42)      == [(42, 0)])
 
          ------ The following fails because runStmt is a recursive function
          ------ that must execute to completion.
          --  && ((take 5 (execxy (While (Neg Flip) $
          --                               [(AssignX (Add X $ Int 1))])))
          --                                  == [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)])
 
 
main :: IO ()
main = do putStrLn $ "test_stmt: " ++ (show $ execPgm (coundToN 42))
          putStrLn $ "test_stmt: " ++ (show $ execPgm $ (pgm . head) $ execPgm [AssignX (Int 1), AssignY X])
          putStrLn $ "test_stmt: " ++ (show test_stmt)
          putStrLn $ "test_arith: " ++ (show test_arith)
          putStrLn $ "test_bool: " ++ (show test_bool)

-- 

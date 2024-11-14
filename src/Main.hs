module Main where

data AExp = Int Int
          | X | Y -- Variables
          | Add AExp AExp
    deriving Show

evalA :: Int -> Int -> AExp -> Int
evalA _ _ (Int n)    = n
evalA x y (Add n m)  = (evalA x y n) + (evalA x y m)
evalA x _ X    = x
evalA _ y Y    = y

test_evalA =    (evalA 2 2  (Int 42)        == 42)
             && (evalA 2 42 Y               == 42)
             && (evalA 2 40 (Add (Int 2) Y) == 42)
             && (evalA 2 40 (Add X       Y) == 42)


data BExp = Bool Bool
          | LessThan AExp AExp

evalB :: Int -> Int -> BExp -> Bool
evalB _ _ (Bool b)       = b
evalB x y (LessThan l r) = (evalA x y l) < (evalA x y r)

test_evalB =    (evalB 2 2  (Bool True)     == True)
             && (evalB 2 2  (LessThan X Y)  == False)
             && (evalB 2 3  (LessThan X Y)  == True)
             && (evalB 3 2  (LessThan X Y)  == False)


main :: IO ()
main = do putStrLn $ "test_evalA: " ++ (show test_evalA)
          putStrLn $ "test_evalB: " ++ (show test_evalB)

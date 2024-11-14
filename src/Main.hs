module Main where

data AExp = Int Int
          | X | Y -- Variables
          | Add AExp AExp
    deriving Show

eval :: Int -> Int -> AExp -> Int
eval _ _ (Int n)    = n
eval x y (Add n m)  = (eval x y n) + (eval x y m)
eval x _ X    = x
eval _ y Y    = y

test_eval =     (eval 2 2  (Int 42)        == 42)
             && (eval 2 42 Y               == 42)
             && (eval 2 40 (Add (Int 2) Y) == 42)
             && (eval 2 40 (Add X       Y) == 42)

main :: IO ()
main = putStrLn $ show test_eval

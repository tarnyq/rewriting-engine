module Main (main) where

import System.Environment (getArgs)

main :: IO ()
main = do
        putStr "Hello"
        args <- getArgs
        mapM_ putArg args
        putStrLn "."
    where
        putArg s = do
            putStr ", "
            putStr s


{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module Lib
    ( someFunc
    ) where


someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- average    :: [Int] -> Int
-- average xs = sum xs `div` length xs


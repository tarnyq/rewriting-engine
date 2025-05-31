{-@ GHC_OPTIONS -fplugin=LiquidHaskell @-}

module Main (main) where

import Lib

main :: IO ()
main = someFunc

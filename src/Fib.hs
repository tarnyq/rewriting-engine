-- Testing "symbolic execution" based on:
-- https://nikivazou.github.io/lh-course/Lecture_05_ProofsPrograms.html

{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
{-@ LIQUID "--reflection" @-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Fib () where

import Language.Haskell.Liquid.ProofCombinators

{-@ reflect fib @-}
{-@ fib :: Nat -> Nat @-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-@ fibTwo :: () -> { fib 2 = 1 } @-}
fibTwo :: () -> Proof
fibTwo _ =   fib 2
         === fib 1 + fib 0
                  *** QED

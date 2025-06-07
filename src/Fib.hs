-- Testing "symbolic execution" based on:
-- https://nikivazou.github.io/lh-course/Lecture_05_ProofsPrograms.html

{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
{-# OPTIONS_GHC -fplugin-opt=LiquidHaskell:--verbose #-}
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
         === 1
         *** QED

----------------------------------------------------------------------
-- Notes for cjs

--  The `Proof` type has a single inhabitant, so thus only two
--  (non-equivalent) refinements: The `Proof` type itself, and
--  ⊥, `Proof` with its one inhabitant removed.

--    The SMT solver proves that the given type is not uninhabited, which
--    implies that it proves, e.g., `fib 2 = fib 2`, or whatever's on the RHS.
--    LH also proves that the function terminates.
   {-@ ff :: { v:Proof | fib 2 = fib 2 } @-}
-- {-@ ff :: { v:Proof | true } @-} -- SMT solver checks that RHS expr is true.
-- {-@ ff :: v:Proof @-}
-- {-@ ff :: Proof @-}
ff :: Proof
ff =  trivial *** QED
-- =  ()

--  See how LH can dig into the function definition (code executed at
--  runtime) and "symbolicly execute" it to generate proofs about what
--  it will do.

{-@ reflect c @-}       -- w/o this, gives 'Unbound symbol Fib.c'
{-@ c :: Nat -> Nat @-}
c :: Int -> Int
c _ = 9         -- LH can discover that e.g. 'negate 4' does not LH-typecheck


-- {-@ cP :: { v:Proof | c 1 == 9   } @-}
   {-@ cP :: { v:Proof | c 1 == c 2 } @-}
cP :: Proof
cP = c 1 === 9 === c 2 *** QED

-- This does not work: cP = trival *** QED
--
--  This is apparently not trivial, because we needed ourselves to produce
--  a lemma containing 'c 1 === c2' as a hint to LH to make it able to
--  evaluate the refinement '| c 1 == c 2'.
--
--  The lemmas we provide are verified; if we change '9' to '8' above, LH
--  will point out the error.

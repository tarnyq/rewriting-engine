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

----------------------------------------------------------------------
-- Tutorial §5.5 Proofs by Natural Induction exercise

{-@ reflect sumTo @-}
{-@ sumTo :: lo:Nat -> hi:{Nat | lo <= hi} -> Nat / [hi]@-}
sumTo :: Int -> Int -> Int
--sumTo lo hi = if lo == hi then 0 else hi + sumTo lo (hi-1)
sumTo lo hi
    | lo == hi  = 0
    | otherwise = hi + sumTo lo (hi-1)


-- {-@ sumToN :: n:Nat -> { v:Proof | sumTo 0 n = n * (n + 1) / 2 } @-}
-- { forall n:Nat. (sumToN n :: { sumTo 0 n = n * (n + 1) / 2 } @-}

{-  This works fine with Z3 4.15, but on 4.8.12 (distributed by Debian 12)
    it appears to trigger an infinite loop bug[1] that causes a clean build
    to stall. (Interrupting the build and continuing it will continue
    fine.) Since we have plenty of other stuff testing LH, we can just
    leave this out.
    [1]: https://github.com/ucsd-progsys/liquidhaskell/issues/2444

{-@ sumToN :: n:Nat -> { sumTo 0 n = n * (n + 1) / 2 } @-}
sumToN :: Int -> Proof
sumToN 0 =     sumTo 0 0        -- base case
           === 0
           *** QED
sumToN n =         sumTo 0 n    -- inductive case
           === n + sumTo 0 (n-1)            ? (sumToN (n-1))
           === n + (n-1)*(n-1 + 1) `div` 2
           *** QED
-}

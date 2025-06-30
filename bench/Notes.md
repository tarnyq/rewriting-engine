Benchmark Notes
===============

Haskell `bin/benchmark`
-----------------------

Parameters tracked:
- `n(M)`: loop count (millions)
- `MUT(s)`: mutator time, seconds spent running the program code proper
- `GC(s)`: seconds spent doing GC
- `GC(GB)`: total gigabytes copied during GC

Timed on a Ryzen 7 7735HS (cjs/njr's mini-desktop) unless otherwise indicated.

### Initial tests on "naïve" code

    n(M)    MUT(s)  GC(s)   GC(GB)
    ───────────────────────────────────────────────────────────────
    10      25      6       7.2     §1
     5      12.5    3       3.6
     2       4.7    1.3     1.6
     1       2.5    0.7     0.8
    ───────────────────────────────────────────────────────────────
    10      26      0.0     0.0     §2 AExp, Int !Integer
    ───────────────────────────────────────────────────────────────
    10      11.6    0.0     0.0     §3 Unroll


§1: This seems linear in time, memory usage and GC for n=1…10, so So
we probably don't have major issues with our use of Haskell except that
ideally GC time would be near 0.

§2: The `!` operator reduces only to WHNF (weak head normal form),
so the "internals" of the var can still be a massive stream of thunks.
Thus, a `!state` in `eval` wouldn't make much difference (we'd need to go to
_full_ normal form to have that work, but `Int !Integer` in `AExp` reduces
often because that's where we are doing our arithemtic for this particular
program.

§3 Unroll: Replace the recursive calls to `eval` iterating across the set
of rewrite rules, with an `orElse` that "iterates" by doing the call to the
next rule itself, meaning that `eval` is called just once on the set of all
rules combined with `orElse`. In other words, we've replaced a loop with a
call to a call to a call … effectively unrolling it, which then (we guess)
allows GHC to remove duplicated tests in the sequence, which it could not
do otherwise. (We presume that GHC doesn't unroll the loop itself because
the list of rules, at a couple of dozen, is not tiny.)


    n(M)    MUT(s)  GC(s)   GC(GB)
    ───────────────────────────────────────────────────────────────
    10      13      0       0       Original
    10      15      0       0       Sorted rules
    ───────────────────────────────────────────────────────────────




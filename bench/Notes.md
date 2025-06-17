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
    10      25      6       7.2
     5      12.5    3       3.6
     2       4.7    1.3     1.6
     1       2.5    0.7     0.8

This seems linear in time, memory usage and GC for n=1…10, so So we
probably don't have major issues with our use of Haskell except that
ideally GC time would be near 0.

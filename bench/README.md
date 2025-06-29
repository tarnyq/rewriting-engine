Benchmarking K vs. KMonad
=========================

This `bench/` subdir contains the the K versions of programs and semantics
that we are benchmarking against similar versions in Haskell, which are
under `src/` etc. at the top level.

This currently is mostly hardcoded, even to the number of iterations run.
It needs to be parametrized should we want to benchmark more than just the
'sum.imp' program for the 'Imp' language.

### Benchmark Script

`Bench` is used to build, run and benchmark Haskell and K
programs/semantics. It's currently hard-coded to run the 'sum.imp' program
using 'Imp' semantics.

Given the `h` parameter it will run the Haskell version; it first runs
the top-level `Test` to build all the Haskell stuff and then runs the
compiled `benchmark` executable.

Given the `k` parameter it will use `kompile`, `krun` etc. to do the
benchmark. These must be supplied by the system.

The `kimage` script is a hint on one way to make K available; it will use
[dent] start a container from Runtime Verification's official release of K
7.1 on Docker Hub. The `-S` option will need to be modified to share into
the container whatever directory you're using for this repo on your host.
(If you don't want to use Dent, this can all be done in the fairly obvious
way using `docker run` with appropriate parameters.)

### Other Files

- `imp.k`: The K syntax and semantics for 'Imp' from [lesson 5] of the K
  [`pl-tutorial`] repo.
- `sum.imp`: The 'sum' program from the tutorial above, modified to do the
  appropriate number of loops for the benchmark.



<!-------------------------------------------------------------------->
[`pl-tutorial`]: https://github.com/runtimeverification/pl-tutorial
[dent]: https://github.com/cynic-net/dent
[lesson 5]: https://github.com/runtimeverification/pl-tutorial/blob/master/1_k/2_imp/lesson_5/imp.md

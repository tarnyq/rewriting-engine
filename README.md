While the idea behind the K framework---the semantics-first approach to language
development is extremely compelling, it still feels like a toy language, or a
research project.
It lacks many of the ecosystem tools taken for granted in other frameworks,
such as a unit-testing testing framework, incremental compilation, and dependency management.
While at it's core, K is a functional programming language it lacks many of the paradigms
that make functional programming enjoyable---second-order functions such
as `map` and `accumulate`.

None of these are deal breakers, but it is death by a thousand paper-cuts.
It prevents people from taking K seriously, and slows more wide-spread adoption.
There is nothing stopping us from implementing all these features,
but is it what we want to spend our time on? Or do we want to work with our
core strengths?

With these thoughts in mind we may ask:
do we really need K to be a whole new language? Could we instead implement it
as a library or tool over some other language?

The pillars that make K so powerful are:

1.  its simple, yet powerful, mathematical foundations---matching and
    reachabilty logic;
2.  ease of abstraction---features like configuration abstraction, contextual
    heating and cooling, while still allowing for non-determinism make language
    semantics specification easy.
3.  effecient concrete execution (enabling test-driven semantic development),
    with reasonable symbolic execution (enabling reachabilty reasoning,
    semantics-based compilation, abstract model checking...).

Expecting some existing language to support all of these out of the box
is too much to expect---indeed we may even find ourselves out of a job if there
was one: what is left for K to do? But can we find some language that gets us
much of the way there, and allowing us to fill the gaps with libraries and language
extensions?

The first requirement rules out "traditional" programming languages such as Rust,
C, and Python. It implies we should use languages towards the functional end of
the spectrum.

Haskell is a strong candidate. It is reduced to [Haskell Core], essentially
strongly typed lambda calculus with fixpoints. It has powerful abstraction
techniques including monads, and ADTs. These don't take things as far as we need
but get us pretty far. Features such as [quasiquoting] may allow us to write
rules using concrete programming language syntax. [LiquidHaskell] allows
theorem proving. While I have not got them running there seem to be symbolic
rewriting engines such as G2. We may also take the approach of translating Haskell Core to Maude.


[quasiquoting]: https://www.cs.tufts.edu/comp/150FP/archive/geoff-mainland/quasiquoting.pdf
[Haskell Core]: https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf

Additional Reading
------------------

The following are some resources I found interesting, and may or may not
be relevant to implementing in Haskell.

*   [Easy to follow explanation of Haskell Core](https://serokell.io/blog/haskell-to-core)
*   [Open type families](https://serokell.io/blog/type-families-haskell)
*   [Ivory](https://raw.githubusercontent.com/GaloisInc/ivory/refs/heads/master/ivory-paper/ivory.pdf)
*   [SMT in Haskell](https://hackage.haskell.org/package/sbv)

The Expression Problem: Extensible Type Heirarchies

*   https://wadler.blogspot.com/2008/02/data-types-la-carte.html
*   https://reasonablypolymorphic.com/blog/better-data-types-a-la-carte/index.html
*   https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409
*   https://stackoverflow.com/questions/6889715/extending-a-datatype-in-haskell


Advantages
----------

-   Could we use the type system to infer confluence? Semantic rules that don't
    touch non-thread-local state? This may make summarization of
    non-deterministic languages more feasible.

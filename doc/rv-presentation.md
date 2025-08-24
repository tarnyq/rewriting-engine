There are many advantages to this approach.
To keep things simple, we are going to talk about three.

1.  Major simplification of architecture:

    *   Lower maintenance burden:
        -   no longer dealing with toolchain across four different languages.
        -   large classes of maintenance go away:
                Incremental compilation, packaging,
                dependency management testing frameworks...

    *   Leverage Haskell's ecosystem
        -   robust set of high-quality libraries for hooks
        -   clean mathematical formalization of hooks with side-effects.
            (Not only IO, but also mutable datastructures, FFI...)

        -   LiquidHaskell for verifying simple lemmas
        -   Possiblilty of translating to Lean/Coq/Rocq; Agda integration

2.  Symbolic vs Concrete execution becomes a dial rather than a switch

    -   We can choose how much we want symbolic and how much concrete. So, we
        can pay a lower price for symbolic execution.

    -   Symbolic engine is no longer an interpreter for Kore, but can take
        advantage of the usual optimizations currently only available to the
        concrete excution engine.

3.  Quasiquoting and Template Haskell

    *   Ability to use concrete syntax in Haskell

    *   Single language for "meta-level" and "object-level" will enable quickly
        writing strategies for execution.

        e.g. taking advantage of confluence, and multiple CPU core
        for concurrent semantics.

---

Our goals are to figure out how to scale the semantics-first to real world
langauges and more complex use cases. In particular, we want to figure out how
to deal with non-determinism, e.g. for MediK.

Presentation Objective:

-   Convince RV that this is the right way in the long term.
-   We'd like advice on implementation of
-   We need a grant to get off the ground. Perhaps we can write an
    implementation of EVM in this fashion? We can emit kore configurations for
    integration with the existing K infrastructure.


Rationale
=========

K's strong points are its:

-   Ease of readability through the use of concrete syntax in the language
    definition.
-   General symbolic execution engine.
-   Reasonably fast concrete execution engine: Capable of running test suites
    within an order of magnitude of real-world languages.


Its weak points are:

-   K's idiosyncracies are often a huge time sink.
-   it's very hard to convince other people to take K seriously because of it's
    idiosyncracies.
-   we're spending a lot of time re-inventing a lot of wheels for no good reason
    (the entire functional base language, packaging and dependency management)

Our conclusion is that K's implementation as a fully independent language is not
worth the problems. If we can choose a base language with a strong mathematical
backing, can we bring in K's strong points?

-   Haskell replaces K's static component
-   Rewriting is implemented as a library in Haskell
    *   We use a State-like Monad for rewriting
    *   Reader/Writer-like Monad for configuration composition.
-   Quasi-quoting to provide allow usage of concrete language syntax in language
    definitions.

We think that this is a better way to do what K does.


Advantages
----------

Haskell's verification ecosystem:

-   Liquid Haskell: Refinement types for proofs of correctness, checking
    termination, etc.
-   Existing tools for translation to Coq, Lean, etc.


Symbolic execution:

-   The symbolic backend becomes compiled code, rather than an interpreted Kore
    definition.
-   There is no longer a sharp boundry between concrete and symbolic execution.
    We can turn a dial between the two at a very granualar level.


Concrete execution:

-   Performance advantages of having a powerful optimizing compiler.

Language design:

-   Template haskell and quasiquoting give us reflection and a metalevel.
-   Large library ecosystem that can be used directly in our semantics.
-   A more robust static-language/equational-reasoning.
    We get robust parametric sorts such as lists, maps, sets...
    Higher order functions such as `fmap`, `foldl`...

Abstraction

-   Sensible implementations of parametric types, higher-order functions:
    List/Map/Set, fmap, foldl...
-   State/Reader/Writer monads give us configuration abstraction.
-   Clean mathematical semantics for hooks with side-effects, using e.g. IO, ST
    Monads
-   Sensible parametric semantics via type classes rather than md
    selectors/conditional compilation

Maintenance:

-   Shed a lot of the overhead of implementing our own language. No longer need
    to use four different languages.

Less re-inventing the wheel:

-   Package and dependency management, easy FFI
-   Incremental compilation

Disadvantages
-------------

-   This is a wholesale re-implementation of K. As such, it is a big project,
    and would take a year or two to reach where we are with K.
-   We can maintain compatability with K, by emitting Kore


Demo
====

-   IMP, and its performance vs K: We can show the direct implementation that is
    slower than K, and the inlined version that is faster than K.
-   Semantics based compilation is just inlining of rules.
-   Verification: Prove Sum-to-N


Additional functionality needed
===============================


Tasks remaining for this:

-   Bring over hand-optimized Imp from other branch

-   Monadic IMP: Needed for implementation of Semantics-based Compilation and
    symbolic execution.

    -   We need template Haskell to make this presentable.

-   SBC: Once we have monadic verions working SBC for sum to N can be defined by
    combining rules using the `>>=` operator.

-   Verification:

    -   Version of Imp that uses SBV rather than native Haskell datastructures.
    -   Verify that sum-to-n returns n\*(n-1)/2

-   Handwritten (non-compiling) example of a few rules demonstrating what
    Quasiquoting will look like.

-   Outline of how we plan on handling non-determinism (use Imp-with-threads as
    an example)


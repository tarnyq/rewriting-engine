Lets prefix this by saying that this is a view of what the end-goal is, and
not how we get there in an incremental step-by-step manner.

Think of this as the starting point for a discussion, rather than a rigid plan.
That said, I feel sometime its important to have an idea of what ideal implementation
would look like before laying out the roadmap to get there.

--------------------------------------------------------------------------------

What would it take to scale K to larger general purpose languages?

Two main conclusions:

1.  K doesn't need to be a standalone langauge.

    - The static component of K re-invents a run-of-the-mill functional programming language.
    - Packaging and dependency management are hard
    - Compilation times are frustrating.

    - Allows *users* to implement more complex stratergies;
      e.g. take advantage of confluence of subsets of language for own evaluation strategy
      e.g. use physical cores for concurrent languages

    * In general, all this work is pulling us away from focusing on our core competency.

2.  Having the rewriting engine as an interpreter for Kore definitions doesn't
    seem like the right approach.

    -   Symbolic vs concrete should be a dial rather than a switch.

    -   If we only use a little bit of symbolic execution, we should only pay a
        small cost.

--------------------------------------------------------------------------------

So, can we avoid making K a standalone language, and instead have it as a
library or framework within another langage? Ideally, the static language would
be provided by this language, and Rewriting and symbolic execution would be a
library on top of it.

One of the important properties we need is that language has a formalish
mathematical semantics and a good type system.

Rules out trad imperative languages like C++, Java, Rust, Go.

Candidates are Maude; Lisp/Racquet; Haskell

*   We say how the Maude backend went; At the end of the day Maude is still a
    research/academic language.
*   Racquet is a strong candidate, but
*   I'm more familiar with Haskell;

--------------------------------------------------------------------------------

* Mathematical basis: Typed Lambda Calculus / System F
* Formal reasoning: Translations to Rocq/Lean, integration with Agda, LiquidHaskell
* Great type system and abstractions:
      Goodies like second order functions fmap; GADTs...
      Monads; typeclasses...
* Great abstractions: typeclasses; Monads; Taggless final
* MetaProgramming: Quasiquoting; Template Haskell; Deriving;

--------------------------------------------------------------------------------

So, what would K look like if we took this approach?
We've got a prototype:

-   Example K definition
    -   Hacky/Dummy Template Haskell example
-   Performance (abstracted; handoptimized; summarized)
-   Symbolic Execution; We will need some Template Haskell to remove redundancy here.
-   Modelling IO:
    -   Other applications mutable stack for speeding up KEVM
    -   Alternative implementations for Network and account backings for blockchains
    -   More general models/formalizations of KIO/Posix IO interface

--------------------------------------------------------------------------------

Conclusion:

-   This is quite a radical proposal, and we will need to figure out how to get
    to something like this incrementally.
-   The close integration between the backends, and frontends makes a lot more
    possible.


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

I've been thinking lately what we need to get K to scale to larger real-world
languages.

-   Its too hard to write efficient custom execution strategies: e.g. I know my language
    has a confluent portion, and a non-deterministic portion. There are particular
    strategies I can use that reduces the interleavings that model checking and
    symbolic execution can use.

    These can in theory be implemented in K, but its quite a mess,
    requiring us to use pyk to orchestrate the Haskell/Rust symbolic engine
    along with the concrete backend and so on.

-   Symbolic execution needs to fallback to concrete execution as much as possible
    to get any sort of effiency.

-   This whole business of the symbolic backend being an interpreter over Kore
    seems really inefficient, especially when dealing with narrowing focused
    reasoning, that does not bring in SMT solvers.

-   I've also been finding this four part ochestra of Java, Python, C/LLVM and
    Haskell frustrating to manage.
    K also does a really poor job of things you expect first-class support
    for any real language. e.g. unit tests, dependency management,

My conclusion is that "K as a standalone language" makes us to way too much work
of our own, without enough pay-off.
Ideally, we want K to be a library in another language that gives us a few
important features.


So, what features do we need? what makes K so powerful?

1.  a.  Functional static language, with powerful typesytem, and good abstractions
    b.  rewriting for defining transition systems.
2.  Fast concrete execution; allows us to run language test suites.
3.  General rewriting engine that gives us a lot of flexibilty
4.  Reflection, and meta-programming via pyk have made prototyping and extending
    K much much earier lately.

Point 1 rules out pretty much all imperative languages.
In fact, there are only a few strong candidates that come to mind:

1.  Maude; very researchy, poor ecosystem, difficult to extend
2.  LISP/Racket
3.  Haskell

There are many advantages to this approach.
To keep things simple, we're going to talk about three.

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


    *   Existing verification infrastructure
        *   LiquidHaskell's refinement types allow proving totality.
        *   Farm out theorem proving to Lean/Agda/Rocq building on existing implementations.

        https://digitalcommons.chapman.edu/cgi/viewcontent.cgi?article=1003&context=eecs_theses
        https://github.com/holcombet/hs-to-lean/tree/main

        https://arxiv.org/abs/1711.09286
        https://github.com/plclub/hs-to-coq


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



Other advantages:
-- Eventually, this instance could be extended soundly allow
-- more advanced features such as mutable datastructures. e.g. an optimized
-- stack implementation for EVM.
--


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


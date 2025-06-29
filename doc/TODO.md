To-Do List
==========

General QoL:
- Easy way to specify to test that we want to start a `ghci` as if we'd
  done `:load src/BExp.hs` or whatever, so we can just start typing.
- Stack things to investigate and document:
  - [Stack's script interpreter], `stack FNAME`, where _fname_ is a Haskell
    source file that has been marked executable (`chmod +x …`).

Build system:
- Combine common/version-specific info into `.build/stack.yaml` and use
  `--stack-yaml` (or `$STACK_YAML`?) to use it, removing top-level
  `stack.yaml` symlink.

Framework Structure:
- Examine [Recursion Schemes][] (Banannas/Lenses/Envelopes/Barbed Wire) for
  in-place reduction rather using a heating rule to replacing the expr with
  a hole and consing the separate expression, reducing, and then filling
  the hole with the car, leaving the cdr.

Performance:
- For performance, we generally want states to be as strict as possible by
  default because otherwise we can get large space leaks that are expensive
  to allocate and GC. However, this changes the semantics somewhat in that
  certain structures that can be evaluated with lazy elements may not be
  evaluable strictly. So what we want in the long run is probably for
  semantics to be strict by default, but optionally allow developers to
  make lazy semantics (and assume that they understand all that implies).
- For "strict mode," it would seem to make sense to use `deepseq` on the
  State after each rewrite step to force it to HNF. This avoids
  "strict-only" developer having to learn about lazyness and litter their
  code with strictness annotations. This would be done with something like
  importing `NFData` from `Control.Parallel.Strartegies`, `deepseq` from
  `Control.DeepSeq`, and ``eval rewrites xstate = xstate `deepseq` (eval'
  rewrites xstate) where …``.

Documentation:
- Explain big step vs. small step semantics, where big step semantics
  reduces `Prg{ x=1+1+1 }` to `Store{ x=3 }` in one step, whereas small
  step semantics reduces `Prg{ x=1+1+1 }` to `Prg{ x=2+1 }` etc.,
  eventually getting to `Store{x = 3}` after several steps. (Or can we do
  something in the code that makes this clear?)


Roadmap
-------

### Implement IMP strictly paralleling (determinized) K Tutorial Imp

Our goal here is to define a rewriting logic semantics for Imp. Each
rewrite rule is implemented as a function `State -> Maybe State`, The
function returns a `Just` if the state matches the left hand side of the
rule and the requires clause holds. Otherwise it returns `Nothing`.

These functions are composed in parallel, to give us a function `next ::
State -> Maybe State`, that takes a single execution step. Returning `None`
indicates that no rule applied, and execution has terminated. Otherwise, if
a rule applies, `next` will return the result of applying that rule.
Initially, we will only allow deterministic semantics, by composing rules
via an `orElse` combinator, rather than true parallel composition.
Iteratively applying the `next` function gives us an `eval` function, `eval
:: State -> State` that evaluates a program to completion.

Initially, we will begin with the "calculator" sub-language of Imp--AExp
and BExp without variables. Next we will introduce variable look ups.
Finally, we introduce control flow, assignments etc. Strictness and
variable looks ups will probably be the trickiest part of this semantics.

*   Implement seqstrict simply, without too much thought for abstraction
    and generality. Use this to implement arithmetic and boolean operations.
*   Implement variable lookup. This will need some kind of a reader monad.
*   Revisit strictness, cleanup and generalization.
*   Introduce control flow, assignments etc.

### Implement Imp idiomatically

Use Haskell idioms to implement Imp sensibly.

-   Clean up the AST
-   Consider using the bang operator to improve performance.
-   Verify sum-to-n in Imp.
-   Benchmark comparing to KTutImp.

### Mini-EVM semantics

Stack machine similar to EVM, that includes opcodes, gas,
and precompiled opcodes.

-   Benchmark vs analogous mini-EVM in K
-   Optimize using bang operator; mutable stack datastructure; etc;
-   Demonstrate clean handling of side-effects
-   Verify sum to n
-   Demonstrate CSE[OP] via sequential composition of rewrites and inlining.

### Non-Determinism

-   Demonstrate that we can cleanly build a non-deterministic semantics.
    Rewrites are composed using an actual sequential composition operator.
    `next` and `eval` will have signature `State -> [State]`.
-   Use liquid haskell to prove that a semantics is deterministic, even if
    implelemented using this non-deterministic fragment.

### Rules implemented in the concrete syntax using Quasi-Quoting

### Extensible Sort Heirarchies

A common technique in K is to extend the AST syntax returned by the parse
with additional constructors to ease writing out the semantics. This helps
keep the semantics modular, yet extensible.

### Functional Programming Language (e.g. KLambda)



<!-------------------------------------------------------------------->
[Recursion Schemes]: https://reasonablypolymorphic.com/blog/recursion-schemes/index.html
[Stack's script interpreter]: https://docs.haskellstack.org/en/stable/topics/scripts/
[lhtut]: https://ucsd-progsys.github.io/liquidhaskell-tutorial/

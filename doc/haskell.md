Haskell Language and Tools Notes
================================

Language
--------

### Haskell Language Extensions

* [`StandaloneDeriving`][ext-standalone-deriving] lets us use `deriving
  instance …` declarations separate from the `data` declarations to which
  one would normally add a `deriving …` clause. This is primarily useful to
  avoid littering the data declarations with `deriving Show` noise, instead
  moving that to a separate section of the file (or even another file).


Tools
-----

The standard compiler and interpreter is [GHC], and this is the only one
supported by [Liquid Haskell]. Additionally, Liquid Haskell supports only
specific point versions of GHC (e.g., 9.8.2 but not 9.8.4). For more on
this see [`lhver/README.md`].

[The Haskell Tool Stack][Stack] (usually just called "Stack," after its
`stack` command) handles the installation and use of GHC, associated tools
such as [HPack] and [Cabal], third-party packages from [Hackage], and so
on. Stack is installed by the top-level `Test` script if necessary; this is
a user, not a global system, install.

Stack uses a _resolver,_ specified in `stack.yaml`, to determine the
specific GHC version, Hackage package subset, etc. that are available. This
is typically one of the [Stackage] releases of stable configurations. The
resolver brings in default versions of software; but these versions can be
overridden so that you can use, e.g., GHC 9.8.2 instead of the default
9.8.4 supplied by Stackage [LTS 23.24]. Generally, only changing point
releases (or sometimes increasing the minor release version) will maintain
compatibility with what the resolver supplies by default.

GHC versions change relatively quickly compared to Stackage LTS releases.
As of this writing our minimum version is the latest [LTS 23.24] with GHC
downgraded to 9.8.2 for Liquid Haskell. Later versions (GHC 9.10 and above,
with a Stackage nightly release) may work, and we would consider moving to
that as a minimum version if we find that newer language extensions or
other features significantly increase the usability of KMonad.

### GHCi

[GHCi] is GHC's REPL. When asked to load a source code file it will load
the GHC-compiled version if that's up to date, otherwise it will load the
source. It's useful for investigating and doing quick sanity checks of your
code to convince yourself that the type system is doing what you think it
is.

In a Stack project it's typically started with `stack ghci` which will
build and load all the project's modules. Useful options include:
- `--no-load`: Do not load modules on startup. This is helpful if you want
  to avoid building/loading everything but instead explicitly load part
  of your project at the prompt.

You will have available only the default exports from the modules loaded at
startup. When examining an individual module, typically you want to use
`:load MODULE|FILE` (or `:load!` to defer type errors) which will rebuild
it and make all definitions (including non-exported ones) available as
unqualified names in the current environment.



<!-------------------------------------------------------------------->
<!-- Langauge -->
[ext-standalone-deriving]: https://downloads.haskell.org/~ghc/9.8.2/docs/users_guide/exts/standalone_deriving.html

<!-- Tools -->
[Cabal]: https://www.haskell.org/cabal/
[GHC]: https://www.haskell.org/ghc/
[GHCi]: https://downloads.haskell.org/ghc/9.8.4/docs/users_guide/ghci.html
[HPack]: https://github.com/sol/hpack
[Hackage]: https://hackage.haskell.org/
[LTS 23.24]: https://www.stackage.org/lts-23.24
[Liquid Haskell]: https://ucsd-progsys.github.io/liquidhaskell/
[Stack]: https://docs.haskellstack.org/en/stable/
[Stackage]: https://www.stackage.org/
[`lhver/README.md`]: ../lhver/README.md

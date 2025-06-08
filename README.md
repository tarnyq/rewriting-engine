KMonad
======

This is a proof-of-concept reimplementation of [K] as a Haskell library.
See the [rationale] for more on why we're doing this.

### Haskell Usage and Style Notes

For general hints for new users (or a refresher for those who have
forgotten) about the Haskell langauge and tools, see [`doc/haskell.md`].

- The formatting does not follow a strict set of rules, but instead
  is designed to be the most readable for any particular case, which
  means that it is not perfectly consistent. That's fine: readability
  is more important than appeasing "the hobgoblin of little minds."
- We do not use `UnicodeSyntax` because, while this is great for Haskell,
  it is not accepted by LiquidHaskell. (See issue [lh#2551].)



<!-------------------------------------------------------------------->
[K]: https://kframework.org
[rationale]: ./doc/rationale.md

[`doc/haskell.md`]: ./doc/haskell.md
[lh#2551]: https://github.com/ucsd-progsys/liquidhaskell/issues/2551

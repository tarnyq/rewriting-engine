KMonad
======

This is a proof-of-concept reimplementation of [K] as a Haskell library.
See the [rationale] for more on why we're doing this.

### Haskell Usage and Style Notes

- The formatting does not follow a strict set of rules, but instead
  is designed to be the most readable for any particular case, which
  means that it is not perfectly consistent. That's fine: readability
  is more important than appeasing "the hobgoblin of little minds."
- We do not use `UnicodeSyntax` because, while this is great for Haskell,
  it is not accepted by LiquidHaskell. (See issue [lh#2551].)


<!-------------------------------------------------------------------->
[K]: https://kframework.org
[rationale]: ./doc/rationale.md

[lh#2551]: https://github.com/ucsd-progsys/liquidhaskell/issues/2551

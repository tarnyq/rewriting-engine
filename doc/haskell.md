Haskell Language and Tools Notes
================================

### Haskell Language Extensions

* [`StandaloneDeriving`][ext-standalone-deriving] lets us use `deriving
  instance …` declarations separate from the `data` declarations to which
  one would normally add a `deriving …` clause. This is primarily useful to
  avoid littering the data declarations with `deriving Show` noise, instead
  moving that to a separate section of the file (or even another file).


<!-------------------------------------------------------------------->
[ext-standalone-deriving]: https://downloads.haskell.org/~ghc/9.8.2/docs/users_guide/exts/standalone_deriving.html

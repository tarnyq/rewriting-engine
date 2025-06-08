To-Do List
==========

- cjs: Work through [Liquid Haskell tutorial][lhtut] §1-3, maybe §4.

General QoL:
- Make `ghci` use settings in `.inputrc`.
- Easy way to specify to test that we want to start a `ghci` as if we'd
  done `:load src/BExp.hs` or whatever, so we can just start typing.

Build system:
- Combine common/version-specific info into `.build/stack.yaml` and use
  `--stack-yaml` (or `$STACK_YAML`?) to use it, removing top-level
  `stack.yaml` symlink.



<!-------------------------------------------------------------------->
[lhtut]: https://ucsd-progsys.github.io/liquidhaskell-tutorial/

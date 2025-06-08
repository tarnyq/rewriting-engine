To-Do List
==========

- cjs: Work through [Liquid Haskell tutorial][lhtut] §1-3, maybe §4.

General QoL:
- Make `ghci` use settings in `.inputrc`.
- Easy way to specify to test that we want to start a `ghci` as if we'd
  done `:load src/BExp.hs` or whatever, so we can just start typing.
- Stack things to investigate and document:
  - `stack exec -- which ghci` to find paths to programs you want to run
    without the Stack environment.
  - [Stack's script interpreter], `stack FNAME`, where _fname_ is a Haskell
    source file that has been marked executable (`chmod +x …`).

Build system:
- Combine common/version-specific info into `.build/stack.yaml` and use
  `--stack-yaml` (or `$STACK_YAML`?) to use it, removing top-level
  `stack.yaml` symlink.



<!-------------------------------------------------------------------->
[Stack's script interpreter]: https://docs.haskellstack.org/en/stable/topics/scripts/
[lhtut]: https://ucsd-progsys.github.io/liquidhaskell-tutorial/

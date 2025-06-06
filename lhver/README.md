lhver
=====

This directory contains configuration files for various versions of
LiquidHaskell to allow us to test against different versions. It's expected
that e.g. `stack.yaml` at the top level will be a symlink to
`lhver/$VER-stack.yaml`. (Be warned: A Git for Windows checkout will
usually copy the file instead of creating a symlink; don't check in a
copy!)

There is unfortunately currently a good deal of duplication for
configuration that's not directly specifying things related to the
LiquidHaskell version; we should look at fixing this, possibly by having
`Test` generate the files from separate common and version-specific
components.

For some resolvers, Liquid Haskell requires a specific point version of GHC
that is not the default one from the resolver, probably due to its
dependency on some GHC internals that breaks it with certain internal
changes. Always use the GHC that matches the LH version, not a later minor
release.

Further general information and examples can be found in:
* The Liquid Haskell [Installation][lh-inst], documentation.
* The [`stack.yaml` reference].
* For `stack.yaml`, the files in the [`stack/`][samples] subdirectory of
  the [lh-plugin-demo] repo ("Example Project 1" in the install docs above).
* [lh-plugin-demo-client][] ("Example Project 2" from the install docs above).
* The source code in [liquidhaskell]. You will want to check out the tag
  for the release you're using, e.g., `v0.9.8.2`. (You can get a list of
  all releases with `git tag | grep ^v`.)



<!-------------------------------------------------------------------->
[`stack.yaml` reference]: https://docs.haskellstack.org/en/stable/configure/yaml/
[lh-inst]: https://ucsd-progsys.github.io/liquidhaskell/install/
[lh-plugin-demo-client]: https://github.com/ucsd-progsys/lh-plugin-demo-client
[lh-plugin-demo]: https://github.com/ucsd-progsys/lh-plugin-demo
[liquidhaskell]: https://github.com/ucsd-progsys/liquidhaskell.git
[samples]: https://github.com/ucsd-progsys/lh-plugin-demo/tree/main/stack

Examining GHC Core Output
=========================

The `Test` script ensures that the `ghc-core` command, which generates
readable (and colourized, if you like) GHC core output is available via
`stack exec -- ghc-core`. (The initial `--` is to ensure subsequent options
go to `ghc-core`, not `stack exec`.)

`Test -g` will generate core files for `src/*.hs`, named for the current
commit. This is not fast, but is useful for automatically generating core
files as you move through commits that let you compare the generated code.

Typical commands are as follows. Note that this will produce intermediate
files (`*.hi` interface definitions, `*.o` object files, etc.) in the
source directories. There appears to be no way to move these at the moment,
so you'll want to ensure you remove them yourself (e.g. with `git clean -f`).

    #   Colorized output through a pager.
    stack exec -- ghc-core --no-cast --no-asm src/KImp.hs

    #   Readable output in Vim, for easier searching etc.
    stack exec -- ghc-core --no-syntax --no-cast --no-asm src/KImp.hs | vi -

By default `ghc-core` uses `-O2` GHC optimization; you can specify `-- -O1`
or `-- -O0` before the filename to pass in a GHC option to use a lower
optimisation level.

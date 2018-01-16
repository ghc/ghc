Nix-style Local Builds
======================

Nix-style local builds are a new build system implementation inspired by Nix.
The Nix-style local build system is commonly called "new-build" for short after the ``cabal new-*`` family of commands that control it.
However those names are only temporary until Nix-style local builds becomes the default.

Nix-style local builds combine the best of non-sandboxed and sandboxed Cabal:

1. Like sandboxed Cabal today, we build sets of independent local
   packages deterministically and independent of any global state.
   new-build will never tell you that it can't build your package
   because it would result in a "dangerous reinstall." Given a
   particular state of the Hackage index, your build is completely
   reproducible. For example, you no longer need to compile packages
   with profiling ahead of time; just request profiling and new-build
   will rebuild all its dependencies with profiling automatically.

2. Like non-sandboxed Cabal today, builds of external packages are
   cached in ``~/.cabal/store``, so that a package can be built once,
   and then reused anywhere else it is also used. No need to continually
   rebuild dependencies whenever you make a new sandbox: dependencies
   which can be shared, are shared.

Nix-style local builds were first released as beta in cabal-install 1.24.
They currently work with all versions of GHC supported by that release: GHC 7.0 and later.

Some features described in this manual are not implemented. If you need
them, please give us a shout and we'll prioritize accordingly.



.. toctree::
   nix-local-build

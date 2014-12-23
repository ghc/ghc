Shaking up GHC
==============

As part of my 6-month research secondment to Microsoft Research in Cambridge I am taking up the challenge of migrating the current [GHC](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) build system based on standard `make` into a new and (hopefully) better one based on [Shake](https://github.com/ndmitchell/shake/blob/master/README.md). If you are curious about the project you can find more details on the [wiki page](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake) and in this [blog post](https://blogs.ncl.ac.uk/andreymokhov/shaking-up-ghc/).

This is supposed to go into the `shake` directory of the GHC source tree (as a submodule).

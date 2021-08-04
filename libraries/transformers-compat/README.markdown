transformers-compat
===================

[![Hackage](https://img.shields.io/hackage/v/transformers-compat.svg)](https://hackage.haskell.org/package/transformers-compat) [![Build Status](https://secure.travis-ci.org/ekmett/transformers-compat.png?branch=master)](http://travis-ci.org/ekmett/transformers-compat)

This provides a thin compatibility shim on top of transformers-0.2 to add the types that were added in transformers-0.3.

This enables users to maintain haskell-platform compatibility, while still gaining access ot the new functionality.

Related packages
----------------
The `writer-cps-transformers` package backports the
`Control.Monad.Trans.{RWS,Writer}.CPS` modules that were introduced in
`transformers-0.5.6.0`. There are also a variety of companion packages which
backport orphan instances for these types. One example is `writer-cps-mtl`,
which backports instances of type classes from the `mtl` library.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

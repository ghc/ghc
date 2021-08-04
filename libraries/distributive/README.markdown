distributive
============


[![Hackage](https://img.shields.io/hackage/v/distributive.svg)](https://hackage.haskell.org/package/distributive) [![Build Status](https://github.com/ekmett/distributive/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/distributive/actions?query=workflow%3AHaskell-CI)

This package provides the notion that is categorically dual to `Traversable`.

A `Distributive` `Functor` is one that you can push any functor inside of.

```haskell
distribute :: (Functor f, Distributive g) => f (g a) -> g (f a)
```

Compare this with the corresponding Traversable notion, `sequenceA`.

```haskell
sequenceA :: (Applicative f, Traversable g) => g (f a) -> f (g a)
```

This package includes instances for common types, and includes other methods similar to `traverse` which fuse the use of `fmap`.

We only require `Functor` rather than some dual notion to `Applicative`, because the latter cannot meaningfully exist in Haskell
since all comonoids there are trivial.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

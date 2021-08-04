comonad
=======

[![Build Status](https://github.com/ekmett/comonad/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/comonad/actions?query=workflow%3AHaskell-CI)

This package provides comonads, the categorical dual of monads. The typeclass
provides three methods:  `extract`, `duplicate`, and `extend`.

    class Functor w => Comonad w where
        extract :: w a -> a
        duplicate :: w a -> w (w a)
        extend :: (w a -> b) -> w a -> w b

There are two ways to define a comonad:

I. Provide definitions for `extract` and `extend` satisfying these laws:

    extend extract      = id
    extract . extend f  = f
    extend f . extend g = extend (f . extend g)

In this case, you may simply set `fmap` = `liftW`.

These laws are directly analogous to the [laws for
monads](https://wiki.haskell.org/Monad_laws). The comonad laws can
perhaps be made clearer by viewing them as stating that Cokleisli composition
must be a) associative and b) have `extract` for a unit:

    f =>= extract   = f
    extract =>= f   = f
    (f =>= g) =>= h = f =>= (g =>= h)

II. Alternately, you may choose to provide definitions for `fmap`,
`extract`, and `duplicate` satisfying these laws:

    extract . duplicate      = id
    fmap extract . duplicate = id
    duplicate . duplicate    = fmap duplicate . duplicate

In this case, you may not rely on the ability to define `fmap` in
terms of `liftW`.

You may, of course, choose to define both `duplicate` _and_ `extend`.
In that case, you must also satisfy these laws:

    extend f  = fmap f . duplicate
    duplicate = extend id
    fmap f    = extend (f . extract)

These implementations are the default definitions of `extend` and`duplicate` and
the definition of `liftW` respectively.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

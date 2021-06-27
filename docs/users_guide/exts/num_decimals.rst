.. _num-decimals:

Fractional looking integer literals
-----------------------------------

.. extension:: NumDecimals
    :shortdesc: Enable support for 'fractional' integer literals.

    :since: 7.8.1

    Allow the use of scientific notation style borrowed from floating-point literal syntax for integral types.

Haskell 2010 and Haskell 98 define floating literals with the syntax
``1.2e6``, resembling scientific notation. These literals have the type ``Fractional a => a``.

The language extension :extension:`NumDecimals` allows you to also use the
scientific notation and floating point literal syntax for instances of
``Num``, and have values like ``1.2e6 :: Num a => a`` and ``5e10 :: Num a => a``
. This applies only to literals that really turn out to have integral
values. For example ``1.23e1 :: Fractional a => a`` since ``1.23e1 == 12.3``,
however ``1.23e2 :: Num a => a`` as ``1.23e2 == 123``.

Integral literals written using scientific notation will be desugared using
``fromInteger``, whereas any literals which aren't integral will be desugared
using ``fromRational`` as usual.

Note that regular floating point literals (without exponents) will also be
desugared via ``fromInteger`` and assigned type ``Num a => a`` if they
represent an integral value. For example ``1.0 :: Num a => a``, but
``1.1 :: Fractional a => a``.

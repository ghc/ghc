.. _negative-literals:

Negative literals
-----------------

.. extension:: NegativeLiterals
    :shortdesc: Enable support for negative literals.

    :since: 7.8.1

    Enable the use of un-parenthesized negative numeric literals.

The literal ``-123`` is, according to Haskell98 and Haskell 2010,
desugared as ``negate (fromInteger 123)``. The language extension
:extension:`NegativeLiterals` means that it is instead desugared as
``fromInteger (-123)``.

This can make a difference when the positive and negative range of a
numeric data type don't match up. For example, in 8-bit arithmetic -128
is representable, but +128 is not. So ``negate (fromInteger 128)`` will
elicit an unexpected integer-literal-overflow message.



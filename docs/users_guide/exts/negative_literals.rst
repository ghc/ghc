.. _negative-literals:

Negative literals
-----------------

.. extension:: NegativeLiterals
    :shortdesc: Enable support for negative literals.

    :since: 7.8.1

    Enable negative numeric literals.

The literal ``-123`` is, according to Haskell98 and Haskell 2010,
two tokens, a unary minus (``-``) and the number 123, and is
desugared as ``negate (fromInteger 123)``. The language extension
:extension:`NegativeLiterals` causes it to be treated as a single
token and desugared as ``fromInteger (-123)``.

This can be useful when the positive and negative range of a numeric
data type don't match up. For example, in 8-bit arithmetic -128
is representable, but +128 is not. So ``negate (fromInteger 128)``
will elicit an unexpected integer-literal-overflow message.

Whitespace can be inserted, as in ``- 123``, to force interpretation
as two tokens.

One pitfall is that with :extension:`NegativeLiterals`, ``x-1`` will
be parsed as ``x`` applied to the argument ``-1``, which is usually
not what you want.  ``x - 1`` or even ``x- 1`` can be used instead
for subtraction.


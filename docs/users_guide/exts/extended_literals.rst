.. _extended-literals:

Sized primitive literal syntax
------------------------------

.. extension:: ExtendedLiterals
    :shortdesc: Allow numeric literal postfix syntax for unboxed integers.

    :since: 9.8.1

    Allows defining unboxed numeric primitive values through ``#Type`` suffixes
    on numeric literals e.g. ``0xFF#Word8 :: Word8#``.

The :extension:`MagicHash` extension enables some new literals, including ``3#
:: Int#``, ``3## :: Word#``. This does not extend to all unboxed values. For
example, there is no literal syntax for ``Word8#``: you must write something
such as ``wordToWord8 (3## :: Word#) :: Word8#``.

:extension:`ExtendedLiterals` enables further syntax for defining primitive
numeric literals. Suffix any Haskell integer lexeme with a hash sign ``#``
followed by a primitive numeric type (without its hash suffix) to obtain a value
of that type. For example, ``0xFF#Word8 :: Word8#``. There must be no spaces
between the parts of the literal.

The primitive numeric types allowed are:

- ``Int8#``
- ``Int16#``
- ``Int32#``
- ``Int64#``
- ``Int#``
- ``Word8#``
- ``Word16#``
- ``Word32#``
- ``Word64#``
- ``Word#``

All types permit any positive and negative Haskell integer lexeme. Defining a
literal with a value that can't fit in its requested type will emit an overflow
warning by default, the same as boxed numeric literals (see
:ghc-flag:`-Woverflowed-literals`).

As with :extension:`MagicHash`, this extension does not bring anything into
scope, nor change any semantics. The syntax only applies to numeric literals.
You may want to import ``GHC.Exts`` (see :ref:`primitives`) to refer to the
types of the literals you define.

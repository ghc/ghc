Linear types
============

.. extension:: LinearTypes
    :shortdesc: Enable linear types.

    :since: 8.10.1

    Enable the linear arrow ``a #-> b`` and the multiplicity-polymorphic arrow
    ``a # m -> b``.

The linear types extension enables support for linear functions, as described
in the paper "`Linear Haskell: practical linearity in a higher-order polymorphic language
<https://www.microsoft.com/en-us/research/publication/linear-haskell-practical-linearity-higher-order-polymorphic-language/>`__" (POPL 2018)
and GHC proposal "`Linear types <https://github.com/ghc-proposals/ghc-proposals/pull/111>`__".

This extension adds a linear variant of the normal function arrow ``a -> b``
written as ``a #-> b``. In brief, the right hand side of a linear function
must consume its argument exactly once. This can be achieved:

- by returning the argument unmodified,
- by passing the argument to a different linear function,
- for algebraic data types, by pattern matching on the argument and consuming
  every field exactly once,
- for functions, by calling and consuming the result of the call exactly once.

If :extension:`UnicodeSyntax` is enabled, the ``#->`` arrow can be written as ``⊸``.

To allow uniform handling of linear ``a #-> b`` and unrestricted ``a -> b``
functions, there is a new function type ``a # m -> b``. Here, ``m`` is a type
of new kind ``Multiplicity``. We have:

::

    data Multiplicity = One | Many  -- Defined in GHC.Types

    type a #-> b = a # 'One  -> b
    type a  -> b = a # 'Many -> b

(See :ref:`promotion`).

The multiplicity-polymorphic arrow ``a # m -> b`` is available in a prefix
version as ``GHC.Types.FUN m a b``, which can be applied partially.

Linear and multiplicity-polymorphic functions must be declared as such; in the
absence of a type signature, functions are presumed to be unrestricted.

Printing multiplicity-polymorphic types
---------------------------------------
If :extension:`LinearTypes` is disabled, multiplicity variables in types are defaulted
to ``Many`` when printing, in the same manner as described in :ref:`printing-levity-polymorphic-types`.
In other words, without :extension:`LinearTypes`, multiplicity-polymorphic functions
``a # m -> b`` are printed as normal Haskell2010 functions ``a -> b``. This allows
existing libraries to be generalized to linear types in a backwards-compatible
manner; the general types are visible only if the user has enabled
:extension:`LinearTypes`.
(Note that a library can declare a linear function in the contravariant position,
i.e. take a linear function as an argument. In this case, linearity cannot be
hidden; it is an essential part of the exposed interface.)

Data types
----------
By default, all fields in algebraic data types are linear. Given

::

    data T1 a = MkT1 a

the value ``MkT1 x`` can be constructed and deconstructed in a linear context:

::

    construct :: a #-> MkT1 a
    construct x = MkT1 x

    deconstruct :: MkT1 a #-> a
    deconstruct (MkT1 x) = x  -- must consume `x` exactly once

This behavior can be customized using the GADT syntax. Given

::

    data T2 a b c where
        MkT2 :: a -> b #-> c #-> T2 a b  -- Note unrestricted arrow in the first argument

the value ``MkT2 x y z`` can be constructed only if ``x`` is available
in an unrestricted context. On the other hand, a linear function which
is matching on ``MkT2 x y z`` must consume ``y`` and ``z`` exactly once,
but there is no restriction on ``x``.

For backwards compatibility, constructors using linear fields are generalized
to multiplicity-polymorphic functions. For example, the type of ``MkT1`` is
``a # n -> MkT1 a``. The additional multiplicity argument ``n`` is marked as
inferred (see :ref:`inferred-vs-specified`), so that there is no conflict with visible
type application. If there are multiple linear fields, each one gets a corresponding
multiplicity variable; for example, ``MkT2 :: a #-> b # n -> c # o -> T2 a b c``.

If :extension:`LinearTypes` is disabled, all fields are considered to be linear
fields, including GADT fields defined with the ``->`` arrow. This does not change
the type seen by the user who is not interested in linear types, since
the multiplicity-polymorphic constructors are printed as unrestricted.

In a ``newtype`` declaration, the field must be linear.

Limitations
-----------
This is the first version of linear types and has several limitations. We
encourage you to experiment and report issues in the GHC bug tracker
`the GHC bug tracker <https://gitlab.haskell.org/ghc/ghc/issues>`__,
adding the tag `LinearTypes`.

- There is currently no support for multiplicity annotations such as
  ``x :: a # p``, ``\(x :: a # p) -> ...``.
- Multiplicity polymorphism is incomplete and experimental. In the future, we plan
  to add two type families for adding and multiplying multiplicities, and a
  submultiplicity constraint. Currently, the multiplicity solver handles only
  basic cases, by approximating sums and products by ``Many`` and
  submultiplicity by equality ``p ~ q``.
- All ``case``, ``let`` and ``where`` statements consume the argument in an
  unrestricted context. Only a function declaration can perform pattern matching linearly.
- There is no support for linear pattern synonyms.
- ``@``-patterns and view patterns are not linear.
- The projection function for a record with a single linear field should be
  multiplicity-polymorphic; currently it's unrestricted.
- GHC libraries have not yet been updated to cover linearity.

.. _template-haskell:


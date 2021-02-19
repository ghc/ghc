Linear types
============

.. extension:: LinearTypes
    :shortdesc: Enable linear types.

    :since: 9.0.1

    Enable the linear arrow ``a %1 -> b`` and the multiplicity-polymorphic arrow
    ``a %m -> b``.

**This extension is currently considered experimental, expect bugs,
warts, and bad error messages; everything down to the syntax is
subject to change**.  See, in particular,
:ref:`linear-types-limitations` below. We encourage you to experiment
with this extension and report issues in the GHC bug tracker `the GHC
bug tracker <https://gitlab.haskell.org/ghc/ghc/issues>`__, adding the
tag ``LinearTypes``.

A function ``f`` is linear if: when its result is consumed *exactly
once*, then its argument is consumed *exactly once*. Intuitively, it
means that in every branch of the definition of ``f``, its argument
``x`` must be used exactly once. Which can be done by

* Returning ``x`` unmodified
* Passing ``x`` to a *linear* function
* Pattern-matching on ``x`` and using each argument exactly once in the
  same fashion.
* Calling it as a function and using the result exactly once in the same
  fashion.

With ``-XLinearTypes``, you can write ``f :: a %1 -> b`` to mean that
``f`` is a linear function from ``a`` to ``b``.  If
:extension:`UnicodeSyntax` is enabled, the ``%1 ->`` arrow can be
written as ``⊸``.

To allow uniform handling of linear ``a %1 -> b`` and unrestricted ``a
-> b`` functions, there is a new function type ``a %m -> b``.
Here, ``m`` is a type of new kind ``Multiplicity``. We have:

::

    data Multiplicity = One | Many  -- Defined in GHC.Types

    type a %1 -> b = a %One  -> b
    type a  -> b = a %Many -> b

(See :ref:`promotion`).

We say that a variable whose multiplicity constraint is ``Many`` is
*unrestricted*.

The multiplicity-polymorphic arrow ``a %m -> b`` is available in a prefix
version as ``GHC.Exts.FUN m a b``, which can be applied
partially. See, however :ref:`linear-types-limitations`.

Linear and multiplicity-polymorphic arrows are *always declared*,
never inferred. That is, if you don't give an appropriate type
signature to a function, it will be inferred as being a regular
function of type ``a -> b``.

Data types
----------
By default, all fields in algebraic data types are linear (even if
``-XLinearTypes`` is not turned on). Given

::

    data T1 a = MkT1 a

the value ``MkT1 x`` can be constructed and deconstructed in a linear context:

::

    construct :: a %1 -> T1 a
    construct x = MkT1 x

    deconstruct :: T1 a %1 -> a
    deconstruct (MkT1 x) = x  -- must consume `x` exactly once

When used as a value, ``MkT1`` is given a multiplicity-polymorphic
type: ``MkT1 :: forall {m} a. a %m -> T1 a``. This makes it possible
to use ``MkT1`` in higher order functions. The additional multiplicity
argument ``m`` is marked as inferred (see
:ref:`inferred-vs-specified`), so that there is no conflict with
visible type application. When displaying types, unless
``-XLinearTypes`` is enabled, multiplicity polymorphic functions are
printed as regular functions (see :ref:`printing-linear-types`);
therefore constructors appear to have regular function types.

::

    mkList :: [a] -> [T1 a]
    mkList xs = map MkT1 xs

Hence the linearity of type constructors is invisible when
``-XLinearTypes`` is off.

Whether a data constructor field is linear or not can be customized using the GADT syntax. Given

::

    data T2 a b c where
        MkT2 :: a -> b %1 -> c %1 -> T2 a b c -- Note unrestricted arrow in the first argument

the value ``MkT2 x y z`` can be constructed only if ``x`` is
unrestricted. On the other hand, a linear function which is matching
on ``MkT2 x y z`` must consume ``y`` and ``z`` exactly once, but there
is no restriction on ``x``.

It is also possible to define a multiplicity-polymorphic field:

::
    data T3 a m where
        MkT3 :: a %m -> T3 a m

While linear fields are generalized (``MkT1 :: forall {m} a. a %m -> T1 a``
in the previous example), multiplicity-polymorphic fields are not;
it is not possible to directly use ``MkT3`` as a function ``a -> T3 a 'One``.

If :extension:`LinearTypes` is disabled, all fields are considered to be linear
fields, including GADT fields defined with the ``->`` arrow.

In a ``newtype`` declaration, the field must be linear. Attempting to
write an unrestricted newtype constructor with GADT syntax results in
an error.

.. _printing-linear-types:

Printing multiplicity-polymorphic types
---------------------------------------
If :extension:`LinearTypes` is disabled, multiplicity variables in types are defaulted
to ``Many`` when printing, in the same manner as described in :ref:`printing-levity-polymorphic-types`.
In other words, without :extension:`LinearTypes`, multiplicity-polymorphic functions
``a %m -> b`` are printed as normal Haskell2010 functions ``a -> b``. This allows
existing libraries to be generalized to linear types in a backwards-compatible
manner; the general types are visible only if the user has enabled
:extension:`LinearTypes`.
(Note that a library can declare a linear function in the contravariant position,
i.e. take a linear function as an argument. In this case, linearity cannot be
hidden; it is an essential part of the exposed interface.)

.. _linear-types-limitations:

Limitations
-----------
Linear types are still considered experimental and come with several
limitations. If you have read the full design in the proposal (see
:ref:`linear-types-references` below), here is a run down of the
missing pieces.

- Multiplicity polymorphism is incomplete and experimental. You may
  have success using it, or you may not. Expect it to be really unreliable.
- There is currently no support for multiplicity annotations such as
  ``x :: a %p``, ``\(x :: a %p) -> ...``.
- A ``case`` expression may consume its scrutinee ``One`` time,
  or ``Many`` times. But the inference is still experimental, and may
  over-eagerly guess that it ought to consume the scrutinee ``Many`` times.
- All ``let`` and ``where`` statements consume their right hand side
  ``Many`` times. That is, the following will not type check:

  ::

      g :: A %1 -> (A, B)
      h :: A %1 -> B %1 -> C

      f :: A %1 -> C
      f x =
        let (y, z) = g x in h y z

  This can be worked around by defining extra functions which are
  specified to be linear, such as:

  ::

      g :: A %1 -> (A, B)
      h :: A %1 -> B %1 -> C

      f :: A %1 -> C
      f x = f' (g x)
        where
          f' :: (A, B) %1 -> C
          f' (y, z) = h y z
- There is no support for linear pattern synonyms.
- ``@``-patterns and view patterns are not linear.
- The projection function for a record with a single linear field should be
  multiplicity-polymorphic; currently it's unrestricted.
- Attempting to use of linear types in Template Haskell will probably
  not work.

.. _linear-types-references:

Design and further reading
--------------------------

* The design for this extension is described in details in the `Linear
  types proposal
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst>`__
* This extension has been originally conceived of in the paper `Linear
  Haskell: practical linearity in a higher-order polymorphic language
  <https://www.microsoft.com/en-us/research/publication/linear-haskell-practical-linearity-higher-order-polymorphic-language/>`__
  (POPL 2018)
* There is a `wiki page dedicated to the linear types extension <https://gitlab.haskell.org/ghc/ghc/-/wikis/linear-types>`__

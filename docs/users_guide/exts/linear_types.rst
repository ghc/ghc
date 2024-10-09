.. _linear-types:

Linear types
============

.. extension:: LinearTypes
    :shortdesc: Allow writing of linear arrow types.
        Implies :extension:`MonoLocalBinds`.

    :implies: :extension:`MonoLocalBinds`

    :since: 9.0.1
    :status: Experimental

    Enable the linear arrow ``a %1 -> b`` and the multiplicity-polymorphic arrow
    ``a %m -> b``.

**This extension is currently considered experimental, expect bugs,
warts, and bad error messages; everything down to the syntax is
subject to change**.  See, in particular,
:ref:`linear-types-limitations` below. We encourage you to experiment
with this extension and report issues in `the GHC
bug tracker <https://gitlab.haskell.org/ghc/ghc/issues>`__, adding the
tag ``LinearTypes``.

A function ``f`` is linear if: when its result is consumed *exactly
once*, then its argument is consumed *exactly once*. Intuitively, it
means that in every branch of the definition of ``f``, its argument
``x`` must be used exactly once. Which can be done by

* Returning ``x`` unmodified.
* Passing ``x`` to a *linear* function and using the result exactly once
  in the same fashion.
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
function of type ``a -> b``. The same principle holds for
representation polymorphism (see :ref:`representation-polymorphism-defaulting`).

Expressions
-----------

When defining a function either as a lambda ``\x -> u`` or with
equations ``f x = u``, the multiplicity of the variable ``x`` will be
inferred from the context. For equations, the context will typically
be a type signature. For instance here is a linear function

::

    f :: (a -> b) %1 -> a -> b
    f g x = g x

In this example, ``g`` must be used linearly while ``x`` is
unrestricted.

Bindings
~~~~~~~~

Let and where bindings can be linear as well, the multiplicity of
bindings is typically inferred

::

    f :: A %1 -> B
    g :: B %1 -> C

    h :: A %1 -> C
    h x = g y
      where
        y = f x

If you don't want, or aren't able, to rely on inference, let and where
bindings can be annotated with a multiplicity

::

    f :: A %1 -> B
    g :: B %1 -> C

    h :: A %1 -> C
    h x = g y
      where
        %1 y = f x

The precise rules are, that you can annotate a binding with a
multiplicity if:

- The binding is not top-level
- The binding is non-recursive
- The binding is a pattern binding (including a simple variable)
  ``p=e`` (you can't write ``let %1 f x = u``, instead write ``let %1
  f = \x -> u``)
- Either ``p`` is strict (see :ref:`strict-patterns-for-linear` below) or ``p`` is a variable. In
  particular neither ``x@y`` nor ``(x)`` are covered by “is a
  variable”

When there's no multiplicity annotation, the multiplicity is inferred
as follows:

- Toplevel bindings are inferred as having multiplicity ``Many``
- Recursive bindings are inferred as having multiplicity ``Many``
- Lazy non-variable pattern bindings are inferred as having
  multiplicity ``Many`` (note that in let- and where-bindings,
  patterns are lazy by default, so that ``let (x,y) = rhs`` always
  have multiplicity ``Many``, whereas ``let !(x,y) = rhs`` can have
  multiplicity ``1``).
- In all other cases, including function bindings ``let f x1...xn = rhs``,
  the multiplicity is inferred from the term.

When ``-XMonoLocalBinds`` is off, the following also holds:

- Multiplicity-annotated non-variable pattern-bindings (such as
  ``let %1 !(x,y) = rhs``) are never generalised.
- Non-variable pattern bindings which are inferred as polymorphic or
  qualified are inferred as having multiplicity ``Many``.

.. _strict-patterns-for-linear:

Strict patterns
~~~~~~~~~~~~~~~

GHC considers that non-variable lazy patterns consume the scrutinee
with multiplicity ``Many``. In practice, a pattern is strict (hence
can be linear) if (otherwise the pattern is lazy):

- The pattern is a case alternative and isn't annotated with a ``~``
- The pattern is a let-binding, and is annotated with a ``!``
- The pattern is a let-binding, :extension:`Strict` is on, and isn't
  annotated with a ``~``
- The pattern is nested inside a strict pattern

Here are some examples of the impact on linear typing:

Without ``-XStrict``::

   -- good
   let %1 x = u in …

   -- good
   let %1 !x = u in …

   -- bad
   let %1 (x, y) = u in …

   -- good
   let %Many (x, y) = u in …

   -- good
   let %1 !(x, y) = u in …

   -- good
   let %1 (!(x, y)) = u in …

   -- inferred unrestricted
   let (x, y) = u in …

   -- can be inferred linear
   case u of (x, y) -> …

   -- inferred unrestricted
   case u of ~(x, y) -> …

With ``-XStrict``::

   -- good
   let %1 x = u in …

   -- good
   let %1 !x = u in …

   -- good
   let %1 (x, y) = u in …

   -- bad
   let %1 ~(x, y) = u in …

   -- good
   let %Many ~(x, y) = u in …

   -- can be inferred linear
   let (x, y) = u in …

   -- inferred unrestricted
   let ~(x, y) = u in …

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
it is not possible to directly use ``MkT3`` as a function ``a -> T3 a One``.

If :extension:`LinearTypes` is disabled, all fields are considered to be linear
fields, including GADT fields defined with the ``->`` arrow.

In a ``newtype`` declaration, the field must be linear. Attempting to
write an unrestricted newtype constructor with GADT syntax results in
an error.

.. _printing-linear-types:

Printing multiplicity-polymorphic types
---------------------------------------
If :extension:`LinearTypes` is disabled, multiplicity variables in types are defaulted
to ``Many`` when printing, in the same manner as described in :ref:`printing-representation-polymorphic-types`.
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
  (Multiplicity multiplication is not supported yet.)
- There is currently no support for multiplicity annotations on
  function arguments such as ``\(%p x :: a) -> ...``, only on
  let-bound variables.
- A ``case`` expression may consume its scrutinee ``One`` time,
  or ``Many`` times. But the inference is still experimental, and may
  over-eagerly guess that it ought to consume the scrutinee ``Many`` times.
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

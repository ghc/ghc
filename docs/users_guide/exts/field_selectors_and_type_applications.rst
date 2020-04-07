.. _field-selectors-and-type-applications:

Field selectors and ``TypeApplications``
----------------------------------------

Field selectors can be used in conjunction with :extension:`TypeApplications`,
as described in :ref:`visible-type-application`. The type of a field selector
is constructed by using the surrounding definition as context. This section
provides a specification for how this construction works. We will explain it
by considering three different forms of field selector, each of which is a
minor variation of the same general theme.

Field selectors for Haskell98-style data constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider the following example: ::

  data T a b = MkT { unT :: forall e. Either e a }

This data type uses a Haskell98-style declaration. The only part of this data
type that is not Haskell98 code is ``unT``, whose type uses higher-rank
polymorphism (:ref:`arbitrary-rank-polymorphism`). To construct the type of
the ``unT`` field selector, we will assemble the following:

1. The type variables quantified by the data type head
   (``forall a b. <...>``).
2. The return type of the data constructor
   (``<...> T a b -> <...>``). By virtue of this being a Haskell98-style
   declaration, the order of type variables in the return type will always
   coincide with the order in which they are quantified.
3. The type of the field
   (``<...> forall e. Either e a``).

The final type of ``unT`` is therefore
``forall a b. T a b -> forall e. Either e a``. As a result, one way to use
``unT`` with :extension:`TypeApplications` is
``unT @Int @Bool (MkT (Right 1)) @Char``.

Field selectors for GADT constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Field selectors for GADT constructors (:ref:`gadt-style`) are slightly more
involved. Consider the following example: ::

  data G a b where
    MkG :: forall x n a. (Eq a, Show n)
        => { unG1 :: forall e. Either e (a, x), unG2 :: n } -> G a (Maybe x)

The ``MkG`` GADT constructor has two records, ``unG1`` and ``unG2``.
However, only ``unG1`` can be used as a top-level field selector. ``unG2``
cannot because it is a "hidden" selector (see :ref:`existential-records`); its
type mentions a free variable ``n`` that does not appear in the result type
``G a (Maybe x)``. On the other hand, the only free type variables in the type
of ``unG1`` are ``a`` and ``x``, so ``unG1`` is fine to use as a top-level
function.

To construct the type of the ``unG1`` field selector, we will assemble
the following:

1. The subset of type variables quantified by the GADT constructor that are
   mentioned in the return type. Note that the order of these variables follows
   the same principles as in :ref:`ScopedSort`.
   If the constructor explicitly quantifies its type variables at the beginning
   of the type, then the field selector type will quantify them in the same
   order (modulo any variables that are dropped due to not being mentioned in
   the return type).
   If the constructor implicitly quantifies its type variables, then the field
   selector type will quantify them in the left-to-right order that they appear
   in the field itself.

   In this example, ``MkG`` explicitly quantifies ``forall x n a.``, and of
   those type variables, ``a`` and ``x`` are mentioned in the return type.
   Therefore, the type of ``unG1`` starts as ``forall x a. <...>``.
   If ``MkG`` had not used an explicit ``forall``, then they would have instead
   been ordered as ``forall a x. <...>``, since ``a`` appears to the left of
   ``x`` in the field type.
2. The GADT return type
   (``<...> G a (Maybe x) -> ...``).
3. The type of the field
   (``<...> -> forall e. Either e (a, x)``).

The final type of ``unG1`` is therefore
``forall x a. G a (Maybe x) -> forall e. Either e (a, x)``. As a result, one
way to use ``unG1`` with :extension:`TypeApplications` is
``unG1 @Int @Bool (MkG (Right (True, 42)) ()) @Char``.

Field selectors for pattern synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Certain record pattern synonyms (:ref:`record-patsyn`) can give rise to
top-level field selectors. Consider the following example: ::

  pattern P :: forall a. Read a
            => forall n. (Eq a, Show n)
            => (forall e. Either e (a, Bool)) -> n -> G a (Maybe Bool)
  pattern P {unP1, unP2} = MkG unP1 unP2

We can only make field selectors for pattern synonym records that do not
mention any existential type variables whatsoever in their types, per
:ref:`record-patsyn`. (This is a stronger requirement than for GADT records,
whose types can mention existential type variables provided that they are also
mentioned in the return type.) We can see that ``unP2`` cannot be used as a
top-level field selector since its type has a free type variable ``n``, which
is existential. ``unP1`` is fine, on the other hand, as its type only has one
free variable, the universal type variable ``a``.

To construct the type of the ``unP1`` field selector, we will assemble
the following:

1. The universal type variables
   (``forall a. <...>``).
2. The required constraints
   (``<...> Read a => <...>``).
3. The pattern synonym return type
   (``<...> G a (Maybe Bool) -> <...>``).
4. The type of the field
   (``<...> -> forall e. Either e (a, Bool)``).

The final type of ``unP1`` is therefore
``forall a. Read a => G a (Maybe Bool) -> forall e. Either e (a, Bool)``. As a
result, one way to use ``unP1`` with :extension:`TypeApplications` is
``unP1 @Double (MkG (Right (4.5, True)) ()) @Char``.

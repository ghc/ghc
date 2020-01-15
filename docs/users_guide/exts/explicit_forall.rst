.. _explicit-foralls:

Explicit universal quantification (forall)
------------------------------------------

.. extension:: ExplicitForAll
    :shortdesc: Enable explicit universal quantification.
        Implied by :extension:`ScopedTypeVariables`, :extension:`LiberalTypeSynonyms`,
        :extension:`RankNTypes` and :extension:`ExistentialQuantification`.

    :since: 6.12.1

    Allow use of the ``forall`` keyword in places where universal quantification
    is implicit.

Haskell type signatures are implicitly quantified. When the language
option :extension:`ExplicitForAll` is used, the keyword ``forall`` allows us to
say exactly what this means. For example: ::

    g :: b -> b

means this: ::

    g :: forall b. (b -> b)

The two are treated identically, except that the latter may bring type variables
into scope (see :ref:`scoped-type-variables`).

This extension also enables explicit quantification of type and kind variables
in :ref:`data-instance-declarations`, :ref:`type-instance-declarations`,
:ref:`closed-type-families`, :ref:`assoc-inst`, and :ref:`rewrite-rules`.

Notes:

- As well in type signatures, you can also use an explicit ``forall``
  in an instance declaration: ::

      instance forall a. Eq a => Eq [a] where ...

- If the :ghc-flag:`-Wunused-foralls` flag is enabled, a warning will be emitted
  when you write a type variable in an explicit ``forall`` statement that is
  otherwise unused. For instance: ::

    g :: forall a b. (b -> b)

  would warn about the unused type variable `a`.



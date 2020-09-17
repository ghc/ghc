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

- As well as in type signatures, you can also use an explicit ``forall``
  in an instance declaration: ::

      instance forall a. Eq a => Eq [a] where ...

  Note that the use of ``forall``\s in instance declarations is somewhat
  restricted in comparison to other types. For example, instance declarations
  are not allowed to contain nested ``forall``\s. See
  :ref:`formal-instance-syntax` for more information.

- If the :ghc-flag:`-Wunused-foralls` flag is enabled, a warning will be emitted
  when you write a type variable in an explicit ``forall`` statement that is
  otherwise unused. For instance: ::

    g :: forall a b. (b -> b)

  would warn about the unused type variable `a`.

.. _forall-or-nothing:

The ``forall``-or-nothing rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In certain forms of types, type variables obey what is known as the
"``forall``-or-nothing" rule: if a type has an outermost, explicit,
invisible ``forall``, then all of the type variables in the type must be
explicitly quantified. These two examples illustrate how the rule works: ::

  f  :: forall a b. a -> b -> b         -- OK, `a` and `b` are explicitly bound
  g  :: forall a. a -> forall b. b -> b -- OK, `a` and `b` are explicitly bound
  h  :: forall a. a -> b -> b           -- Rejected, `b` is not in scope

The type signatures for ``f``, ``g``, and ``h`` all begin with an outermost
invisible ``forall``, so every type variable in these signatures must be
explicitly bound by a ``forall``. Both ``f`` and ``g`` obey the
``forall``-or-nothing rule, since they explicitly quantify ``a`` and ``b``. On
the other hand, ``h`` does not explicitly quantify ``b``, so GHC will reject
its type signature for being improperly scoped.

In places where the ``forall``-or-nothing rule takes effect, if a type does
*not* have an outermost invisible ``forall``, then any type variables that are
not explicitly bound by a ``forall`` become implicitly quantified. For example: ::

  i :: a -> b -> b             -- `a` and `b` are implicitly quantified
  j :: a -> forall b. b -> b   -- `a` is implicitly quantified
  k :: (forall a. a -> b -> b) -- `b` is implicitly quantified
  type L :: forall a -> b -> b -- `b` is implicitly quantified

GHC will accept ``i``, ``j``, and ``k``'s type signatures, as well as ``L``'s
kind signature. Note that:

- ``j``'s signature is accepted despite its mixture of implicit and explicit
  quantification. As long as a ``forall`` is not an outermost one, it is fine
  to use it among implicitly bound type variables.
- ``k``'s signature is accepted because the outermost parentheses imply that
  the ``forall`` is not an outermost ``forall``. The ``forall``-or-nothing
  rule is one of the few places in GHC where the presence or absence of
  parentheses can be semantically significant!
- ``L``'s signature begins with an outermost ``forall``, but it is a *visible*
  ``forall``, not an invisible ``forall``, and therefore does not trigger the
  ``forall``-or-nothing rule.

The ``forall``-or-nothing rule takes effect in the following places:

- Type signature declarations for functions, values, and class methods
- Expression type annotations
- Instance declarations
- :ref:`class-default-signatures`
- Type signatures in a :ref:`specialize-pragma` or
  :ref:`specialize-instance-pragma`
- :ref:`standalone-kind-signatures`
- Type signatures for :ref:`gadt` constructors
- Type signatures for :ref:`pattern-synonyms`
- :ref:`data-instance-declarations`, :ref:`type-instance-declarations`,
  :ref:`closed-type-families`, and :ref:`assoc-inst`
- :ref:`rewrite-rules` in which the type variables are explicitly quantified

Notes:

- :ref:`pattern-type-sigs` are a notable example of a place where
  types do *not* obey the ``forall``-or-nothing rule. For example, GHC will
  accept the following: ::

    f (g :: forall a. a -> b) x = g x :: b

  Furthermore, :ref:`rewrite-rules` do not obey the ``forall``-or-nothing rule
  when their type variables are not explicitly quantified: ::

    {-# RULES "f" forall (g :: forall a. a -> b) x. f g x = g x :: b #-}

- GADT constructors are extra particular about their ``forall``\ s. In addition
  to adhering to the ``forall``-or-nothing rule, GADT constructors also forbid
  nested ``forall``\ s. For example, GHC would reject the following GADT: ::

    data T where
      MkT :: (forall a. a -> b -> T)

  Because of the lack of an outermost ``forall`` in the type of ``MkT``, the
  ``b`` would be implicitly quantified. In effect, it would be as if one had
  written ``MkT :: forall b. (forall a. a -> b -> T)``, which contains nested
  ``forall``\ s. See :ref:`formal-gadt-syntax`.

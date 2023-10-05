Equality constraints and Coercible constraint
=============================================

.. _equality-constraints:

Equality constraints
--------------------

A type context can include equality constraints of the form ``t1 ~ t2``,
which denote that the types ``t1`` and ``t2`` need to be the same. In
the presence of type families, whether two types are equal cannot
generally be decided locally. Hence, the contexts of function signatures
may include equality constraints, as in the following example: ::

    sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2

where we require that the element type of ``c1`` and ``c2`` are the
same. In general, the types ``t1`` and ``t2`` of an equality constraint
may be arbitrary monotypes; i.e., they may not contain any quantifiers,
independent of whether higher-rank types are otherwise enabled.

Equality constraints can also appear in class and instance contexts. The
former enable a simple translation of programs using functional
dependencies into programs using family synonyms instead. The general
idea is to rewrite a class declaration of the form ::

    class C a b | a -> b

to ::

    class (F a ~ b) => C a b where
      type F a

That is, we represent every functional dependency (FD) ``a1 .. an -> b``
by an FD type family ``F a1 .. an`` and a superclass context equality
``F a1 .. an ~ b``, essentially giving a name to the functional
dependency. In class instances, we define the type instances of FD
families in accordance with the class head. Method signatures are not
affected by that process.

.. index::
   pair: Type equality constraints; kind heterogeneous

Heterogeneous equality
----------------------

GHC also supports *kind-heterogeneous* equality, which relates two types of
potentially different kinds. Heterogeneous equality is spelled ``~~``. Here
are the kinds of ``~`` and ``~~`` to better understand their difference: ::

  (~)  :: forall k. k -> k -> Constraint
  (~~) :: forall k1 k2. k1 -> k2 -> Constraint

Users will most likely want ``~``, but ``~~`` is available if GHC cannot know,
a priori, that the two types of interest have the same kind. Evidence that
``(a :: k1) ~~ (b :: k2)`` tells GHC both that ``k1`` and ``k2`` are the same
and that ``a`` and ``b`` are the same.

Because ``~`` is the more common equality relation, GHC prints out ``~~`` like
``~`` unless :ghc-flag:`-fprint-equality-relations` is set.

Unlifted heterogeneous equality
-------------------------------

Internal to GHC is yet a third equality relation ``(~#)``. It is heterogeneous
(like ``~~``) and is used only internally. It may appear in error messages
and other output only when :ghc-flag:`-fprint-equality-relations` is enabled.

.. _coercible:

The ``Coercible`` constraint
----------------------------

The constraint ``Coercible t1 t2`` is similar to ``t1 ~ t2``, but
denotes representational equality between ``t1`` and ``t2`` in the sense
of Roles (:ref:`roles`). It is exported by :base-ref:`Data.Coerce.`, which also
contains the documentation. More details and discussion can be found in the
paper
`"Safe Coercions" <https://www.microsoft.com/en-us/research/uploads/prod/2018/05/coercible-JFP.pdf>`__.



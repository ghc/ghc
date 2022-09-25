Type-level data declarations
============================

.. extension:: TypeData
    :shortdesc: Enable type data declarations.

    :since: 9.6.1

    Allow ``type data`` declarations, which define constructors at the type level.

This extension facilitates type-level (compile-time) programming by
allowing a type-level counterpart of ``data`` declarations, such as this
definition of type-level natural numbers: ::

    type data Nat = Zero | Succ Nat

This is similar to the corresponding ``data`` declaration, except that
the constructors ``Zero`` and ``Succ`` it introduces belong to the type
constructor namespace, so they can be used in types, such as the type
of length-indexed vectors: ::

    data Vec :: Type -> Nat -> Type where
      Nil  :: Vec a Zero
      Cons :: a -> Vec a n -> Vec a (Succ n)

:extension:`TypeData` is a more fine-grained alternative to the
:extension:`DataKinds` extension, which defines *all* the constructors
in *all* ``data`` declarations as both data constructors and type
constructors.

A ``type data`` declaration has the same syntax as a ``data`` declaration,
either an ordinary algebraic data type or a GADT, prefixed with the keyword
``type``, except that it may not contain
a datatype context (even with :extension:`DatatypeContexts`),
labelled fields,
strictness flags, or
a ``deriving`` clause.

The only constraints permitted in the types of constructors are
equality constraints, e.g.: ::

    type data P :: Type -> Type -> Type where
      MkP :: (a ~ Natural, b ~~ Char) => P a b

Because ``type data`` declarations introduce type constructors, they do
not permit constructors with the same names as types, so the following
declaration is invalid: ::

    type data T = T     // Invalid

The compiler will reject this declaration, because the type constructor
``T`` is defined twice (as the datatype being defined and as a type
constructor).

The main type constructor of a ``type data`` declaration can be defined
recursively, as in the ``Nat`` example above, but its constructors may not
be used in types within the same mutually recursive group of declarations,
so the following is forbidden: ::

    type data T f = K (f (K Int))  // Invalid

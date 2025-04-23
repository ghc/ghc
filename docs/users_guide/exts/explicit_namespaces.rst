.. _explicit-namespaces:

Explicit namespaces in import/export
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: ExplicitNamespaces
    :shortdesc: Allow use of the keyword ``type`` to specify the namespace of
        entries in imports and exports.

    :implied by: :extension:`TypeOperators`, :extension:`TypeFamilies`
    :since: 7.6.1
    :status: Included in :extension:`GHC2024`

    Enable use of explicit namespaces in module export lists, patterns, and expressions.

In an import or export list, such as ::

      module M( f, (++) ) where ...
        import N( f, (++) )
        ...

the entities ``f`` and ``(++)`` are *values*. However, with type
operators (:ref:`type-operators`) it becomes possible to declare
``(++)`` as a *type constructor*. In that case, how would you export or
import it?

The :extension:`ExplicitNamespaces` extension allows you to prefix the name of
a type constructor in an import or export list with "``type``" to
disambiguate this case, thus: ::

      module M( f, type (++) ) where ...
        import N( f, type (++) )
        ...
      module N( f, type (++) ) where
        data family a ++ b = L a | R b

It is also possible to use the ``type`` namespace specifier in subordinate
import and export lists:
::

      module N (C(type (#))) where
        class C a b where
          type a # b
          (#) :: a -> b -> (a # b)
      module M where
        import N as T (C(type (#)))
        import N as D (C((#)))
        -- (T.#) is the associated type
        -- (D.#) is the class method

The extension :extension:`ExplicitNamespaces` is implied by
:extension:`TypeOperators` and (for some reason) by :extension:`TypeFamilies`.

In addition, with :extension:`PatternSynonyms` you can prefix the name of a
data constructor in an import or export list with the keyword
``pattern``, to allow the import or export of a data constructor without
its parent type constructor (see :ref:`patsyn-impexp`).

Furthermore, :extension:`ExplicitNamespaces` permits the use of the ``type``
keyword in patterns and expressions::

  f (type t) x = ...       -- in a pattern
  r = f (type Integer) 10  -- in an expression

This is used in conjunction with :extension:`RequiredTypeArguments`.

When :extension:`ExplicitNamespaces` is enabled, it is possible to use the
``type`` and ``data`` keywords to specify the namespace of the name used in
a fixity signature or a ``WARNING``/``DEPRECATED`` pragma. This can be useful for disambiguating
between names in different namespaces that may conflict with each other.

Here is an example of using namespace specifiers to set different fixities for
type-level and term-level operators: ::

  type f $ a = f a
  f $ a = f a

  infixl 9 type $ -- type-level $ is left-associative with priority 9
  infixr 0 data $ -- term-level $ is right-associative with priority 0

Similarly, it can be used in pragmas to deprecate only one name in a namespace: ::

  data Solo = MkSolo

  pattern Solo = MkSolo
  {-# DEPRECATED data Solo "Use `MkSolo` instead" #-}

  type family Head xs where
    Head (x : _) = x

  pattern Head x <- (head -> x)

  {-# WARNING in "x-partial" data Head "this is a partial type synonym" #-}

It is considered bad practice to use a fixity signature, ``WARNING`` pragma, or
``DEPRECATED`` pragma for a type-level name without an explicit ``type`` namespace, and
doing so will become an error in a future version of GHC.

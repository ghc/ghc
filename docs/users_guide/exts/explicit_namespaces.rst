.. _explicit-namespaces:

Explicit namespaces in import/export
------------------------------------

.. extension:: ExplicitNamespaces
    :shortdesc: Allow use of the ``type`` and ``data`` keywords to specify the namespace of
        entries in import/export lists and in other contexts.

    :implied by: :extension:`TypeOperators`, :extension:`TypeFamilies`
    :since: 7.6.1
    :status: Included in :extension:`GHC2024`

    Enable use of explicit namespace specifiers ``type`` and ``data``
    in import declarations, module export lists, fixity declarations,
    and warning/deprecation pragmas; as well as the ``type`` namespace
    specifier in expressions and patterns.

The ``type`` keyword in import/export lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Since:** GHC 7.6

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

The ``data`` keyword in import/export lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Since:** GHC 9.14

In an import or export list, such as ::

  module M( T ) where ...
    import N( T )
    ...

the entity ``T`` refers to a *type constructor*, even if there is also a data
constructor or pattern synonym of the same name.

For a concrete example, consider the declaration ``data Proxy t = Proxy``
and the following imports: ::

  import Data.Proxy (Proxy(Proxy))  -- imports both constructors
  import Data.Proxy (Proxy(..))     -- imports both constructors
  import Data.Proxy (Proxy)         -- imports the type constructor only
  import Data.Proxy (type Proxy)    -- imports the type constructor only

However, how would one import only the data constructor? There are two options: ::

  import Data.Proxy (data Proxy)     -- imports the data constructor only
  import Data.Proxy (pattern Proxy)  -- imports the data constructor only

The ``data`` keyword enables the import or export a data constructor without its
parent type constructor.

The ``pattern`` keyword does the same, with only a few differences:

* Required compiler versions and flags

  - ``pattern`` is provided by the :extension:`PatternSynonyms` extension and requires GHC ≥7.8
  - ``data`` is enabled by :extension:`ExplicitNamespaces` and requires GHC ≥9.14

  See :ref:`patsyn-impexp`.

* Restrictions on use

  - ``pattern`` is restricted to top-level imports of pattern synonyms and data
    constructors: ::

      import N (pattern P)      -- ok    (top-level)
      import N (T(pattern P))   -- error (subordinate)
      import N (pattern f)      -- error (term or field selector)

  - ``data`` is also permitted in subordinate import/export lists, and is
    applicable to term declarations (functions and constants) and field
    selectors: ::

      import N (data P)         -- ok  (top-level)
      import N (T(data P))      -- ok  (subordinate)
      import N (data f)         -- ok  (term or field selector)

The ``data`` keyword is preferred over ``pattern`` in import/export lists unless
there is a need to support older GHC versions.

Explicit namespaces in fixity declarations and warning/deprecation pragmas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Since:** GHC 9.10

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

  data Solo a = MkSolo a

  pattern Solo x = MkSolo x
  {-# DEPRECATED data Solo "Use `MkSolo` instead" #-}

  type family Head xs where
    Head (x : _) = x

  pattern Head x <- (head -> x)

  {-# WARNING in "x-partial" data Head "this is a partial type synonym" #-}

It is considered bad practice to use a fixity signature, ``WARNING`` pragma, or
``DEPRECATED`` pragma for a type-level name without an explicit ``type`` namespace, and
doing so will become an error in a future version of GHC.

The ``type`` keyword in expressions and patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Since:** GHC 9.10

Furthermore, :extension:`ExplicitNamespaces` permits the use of the ``type``
keyword in patterns and expressions::

  f (type t) x = ...       -- in a pattern
  r = f (type Integer) 10  -- in an expression

This is used in conjunction with :extension:`RequiredTypeArguments`.

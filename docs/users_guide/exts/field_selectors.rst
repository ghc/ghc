.. _field-selectors:

Field Selectors
------------

.. extension:: FieldSelectors
    :shortdesc: Generate field selector functions.
        Implied by :extension:`Haskell98`

    :implied by: :extension:`Haskell98`
    :since: 9.2.1

Set the visibility of `record field selector functions
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-500003.15.1>__.

``NoFieldSelectors`` negates this feature, and allows uses of field labels that overlap with other names.
Field labels are still usable within record construction, updates and patterns.

Given a data structure

    data Foo = Foo { bar :: Int, baz :: String }

The following will be available:

1. the type constructor ``Foo``
2. the data constructor ``Foo``
3. the fields ``bar`` and ``baz`` for record construction, update, and patterns
4. the two functions ``bar`` and ``baz``, which are ``Foo -> Int`` and ``Foo -> String``

If the language extension ``NoFieldSelectors`` is enabled, items (1), (2), and (3)
will still be generated, but (4) will not.

Wildcard exports will work as before, except for the two functions. Even if
these functions are otherwise defined, the wildcard will not export them.
Exporting the names for record construction now has to be specific to the
record. Without ambiguitiy, previously this was equivalent

.. code-block:: haskell

    module A where (Foo(Foo, bar, baz))
    data Foo = Foo { bar :: Int, baz :: Int }

.. code-block:: haskell

    module B where (Foo(Foo, bar), baz)
    data Foo = Foo { bar :: Int, baz :: Int }

Under ``NoFieldSelectors``, these two export statements are now different. The
first one will export the field ``baz``, but not the function ``baz``, while the
second one will export the function ``baz`` (assuming one is defined), but not
the field ``baz``. Because of this change, writing out all selector functions by
hand is still different, because they all have to be exported separately.

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module Exports where (Foo(Foo, bar, baz))
    data Foo = Foo { bar :: Int, baz :: Int }

    bar (Foo x _) = x
    baz (Foo _ x) = x

is different from

.. code-block:: haskell

    module Exports where (Foo(Foo, bar, baz))
    data Foo = Foo { bar :: Int, baz :: Int }

Because the functions in the first example don't get exported.

Let's take a module ``A`` with a function with the same name as a field, with
the extension enabled:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module A where (Foo(Foo, bar, baz))
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

Which would be equivalent to:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module A where (Foo(..))
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

A second module, ``B``, which does not export the selector ``baz`` of
constructor ``Foo``, but instead exports the toplevel binder ``baz``. The fields
can still be used when exported (as in module ``A``).

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module B where (Foo(Foo, bar), baz)
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

Using ``baz`` as a field when importing ``B`` will fail, because the field
``baz`` is not in scope anymore, because it is not exported by ``B``.

.. code-block:: haskell

    import B
    foo = Foo 23 42
    foo { baz = 1 }

However, it is possible to use the imported variable ``baz``, because ``B`` exports it.

.. code-block:: haskell

    import B
    main = print baz

If you wanted to use both, you'd have to export both explicitly:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module C where (Foo(Foo, bar, baz), baz)
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

Now ``baz`` here assigns the value ``42`` to the field ``baz``.

.. code-block:: haskell

   import C
    foo = Foo 23 1
    foo { baz = baz }

Note that if you have multiple datatypes with the same field name, you need :extension:`DuplicateRecordFields` to disambiguate them.

See also the `Import and export of record fields` section of :ref:`duplicate-record-fields`.

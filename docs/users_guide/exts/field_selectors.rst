.. _field-selectors:

Field selectors
---------------

.. extension:: FieldSelectors
    :shortdesc: Control visibility of field selector functions.

    :since: 9.2.1

    Make `record field selector functions
    <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-500003.15.1>`_
    visible in expressions.

By default, the :extension:`FieldSelectors` extension is enabled, so defining a
record datatype brings a selector function into scope for each field in the
record. :extension:`NoFieldSelectors` negates this feature, making it possible
to:

- declare a top-level binding with the same name as a field, and
- refer to this top-level binding unambiguously in expressions.

Field labels are still usable within record construction, updates and pattern
matching.

For example, given a datatype definition ::

    data Foo = MkFoo { bar :: Int, baz :: String }

The following will be available:

1. the type constructor ``Foo``;
2. the data constructor ``MkFoo``;
3. the fields ``bar`` and ``baz`` for record construction, update, and pattern
   matching; and
4. the selector functions ``bar :: Foo -> Int`` and ``baz :: Foo -> String``.

If the :extension:`NoFieldSelectors` extension is enabled at the datatype
definition site, items (1), (2), and (3) will still be available, but (4) will
not.  Correspondingly, it is permitted to define a top-level binding with the
same name as a field, and using this name in an expression unambiguously refers
to the non-field.  For exmaple, the following is permitted: ::

    data Foo = MkFoo { bar :: Int, baz :: String }
    bar = ()  -- does not conflict with `bar` field
    baz = bar -- unambiguously refers to `bar` the unit value, not the field

If you have multiple datatypes with the same field name, you need to enable
:extension:`DuplicateRecordFields` to allow them to be declared simultaneously.
It is never permitted for a single module to define multiple top-level bindings
with the same name.

The :extension:`DisambiguateRecordFields` extension (implied by
:extension:`DuplicateRecordFields`) is useful in conjunction with
:extension:`NoFieldSelectors`, because it excludes non-fields from consideration
when resolving field names in record construction, update and pattern matching.


Import and export of selector functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under :extension:`FieldSelectors`, these modules are equivalent: ::

    module A (Foo(MkFoo, bar, baz)) where
      data Foo = MkFoo { bar :: Int, baz :: Int }

    module B (Foo(MkFoo, bar), baz) where
      data Foo = MkFoo { bar :: Int, baz :: Int }

Under :extension:`NoFieldSelectors`, these two export statements are now
different. The first one will export the field ``baz``, but not the top-level
binding ``baz``, while the second one would export the top-level binding ``baz``
(if one were defined), but not the field ``baz``.

Because of this change, using :extension:`NoFieldSelectors` and writing out
selector functions explicitly is different to using :extension:`FieldSelectors`:
in the former case the fields and functions must be exported separately.  For
example, here the selector functions are not exported: ::

    {-# LANGUAGE NoFieldSelectors #-}
    module M (Foo(MkFoo, bar, baz)) where
      data Foo = MkFoo { bar :: Int, baz :: Int }

      bar (MkFoo x _) = x
      baz (MkFoo _ x) = x

whereas here the selector functions are exported: ::

    {-# LANGUAGE FieldSelectors #-}
    module M (Foo(MkFoo, bar, baz)) where
      data Foo = MkFoo { bar :: Int, baz :: Int }

Wildcard exports will export the field labels, but will not export a top-level
binding that happens to have the same name.  In the examples above, exporting
``Foo(..)`` is (still) equivalent to exporting ``Foo(MkFoo, bar, baz)``.

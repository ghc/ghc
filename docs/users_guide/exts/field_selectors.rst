.. _field-selectors:

Field Selectors
------------

.. extension:: FieldSelectors
    :shortdesc: Generete field selector functions.
        Implied by :extension:`Haskell98`

    :implied by: :extension:`Haskell98`
    :since: 9.2.1

Set the visibility of `record field selector functions
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-500003.15.1>__.

``NoFieldSelectors`` negates this feature, and allows uses of field names that overlap with other names.
Selectors are still usable within record construction, updates and patterns.

Note that if you have multiple datatypes with the same field name, you need `DuplicateRecordFields` to disambiguate them.
Also, ambigous selectors should be imported/exported along with a constructor, because currently there is no other way to distinguish them.

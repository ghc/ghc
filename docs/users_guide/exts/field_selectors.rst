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

Note that if you have multiple datatypes with the same field name, you need `DuplicateRecordFields` to disambiguate them.

See also the `Import and export of record fields` section of :ref:`duplicate-record-fields`.

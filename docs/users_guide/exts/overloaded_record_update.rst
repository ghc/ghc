.. _overloadedrecordupdate:

Overloaded record update
~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: OverloadedRecordUpdate
    :shortdesc: ``OverloadedRecordUpdate``

    :since: 9.2.0

    Provides record '.' syntax in record updates e.g. ``x{foo.bar = 1}``.

``OverloadedRecordUpdate`` works by desugaring record ``.`` update
expressions to expressions involving the functions ``setField`` and
``getField``.

At this time, ``RebindableSyntax`` must be enabled when
``OverloadedRecordUpdate`` is and users are required to provide
definitions for ``getField`` and ``setField``.

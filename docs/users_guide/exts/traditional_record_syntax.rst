.. _traditional-record-syntax:

Traditional record syntax
-------------------------

.. extension:: NoTraditionalRecordSyntax
    :shortdesc: Disable support for traditional record syntax
        (as supported by Haskell 98) ``C {f = x}``

    :since: 7.4.1

    Disallow use of record syntax.

Traditional record syntax, such as ``C {f = x}``, is enabled by default.
To disable it, you can use the :extension:`NoTraditionalRecordSyntax` extension.



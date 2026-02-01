.. _multiline-strings:

Qualified string literals
-------------------------

.. extension:: QualifiedStrings
    :shortdesc: Enable qualified string literals.

    :since: 9.16.1

    Enable qualified string literals.

The ``-XQualifiedStrings`` extension allows string literals to be qualified using module qualification syntax. An expression of the form ``M."my string"`` thus stands for ``M.fromString "my string"``.

This allows more precise control over the ``fromString`` function than :extension:`OverloadedStrings`, which overloads all string literals for an entire module.

If :extension:`MultilineStrings` is also enabled, ``M."""test"""`` will also work, desugaring the multiline string first, then the qualified string literal.

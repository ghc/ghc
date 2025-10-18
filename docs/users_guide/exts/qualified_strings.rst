.. _multiline-strings:

Qualified string literals
-------------------------

.. extension:: QualifiedStrings
    :shortdesc: Enable qualified string literals.

    :since: 9.16.1

    Enable qualified string literals.

With ``-XOverloadedStrings``, a string literal ``"test"`` will be desugared as ``GHC.Exts.fromString "test"``. This is enabled for an entire module, there's no way to scope the desugaring to a single location. With ``-XQualifiedStrings``, you can explicitly specify where ``fromString`` is loaded from with ``M."test"``, which would desugar to ``M.fromString "test"``.

If ``-XMultilineStrings`` is also enabled, ``M."""test"""`` will also work, desugaring the multiline string first, then the qualified string literal.

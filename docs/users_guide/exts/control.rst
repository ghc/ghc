.. _options-language:

Controlling extensions
----------------------

.. index::
   single: language; option
   single: options; language
   single: extensions; options controlling

Language extensions can be controlled (i.e. allowed or not) in two ways:

-  Every language extension can be switched on by a command-line flag
   "``-X...``" (e.g. ``-XTemplateHaskell``), and switched off by the
   flag "``-XNo...``"; (e.g. ``-XNoTemplateHaskell``).

-  Language extensions can also be enabled using the ``LANGUAGE`` pragma, thus
   ``{-# LANGUAGE TemplateHaskell #-}`` (see :ref:`language-pragma`).

.. extension:: Haskell2010
    :shortdesc: Use the Haskell 2010 language variant.

    Compile Haskell 2010 language variant. Enables the
    following language extensions:

    .. hlist::

     * :extension:`ImplicitPrelude`
     * :extension:`StarIsType`
     * :extension:`CUSKs`
     * :extension:`MonomorphismRestriction`
     * :extension:`DatatypeContexts`
     * :extension:`TraditionalRecordSyntax`
     * :extension:`EmptyDataDecls`
     * :extension:`ForeignFunctionInterface`
     * :extension:`PatternGuards`
     * :extension:`DoAndIfThenElse`
     * :extension:`RelaxedPolyRec`


.. extension:: Haskell98
    :shortdesc: Use the Haskell 98 language variant.

    Compile using Haskell 98 language variant. Enables the
    following language extensions:

    .. hlist::

     * :extension:`ImplicitPrelude`
     * :extension:`StarIsType`
     * :extension:`CUSKs`
     * :extension:`MonomorphismRestriction`
     * :extension:`NPlusKPatterns`
     * :extension:`DatatypeContexts`
     * :extension:`TraditionalRecordSyntax`
     * :extension:`NondecreasingIndentation`



Although not recommended, the deprecated :ghc-flag:`-fglasgow-exts` flag enables
a large swath of the extensions supported by GHC at once.

.. ghc-flag:: -fglasgow-exts
    :shortdesc: Deprecated. Enable most language extensions;
        see :ref:`options-language` for exactly which ones.
    :type: dynamic
    :reverse: -fno-glasgow-exts
    :category: misc

    The flag ``-fglasgow-exts`` is equivalent to enabling the following extensions:

    .. include:: ../what_glasgow_exts_does.rst

    Enabling these options is the *only* effect of ``-fglasgow-exts``. We are trying
    to move away from this portmanteau flag, and towards enabling features
    individually.



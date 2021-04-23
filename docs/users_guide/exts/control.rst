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

.. extension:: GHC2021
    :shortdesc: Use GHC’s set of default language extensions from 2021

    GHC blesses a number of extensions, beyond Haskell 2010, to be suitable to
    turned on by default. These extensions are considered to be stable and
    conservative.

    ``GHC2021`` is used by GHC if neither ``Haskell98`` nor ``Haskell2010`` is
    turned on explicitly. Since later versions of GHC may use a later
    ``GHC20xx`` by default, users are advised to declare the language set
    explicitly with ``-XGHC2021``.

    Note that, because GHC2021 includes a number of non-standardized
    extensions, the stability guarantees it provides are not quite as strong as
    those provided by, e.g., :extension:`Haskell2010`. While GHC does take
    pains to avoid changing the semantics of these extensions, changes may
    still happen (e.g. the simplified subsumption change introduced in GHC 9.0
    which caused GHC to reject some programs using :extension:`RankNTypes`).

    The ``GHC2021`` language set comprises the following extensions:

    .. hlist::

     * :extension:`BangPatterns`
     * :extension:`BinaryLiterals`
     * :extension:`ConstrainedClassMethods`
     * :extension:`ConstraintKinds`
     * :extension:`DeriveDataTypeable`
     * :extension:`DeriveFoldable`
     * :extension:`DeriveFunctor`
     * :extension:`DeriveGeneric`
     * :extension:`DeriveLift`
     * :extension:`DeriveTraversable`
     * :extension:`DoAndIfThenElse`
     * :extension:`EmptyCase`
     * :extension:`EmptyDataDecls`
     * :extension:`EmptyDataDeriving`
     * :extension:`ExistentialQuantification`
     * :extension:`ExplicitForAll`
     * :extension:`FieldSelectors`
     * :extension:`FlexibleContexts`
     * :extension:`FlexibleInstances`
     * :extension:`ForeignFunctionInterface`
     * :extension:`GADTSyntax`
     * :extension:`GeneralisedNewtypeDeriving`
     * :extension:`HexFloatLiterals`
     * :extension:`ImplicitPrelude`
     * :extension:`ImportQualifiedPost`
     * :extension:`InstanceSigs`
     * :extension:`KindSignatures`
     * :extension:`MonomorphismRestriction`
     * :extension:`MultiParamTypeClasses`
     * :extension:`NamedFieldPuns`
     * :extension:`NamedWildCards`
     * :extension:`NumericUnderscores`
     * :extension:`PatternGuards`
     * :extension:`PolyKinds`
     * :extension:`PostfixOperators`
     * :extension:`RankNTypes`
     * :extension:`RelaxedPolyRec`
     * :extension:`ScopedTypeVariables`
     * :extension:`StandaloneDeriving`
     * :extension:`StandaloneKindSignatures`
     * :extension:`StarIsType`
     * :extension:`TraditionalRecordSyntax`
     * :extension:`TupleSections`
     * :extension:`TypeApplications`
     * :extension:`TypeOperators`
     * :extension:`TypeSynonymInstances`


.. extension:: Haskell2010
    :shortdesc: Use the Haskell 2010 language variant.

    Compile Haskell 2010 language variant. Enables the
    following language extensions:

    .. hlist::

     * :extension:`CUSKs`
     * :extension:`DatatypeContexts`
     * :extension:`DoAndIfThenElse`
     * :extension:`EmptyDataDecls`
     * :extension:`FieldSelectors`
     * :extension:`ForeignFunctionInterface`
     * :extension:`ImplicitPrelude`
     * :extension:`MonomorphismRestriction`
     * :extension:`PatternGuards`
     * :extension:`RelaxedPolyRec`
     * :extension:`StarIsType`
     * :extension:`TraditionalRecordSyntax`


.. extension:: Haskell98
    :shortdesc: Use the Haskell 98 language variant.

    Compile using Haskell 98 language variant. Enables the
    following language extensions:

    .. hlist::

     * :extension:`CUSKs`
     * :extension:`DatatypeContexts`
     * :extension:`FieldSelectors`
     * :extension:`ImplicitPrelude`
     * :extension:`MonomorphismRestriction`
     * :extension:`NPlusKPatterns`
     * :extension:`NondecreasingIndentation`
     * :extension:`StarIsType`
     * :extension:`TraditionalRecordSyntax`



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



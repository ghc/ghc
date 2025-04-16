.. _options-language:

Controlling editions and extensions
-----------------------------------

.. index::
   single: language; option
   single: options; language
   single: extensions; options controlling
   single: editions; language

GHC supports multiple language editions: :extension:`Haskell98`,
:extension:`Haskell2010`, :extension:`GHC2021` and :extension:`GHC2024`.  Each
language edition consists of a collection of language extensions, and there are
many other language extensions not currently part of a language edition but that
can be enabled explicitly.

Currently, :extension:`GHC2021` is used by default if no other language edition
is explicitly requested, for backwards compatibility purposes. Since later
versions of GHC may use a different language edition by default, users are
advised to declare a language edition explicitly.  Using :extension:`GHC2024` is
recommended for new code.

A language edition can be selected:

-  at the package level, e.g. using ``default-language: GHC2024`` in a
   ``.cabal`` file;

-  with a command-line flag prefixed by "``-X...``" (e.g. ``-XGHC2024``); or

-  for an individual module using the :pragma:`LANGUAGE` pragma, e.g.
   ``{-# LANGUAGE GHC2024 #-}``.

Selecting a language edition overrides any previous selection. It is not
possible to disable a language edition.

Similarly, language extensions can be controlled (either enabled or disabled):

-  at the package level, e.g. using ``default-extensions: TemplateHaskell`` in a
   ``.cabal`` file;

-  with command-line flags, switched on by a command-line flag
   "``-X...``" (e.g. ``-XTemplateHaskell``), and switched off by the
   flag "``-XNo...``"; (e.g. ``-XNoTemplateHaskell``);

-  for an individual module using the :pragma:`LANGUAGE` pragma, e.g.
   ``{-# LANGUAGE TemplateHaskell #-}`` or ``{-# LANGUAGE NoTemplateHaskell #-}``.

.. extension:: GHC2024
    :shortdesc: Use GHC’s set of default language extensions from 2024

    :since: 9.10.1

    GHC blesses a number of extensions, beyond Haskell 2010, to be suitable to
    turned on by default. These extensions are considered to be stable and
    conservative.

    Note that, because GHC2024 includes a number of non-standardized
    extensions, the stability guarantees it provides are not quite as strong as
    those provided by, e.g., :extension:`Haskell2010`. While GHC does take
    pains to avoid changing the semantics of these extensions, changes may
    still happen (e.g. the simplified subsumption change introduced in GHC 9.0
    which caused GHC to reject some programs using :extension:`RankNTypes`).

    The ``GHC2024`` language edition includes the following extensions:

    .. hlist::

     * :extension:`BangPatterns`
     * :extension:`BinaryLiterals`
     * :extension:`ConstrainedClassMethods`
     * :extension:`ConstraintKinds`
     * :extension:`DataKinds`
     * :extension:`DeriveDataTypeable`
     * :extension:`DeriveFoldable`
     * :extension:`DeriveFunctor`
     * :extension:`DeriveGeneric`
     * :extension:`DeriveLift`
     * :extension:`DeriveTraversable`
     * :extension:`DerivingStrategies`
     * :extension:`DisambiguateRecordFields`
     * :extension:`DoAndIfThenElse`
     * :extension:`EmptyCase`
     * :extension:`EmptyDataDecls`
     * :extension:`EmptyDataDeriving`
     * :extension:`ExistentialQuantification`
     * :extension:`ExplicitForAll`
     * :extension:`ExplicitNamespaces`
     * :extension:`FieldSelectors`
     * :extension:`FlexibleContexts`
     * :extension:`FlexibleInstances`
     * :extension:`ForeignFunctionInterface`
     * :extension:`GADTs`
     * :extension:`GADTSyntax`
     * :extension:`GeneralisedNewtypeDeriving`
     * :extension:`HexFloatLiterals`
     * :extension:`ImplicitPrelude`
     * :extension:`ImportQualifiedPost`
     * :extension:`InstanceSigs`
     * :extension:`KindSignatures`
     * :extension:`LambdaCase`
     * :extension:`MonoLocalBinds`
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
     * :extension:`RoleAnnotations`
     * :extension:`ScopedTypeVariables`
     * :extension:`StandaloneDeriving`
     * :extension:`StandaloneKindSignatures`
     * :extension:`StarIsType`
     * :extension:`TraditionalRecordSyntax`
     * :extension:`TupleSections`
     * :extension:`TypeApplications`
     * :extension:`TypeOperators`
     * :extension:`TypeSynonymInstances`
     * :extension:`ImplicitStagePersistence`

.. extension:: GHC2021
    :shortdesc: Use GHC’s set of default language extensions from 2021

    :since: 9.2.1

    See :extension:`GHC2024` for general comments about ``GHC20xx`` language
    editions.

    Also note that due to a `minor oversight
    <https://github.com/ghc-proposals/ghc-proposals/issues/551>`_, enabling
    this edition behaves slightly differently than enabling each of its
    constituent extensions. Specifically, while :extension:`TypeOperators` implies
    :extension:`ExplicitNamespaces`, :extension:`ExplicitNamespaces` is not included
    in :extension:`GHC2021`. Moreover, while :extension:`GADTs` is not part of
    :extension:`GHC2021`, the combination of :extension:`GADTSyntax` and
    :extension:`ExistentialQuantification` is enough to define and use GADTs.

    The ``GHC2021`` language edition includes the following extensions:

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
     * :extension:`NoExplicitNamespaces <ExplicitNamespaces>`
     * :extension:`ImplicitStagePersistence`


.. extension:: Haskell2010
    :shortdesc: Use the Haskell 2010 language edition.

    Compile using the Haskell 2010 language edition, as specified by the
    `Haskell 2010 report <https://www.haskell.org/onlinereport/haskell2010/>`_.
    GHC aims to behave mostly as a Haskell 2010 compiler, but there are a few
    known deviations from the standard (see :ref:`vs-Haskell-defn`).

    The ``Haskell2010`` language edition includes the following language extensions:

    .. hlist::

     * :extension:`CUSKs`
     * :extension:`DatatypeContexts`
     * :extension:`DeepSubsumption`
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
     * :extension:`ImplicitStagePersistence`


.. extension:: Haskell98
    :shortdesc: Use the Haskell 98 language edition.

    Compile using the Haskell 98 language edition, as specified by the `Haskell
    98 report <https://www.haskell.org/onlinereport/>`_.  GHC aims to behave
    mostly as a Haskell 98 compiler, but there are a few known deviations from
    the standard (see :ref:`vs-Haskell-defn`).

    The ``Haskell98`` language edition includes the following language extensions:

    .. hlist::

     * :extension:`CUSKs`
     * :extension:`DatatypeContexts`
     * :extension:`DeepSubsumption`
     * :extension:`FieldSelectors`
     * :extension:`ImplicitPrelude`
     * :extension:`MonomorphismRestriction`
     * :extension:`NPlusKPatterns`
     * :extension:`NondecreasingIndentation`
     * :extension:`StarIsType`
     * :extension:`TraditionalRecordSyntax`
     * :extension:`ImplicitStagePersistence`



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



.. _options-sanity:

Warnings and sanity-checking
----------------------------

.. index::
   single: sanity-checking options
   single: warnings

GHC has a number of options that select which types of non-fatal error
messages, otherwise known as warnings, can be generated during compilation.
Some options control individual warnings and others control collections
of warnings.
Use ``-W⟨wflag⟩`` to turn on an individual warning or a collection, or use
``-Wno-⟨wflag⟩`` to turn it off.
Use ``-Werror`` to make all warnings into fatal errors, or ``-Werror=⟨wflag⟩`` to
make a specific warning into an error. Reverse this with ``-Wwarn`` to make all
warnings non-fatal, or ``-Wwarn=⟨wflag⟩`` to make a specific warning non-fatal.

.. note::
   In GHC < 8 the syntax for ``-W⟨wflag⟩`` was ``-fwarn-⟨wflag⟩``
   (e.g. ``-fwarn-incomplete-patterns``).
   This spelling is deprecated, but still accepted for backwards compatibility.
   Likewise, ``-Wno-⟨wflag⟩`` used to be ``fno-warn-⟨wflag⟩``
   (e.g. ``-fno-warn-incomplete-patterns``).


Warning groups
==============

The following flags are simple ways to select standard "packages" of
warnings. They can be reversed using ``-Wno-⟨group⟩``, which has the same effect
as ``-Wno-...`` for every individual warning in the group.

.. ghc-flag:: -Wdefault
    :shortdesc: enable default flags
    :type: dynamic
    :reverse: -Wno-default
    :category:

    :since: 8.0

    By default, you get a standard set of warnings which are
    generally likely to indicate bugs in your program. These are:

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Woverlapping-patterns`
        * :ghc-flag:`-Wwarnings-deprecations`
        * :ghc-flag:`-Wdeprecations`
        * :ghc-flag:`-Wdeprecated-flags`
        * :ghc-flag:`-Wunrecognised-pragmas`
        * :ghc-flag:`-Wduplicate-exports`
        * :ghc-flag:`-Wderiving-defaults`
        * :ghc-flag:`-Woverflowed-literals`
        * :ghc-flag:`-Wempty-enumerations`
        * :ghc-flag:`-Wmissing-fields`
        * :ghc-flag:`-Wmissing-methods`
        * :ghc-flag:`-Wwrong-do-bind`
        * :ghc-flag:`-Wsimplifiable-class-constraints`
        * :ghc-flag:`-Wtyped-holes`
        * :ghc-flag:`-Wdeferred-type-errors`
        * :ghc-flag:`-Wpartial-type-signatures`
        * :ghc-flag:`-Wunsupported-calling-conventions`
        * :ghc-flag:`-Wdodgy-foreign-imports`
        * :ghc-flag:`-Winline-rule-shadowing`
        * :ghc-flag:`-Wunsupported-llvm-version`
        * :ghc-flag:`-Wmissed-extra-shared-lib`
        * :ghc-flag:`-Wtabs`
        * :ghc-flag:`-Wunrecognised-warning-flags`
        * :ghc-flag:`-Winaccessible-code`
        * :ghc-flag:`-Wstar-binder`
        * :ghc-flag:`-Wstar-is-type`
        * :ghc-flag:`-Woperator-whitespace-ext-conflict`
        * :ghc-flag:`-Wambiguous-fields`
        * :ghc-flag:`-Wunicode-bidirectional-format-characters`
        * :ghc-flag:`-Wgadt-mono-local-binds`
        * :ghc-flag:`-Wtype-equality-requires-operators`
        * :ghc-flag:`-Wtype-equality-out-of-scope`
        * :ghc-flag:`-Wbadly-staged-types`
        * :ghc-flag:`-Winconsistent-flags`
        * :ghc-flag:`-Wnoncanonical-monoid-instances`
        * :ghc-flag:`-Wnoncanonical-monad-instances`
        * :ghc-flag:`-Wdata-kinds-tc`

.. ghc-flag:: -W
    :shortdesc: enable normal warnings
    :type: dynamic
    :reverse: -Wno-extra
    :category:

    Provides the standard warnings plus

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Wunused-binds`
        * :ghc-flag:`-Wunused-matches`
        * :ghc-flag:`-Wunused-foralls`
        * :ghc-flag:`-Wunused-imports`
        * :ghc-flag:`-Wincomplete-patterns`
        * :ghc-flag:`-Wdodgy-exports`
        * :ghc-flag:`-Wdodgy-imports`
        * :ghc-flag:`-Wunbanged-strict-patterns`

.. ghc-flag:: -Wextra
    :shortdesc: alias for :ghc-flag:`-W`
    :type: dynamic
    :reverse: -Wno-extra

    Alias for :ghc-flag:`-W`

.. ghc-flag:: -Wall
    :shortdesc: enable almost all warnings (details in :ref:`options-sanity`)
    :type: dynamic
    :reverse: -Wno-all
    :category:

    Turns on all warning options that indicate potentially suspicious
    code. They include all  warnings in :ghc-flag:`-Wextra`, plus:

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Whi-shadowing`
        * :ghc-flag:`-Wincomplete-record-updates`
        * :ghc-flag:`-Wincomplete-record-selectors`
        * :ghc-flag:`-Wincomplete-uni-patterns`
        * :ghc-flag:`-Wmissing-pattern-synonym-signatures`
        * :ghc-flag:`-Wmissing-signatures`
        * :ghc-flag:`-Wname-shadowing`
        * :ghc-flag:`-Worphans`
        * :ghc-flag:`-Wredundant-record-wildcards`
        * :ghc-flag:`-Wstar-is-type`
        * :ghc-flag:`-Wtrustworthy-safe`
        * :ghc-flag:`-Wtype-defaults`
        * :ghc-flag:`-Wunused-do-bind`
        * :ghc-flag:`-Wunused-record-wildcards`
        * :ghc-flag:`-Wincomplete-export-warnings`
        * :ghc-flag:`-Wderiving-typeable`

.. ghc-flag:: -Weverything
    :shortdesc: enable all warnings supported by GHC
    :type: dynamic
    :reverse: -w
    :category:

    :since: 8.0

    Turns on every single warning supported by the compiler.

.. ghc-flag:: -Wcompat
    :shortdesc: enable future compatibility warnings
        (details in :ref:`options-sanity`)
    :type: dynamic
    :reverse: -Wno-compat
    :category:

    :since: 8.0

    Turns on warnings that will be enabled by default in the future, but remain
    off in normal compilations for the time being. This allows library authors
    eager to make their code future compatible to adapt to new features before
    they even generate warnings.

    This currently enables

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Wcompat-unqualified-imports`
        * :ghc-flag:`-Wimplicit-rhs-quantification`
        * :ghc-flag:`-Wdeprecated-type-abstractions`

.. ghc-flag:: -w
    :shortdesc: disable all warnings
    :type: dynamic
    :category:

    Turns off all warnings, including the standard ones and those that
    :ghc-flag:`-Wall` doesn't enable.

.. ghc-flag:: -Wnot
    :shortdesc: *(deprecated)* Alias for :ghc-flag:`-w`
    :type: dynamic

    Deprecated alias for :ghc-flag:`-w`

When a warning is emitted, the specific warning flag which controls
it is shown, but the group can optionally be shown as well:

.. ghc-flag:: -fshow-warning-groups
    :shortdesc: show which group an emitted warning belongs to.
    :type: dynamic
    :reverse: -fno-show-warning-groups
    :category:

    :default: off

    When showing which flag controls a warning, also show the
    respective warning group flag(s) that warning is contained in.


Treating warnings as fatal errors
=================================

These options control which warnings are considered fatal and cause compilation
to abort.

.. ghc-flag:: -Werror
    :shortdesc: make warnings fatal
    :type: dynamic
    :reverse: -Wwarn
    :category:

    :since: 6.8 (``-Wwarn``)

    Makes any warning into a fatal error. Useful so that you don't miss
    warnings when doing batch compilation. To reverse ``-Werror`` and stop
    treating any warnings as errors use ``-Wwarn``, or use ``-Wwarn=⟨wflag⟩``
    to stop treating specific warnings as errors.

.. ghc-flag:: -Werror=⟨wflag⟩
    :shortdesc: make a specific warning fatal
    :type: dynamic
    :reverse: -Wwarn=⟨wflag⟩
    :category:
    :noindex:

    :implies: ``-W⟨wflag⟩``

    Makes a specific warning into a fatal error. The warning will be enabled if
    it hasn't been enabled yet. Can be reversed with ``-Wwarn=⟨wflag⟩``.

    ``-Werror=⟨group⟩`` has the same effect as ``-Werror=...`` for each warning
    flag in the group (for example, ``-Werror=compat`` will turn every warning
    in the :ghc-flag:`-Wcompat` group into a fatal error).

.. ghc-flag:: -Wwarn
    :shortdesc: make warnings non-fatal
    :type: dynamic
    :reverse: -Werror
    :category:

    Warnings are treated only as warnings, not as errors. This is the
    default, but can be useful to negate a :ghc-flag:`-Werror` flag.

.. ghc-flag:: -Wwarn=⟨wflag⟩
    :shortdesc: make a specific warning non-fatal
    :type: dynamic
    :reverse: -Werror=⟨wflag⟩
    :category:
    :noindex:

    Causes a specific warning to be treated as normal warning, not fatal error.

    Note that it doesn't fully negate the effects of ``-Werror=⟨wflag⟩`` - the
    warning will still be enabled.

    ``-Wwarn=⟨group⟩`` has the same effect as ``-Wwarn=...`` for each warning
    flag in the group (for example, ``-Wwarn=compat`` will mark every warning in
    the :ghc-flag:`-Wcompat` group as non-fatal).

.. ghc-flag:: -Wno-error=⟨wflag⟩
    :shortdesc: make a specific warning non-fatal
    :type: dynamic
    :reverse: -Werror=⟨wflag⟩
    :category:
    :noindex:

    Alternative spelling for ``-Wwarn=⟨wflag⟩``.


Individual warning options
==========================

The full set of warning options is described below. To turn off any
warning, simply give the corresponding ``-Wno-...`` option on the
command line. For backwards compatibility with GHC versions prior to 8.0,
all these warnings can still be controlled with ``-f(no-)warn-*`` instead
of ``-W(no-)*``.

.. ghc-flag:: -Wunrecognised-warning-flags
    :shortdesc: throw a warning when an unrecognised ``-W...`` flag is
        encountered on the command line.
    :type: dynamic
    :reverse: -Wno-unrecognised-warning-flags
    :category:

    :since: 8.0

    :default: on

    Enables warnings when the compiler encounters a ``-W...`` flag that is not
    recognised.

.. ghc-flag:: -Wcompat-unqualified-imports
    :shortdesc: Report unqualified imports of core libraries which are expected
      to cause compatibility problems in future releases.
    :type: dynamic
    :reverse: -Wno-compat-unqualified-imports
    :category:

    :since: 8.10

    Warns on unqualified imports of core library modules which are subject to
    change in future GHC releases. Currently the following modules are covered
    by this warning:

     - ``Data.List`` due to the future addition of ``Data.List.singleton`` and
       specialisation of exports to the ``[]`` type. See the
       `mailing list <https://groups.google.com/forum/#!topic/haskell-core-libraries/q3zHLmzBa5E>`_
       for details.

    This warning can be addressed by either adding an explicit import list or
    using a ``qualified`` import.

.. ghc-flag:: -Wprepositive-qualified-module
    :shortdesc: Report imports with a leading/prepositive "qualified"
    :type: dynamic
    :reverse: -Wno-prepositive-qualified-module
    :category:

    :since: 8.10

    Normally, imports are qualified prepositively: ``import qualified M``.
    By using :extension:`ImportQualifiedPost`, the qualified keyword can be used after the module name.
    Like so: ``import M qualified``. This will warn when the first, prepositive syntax is used.

.. ghc-flag:: -Wtyped-holes
    :shortdesc: Report warnings when :ref:`typed hole <typed-holes>` errors are
        :ref:`deferred until runtime <defer-type-errors>`. See
        :ghc-flag:`-fdefer-typed-holes`.
    :type: dynamic
    :reverse: -Wno-typed-holes
    :category:

    :since: 7.8

    :default: on

    Determines whether the compiler reports typed holes warnings. Has no
    effect unless typed holes errors are deferred until runtime. See
    :ref:`typed-holes` and :ref:`defer-type-errors`.

.. ghc-flag:: -Wdeferred-type-errors
    :shortdesc: Report warnings when :ref:`deferred type errors
        <defer-type-errors>` are enabled. This option is enabled by
        default. See :ghc-flag:`-fdefer-type-errors`.
    :type: dynamic
    :reverse: -Wno-deferred-type-errors
    :category:

    :since: 8.0

    :default: on

    Causes a warning to be reported when a type error is deferred until
    runtime. See :ref:`defer-type-errors`.

.. ghc-flag:: -Wdeferred-out-of-scope-variables
    :shortdesc: Report warnings when variable out-of-scope errors are
        :ref:`deferred until runtime <defer-type-errors>`.
        See :ghc-flag:`-fdefer-out-of-scope-variables`.
    :type: dynamic
    :reverse: -Wno-deferred-out-of-scope-variables
    :category:

    :since: 8.0

    Warn when a deferred out-of-scope variable is encountered.
    See :ref:`defer-type-errors`.

.. ghc-flag:: -Wpartial-type-signatures
    :shortdesc: warn about holes in partial type signatures when
        :extension:`PartialTypeSignatures` is enabled. Not applicable when
        :extension:`PartialTypeSignatures` is not enabled, in which case
        errors are generated for such holes.
    :type: dynamic
    :reverse: -Wno-partial-type-signatures
    :category:

    :since: 7.10

    :default: on

    Determines whether the compiler reports holes in partial type
    signatures as warnings. Has no effect unless
    :extension:`PartialTypeSignatures` is enabled, which controls whether
    errors should be generated for holes in types or not. See
    :ref:`partial-type-signatures`.

.. ghc-flag:: -fhelpful-errors
    :shortdesc: Make suggestions for mis-spelled names.
    :type: dynamic
    :reverse: -fno-helpful-errors
    :category:

    :since: 7.4

    :default: on

    When a name or package is not found in scope, make suggestions for
    the name or package you might have meant instead.

.. ghc-flag:: -Wunrecognised-pragmas
    :shortdesc: warn about uses of pragmas that GHC doesn't recognise
    :type: dynamic
    :reverse: -Wno-unrecognised-pragmas
    :category:

    :since: 6.10

    :default: on

    Causes a warning to be emitted when a pragma that GHC doesn't
    recognise is used. As well as pragmas that GHC itself uses, GHC also
    recognises pragmas known to be used by other tools, e.g.
    ``OPTIONS_HUGS`` and ``DERIVE``.

.. ghc-flag:: -Wmisplaced-pragmas
    :shortdesc: warn about uses of file header pragmas in the module body
    :type: dynamic
    :reverse: -Wno-misplaced-pragmas
    :category:

    :since: 9.4

    :default: on

    Warn when a pragma that should only appear in the header of a module,
    such as a `LANGUAGE` or `OPTIONS_GHC` pragma, appears in the body of
    the module instead.

.. ghc-flag:: -Wmissed-specialisations
    :shortdesc: warn when specialisation of an imported, overloaded function
        fails.
    :type: dynamic
    :reverse: -Wno-missed-specialisations
    :category:

    :since: 8.0

    :default: off

    Emits a warning if GHC cannot specialise an overloaded function, usually
    because the function needs an ``INLINABLE`` pragma. Reports when the
    situation arises during specialisation of an imported function.

    This form is intended to catch cases where an imported function
    that is marked as ``INLINABLE`` (presumably to enable specialisation)
    cannot be specialised as it calls other functions that are themselves not
    specialised.

    Note that this warning will not throw errors if used with
    :ghc-flag:`-Werror`.

.. ghc-flag:: -Wmissed-specializations
    :shortdesc: alias for :ghc-flag:`-Wmissed-specialisations`
    :type: dynamic
    :reverse: -Wno-missed-specializations

    Alias for :ghc-flag:`-Wmissed-specialisations`

.. ghc-flag:: -Wall-missed-specialisations
    :shortdesc: warn when specialisation of any overloaded function fails.
    :type: dynamic
    :reverse: -Wno-all-missed-specialisations
    :category:

    :since: 8.0

    :default: off

    Emits a warning if GHC cannot specialise an overloaded function, usually
    because the function needs an ``INLINABLE`` pragma. Reports
    all such situations.

    Note that this warning will not throw errors if used with
    :ghc-flag:`-Werror`.

.. ghc-flag:: -Wall-missed-specializations
    :shortdesc: alias for :ghc-flag:`-Wall-missed-specialisations`
    :type: dynamic
    :reverse: -Wno-all-missed-specializations

    Alias for :ghc-flag:`-Wall-missed-specialisations`

.. ghc-flag:: -Wextended-warnings
    :shortdesc: warn about uses of functions & types that have WARNING or
        DEPRECATED pragmas, across all categories
    :type: dynamic
    :reverse: -Wno-extended-warnings
    :category:

    :since: 9.8.1

    :default: on

    .. index::
       pair: deprecations; warnings

    Causes a warning to be emitted when a module, function or type with a
    ``WARNING`` or ``DEPRECATED`` pragma is used, regardless of the category
    which may be associated with the pragma. See
    :ref:`warning-deprecated-pragma` for more details on the pragmas.  This
    implies :ghc-flag:`-Wdeprecations` and all ``-Wx-⟨category⟩`` flags.

.. ghc-flag:: -Wx-⟨category⟩
    :shortdesc: warn about uses of functions & types that have WARNING pragmas
        with the given category
    :type: dynamic
    :reverse: -Wno-x-⟨category⟩
    :category:

    :since: 9.8.1

    :default: on

    .. index::
       pair: deprecations; warnings

    Causes a warning to be emitted when a module, function or type with a
    ``WARNING in "x-⟨category⟩"`` pragma is used. See
    :ref:`warning-deprecated-pragma` for more details on the pragmas.

.. ghc-flag:: -Wdeprecations
    :shortdesc: warn about uses of functions & types that have DEPRECATED pragmas,
        or WARNING pragmas with the ``deprecated`` category.
    :type: dynamic
    :reverse: -Wno-deprecations
    :category:

    :default: on

    .. index::
       pair: deprecations; warnings

    Causes a warning to be emitted when a module, function or type with
    ``DEPRECATED pragma``, or a ``WARNING`` pragma with the ``deprecated``
    category, is used. See :ref:`warning-deprecated-pragma` for more details on
    the pragmas.

.. ghc-flag:: -Wwarnings-deprecations
    :shortdesc: warn about uses of functions & types that have DEPRECATED pragmas,
        or WARNING pragmas with the ``deprecated`` category.
        Alias for :ghc-flag:`-Wdeprecations`.
    :type: dynamic
    :reverse: -Wno-warnings-deprecations
    :category:

    :since: 6.10

    :default: on

    .. index::
       pair: deprecations; warnings

    Causes a warning to be emitted when a module, function or type with
    ``DEPRECATED pragma``, or a ``WARNING`` pragma with the ``deprecated``
    category, is used. See :ref:`warning-deprecated-pragma` for more details on
    the pragmas. An alias for :ghc-flag:`-Wdeprecations`.

.. ghc-flag:: -Wnoncanonical-monad-instances
    :shortdesc: warn when ``Applicative`` or ``Monad`` instances have
        noncanonical definitions of ``return``, ``pure``, ``(>>)``,
        or ``(*>)``.
        See flag description in :ref:`options-sanity` for more details.
    :type: dynamic
    :reverse: -Wno-noncanonical-monad-instances
    :category:

    :since: 8.0

    :default: on

    Warn if noncanonical ``Applicative`` or ``Monad`` instances
    declarations are detected.

    When this warning is enabled, the following conditions are verified:

    In ``Monad`` instances declarations warn if any of the following
    conditions does not hold:

     * If ``return`` is defined it must be canonical (i.e. ``return = pure``).
     * If ``(>>)`` is defined it must be canonical (i.e. ``(>>) = (*>)``).

    Moreover, in ``Applicative`` instance declarations:

     * Warn if ``pure`` is defined backwards (i.e. ``pure = return``).
     * Warn if ``(*>)`` is defined backwards (i.e. ``(*>) = (>>)``).

.. ghc-flag:: -Wnoncanonical-monadfail-instances
    :shortdesc: *(deprecated)*
        warn when ``Monad`` or ``MonadFail`` instances have
        noncanonical definitions of ``fail``.
    :type: dynamic
    :reverse: -Wno-noncanonical-monadfail-instances
    :category:

    :since: 8.0

    This warning is deprecated. It no longer has any effect since GHC 8.8.
    It was used during the transition period of the MonadFail proposal,
    to detect when an instance of the ``Monad`` class was not defined
    via ``MonadFail``, or when a ``MonadFail`` instance was defined
    backwards, using the method in ``Monad``.

.. ghc-flag:: -Wnoncanonical-monoid-instances
    :shortdesc: warn when ``Semigroup`` or ``Monoid`` instances have
        noncanonical definitions of ``(<>)`` or ``mappend``.
        See flag description in :ref:`options-sanity` for more details.
    :type: dynamic
    :reverse: -Wno-noncanonical-monoid-instances
    :category:

    :since: 8.0

    :default: on

    Warn if noncanonical ``Semigroup`` or ``Monoid`` instances
    declarations are detected.

    When this warning is enabled, the following conditions are verified:

    In ``Monoid`` instances declarations warn if any of the following
    conditions does not hold:

     * If ``mappend`` is defined it must be canonical
       (i.e. ``mappend = (Data.Semigroup.<>)``).

    Moreover, in ``Semigroup`` instance declarations:

     * Warn if ``(<>)`` is defined backwards (i.e. ``(<>) = mappend``).

.. ghc-flag:: -Wmissing-monadfail-instances
    :shortdesc: *(deprecated)*
        Warn when a failable pattern is used in a do-block that does
        not have a ``MonadFail`` instance.
    :type: dynamic
    :reverse: -Wno-missing-monadfail-instances
    :category:

    :since: 8.0

    This warning is deprecated. It no longer has any effect since GHC 8.8.
    It was used during the transition period of the MonadFail proposal,
    to warn when a failable pattern is used in a do-block that does not have
    a ``MonadFail`` instance.

.. ghc-flag:: -Wsemigroup
    :shortdesc: *(deprecated)*
        Warn when a ``Monoid`` is not ``Semigroup``, and on non-``Semigroup``
        definitions of ``(<>)``
    :type: dynamic
    :reverse: -Wno-semigroup
    :category:

    :since: 8.0

    .. index::
       single: semigroup; warning

    This warning is deprecated. It no longer has any effect since GHC 9.8.
    It was used during the transition period of the semigroup proposal,
    to warn when an instance of ``Monoid`` was not an instance of ``Semigroup``,
    or when a custom local operator ``(<>)`` could clash with `(<>)`,
    now exported from ``Prelude``.

.. ghc-flag:: -Wdeprecated-flags
    :shortdesc: warn about uses of commandline flags that are deprecated
    :type: dynamic
    :reverse: -Wno-deprecated-flags
    :category:

    :since: 6.10

    :default: on

    .. index::
       single: deprecated flags

    Causes a warning to be emitted when a deprecated command-line flag
    is used.

.. ghc-flag:: -Wunsupported-calling-conventions
    :shortdesc: warn about use of an unsupported calling convention
    :type: dynamic
    :reverse: -Wno-unsupported-calling-conventions
    :category:

    :since: 7.6

    Causes a warning to be emitted for foreign declarations that use
    unsupported calling conventions. In particular, if the ``stdcall``
    calling convention is used then it will be treated as ``ccall``.

.. ghc-flag:: -Wdodgy-foreign-imports
    :shortdesc: warn about dodgy foreign imports
    :type: dynamic
    :reverse: -Wno-dodgy-foreign-imports
    :category:

    :since: 6.10

    Causes a warning to be emitted for foreign imports of the following
    form: ::

        foreign import "f" f :: FunPtr t

    on the grounds that it probably should be ::

        foreign import "&f" f :: FunPtr t

    The first form declares that ``f`` is a (pure) C function that takes
    no arguments and returns a pointer to a C function with type ``t``,
    whereas the second form declares that ``f`` itself is a C function
    with type ``t``. The first declaration is usually a mistake, and one
    that is hard to debug because it results in a crash, hence this
    warning.

.. ghc-flag:: -Wdodgy-exports
    :shortdesc: warn about dodgy exports
    :type: dynamic
    :reverse: -Wno-dodgy-exports
    :category:

    :since: 6.12

    Causes a warning to be emitted when a datatype ``T`` is exported
    with all constructors, i.e. ``T(..)``, but is it just a type
    synonym.

    Also causes a warning to be emitted when a module is re-exported,
    but that module exports nothing.

.. ghc-flag:: -Wdodgy-imports
    :shortdesc: warn about dodgy imports
    :type: dynamic
    :reverse: -Wno-dodgy-imports
    :category:

    :since: 6.8

    Causes a warning to be emitted in the following cases:

    -  When a datatype ``T`` is imported with all constructors, i.e.
       ``T(..)``, but has been exported abstractly, i.e. ``T``.

    -  When an ``import`` statement hides an entity that is not
       exported.

.. ghc-flag:: -Woverflowed-literals
    :shortdesc: warn about literals that will overflow their type
    :type: dynamic
    :reverse: -Wno-overflowed-literals
    :category:

    :since: 7.8

    Causes a warning to be emitted if a literal will overflow, e.g.
    ``300 :: Word8``.

.. ghc-flag:: -Wempty-enumerations
    :shortdesc: warn about enumerations that are empty
    :type: dynamic
    :reverse: -Wno-empty-enumerations
    :category:

    :since: 7.8

    Causes a warning to be emitted if an enumeration is empty, e.g.
    ``[5 .. 3]``.

.. ghc-flag:: -Wderiving-defaults
    :shortdesc: warn about default deriving when using both
        :extension:`DeriveAnyClass` and :extension:`GeneralizedNewtypeDeriving`
    :type: dynamic
    :reverse: -Wno-deriving-defaults
    :category:

    :since: 8.10

    Causes a warning when both :extension:`DeriveAnyClass` and
    :extension:`GeneralizedNewtypeDeriving` are enabled and no explicit
    deriving strategy is in use.  For example, this would result a
    warning: ::

        class C a
        newtype T a = MkT a deriving C

.. ghc-flag:: -Wduplicate-constraints
    :shortdesc: warn when a constraint appears duplicated in a type signature
    :type: dynamic
    :reverse: -Wno-duplicate-constraints
    :category:

    :since: 7.8

    .. index::
       single: duplicate constraints, warning

    Have the compiler warn about duplicate constraints in a type
    signature. For example ::

        f :: (Eq a, Show a, Eq a) => a -> a

    The warning will indicate the duplicated ``Eq a`` constraint.

    This option is now deprecated in favour of
    :ghc-flag:`-Wredundant-constraints`.

.. ghc-flag:: -Wredundant-constraints
    :shortdesc: Have the compiler warn about redundant constraints in type
        signatures.
    :type: dynamic
    :reverse: -Wno-redundant-constraints
    :category:

    :since: 8.0

    .. index::
       single: redundant constraints, warning

    Have the compiler warn about redundant constraints in a type
    signature. In particular:

    -  A redundant constraint within the type signature itself: ::

            f :: (Eq a, Ord a) => a -> a

       The warning will indicate the redundant ``Eq a`` constraint: it
       is subsumed by the ``Ord a`` constraint.

    -  A constraint in the type signature is not used in the code it
       covers: ::

            f :: Eq a => a -> a -> Bool
            f x y = True

       The warning will indicate the redundant ``Eq a`` constraint: : it
       is not used by the definition of ``f``.)

    Similar warnings are given for a redundant constraint in an instance
    declaration.

    When turning on, you can suppress it on a per-module basis with
    :ghc-flag:`-Wno-redundant-constraints <-Wredundant-constraints>`.
    Occasionally you may specifically want a function to have a more
    constrained signature than necessary, perhaps to leave yourself
    wiggle-room for changing the implementation without changing the
    API. In that case, you can suppress the warning on a per-function
    basis, using a call in a dead binding. For example: ::

        f :: Eq a => a -> a -> Bool
        f x y = True
        where
            _ = x == x  -- Suppress the redundant-constraint warning for (Eq a)

    Here the call to ``(==)`` makes GHC think that the ``(Eq a)``
    constraint is needed, so no warning is issued.

.. ghc-flag:: -Wduplicate-exports
    :shortdesc: warn when an entity is exported multiple times
    :type: dynamic
    :reverse: -Wno-duplicate-exports
    :category:

    :since: at least 5.04

    :default: on

    .. index::
       single: duplicate exports, warning
       single: export lists, duplicates

    Have the compiler warn about duplicate entries in export lists. This
    is useful information if you maintain large export lists, and want
    to avoid the continued export of a definition after you've deleted
    (one) mention of it in the export list.

.. ghc-flag:: -Whi-shadowing
    :shortdesc: *(deprecated)*
        warn when a ``.hi`` file in the current directory shadows a library
    :type: dynamic
    :reverse: -Wno-hi-shadowing
    :category:

    :since: at least 5.04, deprecated

    .. index::
       single: shadowing; interface files

    Causes the compiler to emit a warning when a module or interface
    file in the current directory is shadowing one with the same module
    name in a library or other directory.

    This flag was not implemented correctly and is now deprecated.
    It will be removed in a later version of GHC.

.. ghc-flag:: -Widentities
    :shortdesc: warn about uses of Prelude numeric conversions that are probably
        the identity (and hence could be omitted)
    :type: dynamic
    :reverse: -Wno-identities
    :category:

    :since: 7.2

    Causes the compiler to emit a warning when a Prelude numeric
    conversion converts a type ``T`` to the same type ``T``; such calls are
    probably no-ops and can be omitted. The functions checked for are:
    ``toInteger``, ``toRational``, ``fromIntegral``, and ``realToFrac``.

.. ghc-flag:: -Wimplicit-kind-vars
    :shortdesc: *(deprecated)* warn when kind variables are
        implicitly quantified over.
    :type: dynamic
    :reverse: -Wno-implicit-kind-vars
    :category:

    :since: 8.6

    This warning is deprecated. It no longer has any effect since GHC 8.10.
    It was used to detect if a kind variable is not explicitly quantified
    over. For instance, the following would produce a warning: ::

        f :: forall (a :: k). Proxy a

    This is now an error and can be fixed by explicitly quantifying
    over ``k``: ::

        f :: forall k (a :: k). Proxy a

    or ::

        f :: forall {k} (a :: k). Proxy a

.. ghc-flag:: -Wimplicit-lift
    :shortdesc: warn about implicit ``lift`` in Template Haskell quotes
    :type: dynamic
    :reverse: -Wno-implicit-lift
    :category: warnings

    :since: 9.2

    Template Haskell quotes referring to local variables bound outside
    of the quote are implicitly converted to use ``lift``. For example,
    ``f x = [| reverse x |]`` becomes ``f x = [| reverse $(lift x) |])``.
    This flag issues a warning for every such implicit addition of ``lift``.
    This can be useful when debugging more complex staged programs,
    where an implicit ``lift`` can accidentally conceal a variable
    used at a wrong stage.

.. ghc-flag:: -Wimplicit-prelude
    :shortdesc: warn when the Prelude is implicitly imported
    :type: dynamic
    :reverse: -Wno-implicit-prelude
    :category:

    :since: 6.8

    :default: off

    .. index::
       single: implicit prelude, warning

    Have the compiler warn if the Prelude is implicitly imported. This happens
    unless either the Prelude module is explicitly imported with an ``import
    ... Prelude ...`` line, or this implicit import is disabled (either by
    :extension:`NoImplicitPrelude` or a ``LANGUAGE NoImplicitPrelude``
    pragma).

    Note that no warning is given for syntax that implicitly refers to the
    Prelude, even if :extension:`NoImplicitPrelude` would change whether it
    refers to the Prelude. For example, no warning is given when ``368`` means
    ``Prelude.fromInteger (368::Prelude.Integer)`` (where ``Prelude`` refers
    to the actual Prelude module, regardless of the imports of the module
    being compiled).

.. ghc-flag:: -Wincomplete-patterns
    :shortdesc: warn when a pattern match could fail
    :type: dynamic
    :reverse: -Wno-incomplete-patterns
    :category:

    :since: 5.04

    .. index::
       single: incomplete patterns, warning
       single: patterns, incomplete

    The option :ghc-flag:`-Wincomplete-patterns` warns about places where a
    pattern-match might fail at runtime. The function ``g`` below will
    fail when applied to non-empty lists, so the compiler will emit a
    warning about this when :ghc-flag:`-Wincomplete-patterns` is enabled. ::

        g [] = 2

    This option isn't enabled by default because it can be a bit noisy,
    and it doesn't always indicate a bug in the program. However, it's
    generally considered good practice to cover all the cases in your
    functions, and it is switched on by :ghc-flag:`-W`.


.. ghc-flag:: -Wincomplete-uni-patterns
    :shortdesc: warn when a pattern match in a lambda expression,
        pattern binding or a lazy pattern could fail
    :type: dynamic
    :reverse: -Wno-incomplete-uni-patterns
    :category:

    :since: 7.2

    The flag :ghc-flag:`-Wincomplete-uni-patterns` is similar to
    :ghc-flag:`-Wincomplete-patterns`, except that it applies only to
    lambda-expressions and pattern bindings, constructs that only allow a
    single pattern: ::

        h = \[] -> 2
        Just k = f y

    Furthermore, this flag also applies to lazy patterns, since they are
    syntactic sugar for pattern bindings. For example, ``f ~(Just x) = (x,x)``
    is equivalent to ``f y = let Just x = y in (x,x)``.

.. ghc-flag:: -fmax-pmcheck-models=⟨n⟩
    :shortdesc: soft limit on the number of parallel models the pattern match
        checker should check a pattern match clause against
    :type: dynamic
    :category:

    :since: 8.10

    :default: 30

    The pattern match checker works by assigning symbolic values to each
    pattern. We call each such assignment a 'model'. Now, each pattern match
    clause leads to potentially multiple splits of that model, encoding
    different ways for the pattern match to fail. For example, when matching
    ``x`` against ``Just 4``, we split each incoming matching model into two
    uncovered sub-models: One where ``x`` is ``Nothing`` and one where ``x`` is
    ``Just y`` but ``y`` is not ``4``.

    This can be exponential in the arity of the pattern and in the number of
    guards in some cases. The :ghc-flag:`-fmax-pmcheck-models=⟨n⟩` limit makes sure
    we scale polynomially in the number of patterns, by forgetting refined
    information gained from a partially successful match. For the above example,
    if we had a limit of 1, we would continue checking the next clause with the
    original, unrefined model.

.. ghc-flag:: -Wincomplete-record-updates
    :shortdesc: warn when a record update could fail
    :type: dynamic
    :reverse: -Wno-incomplete-record-updates
    :category:

    :since: 6.4

    .. index::
       single: incomplete record updates, warning
       single: record updates, incomplete

    The function ``f`` below will fail when applied to ``Bar``, so the
    compiler will emit a warning about this when
    :ghc-flag:`-Wincomplete-record-updates` is enabled. ::

        data Foo = Foo { x :: Int }
                 | Bar

        f :: Foo -> Foo
        f foo = foo { x = 6 }

    This option isn't enabled by default because it can be very noisy,
    and it often doesn't indicate a bug in the program.

.. ghc-flag:: -Wincomplete-record-selectors
    :shortdesc: warn when a record selector application could fail
    :type: dynamic
    :reverse: -Wno-incomplete-record-selectors
    :category:

    :since: 9.10

    .. index::
        single: incomplete record selectors, warning
        single: record selectors, incomplete

    When a record selector is applied to a constructor that does not
    contain that field, it will produce an error. For example ::

        data T = T1 | T2 { x :: Int }

        f :: T -> Int
        f a = x a -- `f T1` will fail

        g1 :: HasField "x" t Int => t -> Int
        g1 a = 1 + getField @"x" a

        g2 :: T -> Int
        g2 a = g1 a + 2 -- `g2 T1` will fail as well

    The warning warns about cases like that. It also takes into account
    previously pattern-matched cases, for example ::

        d :: T -> Int
        d T1 = 0
        d a = x a -- would not warn

.. ghc-flag:: -Wmissing-deriving-strategies
    :shortdesc: warn when deriving without mentioning a deriving strategy
    :type: dynamic
    :reverse: -Wno-missing-deriving-strategies
    :category:

    :since: 8.8.1
    :default: off

    The datatype below derives the ``Eq`` typeclass, but doesn't specify a
    strategy. When :ghc-flag:`-Wmissing-deriving-strategies` is enabled,
    the compiler will emit a warning about this. ::

        data Foo a = Foo a
          deriving (Eq)

    The compiler will warn here that the deriving clause doesn't specify a
    strategy. The suggested fix will show which deriving strategies were
    assumed.

    If the warning is enabled, but :extension:`DerivingStrategies` is not
    enabled, the compiler will suggest turning on the
    :extension:`DerivingStrategies` extension.

.. ghc-flag:: -Wmissing-fields
    :shortdesc: warn when fields of a record are uninitialised
    :type: dynamic
    :reverse: -Wno-missing-fields
    :category:

    :since: at least 5.04

    .. index::
       single: missing fields, warning
       single: fields, missing

    This option is on by default, and warns you whenever the
    construction of a labelled field constructor isn't complete, missing
    initialisers for one or more fields. While not an error (the missing
    fields are initialised with bottoms), it is often an indication of a
    programmer error.

.. ghc-flag:: -Wmissing-export-lists
    :shortdesc: warn when a module declaration does not explicitly list all
        exports
    :type: dynamic
    :reverse: -Wno-missing-export-lists
    :category:

    :since: 8.4

    .. index::
       single: missing export lists, warning
       single: export lists, missing

    This flag warns if you declare a module without declaring an explicit
    export list. For example ::

        module M where

          p x = x

    The :ghc-flag:`-Wmissing-export-lists` flag will warn that ``M`` does not
    declare an export list. Declaring an explicit export list for ``M`` enables
    GHC dead code analysis, prevents accidental export of names and can ease
    optimizations like inlining.

.. ghc-flag:: -Wmissing-import-lists
    :shortdesc: warn when an import declaration does not explicitly list all the
        names brought into scope
    :type: dynamic
    :reverse: -Wno-missing-import-lists
    :category:

    :since: 7.0

    .. index::
       single: missing import lists, warning
       single: import lists, missing

    This flag warns if you use an unqualified ``import`` declaration
    that does not explicitly list the entities brought into scope. For
    example ::

        module M where
          import X( f )
          import Y
          import qualified Z
          p x = f x x

    The :ghc-flag:`-Wmissing-import-lists` flag will warn about the import of
    ``Y`` but not ``X`` If module ``Y`` is later changed to export (say) ``f``,
    then the reference to ``f`` in ``M`` will become ambiguous. No warning is
    produced for the import of ``Z`` because extending ``Z``\'s exports would be
    unlikely to produce ambiguity in ``M``.

.. ghc-flag:: -Wmissing-methods
    :shortdesc: warn when class methods are undefined
    :type: dynamic
    :reverse: -Wno-missing-methods
    :category:

    :since: at least 5.04

    :default: on

    .. index::
       single: missing methods, warning
       single: methods, missing

    This option warns you whenever an instance declaration is missing
    one or more methods, and the corresponding class declaration has no default
    declaration for them.

    The ``MINIMAL`` pragma can be used to change which combination of
    methods will be required for instances of a particular class. See
    :ref:`minimal-pragma`.

.. ghc-flag:: -Wmissing-signatures
    :shortdesc: warn about top-level functions without signatures
    :type: dynamic
    :reverse: -Wno-missing-signatures
    :category:

    :since: at least 5.04

    :default: off

    .. index::
       single: type signatures, missing

    If you would like GHC to check that every top-level function/value
    has a type signature, use the :ghc-flag:`-Wmissing-signatures` option.
    As part of the warning GHC also reports the inferred type.

.. ghc-flag:: -Wmissing-exported-sigs
    :shortdesc: *(deprecated)*
        warn about top-level functions without signatures, only if they
        are exported. takes precedence over -Wmissing-signatures
    :type: dynamic
    :reverse: -Wno-missing-exported-sigs
    :category:

    :since: 7.10

    .. index::
       single: type signatures, missing

    This option is now deprecated in favour of
    :ghc-flag:`-Wmissing-exported-signatures`.

.. ghc-flag:: -Wmissing-exported-signatures
    :shortdesc: warn about top-level functions without signatures, only if they
        are exported
    :type: dynamic
    :reverse: -Wno-missing-exported-signatures
    :category:

    :since: 8.0

    :default: off

    .. index::
       single: type signatures, missing

    If you would like GHC to check that every exported top-level
    function/value has a type signature, but not check unexported
    values, use the :ghc-flag:`-Wmissing-exported-signatures`
    option. If this option is used in conjunction with
    :ghc-flag:`-Wmissing-signatures` then every top-level function/value
    must have a type signature. As part of the warning GHC also
    reports the inferred type.

.. ghc-flag:: -Wmissing-local-sigs
    :shortdesc: *(deprecated)*
        warn about polymorphic local bindings without signatures
    :type: dynamic
    :reverse: -Wno-missing-local-sigs
    :category:

    :since: 7.0

    .. index::
       single: type signatures, missing

    This option is now deprecated in favour of
    :ghc-flag:`-Wmissing-local-signatures`.

.. ghc-flag:: -Wmissing-local-signatures
    :shortdesc: warn about polymorphic local bindings without signatures
    :type: dynamic
    :reverse: -Wno-missing-local-signatures
    :category:

    :since: 8.0

    .. index::
       single: type signatures, missing

    If you use the :ghc-flag:`-Wmissing-local-signatures` flag GHC
    will warn you about any polymorphic local bindings. As part of the
    warning GHC also reports the inferred type. The option is off by
    default.

.. ghc-flag:: -Wmissing-pattern-synonym-signatures
    :shortdesc: warn when pattern synonyms do not have type signatures
    :type: dynamic
    :reverse: -Wno-missing-pattern-synonym-signatures
    :category:

    :since: 8.0

    :default: off

    .. index::
         single: type signatures, missing, pattern synonyms

    If you would like GHC to check that every pattern synonym has a
    type signature, use the
    :ghc-flag:`-Wmissing-pattern-synonym-signatures` option. If this
    option is used in conjunction with
    :ghc-flag:`-Wmissing-exported-signatures` then only exported pattern
    synonyms must have a type signature. GHC also reports the inferred
    type.

.. ghc-flag:: -Wmissing-kind-signatures
    :shortdesc: warn when type declarations don't have kind signatures nor CUSKs
    :type: dynamic
    :reverse: -Wno-missing-kind-signatures
    :category:

    :since: 9.2
    :default: off

    .. index::
         single: kind signatures, missing

    If you would like GHC to check that every data, type family,
    type-class definition has a :ref:`standalone kind signature <standalone-kind-signatures>` or a :ref:`CUSK <complete-kind-signatures>`, use the
    :ghc-flag:`-Wmissing-kind-signatures` option.
    You can specify the kind via :extension:`StandaloneKindSignatures`
    or :extension:`CUSKs`.

    Note that :ghc-flag:`-Wmissing-kind-signatures` does not warn about
    associated type families, as GHC considers an associated type family
    declaration to have a CUSK if its enclosing class has a CUSK. (See
    :ref:`complete-kind-signatures` for more on this point.) Therefore, giving
    the parent class a standalone kind signature or CUSK is sufficient to fix
    the warning for the class's associated type families as well.

.. ghc-flag:: -Wmissing-poly-kind-signatures
    :shortdesc: warn when inferred polykinded type or class declaration don't have kind signatures nor CUSKs
    :type: dynamic
    :reverse: -Wno-missing-poly-kind-signatures
    :category:

    :since: 9.8
    :default: off

    .. index::
         single: kind signatures, missing

    This is a restricted version of :ghc-flag:`-Wmissing-kind-signatures`.

    It warns when a declaration defines a type constructor that lacks a :ref:`standalone kind signature <standalone-kind-signatures>`
    and whose inferred kind is polymorphic (which happens with `-PolyKinds`.  For example ::

        data T a = MkT (a -> Int)    -- T :: Type -> Type
                                     -- Not polymorphic, hence no warning
        data W f a = MkW (f a)       -- W :: forall k. (k->Type) -> k -> Type
                                     -- Polymorphic, hence warning!

    It is useful to catch accidentally polykinded types, or to make that polymorphism explicit,
    without requiring a kind signature for every type.

.. ghc-flag:: -Wmissing-exported-pattern-synonym-signatures
    :shortdesc: warn about pattern synonyms without signatures, only if they
        are exported
    :type: dynamic
    :reverse: -Wno-missing-exported-pattern-synonym-signatures
    :category:

    :default: off

    .. index::
       single: type signatures, missing, pattern synonyms

    If you would like GHC to check that every exported pattern synonym has a
    type signature, but not check unexported pattern synonyms, use the
    :ghc-flag:`-Wmissing-exported-pattern-synonym-signatures` option. If this
    option is used in conjunction with
    :ghc-flag:`-Wmissing-pattern-synonym-signatures` then every pattern synonym
    must have a type signature. As part of the warning GHC also reports the
    inferred type.

.. ghc-flag:: -Wname-shadowing
    :shortdesc: warn when names are shadowed
    :type: dynamic
    :reverse: -Wno-name-shadowing
    :category:

    :since: at least 5.04

    .. index::
       single: shadowing, warning

    This option causes a warning to be emitted whenever an inner-scope
    value has the same name as an outer-scope value, i.e. the inner
    value shadows the outer one. This can catch typographical errors
    that turn into hard-to-find bugs, e.g., in the inadvertent capture
    of what would be a recursive call in
    ``f = ... let f = id in ... f ...``.

    The warning is suppressed for names beginning with an underscore.
    For example ::

        f x = do { _ignore <- this; _ignore <- that; return (the other) }

.. ghc-flag:: -Worphans
    :shortdesc: warn when the module contains :ref:`orphan instance declarations
        or rewrite rules <orphan-modules>`
    :type: dynamic
    :reverse: -Wno-orphans
    :category:

    :since: 6.4

    .. index::
       single: orphan instances, warning
       single: orphan rules, warning

    These flags cause a warning to be emitted whenever the module
    contains an "orphan" instance declaration or rewrite rule. An
    instance declaration is an orphan if it appears in a module in which
    neither the class nor the type being instanced are declared in the
    same module. A rule is an orphan if it is a rule for a function
    declared in another module. A module containing any orphans is
    called an orphan module.

    The trouble with orphans is that GHC must pro-actively read the
    interface files for all orphan modules, just in case their instances
    or rules play a role, whether or not the module's interface would
    otherwise be of any use. See :ref:`orphan-modules` for details.

    The flag :ghc-flag:`-Worphans` warns about user-written orphan rules or
    instances.

.. ghc-flag:: -Woverlapping-patterns
    :shortdesc: warn about overlapping patterns
    :type: dynamic
    :reverse: -Wno-overlapping-patterns
    :category:

    :since: at least 5.04

    .. index::
       single: overlapping patterns, warning
       single: patterns, overlapping

    By default, the compiler will warn you if a set of patterns are
    overlapping, e.g., ::

        f :: String -> Int
        f []     = 0
        f (_:xs) = 1
        f "2"    = 2

    where the last pattern match in ``f`` won't ever be reached, as the
    second pattern overlaps it. More often than not, redundant patterns
    is a programmer mistake/error, so this option is enabled by default.

    If the programmer is dead set on keeping a redundant clause,
    for example to prevent bitrot, they can make use of a guard
    scrutinising ``GHC.Exts.considerAccessible`` to prevent the
    checker from flagging the parent clause as redundant: ::

        g :: String -> Int
        g []                       = 0
        g (_:xs)                   = 1
        g "2" | considerAccessible = 2 -- No warning!

    Note that ``considerAccessible`` should come as the last statement of
    the guard in order not to impact the results of the checker. E.g., if
    you write ::

        h :: Bool -> Int
        h x = case (x, x) of
          (True,  True)  -> 1
          (False, False) -> 2
          (True,  False) | considerAccessible, False <- x -> 3

    The pattern-match checker takes you by your word, will conclude
    that ``False <- x`` might fail and warn that the pattern-match
    is inexhaustive. Put ``considerAccessible`` last to avoid such
    confusions.

    Note that due to technical limitations, ``considerAccessible`` will not
    suppress :ghc-flag:`-Winaccessible-code` warnings.

.. ghc-flag:: -Winaccessible-code
    :shortdesc: warn about inaccessible code
    :type: dynamic
    :reverse: -Wno-inaccessible-code
    :category:

    :since: 8.6

    .. index::
       single: inaccessible code, warning
       single: inaccessible

    By default, the compiler will warn you if types make a branch inaccessible.
    This generally requires GADTs or similar extensions.

    Take, for example, the following program ::

        {-# LANGUAGE GADTs #-}

        data Foo a where
         Foo1 :: Foo Char
         Foo2 :: Foo Int

        data TyEquality a b where
                Refl :: TyEquality a a

        checkTEQ :: Foo t -> Foo u -> Maybe (TyEquality t u)
        checkTEQ x y = error "unimportant"

        step2 :: Bool
        step2 = case checkTEQ Foo1 Foo2 of
                 Just Refl -> True -- Inaccessible code
                 Nothing -> False

    The ``Just Refl`` case in ``step2`` is inaccessible, because in order for
    ``checkTEQ`` to be able to produce a ``Just``, ``t ~ u`` must hold, but
    since we're passing ``Foo1`` and ``Foo2`` here, it follows that ``t ~
    Char``, and ``u ~ Int``, and thus ``t ~ u`` cannot hold.

.. ghc-flag:: -Wstar-is-type
     :shortdesc: warn when ``*`` is used to mean ``Data.Kind.Type``
     :type: dynamic
     :reverse: -Wno-star-is-type
     :category:

     :since: 8.6

     The use of ``*`` to denote the kind of inhabited types relies on the
     :extension:`StarIsType` extension, which in a future release will be
     turned off by default and then possibly removed. The reasons for this and
     the deprecation schedule are described in `GHC proposal #143
     <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0143-remove-star-kind.rst>`__.

     This warning allows to detect such uses of ``*`` before the actual
     breaking change takes place. The recommended fix is to replace ``*`` with
     ``Type`` imported from ``Data.Kind``.

.. ghc-flag:: -Wstar-binder
     :shortdesc: warn about binding the ``(*)`` type operator despite
         :extension:`StarIsType`
     :type: dynamic
     :reverse: -Wno-star-binder

     :since: 8.6

     Under :extension:`StarIsType`, a ``*`` in types is not an operator nor
     even a name, it is special syntax that stands for ``Data.Kind.Type``. This
     means that an expression like ``Either * Char`` is parsed as ``Either (*)
     Char`` and not ``(*) Either Char``.

     In binding positions, we have similar parsing rules. Consider the following
     example ::

         {-# LANGUAGE TypeOperators, TypeFamilies, StarIsType #-}

         type family a + b
         type family a * b

     While ``a + b`` is parsed as ``(+) a b`` and becomes a binding position for
     the ``(+)`` type operator, ``a * b`` is parsed as ``a (*) b`` and is rejected.

     As a workaround, we allow to bind ``(*)`` in prefix form::

         type family (*) a b

     This is a rather fragile arrangement, as generally a programmer expects
     ``(*) a b`` to be equivalent to ``a * b``. With :ghc-flag:`-Wstar-binder`
     we warn when this special treatment of ``(*)`` takes place.

.. ghc-flag:: -Wsimplifiable-class-constraints
    :shortdesc: Warn about class constraints in a type signature that can
        be simplified using a top-level instance declaration.
    :type: dynamic
    :reverse: -Wno-simplifiable-class-constraints
    :category:

    :since: 8.2
    :default: on

    .. index::
       single: simplifiable class constraints, warning

    Warn about class constraints in a type signature that can be simplified
    using a top-level instance declaration.  For example: ::

       f :: Eq [a] => a -> a

    Here the ``Eq [a]`` in the signature overlaps with the top-level
    instance for ``Eq [a]``.  GHC goes to some efforts to use the former,
    but if it should use the latter, it would then have an
    insoluble ``Eq a`` constraint.  Best avoided by instead writing: ::

       f :: Eq a => a -> a

.. ghc-flag:: -Wtabs
    :shortdesc: warn if there are tabs in the source file
    :type: dynamic
    :reverse: -Wno-tabs
    :category:

    :since: 6.8

    .. index::
       single: tabs, warning

    Have the compiler warn if there are tabs in your source file.

.. ghc-flag:: -Wtype-defaults
    :shortdesc: warn when defaulting happens
    :type: dynamic
    :reverse: -Wno-type-defaults
    :category:

    :since: at least 5.04

    :default: off

    .. index::
       single: defaulting mechanism, warning

    Have the compiler warn/inform you where in your source the Haskell
    defaulting mechanism for numeric types kicks in. This is useful
    information when converting code from a context that assumed one
    default into one with another, e.g., the ‘default default’ for
    Haskell 1.4 caused the otherwise unconstrained value ``1`` to be
    given the type ``Int``, whereas Haskell 98 and later defaults it to
    ``Integer``. This may lead to differences in performance and
    behaviour, hence the usefulness of being non-silent about this.

.. ghc-flag:: -Wmonomorphism-restriction
    :shortdesc: warn when the Monomorphism Restriction is applied
    :type: dynamic
    :reverse: -Wno-monomorphism-restriction
    :category:

    :since: 6.8

    :default: off

    .. index::
       single: monomorphism restriction, warning

    Have the compiler warn/inform you where in your source the Haskell
    Monomorphism Restriction is applied. If applied silently the MR can
    give rise to unexpected behaviour, so it can be helpful to have an
    explicit warning that it is being applied.

.. ghc-flag:: -Wunsupported-llvm-version
    :shortdesc: Warn when using :ghc-flag:`-fllvm` with an unsupported
        version of LLVM.
    :type: dynamic
    :reverse: -Wno-unsupported-llvm-version
    :category:

    :since: 7.8

    Warn when using :ghc-flag:`-fllvm` with an unsupported version of LLVM.

.. ghc-flag:: -Wmissed-extra-shared-lib
    :shortdesc: Warn when GHCi can't load a shared lib.
    :type: dynamic
    :reverse: -Wno-missed-extra-shared-lib
    :category:

    :since: 8.8

    Warn when GHCi can't load a shared lib it deduced it should load
    when loading a package and analyzing the extra-libraries stanza
    of the target package description.

.. ghc-flag:: -Wunticked-promoted-constructors
    :shortdesc: warn if promoted constructors are not ticked
    :type: dynamic
    :reverse: -Wno-unticked-promoted-constructors
    :category:

    :since: 7.10

    .. index::
       single: promoted constructor, warning

    Warn if a promoted data constructor is used without a tick preceding
    its name.

    For example: ::

        data Nat = Succ Nat | Zero

        data Vec n s where
          Nil  :: Vec Zero a
          Cons :: a -> Vec n a -> Vec (Succ n) a

    Will raise two warnings because ``Zero`` and ``Succ`` are not
    written as ``'Zero`` and ``'Succ``.

    This also applies to list literals since 9.4. For example: ::

      type L = [Int, Char, Bool]

    will raise a warning, because ``[Int, Char, Bool]`` is a promoted list
    which lacks a tick.

.. ghc-flag:: -Wunused-binds
    :shortdesc: warn about bindings that are unused. Alias for
        :ghc-flag:`-Wunused-top-binds`, :ghc-flag:`-Wunused-local-binds` and
        :ghc-flag:`-Wunused-pattern-binds`
    :type: dynamic
    :reverse: -Wno-unused-binds
    :category:

    :since: at least 5.04

    .. index::
       single: unused binds, warning
       single: binds, unused

    Report any function definitions (and local bindings) which are
    unused. An alias for

    -  :ghc-flag:`-Wunused-top-binds`
    -  :ghc-flag:`-Wunused-local-binds`
    -  :ghc-flag:`-Wunused-pattern-binds`

.. ghc-flag:: -Wunused-top-binds
    :shortdesc: warn about top-level bindings that are unused
    :type: dynamic
    :reverse: -Wno-unused-top-binds
    :category:

    :since: 8.0

    .. index::
       single: unused binds, warning
       single: binds, unused

    Report any function definitions which are unused.

    More precisely, warn if a binding brings into scope a variable that
    is not used, except if the variable's name starts with an
    underscore. The "starts-with-underscore" condition provides a way to
    selectively disable the warning.

    A variable is regarded as "used" if

    -  It is exported, or

    -  It appears in the right hand side of a binding that binds at
       least one used variable that is used

    For example: ::

        module A (f) where
        f = let (p,q) = rhs1 in t p  -- No warning: q is unused, but is locally bound
        t = rhs3                     -- No warning: f is used, and hence so is t
        g = h x                      -- Warning: g unused
        h = rhs2                     -- Warning: h is only used in the
                                     -- right-hand side of another unused binding
        _w = True                    -- No warning: _w starts with an underscore

.. ghc-flag:: -Wunused-local-binds
    :shortdesc: warn about local bindings that are unused
    :type: dynamic
    :reverse: -Wno-unused-local-binds
    :category:

    :since: 8.0

    .. index::
       single: unused binds, warning
       single: binds, unused

    Report any local definitions which are unused. For example: ::

        module A (f) where
        f = let (p,q) = rhs1 in t p  -- Warning: q is unused
        g = h x                      -- No warning: g is unused, but is a top-level binding

.. ghc-flag:: -Wunused-pattern-binds
    :shortdesc: warn about pattern match bindings that are unused
    :type: dynamic
    :reverse: -Wno-unused-pattern-binds
    :category:

    :since: 8.0

    .. index::
       single: unused binds, warning
       single: binds, unused

    Warn if a pattern binding binds no variables at all, unless it is a
    lone wild-card pattern, or a banged pattern. For example: ::

        Just _ = rhs3    -- Warning: unused pattern binding
        (_, _) = rhs4    -- Warning: unused pattern binding
        _  = rhs3        -- No warning: lone wild-card pattern
        !() = rhs4       -- No warning: banged pattern; behaves like seq

    In general a lazy pattern binding `p = e` is a no-op if `p` does not
    bind any variables.
    The motivation for allowing lone wild-card patterns is they are not
    very different from ``_v = rhs3``, which elicits no warning; and
    they can be useful to add a type constraint, e.g. ``_ = x::Int``. A
    banged pattern (see :ref:`bang-patterns`) is *not* a no-op, because
    it forces evaluation, and is useful as an alternative to ``seq``.

.. ghc-flag:: -Wunused-imports
    :shortdesc: warn about unnecessary imports
    :type: dynamic
    :reverse: -Wno-unused-imports
    :category:

    :since: at least 5.04

    .. index::
       single: unused imports, warning
       single: imports, unused

    Report any modules that are explicitly imported but never used.
    However, the form ``import M()`` is never reported as an unused
    import, because it is a useful idiom for importing instance
    declarations, which are anonymous in Haskell.

.. ghc-flag:: -Wunused-matches
    :shortdesc: warn about variables in patterns that aren't used
    :type: dynamic
    :reverse: -Wno-unused-matches
    :category:

    :since: at least 5.04

    .. index::
       single: unused matches, warning
       single: matches, unused

    Report all unused variables which arise from term-level pattern matches,
    including patterns consisting of a single variable. For instance
    ``f x y = []`` would report ``x`` and ``y`` as unused. The warning
    is suppressed if the variable name begins with an underscore, thus: ::

        f _x = True

    Note that :ghc-flag:`-Wunused-matches` does not warn about variables which
    arise from type-level patterns, as found in type family and data family
    instances. This must be enabled separately through the
    :ghc-flag:`-Wunused-type-patterns` flag.

.. ghc-flag:: -Wunused-do-bind
    :shortdesc: warn about do bindings that appear to throw away values of types
        other than ``()``
    :type: dynamic
    :reverse: -Wno-unused-do-bind
    :category:

    :since: 6.12

    .. index::
       single: unused do binding, warning
       single: do binding, unused

    Report expressions occurring in ``do`` and ``mdo`` blocks that
    appear to silently throw information away. For instance
    ``do { mapM popInt xs ; return 10 }`` would report the first
    statement in the ``do`` block as suspicious, as it has the type
    ``StackM [Int]`` and not ``StackM ()``, but that ``[Int]`` value is
    not bound to anything. The warning is suppressed by explicitly
    mentioning in the source code that your program is throwing
    something away: ::

        do { _ <- mapM popInt xs ; return 10 }

    Of course, in this particular situation you can do even better: ::

        do { mapM_ popInt xs ; return 10 }

.. ghc-flag:: -Wunused-type-patterns
    :shortdesc: warn about unused type variables which arise from patterns in
        in type family and data family instances
    :type: dynamic
    :reverse: -Wno-unused-type-patterns
    :category:

    :since: 8.0

    .. index::
       single: unused type patterns, warning
       single: type patterns, unused

    Report all unused implicitly bound type variables which arise from
    patterns in type family and data family instances. For instance: ::

        type instance F x y = []

    would report ``x`` and ``y`` as unused on the right hand side. The warning
    is suppressed if the type variable name begins with an underscore, like
    so: ::

        type instance F _x _y = []

    When :extension:`ExplicitForAll` is enabled, explicitly quantified type
    variables may also be identified as unused. For instance: ::

        type instance forall x y. F x y = []

    would still report ``x`` and ``y`` as unused on the right hand side

    Unlike :ghc-flag:`-Wunused-matches`, :ghc-flag:`-Wunused-type-patterns` is
    not implied by :ghc-flag:`-Wall`. The rationale for this decision is that
    unlike term-level pattern names, type names are often chosen expressly for
    documentation purposes, so using underscores in type names can make the
    documentation harder to read.

.. ghc-flag:: -Wunused-foralls
    :shortdesc: warn about type variables in user-written
        ``forall``\\s that are unused
    :type: dynamic
    :reverse: -Wno-unused-foralls
    :category:

    :since: 8.0

    .. index::
       single: unused foralls, warning
       single: foralls, unused

    Report all unused type variables which arise from explicit, user-written
    ``forall`` statements. For instance: ::

        g :: forall a b c. (b -> b)

    would report ``a`` and ``c`` as unused.

.. ghc-flag:: -Wunused-record-wildcards
    :shortdesc: Warn about record wildcard matches when none of the bound variables
      are used.
    :type: dynamic
    :reverse: -Wno-unused-record-wildcards
    :category:

    :since: 8.10

    .. index::
       single: unused, warning, record wildcards

    Report all record wildcards where none of the variables bound implicitly
    are used. For instance: ::


        data P = P { x :: Int, y :: Int }

        f1 :: P -> Int
        f1 P{..} = 1 + 3

    would report that the ``P{..}`` match is unused.

.. ghc-flag:: -Wredundant-bang-patterns
    :shortdesc: Warn about redundant bang patterns.
    :type: dynamic
    :reverse: -Wno-redundant-bang-patterns
    :category:

    :since: 9.2

    .. index::
       single: redundant, warning, bang patterns

    Report dead bang patterns, where dead bangs are bang patterns that under no
    circumstances can force a thunk that wasn't already forced. Dead bangs are a
    form of redundant bangs. The new check is performed in pattern-match coverage
    checker along with other checks (namely, redundant and inaccessible RHSs).
    Given ::


        f :: Bool -> Int
        f True = 1
        f !x   = 2

    The bang pattern on ``!x`` is dead. By the time the ``x`` in the second equation
    is reached, ``x`` will already have been forced due to the first equation
    (``f True = 1``). Moreover, there is no way to reach the second equation without
    going through the first one.

    Note that ``-Wredundant-bang-patterns`` will not warn about dead bangs that appear
    on a redundant clause. That is because in that case, it is recommended to delete
    the clause wholly, including its leading pattern match.

    Dead bang patterns are redundant. But there are bang patterns which are
    redundant that aren't dead, for example: ::


        f !() = 0

    the bang still forces the argument, before we attempt to match on ``()``. But it is
    redundant with the forcing done by the ``()`` match. Currently such redundant bangs
    are not considered dead, and ``-Wredundant-bang-patterns`` will not warn about them.

.. ghc-flag:: -Wredundant-record-wildcards
    :shortdesc: Warn about record wildcard matches when the wildcard binds no patterns.
    :type: dynamic
    :reverse: -Wno-redundant-record-wildcards
    :category:

    :since: 8.10

    .. index::
       single: unused, warning, record wildcards

    Report all record wildcards where the wild card match binds no patterns.
    For instance: ::


        data P = P { x :: Int, y :: Int }

        f1 :: P -> Int
        f1 P{x,y,..} = x + y

    would report that the ``P{x, y, ..}`` match has a redundant use of ``..``.

.. ghc-flag:: -Wredundant-strictness-flags
    :shortdesc: Warn about redundant strictness flags.
    :type: dynamic
    :reverse: -Wno-redundant-strictness-flags
    :category:

    :since: 9.4

    Report strictness flags applied to unlifted types. An unlifted type is
    always strict, and applying a strictness flag has no effect.

    For example: ::

        data T = T !Int#

.. ghc-flag:: -Wwrong-do-bind
    :shortdesc: warn about do bindings that appear to throw away monadic values
        that you should have bound instead
    :type: dynamic
    :reverse: -Wno-wrong-do-bind
    :category:

    :since: 6.12

    .. index::
       single: apparently erroneous do binding, warning
       single: do binding, apparently erroneous

    Report expressions occurring in ``do`` and ``mdo`` blocks that
    appear to lack a binding. For instance
    ``do { return (popInt 10) ; return 10 }`` would report the first
    statement in the ``do`` block as suspicious, as it has the type
    ``StackM (StackM Int)`` (which consists of two nested applications
    of the same monad constructor), but which is not then "unpacked" by
    binding the result. The warning is suppressed by explicitly
    mentioning in the source code that your program is throwing
    something away: ::

        do { _ <- return (popInt 10) ; return 10 }

    For almost all sensible programs this will indicate a bug, and you
    probably intended to write: ::

        do { popInt 10 ; return 10 }

.. ghc-flag:: -Winline-rule-shadowing
    :shortdesc: Warn if a rewrite RULE might fail to fire because the
        function might be inlined before the rule has a chance to fire.
        See :ref:`rules-inline`.
    :type: dynamic
    :reverse: -Wno-inline-rule-shadowing
    :category:

    :since: 7.8

    Warn if a rewrite RULE might fail to fire because the function might
    be inlined before the rule has a chance to fire. See
    :ref:`rules-inline`.

.. ghc-flag:: -Wcpp-undef
    :shortdesc: warn on uses of the `#if` directive on undefined identifiers
    :type: dynamic
    :category:

    :since: 8.2

    This flag passes ``-Wundef`` to the C pre-processor (if its being used)
    which causes the pre-processor to warn on uses of the `#if` directive on
    undefined identifiers.

.. ghc-flag:: -Wunbanged-strict-patterns
    :shortdesc: warn on pattern bind of unlifted variable that is neither bare
        nor banged
    :type: dynamic
    :reverse: -Wno-unbanged-strict-patterns
    :category:

    :since: 8.2

    This flag warns whenever you write a pattern that binds a variable whose
    type is unlifted, and yet the pattern is not a bang pattern nor a bare variable.
    See :ref:`glasgow-unboxed` for information about unlifted types.

.. ghc-flag:: -Wmissing-home-modules
    :shortdesc: warn when encountering a home module imported, but not listed
        on the command line. Useful for cabal to ensure GHC won't pick
        up modules, not listed neither in ``exposed-modules``, nor in
        ``other-modules``.
    :type: dynamic
    :reverse: -Wno-missing-home-modules
    :category:

    :since: 8.2

    When a module provided by the package currently being compiled
    (i.e. the "home" package) is imported, but not explicitly listed in
    command line as a target. Useful for Cabal to ensure GHC won't
    pick up modules, not listed neither in ``exposed-modules``, nor in
    ``other-modules``.

.. ghc-flag:: -Wpartial-fields
    :shortdesc: warn when defining a partial record field.
    :type: dynamic
    :reverse: -Wno-partial-fields
    :category:

    :since: 8.4

    The option :ghc-flag:`-Wpartial-fields` warns about a record field
    ``f`` that is defined in some, but not all, of the constructors of a
    data type, as record selection and update will be partial. For example, when
    :ghc-flag:`-Wpartial-fields` is enabled the compiler will emit a warning at
    the definition of ``Foo`` below: ::

        data Foo = Foo { f :: Int } | Bar

    The warning is suppressed if the field name begins with an underscore. ::

        data Foo = Foo { _f :: Int } | Bar

    Related warnings are :ghc-flag:`-Wincomplete-record-selectors` and
    :ghc-flag:`-Wincomplete-record-updates`,
    which warn at use sites rather than definition sites.

.. ghc-flag:: -Wunused-packages
    :shortdesc: warn when package is requested on command line, but not needed.
    :type: dynamic
    :reverse: -Wno-unused-packages
    :category:

    :since: 8.10

    The option :ghc-flag:`-Wunused-packages` warns about packages, specified on
    command line via :ghc-flag:`-package ⟨pkg⟩` or
    :ghc-flag:`-package-id ⟨unit-id⟩`, but were not needed during compilation.
    If the warning fires it means the specified package wasn't needed for
    compilation.

    This warning interacts poorly with GHCi because most invocations will pass
    a large number of ``-package`` arguments on the initial load. Therefore if
    you modify the targets using ``:load`` or ``:cd`` then the warning will be
    silently disabled if it's enabled (see :ghc-ticket:`21110`).


.. ghc-flag:: -Winvalid-haddock
    :shortdesc: warn when a Haddock comment occurs in an invalid position
    :type: dynamic
    :reverse: -Wno-invalid-haddock
    :category:

    :since: 9.0

    When the ``-haddock`` option is enabled, GHC collects documentation
    comments and associates them with declarations, function arguments, data
    constructors, and other syntactic elements. Documentation comments in
    invalid positions are discarded::

        myValue =
          -- | Invalid (discarded) comment in an expression
          2 + 2

    This warning informs you about discarded documentation comments.
    It has no effect when :ghc-flag:`-haddock` is disabled.

.. ghc-flag:: -Woperator-whitespace-ext-conflict
    :shortdesc: warn on uses of infix operators that would be parsed differently
                were a particular GHC extension enabled
    :type: dynamic
    :reverse: -Wno-operator-whitespace-ext-conflict
    :category:

    :since: 9.2

    When :extension:`TemplateHaskell` is enabled, ``f $x`` is parsed as ``f``
    applied to an untyped splice. But when the extension is disabled, the
    expression is parsed as a use of the ``$`` infix operator.

    To make it easy to read ``f $x`` without checking the enabled extensions,
    one could rewrite it as ``f $ x``, which is what this warning suggests.

    Currently, it detects the following cases:

    * ``$x`` could mean an untyped splice under :extension:`TemplateHaskell`
    * ``$$x`` could mean a typed splice under :extension:`TemplateHaskell`
    * ``%m`` could mean a multiplicity annotation under :extension:`LinearTypes`

    It only covers extensions that currently exist. If you want to enforce a
    stricter policy and always require whitespace around all infix operators,
    use :ghc-flag:`-Woperator-whitespace`.

.. ghc-flag:: -Woperator-whitespace
    :shortdesc: warn on prefix, suffix, and tight infix uses of infix operators
    :type: dynamic
    :reverse: -Wno-operator-whitespace
    :category:

    :since: 9.2

    There are four types of infix operator occurrences, as defined by
    `GHC Proposal #229 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst>`__::

      a ! b   -- a loose infix occurrence
      a!b     -- a tight infix occurrence
      a !b    -- a prefix occurrence
      a! b    -- a suffix occurrence

    A loose infix occurrence of any operator is always parsed as an infix
    operator, but other occurrence types may be assigned a special meaning.
    For example, a prefix ``!`` denotes a bang pattern, and a prefix ``$``
    denotes a :extension:`TemplateHaskell` splice.

    This warning encourages the use of loose infix occurrences of all infix
    operators, to prevent possible conflicts with future language extensions.

.. ghc-flag:: -Wauto-orphans
    :shortdesc: *(deprecated)* Does nothing
    :type: dynamic

    :since: 7.4

    Does nothing.

.. ghc-flag:: -Wmissing-space-after-bang
    :shortdesc: *(deprecated)* Does nothing
    :type: dynamic

    :since: 8.8

    Does nothing.

.. ghc-flag:: -Wderiving-typeable
    :shortdesc: warn when Typeable is derived
    :type: dynamic
    :reverse: -Wno-deriving-typeable
    :category:

    :since: 7.10

    This flag warns when ``Typeable`` is listed in a deriving clause
    or derived with :extension:`StandaloneDeriving`.

    Since GHC 7.10, ``Typeable`` is automatically derived for all types.
    Thus, deriving ``Typeable`` yourself is redundant.

.. ghc-flag:: -Wambiguous-fields
    :shortdesc: warn about ambiguous field selectors or updates
    :type: dynamic
    :category:

    :since: 9.2

    When :extension:`DuplicateRecordFields` is enabled, the option
    :ghc-flag:`-Wambiguous-fields` warns about occurrences of fields in
    selectors or updates that depend on the deprecated mechanism for
    type-directed disambiguation.  This mechanism will be removed in a future
    GHC release, at which point these occurrences will be rejected as ambiguous.
    See the proposal `DuplicateRecordFields without ambiguous field access
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst>`_
    and the documentation on :extension:`DuplicateRecordFields` for further details.

    This warning has no effect when :extension:`DuplicateRecordFields` is
    disabled.

.. ghc-flag:: -Wforall-identifier
    :shortdesc: *(deprecated)* Does nothing
    :type: dynamic

    :since: 9.4

    This warning is deprecated. It no longer has any effect since GHC 9.10.

    In the past, GHC used to accept ``forall`` as a term-level identifier:
    ::

        -- from constraints-0.13
        forall :: forall p. (forall a. Dict (p a)) -> Dict (Forall p)
        forall d = ...

    In accordance with `GHC Proposal #281 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst>`__,
    this is no longer possible, as ``forall`` has become a proper keyword.
    :ghc-flag:`-Wforall-identifier` was used in the migration period before the
    breaking change took place.

.. ghc-flag:: -Wunicode-bidirectional-format-characters
    :shortdesc: warn about the usage of unicode bidirectional layout override characters
    :type: dynamic
    :category:

    :since: 9.0.2

    Explicit unicode bidirectional formatting characters can cause source code
    to be rendered misleadingly in many viewers. We warn if any such character
    is present in the source.

    Specifically, the characters disallowed by this warning
    are those which are a part of the 'Explicit Formatting`
    category of the `Unicode Bidirectional Character Type Listing
    <https://www.unicode.org/reports/tr9/#Bidirectional_Character_Types>`_

.. ghc-flag:: -Wgadt-mono-local-binds
    :shortdesc: warn when pattern matching on a GADT without MonoLocalBinds
    :type: dynamic
    :reverse: -Wno-gadt-mono-local-binds

    :since: 9.4.1

    This warning is triggered on pattern matching involving GADTs,
    if :extension:`MonoLocalBinds` is disabled.
    Type inference can be fragile in this case.

    See the `OutsideIn(X) <https://www.microsoft.com/en-us/research/publication/outsideinx-modular-type-inference-with-local-assumptions/>`__
    paper (section 4.2) and :ref:`mono-local-binds` for more details.

    To resolve this warning, you can enable :extension:`MonoLocalBinds`
    or an extension implying it (:extension:`GADTs` or
    :extension:`TypeFamilies`).

    The warning is also triggered when matching on GADT-like
    pattern synonyms (i.e. pattern synonyms containing equalities in provided
    constraints).

    In previous versions of GHC (9.2 and below), it was an error
    to pattern match on a GADT if neither :extension:`GADTs`
    nor :extension:`TypeFamilies` were enabled.

.. ghc-flag:: -Wtype-equality-out-of-scope
    :shortdesc: warn when type equality ``a ~ b`` is used despite being out of scope
    :type: dynamic
    :reverse: -Wno-type-equality-out-of-scope

    :since: 9.4.1
    :default: on

    In accordance with `GHC Proposal #371
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0371-non-magical-eq.md>`__,
    the type equality syntax ``a ~ b`` is no longer built-in. Instead, ``~`` is
    a regular type operator that can be imported from ``Data.Type.Equality`` or
    ``Prelude``.

    To minimize breakage, a compatibility fallback is provided: whenever ``~``
    is used but is not in scope, the compiler assumes that it stands for a type
    equality constraint. The warning is triggered by any code that relies on
    this fallback. It can be addressed by bringing ``~`` into scope explicitly.

    The likely culprit is that you use :extension:`NoImplicitPrelude` and a
    custom Prelude. In this case, consider updating your custom Prelude to
    re-export ``~`` from ``Data.Type.Equality``.

.. ghc-flag:: -Wtype-equality-requires-operators
    :shortdesc: warn when type equality ``a ~ b`` is used despite being out of scope
    :type: dynamic
    :reverse: -Wno-type-equality-requires-operators

    :since: 9.4.1

    In accordance with `GHC Proposal #371
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0371-non-magical-eq.md>`__,
    the type equality syntax ``a ~ b`` is no longer built-in. Instead, ``~`` is
    a regular type operator that requires the :extension:`TypeOperators` extension.

    To minimize breakage, ``~`` specifically (unlike other type operators) can
    be used even when :extension:`TypeOperators` is disabled. The warning is
    triggered whenever this happens, and can be addressed by enabling the
    extension.

.. ghc-flag:: -Wloopy-superclass-solve
    :shortdesc: *(deprecated)* warn when creating potentially-loopy superclass constraint evidence
    :type: dynamic
    :reverse: -Wno-loopy-superclass-solve

    :since: 9.6.1

    This warning is deprecated. It no longer has any effect since GHC 9.10.
    In the past, :extension:`UndecidableInstances` allowed potentially
    non-terminating evidence for certain superclass constraints.
    This is no longer allowed, as explained in :ref:`undecidable-instances`.
    This warning was used during the transition period.

.. ghc-flag:: -Wterm-variable-capture
    :shortdesc: warn when an implicitly quantified type variable captures a term's name
    :type: dynamic

    :since: 9.8.1

    Under :extension:`RequiredTypeArguments`, implicit quantification of type variables does not take place
    if there is a term variable of the same name in scope.

    For example: ::

       a = 15
       f :: a -> a    -- NoRequiredTypeArguments: The ‘a’ is implicitly quantified
                      -- RequiredTypeArguments:   The ‘a’ refers to the term-level binding

    When :ghc-flag:`-Wterm-variable-capture` is enabled, GHC warns against implicit quantification
    that would stop working under :extension:`RequiredTypeArguments`.

.. ghc-flag:: -Wmissing-role-annotations
    :shortdesc: warn when type declarations don't have role annotations
    :type: dynamic
    :reverse: -Wno-role-annotations-signatures
    :category:

    :since: 9.8.1
    :default: off

    .. index::
         single: roles, missing

    If you would like GHC to check that every data type definition
    has a :ref:`role annotation <role-annotations>`, use the
    :ghc-flag:`-Wmissing-role-annotations` option.
    You can specify the role via :extension:`RoleAnnotations`.

    GHC will not warn about type class definitions with missing role annotations,
    as their default roles are the strictest: all nominal.
    In other words the type-class role cannot be accidentally left
    representational or phantom, which could affected the code correctness.

.. ghc-flag:: -Wimplicit-rhs-quantification
    :shortdesc: warn when type variables on the RHS of a type synonym are implicitly quantified
    :type: dynamic
    :reverse: -Wno-implicit-rhs-quantification
    :category:

    :since: 9.8.1
    :default: off

    In accordance with `GHC Proposal #425
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0425-decl-invis-binders.rst>`__,
    GHC will stop implicitly quantifying over type variables that occur free on the
    right-hand side of a type synonym but are not mentioned on the left-hand side.
    Type synonym declarations that rely on this form of quantification should be rewritten with invisible binders.

    For example: ::

      type T1 :: forall a . Maybe a
      type T1    = 'Nothing :: Maybe a      -- old
      type T1 @a = 'Nothing :: Maybe a      -- new

    This warning detects code that will be affected by this breaking change.

.. ghc-flag:: -Wdeprecated-type-abstractions
    :shortdesc: warn when type abstractions in constructor patterns are used without enabling :extension:`TypeApplications`
    :type: dynamic
    :reverse: -Wno-deprecated-type-abstractions
    :category:

    :since: 9.10.1
    :default: off

    Type abstractions in constructor patterns allow binding existential type variables: ::

      import Type.Reflection (Typeable, typeRep)
      data Ex = forall e. (Typeable e, Show e) => MkEx e
      showEx (MkEx @e a) = show a ++ " :: " ++ show (typeRep @e)

    Note the pattern ``MkEx @e a``, and specifically the ``@e`` binder.

    Support for this feature was added to GHC in version 9.2, but instead of getting
    its own language extension the feature was enabled by a combination of
    :extension:`TypeApplications` and :extension:`ScopedTypeVariables`.
    As per `GHC Proposal #448
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst>`__
    and its amendment `#604 <https://github.com/ghc-proposals/ghc-proposals/pull/604>`__
    we are now transitioning towards guarding this feature behind :extension:`TypeAbstractions` instead.

    As a compatibility measure, GHC continues to support old programs that use type abstractions
    in constructor patterns without enabling the appropriate extension :extension:`TypeAbstractions`,
    but it will stop doing so in a future release.

    This warning detects code that will be affected by this breaking change.

.. ghc-flag:: -Wincomplete-export-warnings
    :shortdesc: warn when some but not all of exports for a name are warned about
    :type: dynamic
    :reverse: -Wno-incomplete-export-warnings

    :since: 9.8.1

    Ino accordance with `GHC Proposal #134
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0134-deprecating-exports-proposal.rst>`__,
    it is now possible to deprecate certain exports of a name without deprecating the name itself.

    As explained in :ref:`warning-deprecated-pragma`, when a name is exported in several ways in the same module,
    but only some of those ways have a warning, it will not end up deprecated when imported in another module.

    For example: ::

        module A (x) where

        x :: Int
        x = 2

        module M (
            {-# WARNING x "deprecated" #-} x
            module A
          )
        import A

     When :ghc-flag:`-Wincomplete-export-warnings` is enabled, GHC warns about exports
     that are not deprecating a name that is deprecated with another export in that module.

.. ghc-flag:: -Wbadly-staged-types
    :shortdesc: warn when type binding is used at the wrong TH stage.
    :type: dynamic
    :reverse: -Wno-badly-staged-types

    :since: 9.10.1

    Consider an example: ::

        tardy :: forall a. Proxy a -> IO Type
        tardy _ = [t| a |]

    The type binding ``a`` is bound at stage 1 but used on stage 2.

    This is badly staged program, and the ``tardy (Proxy @Int)`` won't produce
    a type representation of ``Int``, but rather a local name ``a``.

.. ghc-flag:: -Winconsistent-flags
    :shortdesc: warn when command line options are inconsistent in some way.
    :type: dynamic
    :reverse: -Wno-inconsistent-flags

    :since: 9.8.1
    :default: on

    Warn when command line options are inconsistent in some way.

    For example, when using GHCi, optimisation flags are ignored and a warning is
    issued. Another example is :ghc-flag:`-dynamic` is ignored when :ghc-flag:`-dynamic-too`
    is passed.

.. ghc-flag:: -Wdata-kinds-tc
    :shortdesc: warn when an illegal use of a type or kind without
                :extension:`DataKinds` is caught by the typechecker
    :type: dynamic
    :reverse: -Wno-data-kinds-tc

    :since: 9.10.1

    Introduced in GHC 9.10.1, this warns when an illegal use of a type or kind
    (without having enabled the :extension:`DataKinds` extension) is caught in
    the typechecker (hence the ``-tc`` suffix). These warnings complement the
    existing :extension:`DataKinds` checks (that have existed since
    :extension:`DataKinds` was first introduced), which result in errors
    instead of warnings.

    This warning is scheduled to be changed to an error in a future GHC
    version, at which point the :ghc-flag:`-Wdata-kinds-tc` flag will be
    removed. Users can enable the :extension:`DataKinds` extension to avoid
    issues (thus silencing the warning).

.. ghc-flag:: -Wdefaulted-exception-context
    :shortdesc: warn when an :base-ref:`Control.Exception.Context.ExceptionContext`
                implicit parameter is defaulted to
                :base-ref:`Control.Exception.Context.emptyExceptionContext`.
    :type: dynamic
    :reverse: -Wnop-defaulted-exception-context

    :since: 9.10.1

    Introduced in GHC 9.10.1 with the introduction of an implicit
    :base-ref:`Control.Exception.Context.ExceptionContext`` context to
    :base-ref:`Control.Exception.SomeException`. To preserve compatibility
    with earlier compilers, this constraints is implicitly defaulted to
    :base-ref:`Control.Exception.Context.emptyExceptionContext` when no other
    evidence is available. As this behavior may result in dropped exception context
    this warning is provided to give notice when defaulting occurs.

.. ghc-flag:: -Wview-pattern-signatures
    :shortdesc: warn when a view pattern is used with type signature without
                explicit parens
    :type: dynamic
    :reverse: -Wno-view-pattern-signatures

    :since: 9.12.1

    Introduced in GHC 9.12.1 in accordance with `GHC Proposal #281 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst>`__,
    this flag controls the warning about unparenthesized occurrences of pattern
    signatures on the RHS of view patterns. Example: ::

      f (g -> p :: t) = ()

    The way it is parsed will change in a future GHC release: ::

      f (g -> (p :: t)) = ()  -- current (legacy) parse
      f ((g -> p) :: t) = ()  -- future parse

    Unless ``g`` is an endofuction (i.e. its input and output have the same
    type), this will break the program.

    To make the code forwards-compatible and silence the warning, users are
    advised to add parentheses manually.

If you're feeling really paranoid, the :ghc-flag:`-dcore-lint` option is a good choice.
It turns on heavyweight intra-pass sanity-checking within GHC. (It checks GHC's
sanity, not yours.)

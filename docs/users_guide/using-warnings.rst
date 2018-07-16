.. _options-sanity:

Warnings and sanity-checking
----------------------------

.. index::
   single: sanity-checking options
   single: warnings

GHC has a number of options that select which types of non-fatal error
messages, otherwise known as warnings, can be generated during
compilation. By default, you get a standard set of warnings which are
generally likely to indicate bugs in your program. These are:

.. hlist::
    :columns: 3

    * :ghc-flag:`-Woverlapping-patterns`
    * :ghc-flag:`-Wwarnings-deprecations`
    * :ghc-flag:`-Wdeprecations`
    * :ghc-flag:`-Wdeprecated-flags`
    * :ghc-flag:`-Wunrecognised-pragmas`
    * :ghc-flag:`-Wduplicate-constraints`
    * :ghc-flag:`-Wduplicate-exports`
    * :ghc-flag:`-Woverflowed-literals`
    * :ghc-flag:`-Wempty-enumerations`
    * :ghc-flag:`-Wmissing-fields`
    * :ghc-flag:`-Wmissing-methods`
    * :ghc-flag:`-Wwrong-do-bind`
    * :ghc-flag:`-Wunsupported-calling-conventions`
    * :ghc-flag:`-Wdodgy-foreign-imports`
    * :ghc-flag:`-Winline-rule-shadowing`
    * :ghc-flag:`-Wunsupported-llvm-version`
    * :ghc-flag:`-Wtabs`
    * :ghc-flag:`-Wunrecognised-warning-flags`
    * :ghc-flag:`-Winaccessible-code`
    * :ghc-flag:`-Wstar-binder`

The following flags are simple ways to select standard "packages" of warnings:

.. ghc-flag:: -W
    :shortdesc: enable normal warnings
    :type: dynamic
    :reverse: -w
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

.. ghc-flag:: -Wall
    :shortdesc: enable almost all warnings (details in :ref:`options-sanity`)
    :type: dynamic
    :reverse: -w
    :category:

    Turns on all warning options that indicate potentially suspicious
    code. The warnings that are *not* enabled by :ghc-flag:`-Wall` are

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Wincomplete-uni-patterns`
        * :ghc-flag:`-Wincomplete-record-updates`
        * :ghc-flag:`-Wmonomorphism-restriction`
        * :ghc-flag:`-Wimplicit-prelude`
        * :ghc-flag:`-Wmissing-local-signatures`
        * :ghc-flag:`-Wmissing-exported-signatures`
        * :ghc-flag:`-Wmissing-export-lists`
        * :ghc-flag:`-Wmissing-import-lists`
        * :ghc-flag:`-Wmissing-home-modules`
        * :ghc-flag:`-Widentities`
        * :ghc-flag:`-Wredundant-constraints`
        * :ghc-flag:`-Wpartial-fields`

.. ghc-flag:: -Weverything
    :shortdesc: enable all warnings supported by GHC
    :type: dynamic
    :category:

    Turns on every single warning supported by the compiler.

.. ghc-flag:: -Wcompat
    :shortdesc: enable future compatibility warnings
        (details in :ref:`options-sanity`)
    :type: dynamic
    :reverse: -Wno-compat
    :category:

    Turns on warnings that will be enabled by default in the future, but remain
    off in normal compilations for the time being. This allows library authors
    eager to make their code future compatible to adapt to new features before
    they even generate warnings.

    This currently enables

    .. hlist::
        :columns: 3

        * :ghc-flag:`-Wmissing-monadfail-instances`
        * :ghc-flag:`-Wsemigroup`
        * :ghc-flag:`-Wnoncanonical-monoid-instances`
        * :ghc-flag:`-Wimplicit-kind-vars`

.. ghc-flag:: -Wno-compat
    :shortdesc: Disables all warnings enabled by :ghc-flag:`-Wcompat`.
    :type: dynamic
    :reverse: -Wcompat
    :category:

    Disables all warnings enabled by :ghc-flag:`-Wcompat`.

.. ghc-flag:: -w
    :shortdesc: disable all warnings
    :type: dynamic
    :category:

    Turns off all warnings, including the standard ones and those that
    :ghc-flag:`-Wall` doesn't enable.

These options control which warnings are considered fatal and cause compilation
to abort.

.. ghc-flag:: -Werror
    :shortdesc: make warnings fatal
    :type: dynamic
    :reverse: -Wwarn
    :category:

    Makes any warning into a fatal error. Useful so that you don't miss
    warnings when doing batch compilation.

.. ghc-flag:: -Werror=⟨wflag⟩
    :shortdesc: make a specific warning fatal
    :type: dynamic
    :reverse: -Wwarn=⟨wflag⟩
    :category:
    :noindex:

    :implies: ``-W<wflag>``

    Makes a specific warning into a fatal error. The warning will be enabled if
    it hasn't been enabled yet.

    ``-Werror=compat`` has the same effect as ``-Werror=...`` for each warning
    flag in the :ghc-flag:`-Wcompat` option group.

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

    Note that it doesn't fully negate the effects of ``-Werror=<wflag>`` - the
    warning will still be enabled.

    ``-Wwarn=compat`` has the same effect as ``-Wwarn=...`` for each warning
    flag in the :ghc-flag:`-Wcompat` option group.

When a warning is emitted, the specific warning flag which controls
it is shown.

.. ghc-flag:: -fshow-warning-groups
    :shortdesc: show which group an emitted warning belongs to.
    :type: dynamic
    :reverse: -fno-show-warning-groups
    :category:

    When showing which flag controls a warning, also show the
    respective warning group flag(s) that warning is contained in.

    This option is off by default.

The full set of warning options is described below. To turn off any
warning, simply give the corresponding ``-Wno-...`` option on the
command line. For backwards compatibility with GHC versions prior to 8.0,
all these warnings can still be controlled with ``-f(no-)warn-*`` instead
of ``-W(no-)*``.

.. ghc-flag:: -Wunrecognised-warning-flags
    :shortdesc: throw a warning when an unreconised ``-W...`` flag is
        encountered on the command line.
    :type: dynamic
    :reverse: -Wno-unrecognised-warning-flags
    :category:

    Enables warnings when the compiler encounters a ``-W...`` flag that is not
    recognised.

    This warning is on by default.

.. ghc-flag:: -Wtyped-holes
    :shortdesc: Report warnings when :ref:`typed hole <typed-holes>` errors are
        :ref:`deferred until runtime <defer-type-errors>`. See
        :ghc-flag:`-fdefer-typed-holes`.
    :type: dynamic
    :reverse: -Wno-typed-holes
    :category:

    Determines whether the compiler reports typed holes warnings. Has no
    effect unless typed holes errors are deferred until runtime. See
    :ref:`typed-holes` and :ref:`defer-type-errors`

    This warning is on by default.

.. ghc-flag:: -Wdeferred-type-errors
    :shortdesc: Report warnings when :ref:`deferred type errors
        <defer-type-errors>` are enabled. This option is enabled by
        default. See :ghc-flag:`-fdefer-type-errors`.
    :type: dynamic
    :reverse: -Wno-deferred-type-errors
    :category:

    Causes a warning to be reported when a type error is deferred until
    runtime. See :ref:`defer-type-errors`

    This warning is on by default.

.. ghc-flag:: -fdefer-type-errors
    :shortdesc: Turn type errors into warnings, :ref:`deferring the error until
        runtime <defer-type-errors>`. Implies
        :ghc-flag:`-fdefer-typed-holes` and
        :ghc-flag:`-fdefer-out-of-scope-variables`.
        See also :ghc-flag:`-Wdeferred-type-errors`
    :type: dynamic
    :reverse: -fno-defer-type-errors
    :category:

    :implies: :ghc-flag:`-fdefer-typed-holes`, :ghc-flag:`-fdefer-out-of-scope-variables`

    Defer as many type errors as possible until runtime. At compile time
    you get a warning (instead of an error). At runtime, if you use a
    value that depends on a type error, you get a runtime error; but you
    can run any type-correct parts of your code just fine. See
    :ref:`defer-type-errors`

.. ghc-flag:: -fdefer-typed-holes
    :shortdesc: Convert :ref:`typed hole <typed-holes>` errors into warnings,
        :ref:`deferring the error until runtime <defer-type-errors>`.
        Implied by :ghc-flag:`-fdefer-type-errors`.
        See also :ghc-flag:`-Wtyped-holes`.
    :type: dynamic
    :reverse: -fno-defer-typed-holes
    :category:

    Defer typed holes errors (errors about names with a leading underscore
    (e.g., “_”, “_foo”, “_bar”)) until runtime. This will turn the errors
    produced by :ref:`typed holes <typed-holes>` into warnings. Using a value
    that depends on a typed hole produces a runtime error, the same as
    :ghc-flag:`-fdefer-type-errors` (which implies this option). See :ref:`typed-holes`
    and :ref:`defer-type-errors`.

    Implied by :ghc-flag:`-fdefer-type-errors`. See also :ghc-flag:`-Wtyped-holes`.

.. ghc-flag:: -fdefer-out-of-scope-variables
    :shortdesc: Convert variable out of scope variables errors into warnings.
        Implied by :ghc-flag:`-fdefer-type-errors`.
        See also :ghc-flag:`-Wdeferred-out-of-scope-variables`.
    :type: dynamic
    :reverse: -fno-defer-out-of-scope-variables
    :category:

    Defer variable out-of-scope errors (errors about names without a leading underscore)
    until runtime. This will turn variable-out-of-scope errors into warnings.
    Using a value that depends on a typed hole produces a runtime error,
    the same as :ghc-flag:`-fdefer-type-errors` (which implies this option).
    See :ref:`typed-holes` and :ref:`defer-type-errors`.

    Implied by :ghc-flag:`-fdefer-type-errors`. See also :ghc-flag:`-Wdeferred-out-of-scope-variables`.

.. ghc-flag:: -Wdeferred-out-of-scope-variables
    :shortdesc: Report warnings when variable out-of-scope errors are
        :ref:`deferred until runtime <defer-type-errors>`.
        See :ghc-flag:`-fdefer-out-of-scope-variables`.
    :type: dynamic
    :reverse: -Wno-deferred-out-of-scope-variables
    :category:

    Warn when a deferred out-of-scope variable is encountered.

.. ghc-flag:: -Wpartial-type-signatures
    :shortdesc: warn about holes in partial type signatures when
        :ghc-flag:`-XPartialTypeSignatures` is enabled. Not applicable when
        :ghc-flag:`-XPartialTypesignatures` is not enabled, in which case
        errors are generated for such holes. See
        :ref:`partial-type-signatures`.
    :type: dynamic
    :reverse: -Wno-partial-type-signatures
    :category:

    Determines whether the compiler reports holes in partial type
    signatures as warnings. Has no effect unless
    :ghc-flag:`-XPartialTypeSignatures` is enabled, which controls whether
    errors should be generated for holes in types or not. See
    :ref:`partial-type-signatures`.

    This warning is on by default.

.. ghc-flag:: -fhelpful-errors
    :shortdesc: Make suggestions for mis-spelled names.
    :type: dynamic
    :reverse: -fno-helpful-errors
    :category:

    When a name or package is not found in scope, make suggestions for
    the name or package you might have meant instead.

    This option is on by default.

.. ghc-flag:: -Wunrecognised-pragmas
    :shortdesc: warn about uses of pragmas that GHC doesn't recognise
    :type: dynamic
    :reverse: -Wno-unrecognised-pragmas
    :category:

    Causes a warning to be emitted when a pragma that GHC doesn't
    recognise is used. As well as pragmas that GHC itself uses, GHC also
    recognises pragmas known to be used by other tools, e.g.
    ``OPTIONS_HUGS`` and ``DERIVE``.

    This option is on by default.

.. ghc-flag:: -Wmissed-specialisations
    :shortdesc: warn when specialisation of an imported, overloaded function
        fails.
    :type: dynamic
    :reverse: -Wno-missed-specialisations
    :category:

    Emits a warning if GHC cannot specialise an overloaded function, usually
    because the function needs an ``INLINABLE`` pragma. Reports when the
    situation arises during specialisation of an imported function.

    This form is intended to catch cases where an imported function
    that is marked as ``INLINABLE`` (presumably to enable specialisation)
    cannot be specialised as it calls other functions that are themselves not
    specialised.

    Note that this warning will not throw errors if used with
    :ghc-flag:`-Werror`.

    This option is off by default.

.. ghc-flag:: -Wall-missed-specialisations
    :shortdesc: warn when specialisation of any overloaded function fails.
    :type: dynamic
    :reverse: -Wno-all-missed-specialisations
    :category:

    Emits a warning if GHC cannot specialise an overloaded function, usually
    because the function needs an ``INLINABLE`` pragma. Reports
    all such situations.

    Note that this warning will not throw errors if used with
    :ghc-flag:`-Werror`.

    This option is off by default.

.. ghc-flag:: -Wwarnings-deprecations
    :shortdesc: warn about uses of functions & types that have warnings or
        deprecated pragmas
    :type: dynamic
    :reverse: -Wno-warnings-deprecations
    :category:

    .. index::
       pair: deprecations; warnings

    Causes a warning to be emitted when a module, function or type with
    a ``WARNING`` or ``DEPRECATED pragma`` is used. See
    :ref:`warning-deprecated-pragma` for more details on the pragmas.

    This option is on by default.

.. ghc-flag:: -Wdeprecations
    :shortdesc: warn about uses of functions & types that have warnings or
        deprecated pragmas. Alias for :ghc-flag:`-Wwarnings-deprecations`
    :type: dynamic
    :reverse: -Wno-deprecations
    :category:

    .. index::
       single: deprecations

    Causes a warning to be emitted when a module, function or type with
    a ``WARNING`` or ``DEPRECATED pragma`` is used. See
    :ref:`warning-deprecated-pragma` for more details on the pragmas.
    An alias for :ghc-flag:`-Wwarnings-deprecations`.

    This option is on by default.

.. ghc-flag:: -Wnoncanonical-monad-instances
    :shortdesc: warn when ``Applicative`` or ``Monad`` instances have
        noncanonical definitions of ``return``, ``pure``, ``(>>)``,
        or ``(*>)``.
        See flag description in :ref:`options-sanity` for more details.
    :type: dynamic
    :reverse: -Wno-noncanonical-monad-instances
    :category:

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

    This option is off by default.

.. ghc-flag:: -Wnoncanonical-monadfail-instances
    :shortdesc: warn when ``Monad`` or ``MonadFail`` instances have
        noncanonical definitions of ``fail``.
        See flag description in :ref:`options-sanity` for more details.
    :type: dynamic
    :reverse: -Wno-noncanonical-monadfail-instances
    :category:

    Warn if noncanonical ``Monad`` or ``MonadFail`` instances
    declarations are detected.

    When this warning is enabled, the following conditions are verified:

    In ``Monad`` instances declarations warn if any of the following
    conditions does not hold:

     * If ``fail`` is defined it must be canonical
       (i.e. ``fail = Control.Monad.Fail.fail``).

    Moreover, in ``MonadFail`` instance declarations:

     * Warn if ``fail`` is defined backwards
       (i.e. ``fail = Control.Monad.fail``).

    See also :ghc-flag:`-Wmissing-monadfail-instances`.

    This option is off by default.

.. ghc-flag:: -Wnoncanonical-monoid-instances
    :shortdesc: warn when ``Semigroup`` or ``Monoid`` instances have
        noncanonical definitions of ``(<>)`` or ``mappend``.
        See flag description in :ref:`options-sanity` for more details.
    :type: dynamic
    :reverse: -Wno-noncanonical-monoid-instances
    :category:

    Warn if noncanonical ``Semigroup`` or ``Monoid`` instances
    declarations are detected.

    When this warning is enabled, the following conditions are verified:

    In ``Monoid`` instances declarations warn if any of the following
    conditions does not hold:

     * If ``mappend`` is defined it must be canonical
       (i.e. ``mappend = (Data.Semigroup.<>)``).

    Moreover, in ``Semigroup`` instance declarations:

     * Warn if ``(<>)`` is defined backwards (i.e. ``(<>) = mappend``).

    This warning is off by default. However, it is part of the
    :ghc-flag:`-Wcompat` option group.

.. ghc-flag:: -Wmissing-monadfail-instances
    :shortdesc: Warn when a failable pattern is used in a do-block that does
        not have a ``MonadFail`` instance.
    :type: dynamic
    :reverse: -Wno-missing-monadfail-instances
    :category:

    .. index::
       single: MFP
       single: MonadFail Proposal

    Warn when a failable pattern is used in a do-block that does not have a
    ``MonadFail`` instance.

    See also :ghc-flag:`-Wnoncanonical-monadfail-instances`.

    Being part of the :ghc-flag:`-Wcompat` option group, this warning is off by
    default, but will be switched on in a future GHC release, as part of
    the `MonadFail Proposal (MFP)
    <https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail>`__.

.. ghc-flag:: -Wsemigroup
    :shortdesc: warn when a ``Monoid`` is not ``Semigroup``, and on non-
        ``Semigroup`` definitions of ``(<>)``?
    :type: dynamic
    :reverse: -Wno-semigroup
    :category:

    .. index::
       single: semigroup; warning

    Warn when definitions are in conflict with the future inclusion of
    ``Semigroup`` into the standard typeclasses.

     1. Instances of ``Monoid`` should also be instances of ``Semigroup``
     2. The ``Semigroup`` operator ``(<>)`` will be in ``Prelude``, which
        clashes with custom local definitions of such an operator

    Being part of the :ghc-flag:`-Wcompat` option group, this warning is off by
    default, but will be switched on in a future GHC release.

.. ghc-flag:: -Wdeprecated-flags
    :shortdesc: warn about uses of commandline flags that are deprecated
    :type: dynamic
    :reverse: -Wno-deprecated-flags
    :category:

    .. index::
       single: deprecated flags

    Causes a warning to be emitted when a deprecated command-line flag
    is used.

    This option is on by default.

.. ghc-flag:: -Wunsupported-calling-conventions
    :shortdesc: warn about use of an unsupported calling convention
    :type: dynamic
    :reverse: -Wno-unsupported-calling-conventions
    :category:

    Causes a warning to be emitted for foreign declarations that use
    unsupported calling conventions. In particular, if the ``stdcall``
    calling convention is used on an architecture other than i386 then
    it will be treated as ``ccall``.

.. ghc-flag:: -Wdodgy-foreign-imports
    :shortdesc: warn about dodgy foreign imports
    :type: dynamic
    :reverse: -Wno-dodgy-foreign-import
    :category:

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

    Causes a warning to be emitted if a literal will overflow, e.g.
    ``300 :: Word8``.

.. ghc-flag:: -Wempty-enumerations
    :shortdesc: warn about enumerations that are empty
    :type: dynamic
    :reverse: -Wno-empty-enumerations
    :category:

    Causes a warning to be emitted if an enumeration is empty, e.g.
    ``[5 .. 3]``.

.. ghc-flag:: -Wduplicate-constraints
    :shortdesc: warn when a constraint appears duplicated in a type signature
    :type: dynamic
    :reverse: -Wno-duplicate-constraints
    :category:

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

    .. index::
       single: duplicate exports, warning
       single: export lists, duplicates

    Have the compiler warn about duplicate entries in export lists. This
    is useful information if you maintain large export lists, and want
    to avoid the continued export of a definition after you've deleted
    (one) mention of it in the export list.

    This option is on by default.

.. ghc-flag:: -Whi-shadowing
    :shortdesc: warn when a ``.hi`` file in the current directory shadows a library
    :type: dynamic
    :reverse: -Wno-hi-shadowing
    :category:

    .. index::
       single: shadowing; interface files

    Causes the compiler to emit a warning when a module or interface
    file in the current directory is shadowing one with the same module
    name in a library or other directory.

.. ghc-flag:: -Widentities
    :shortdesc: warn about uses of Prelude numeric conversions that are probably
        the identity (and hence could be omitted)
    :type: dynamic
    :reverse: -Wno-identities
    :category:

    Causes the compiler to emit a warning when a Prelude numeric
    conversion converts a type ``T`` to the same type ``T``; such calls are
    probably no-ops and can be omitted. The functions checked for are:
    ``toInteger``, ``toRational``, ``fromIntegral``, and ``realToFrac``.

.. ghc-flag:: -Wimplicit-prelude
    :shortdesc: warn when the Prelude is implicitly imported
    :type: dynamic
    :reverse: -Wno-implicit-prelude
    :category:

    .. index::
       single: implicit prelude, warning

    Have the compiler warn if the Prelude is implicitly imported. This happens
    unless either the Prelude module is explicitly imported with an ``import
    ... Prelude ...`` line, or this implicit import is disabled (either by
    :ghc-flag:`-XNoImplicitPrelude` or a ``LANGUAGE NoImplicitPrelude``
    pragma).

    Note that no warning is given for syntax that implicitly refers to the
    Prelude, even if :ghc-flag:`-XNoImplicitPrelude` would change whether it
    refers to the Prelude. For example, no warning is given when ``368`` means
    ``Prelude.fromInteger (368::Prelude.Integer)`` (where ``Prelude`` refers
    to the actual Prelude module, regardless of the imports of the module
    being compiled).

    This warning is off by default.

.. ghc-flag:: -Wimplicit-kind-vars
    :shortdesc: warn when kind variables are brought into scope implicitly despite
        the "forall-or-nothing" rule
    :type: dynamic
    :reverse: -Wno-implicit-kind-vars
    :category:

    :since: 8.6

    `GHC proposal #24
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0024-no-kind-vars.rst>`__
    prescribes to treat kind variables and type variables identically in
    ``forall``, removing the legacy distinction between them.

    Consider the following examples: ::

        f :: Proxy a -> Proxy b -> ()
        g :: forall a b. Proxy a -> Proxy b -> ()

    ``f`` does not use an explicit ``forall``, so type variables ``a`` and ``b``
    are brought into scope implicitly. ``g`` quantifies both ``a`` and ``b``
    explicitly. Both ``f`` and ``g`` work today and will continue to work in the
    future because they adhere to the "forall-or-nothing" rule: either all type
    variables in a function definition are introduced explicitly or implicitly,
    there is no middle ground.

    A violation of the "forall-or-nothing" rule looks like this: ::

        m :: forall a. Proxy a -> Proxy b -> ()

    ``m`` does not introduce one of the variables, ``b``, and thus is rejected.

    However, consider the following example: ::

        n :: forall a. Proxy (a :: k) -> ()

    While ``n`` uses ``k`` without introducing it and thus violates the rule, it
    is currently accepted. This is because ``k`` in ``n`` is considered a kind
    variable, as it occurs in a kind signature. In reality, the line between
    type variables and kind variables is blurry, as the following example
    demonstrates: ::

        kindOf :: forall a. Proxy (a :: k) -> Proxy k

    In ``kindOf``, the ``k`` variable is used both in a kind position and a type
    position. Currently, ``kindOf`` happens to be accepted as well.

    In a future release of GHC, both ``n`` and ``kindOf`` will be rejected per
    the "forall-or-nothing" rule. This warning, being part of the
    :ghc-flag:`-Wcompat` option group, allows to detect this before the actual
    breaking change takes place.

.. ghc-flag:: -Wincomplete-patterns
    :shortdesc: warn when a pattern match could fail
    :type: dynamic
    :reverse: -Wno-incomplete-patterns
    :category:

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
    :shortdesc: warn when a pattern match in a lambda expression or
        pattern binding could fail
    :type: dynamic
    :reverse: -Wno-incomplete-uni-patterns
    :category:

    The flag :ghc-flag:`-Wincomplete-uni-patterns` is similar to
    :ghc-flag:`-Wincomplete-patterns`, except that it applies only to
    lambda-expressions and pattern bindings, constructs that only allow a
    single pattern: ::

        h = \[] -> 2
        Just k = f y

.. ghc-flag:: -fmax-pmcheck-iterations=⟨n⟩
    :shortdesc: the iteration limit for the pattern match checker
    :type: dynamic
    :category:

    :default: 2000000

    Sets how many iterations of the pattern-match checker will perform before
    giving up. This limit is to catch cases where pattern-match checking might
    be excessively costly (due to the exponential complexity of coverage
    checking in the general case). It typically shouldn't be necessary to set
    this unless GHC informs you that it has exceeded the pattern match checker's
    iteration limit (in which case you may want to consider refactoring your
    pattern match, for the sake of future readers of your code.

.. ghc-flag:: -Wincomplete-record-updates
    :shortdesc: warn when a record update could fail
    :type: dynamic
    :reverse: -Wno-incomplete-record-updates
    :category:

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

.. ghc-flag:: -Wmissing-fields
    :shortdesc: warn when fields of a record are uninitialised
    :type: dynamic
    :reverse: -Wno-missing-fields
    :category:

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
    :reverse: -fnowarn-missing-export-lists
    :category:

    :since: 8.4.1

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
    :reverse: -fnowarn-missing-import-lists
    :category:

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

    .. index::
       single: missing methods, warning
       single: methods, missing

    This option is on by default, and warns you whenever an instance
    declaration is missing one or more methods, and the corresponding
    class declaration has no default declaration for them.

    The ``MINIMAL`` pragma can be used to change which combination of
    methods will be required for instances of a particular class. See
    :ref:`minimal-pragma`.

.. ghc-flag:: -Wmissing-signatures
    :shortdesc: warn about top-level functions without signatures
    :type: dynamic
    :reverse: -Wno-missing-signatures
    :category:

    .. index::
       single: type signatures, missing

    If you would like GHC to check that every top-level function/value
    has a type signature, use the :ghc-flag:`-Wmissing-signatures` option.
    As part of the warning GHC also reports the inferred type. The
    option is off by default.

.. ghc-flag:: -Wmissing-exported-sigs
    :shortdesc: *(deprecated)*
        warn about top-level functions without signatures, only if they
        are exported. takes precedence over -Wmissing-signatures
    :type: dynamic
    :reverse: -Wno-missing-exported-sigs
    :category:

    .. index::
       single: type signatures, missing

    This option is now deprecated in favour of
    :ghc-flag:`-Wmissing-exported-signatures`.

.. ghc-flag:: -Wmissing-exported-signatures
    :shortdesc: warn about top-level functions without signatures, only if they
        are exported. takes precedence over -Wmissing-signatures
    :type: dynamic
    :reverse: -Wno-missing-exported-signatures
    :category:

    .. index::
       single: type signatures, missing

    If you would like GHC to check that every exported top-level
    function/value has a type signature, but not check unexported
    values, use the :ghc-flag:`-Wmissing-exported-signatures`
    option. This option takes precedence over
    :ghc-flag:`-Wmissing-signatures`. As part of the warning GHC also
    reports the inferred type. The option is off by default.

.. ghc-flag:: -Wmissing-local-sigs
    :shortdesc: *(deprecated)*
        warn about polymorphic local bindings without signatures
    :type: dynamic
    :reverse: -Wno-missing-local-sigs
    :category:

    .. index::
       single: type signatures, missing

    This option is now deprecated in favour of
    :ghc-flag:`-Wmissing-local-signatures`.

.. ghc-flag:: -Wmissing-local-signatures
    :shortdesc: warn about polymorphic local bindings without signatures
    :type: dynamic
    :reverse: -Wno-missing-local-signatures
    :category:

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

    .. index::
         single: type signatures, missing, pattern synonyms

    If you would like GHC to check that every pattern synonym has a
    type signature, use the
    :ghc-flag:`-Wmissing-pattern-synonym-signatures` option. If this
    option is used in conjunction with
    :ghc-flag:`-Wmissing-exported-signatures` then only exported pattern
    synonyms must have a type signature. GHC also reports the inferred
    type. This option is off by default.

.. ghc-flag:: -Wname-shadowing
    :shortdesc: warn when names are shadowed
    :type: dynamic
    :reverse: -Wno-name-shadowing
    :category:

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

.. ghc-flag:: -Winaccessible-code
    :shortdesc: warn about inaccessible code
    :type: dynamic
    :reverse: -Wno-inaccessible-code
    :category:

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

.. ghc-flag:: -Wstar-binder
     :shortdesc: warn about binding the ``(*)`` type operator despite
         :ghc-flag:`-XStarIsType`
     :type: dynamic
     :reverse: -Wno-star-binder

     Under :ghc-flag:`-XStarIsType`, a ``*`` in types is not an operator nor
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
    :shortdesc: 2arn about class constraints in a type signature that can
        be simplified using a top-level instance declaration.
    :type: dynamic
    :reverse: -Wno-overlapping-patterns
    :category:

    :since: 8.2

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

    This option is on by default. As usual you can suppress it on a
    per-module basis with :ghc-flag:`-Wno-simplifiable-class-constraints
    <-Wsimplifiable-class-constraints>`.

.. ghc-flag:: -Wtabs
    :shortdesc: warn if there are tabs in the source file
    :type: dynamic
    :reverse: -Wno-tabs
    :category:

    .. index::
       single: tabs, warning

    Have the compiler warn if there are tabs in your source file.

.. ghc-flag:: -Wtype-defaults
    :shortdesc: warn when defaulting happens
    :type: dynamic
    :reverse: -Wno-type-defaults
    :category:

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

    This warning is off by default.

.. ghc-flag:: -Wmonomorphism-restriction
    :shortdesc: warn when the Monomorphism Restriction is applied
    :type: dynamic
    :reverse: -Wno-monomorphism-restriction
    :category:

    .. index::
       single: monomorphism restriction, warning

    Have the compiler warn/inform you where in your source the Haskell
    Monomorphism Restriction is applied. If applied silently the MR can
    give rise to unexpected behaviour, so it can be helpful to have an
    explicit warning that it is being applied.

    This warning is off by default.

.. ghc-flag:: -Wunsupported-llvm-version
    :shortdesc: Warn when using :ghc-flag:`-fllvm` with an unsupported
        version of LLVM.
    :type: dynamic
    :reverse: -Wno-monomorphism-restriction
    :category:

    Warn when using :ghc-flag:`-fllvm` with an unsupported version of LLVM.

.. ghc-flag:: -Wunticked-promoted-constructors
    :shortdesc: warn if promoted constructors are not ticked
    :type: dynamic
    :reverse: -Wno-unticked-promoted-constructors
    :category:

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

    This warning is enabled by default in :ghc-flag:`-Wall` mode.

.. ghc-flag:: -Wunused-binds
    :shortdesc: warn about bindings that are unused. Alias for
        :ghc-flag:`-Wunused-top-binds`, :ghc-flag:`-Wunused-local-binds` and
        :ghc-flag:`-Wunused-pattern-binds`
    :type: dynamic
    :reverse: -Wno-unused-binds
    :category:

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
    :shortdesc: warn about unused type variables which arise from patterns
        in type family and data family instances
    :type: dynamic
    :reverse: -Wno-unused-type-patterns
    :category:

    .. index::
       single: unused type patterns, warning
       single: type patterns, unused

    Report all unused type variables which arise from patterns in type family
    and data family instances. For instance: ::

        type instance F x y = []

    would report ``x`` and ``y`` as unused. The warning is suppressed if the
    type variable name begins with an underscore, like so: ::

        type instance F _x _y = []

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

    .. index::
       single: unused foralls, warning
       single: foralls, unused

    Report all unused type variables which arise from explicit, user-written
    ``forall`` statements. For instance: ::

        g :: forall a b c. (b -> b)

    would report ``a`` and ``c`` as unused.

.. ghc-flag:: -Wwrong-do-bind
    :shortdesc: warn about do bindings that appear to throw away monadic values
        that you should have bound instead
    :type: dynamic
    :reverse: -Wno-wrong-do-bind
    :category:

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

    The option :ghc-flag:`-Wpartial-fields` warns about record fields that could
    fail when accessed via a lacking constructor. The function ``f`` below will
    fail when applied to ``Bar``, so the compiler will emit a warning at its
    definition when :ghc-flag:`-Wpartial-fields` is enabled.

    The warning is suppressed if the field name begins with an underscore. ::

        data Foo = Foo { f :: Int } | Bar

If you're feeling really paranoid, the :ghc-flag:`-dcore-lint` option is a good choice.
It turns on heavyweight intra-pass sanity-checking within GHC. (It checks GHC's
sanity, not yours.)

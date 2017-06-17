module Options.Warnings where

import Types

warningsOptions :: [Flag]
warningsOptions =
  [ flag { flagName = "-W"
         , flagDescription = "enable normal warnings"
         , flagType = DynamicFlag
         , flagReverse = "-w"
         }
  , flag { flagName = "-w"
         , flagDescription = "disable all warnings"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-Wall"
         , flagDescription =
           "enable almost all warnings (details in :ref:`options-sanity`)"
         , flagType = DynamicFlag
         , flagReverse = "-w"
         }
  , flag { flagName = "-Wcompat"
         , flagDescription =
           "enable future compatibility warnings " ++
           "(details in :ref:`options-sanity`)"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-compat"
         }
  , flag { flagName = "-Werror"
         , flagDescription = "make warnings fatal"
         , flagType = DynamicFlag
         , flagReverse = "-Wwarn"
         }
  , flag { flagName = "-Werror=<wflag>"
         , flagDescription = "make a specific warning fatal"
         , flagType = DynamicFlag
         , flagReverse = "-Wwarn=<wflag>"
         }
  , flag { flagName = "-Wwarn"
         , flagDescription = "make warnings non-fatal"
         , flagType = DynamicFlag
         , flagReverse = "-Werror"
         }
  , flag { flagName = "-Wwarn=<wflag>"
         , flagDescription = "make a specific warning non-fatal"
         , flagType = DynamicFlag
         , flagReverse = "-Werror=<wflag>"
         }
  , flag { flagName = "-Wunrecognised-warning-flags"
         , flagDescription =
           "throw a warning when an unreconised ``-W...`` flag is "++
           "encountered on the command line."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unrecognised-warning-flags"
         }
  , flag { flagName = "-fshow-warning-groups"
         , flagDescription = "show which group an emitted warning belongs to."
         , flagType = DynamicFlag
         , flagReverse = "-fno-show-warning-groups"
         }
  , flag { flagName = "-fdefer-type-errors"
         , flagDescription =
           "Turn type errors into warnings, :ref:`deferring the error until "++
           "runtime <defer-type-errors>`. Implies "++
           ":ghc-flag:`-fdefer-typed-holes` and "++
           ":ghc-flag:`-fdefer-out-of-scope-variables`. "++
           "See also :ghc-flag:`-Wdeferred-type-errors`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-defer-type-errors"
         }
  , flag { flagName = "-fdefer-typed-holes"
         , flagDescription =
           "Convert :ref:`typed hole <typed-holes>` errors into warnings, "++
           ":ref:`deferring the error until runtime <defer-type-errors>`. "++
           "Implied by :ghc-flag:`-fdefer-type-errors`. "++
           "See also :ghc-flag:`-Wtyped-holes`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-defer-typed-holes"
         }
  , flag { flagName = "-fdefer-out-of-scope-variables"
         , flagDescription =
           "Convert variable out of scope variables errors into warnings. "++
           "Implied by :ghc-flag:`-fdefer-type-errors`. "++
           "See also :ghc-flag:`-Wdeferred-out-of-scope-variables`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-defer-out-of-scope-variables"
         }
  , flag { flagName = "-fhelpful-errors"
         , flagDescription = "Make suggestions for mis-spelled names."
         , flagType = DynamicFlag
         , flagReverse = "-fno-helpful-errors"
         }
  , flag { flagName = "-freverse-errors"
         , flagDescription =
           "Display errors in GHC/GHCi sorted by reverse order of "++
           "source code line numbers."
         , flagType = DynamicFlag
         , flagReverse = "-fno-reverse-errors"
         }
  , flag { flagName = "-fmax-errors"
         , flagDescription =
           "Limit the number of errors displayed in GHC/GHCi."
         , flagType = DynamicFlag
         , flagReverse = "-fno-max-errors"
         }
  , flag { flagName = "-Wdeprecated-flags"
         , flagDescription =
           "warn about uses of commandline flags that are deprecated"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-deprecated-flags"
         }
  , flag { flagName = "-Wduplicate-constraints"
         , flagDescription =
           "warn when a constraint appears duplicated in a type signature"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-duplicate-constraints"
         }
  , flag { flagName = "-Wduplicate-exports"
         , flagDescription = "warn when an entity is exported multiple times"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-duplicate-exports"
         }
  , flag { flagName = "-Whi-shadowing"
         , flagDescription =
           "warn when a ``.hi`` file in the current directory shadows a library"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-hi-shadowing"
         }
  , flag { flagName = "-Widentities"
         , flagDescription =
           "warn about uses of Prelude numeric conversions that are probably "++
           "the identity (and hence could be omitted)"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-identities"
         }
  , flag { flagName = "-Wimplicit-prelude"
         , flagDescription = "warn when the Prelude is implicitly imported"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-implicit-prelude"
         }
  , flag { flagName = "-Wincomplete-patterns"
         , flagDescription = "warn when a pattern match could fail"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-incomplete-patterns"
         }
  , flag { flagName = "-Wincomplete-uni-patterns"
         , flagDescription =
           "warn when a pattern match in a lambda expression or "++
           "pattern binding could fail"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-incomplete-uni-patterns"
         }
  , flag { flagName = "-Wmax-pmcheck-iterations=<N>"
         , flagDescription =
           "the iteration limit for the pattern match checker"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-Wincomplete-record-updates"
         , flagDescription = "warn when a record update could fail"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-incomplete-record-updates"
         }
  , flag { flagName = "-Wmissing-fields"
         , flagDescription = "warn when fields of a record are uninitialised"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-fields"
         }
  , flag { flagName = "-Wmissing-import-lists"
         , flagDescription =
           "warn when an import declaration does not explicitly list all the"++
           "names brought into scope"
         , flagType = DynamicFlag
         , flagReverse = "-fnowarn-missing-import-lists"
         }
  , flag { flagName = "-Wmissing-methods"
         , flagDescription = "warn when class methods are undefined"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-methods"
         }
  , flag { flagName = "-Wmissing-signatures"
         , flagDescription = "warn about top-level functions without signatures"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-signatures"
         }
  , flag { flagName = "-Wmissing-exported-sigs"
         , flagDescription =
           "*(deprecated)* "++
           "warn about top-level functions without signatures, only if they "++
           "are exported. takes precedence over -Wmissing-signatures"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-exported-sigs"
         }
  , flag { flagName = "-Wmissing-exported-signatures"
         , flagDescription =
           "warn about top-level functions without signatures, only if they "++
           "are exported. takes precedence over -Wmissing-signatures"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-exported-signatures"
         }
  , flag { flagName = "-Wmissing-local-sigs"
         , flagDescription =
           "*(deprecated)* "++
           "warn about polymorphic local bindings without signatures"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-local-sigs"
         }
  , flag { flagName = "-Wmissing-local-signatures"
         , flagDescription =
           "warn about polymorphic local bindings without signatures"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-local-signatures"
         }
  , flag { flagName = "-Wmissing-monadfail-instances"
         , flagDescription =
           "warn when a failable pattern is used in a do-block that does " ++
           "not have a ``MonadFail`` instance."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-monadfail-instances"
         }
  , flag { flagName = "-Wsemigroup"
         , flagDescription =
           "warn when a ``Monoid`` is not ``Semigroup``, and on non-" ++
           "``Semigroup`` definitions of ``(<>)``?"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-semigroup"
         }
  , flag { flagName = "-Wmissed-specialisations"
         , flagDescription =
           "warn when specialisation of an imported, overloaded function fails."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missed-specialisations"
         }
  , flag { flagName = "-Wall-missed-specialisations"
         , flagDescription =
           "warn when specialisation of any overloaded function fails."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-all-missed-specialisations"
         }
  , flag { flagName = "-Wmonomorphism-restriction"
         , flagDescription = "warn when the Monomorphism Restriction is applied"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-monomorphism-restriction"
         }
  , flag { flagName = "-Wname-shadowing"
         , flagDescription = "warn when names are shadowed"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-name-shadowing"
         }
  , flag { flagName = "-Wnoncanonical-monad-instances"
         , flagDescription =
           "warn when ``Applicative`` or ``Monad`` instances have "++
           "noncanonical definitions of ``return``, ``pure``, ``(>>)``, "++
           "or ``(*>)``. "++
           "See flag description in :ref:`options-sanity` for more details."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-noncanonical-monad-instances"
         }
  , flag { flagName = "-Wnoncanonical-monadfail-instances"
         , flagDescription =
           "warn when ``Monad`` or ``MonadFail`` instances have "++
           "noncanonical definitions of ``fail``."++
           "See flag description in :ref:`options-sanity` for more details."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-noncanonical-monadfail-instances"
         }
  , flag { flagName = "-Wnoncanonical-monoid-instances"
         , flagDescription =
           "warn when ``Semigroup`` or ``Monoid`` instances have "++
           "noncanonical definitions of ``(<>)`` or ``mappend``. "++
           "See flag description in :ref:`options-sanity` for more details."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-noncanonical-monoid-instances"
         }
  , flag { flagName = "-Worphans"
         , flagDescription =
           "warn when the module contains :ref:`orphan instance declarations "++
           "or rewrite rules <orphan-modules>`"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-orphans"
         }
  , flag { flagName = "-Woverlapping-patterns"
         , flagDescription = "warn about overlapping patterns"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-overlapping-patterns"
         }
  , flag { flagName = "-Wtabs"
         , flagDescription = "warn if there are tabs in the source file"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-tabs"
         }
  , flag { flagName = "-Wtype-defaults"
         , flagDescription = "warn when defaulting happens"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-type-defaults"
         }
  , flag { flagName = "-Wunrecognised-pragmas"
         , flagDescription =
           "warn about uses of pragmas that GHC doesn't recognise"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unrecognised-pragmas"
         }
  , flag { flagName = "-Wunticked-promoted-constructors"
         , flagDescription = "warn if promoted constructors are not ticked"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unticked-promoted-constructors"
         }
  , flag { flagName = "-Wunused-binds"
         , flagDescription =
           "warn about bindings that are unused. Alias for "++
           ":ghc-flag:`-Wunused-top-binds`, :ghc-flag:`-Wunused-local-binds` and "++
           ":ghc-flag:`-Wunused-pattern-binds`"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-binds"
         }
  , flag { flagName = "-Wunused-top-binds"
         , flagDescription = "warn about top-level bindings that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-top-binds"
         }
  , flag { flagName = "-Wunused-local-binds"
         , flagDescription = "warn about local bindings that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-local-binds"
         }
  , flag { flagName = "-Wunused-pattern-binds"
         , flagDescription = "warn about pattern match bindings that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-pattern-binds"
         }
  , flag { flagName = "-Wunused-imports"
         , flagDescription = "warn about unnecessary imports"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-imports"
         }
  , flag { flagName = "-Wunused-matches"
         , flagDescription = "warn about variables in patterns that aren't used"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-matches"
         }
  , flag { flagName = "-Wunused-foralls"
         , flagDescription = "warn about type variables in user-written "++
           "``forall``\\s that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-foralls"
         }
  , flag { flagName = "-Wunused-type-variables"
         , flagDescription = "warn about variables in type family or data "++
           "family instances that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-type-variables"
         }
  , flag { flagName = "-Wunused-do-bind"
         , flagDescription =
           "warn about do bindings that appear to throw away values of types "++
           "other than ``()``"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unused-do-bind"
         }
  , flag { flagName = "-Wwrong-do-bind"
         , flagDescription =
           "warn about do bindings that appear to throw away monadic values "++
           "that you should have bound instead"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-wrong-do-bind"
         }
  , flag { flagName = "-Wunsafe"
         , flagDescription =
           "warn if the module being compiled is regarded to be unsafe. "++
           "Should be used to check the safety status of modules when using "++
           "safe inference. Works on all module types, even those using "++
           "explicit :ref:`Safe Haskell <safe-haskell>` modes (such as "++
           ":ghc-flag:`-XTrustworthy`) and so can be used to have the compiler check "++
           "any assumptions made."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-unsafe"
         }
  , flag { flagName = "-Wsafe"
         , flagDescription =
           "warn if the module being compiled is regarded to be safe. Should "++
           "be used to check the safety status of modules when using safe "++
           "inference. Works on all module types, even those using explicit "++
           ":ref:`Safe Haskell <safe-haskell>` modes (such as "++
           ":ghc-flag:`-XTrustworthy`) and so can be used to have the compiler check "++
           "any assumptions made."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-safe"
         }
  , flag { flagName = "-Wtrustworthy-safe"
         , flagDescription =
           "warn if the module being compiled is marked as "++
           ":ghc-flag:`-XTrustworthy` but it could instead be marked as "++
           ":ghc-flag:`-XSafe`, a more informative bound. Can be used to detect"++
           "once a Safe Haskell bound can be improved as dependencies are updated."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-safe"
         }
  , flag { flagName = "-Wwarnings-deprecations"
         , flagDescription =
           "warn about uses of functions & types that have warnings or "++
           "deprecated pragmas"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-warnings-deprecations"
         }
  , flag { flagName = "-Wdeprecations"
         , flagDescription =
           "warn about uses of functions & types that have warnings or "++
           "deprecated pragmas. Alias for :ghc-flag:`-Wwarnings-deprecations`"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-deprecations"
         }
  , flag { flagName = "-Wamp"
         , flagDescription =
           "*(deprecated)* warn on definitions conflicting with the "++
           "Applicative-Monad Proposal (AMP)"
         , flagType = DynamicFlag
         , flagReverse = "-Wno-amp"
         }
  , flag { flagName = "-Wredundant-constraints"
         , flagDescription =
           "Have the compiler warn about redundant constraints in type"++
           "signatures."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-redundant-constraints"
         }
  , flag { flagName = "-Wdeferred-type-errors"
         , flagDescription =
           "Report warnings when :ref:`deferred type errors "++
           "<defer-type-errors>` are enabled. This option is enabled by "++
           "default. See :ghc-flag:`-fdefer-type-errors`."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-deferred-type-errors"
         }
  , flag { flagName = "-Wtyped-holes"
         , flagDescription =
           "Report warnings when :ref:`typed hole <typed-holes>` errors are "++
           ":ref:`deferred until runtime <defer-type-errors>`. See "++
           ":ghc-flag:`-fdefer-typed-holes`."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-typed-holes"
         }
  , flag { flagName = "-Wdeferred-out-of-scope-variables"
         , flagDescription =
           "Report warnings when variable out-of-scope errors are "++
           ":ref:`deferred until runtime. "++
           "See :ghc-flag:`-fdefer-out-of-scope-variables`."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-deferred-out-of-scope-variables"
         }
  , flag { flagName = "-Wpartial-type-signatures"
         , flagDescription =
           "warn about holes in partial type signatures when "++
           ":ghc-flag:`-XPartialTypeSignatures` is enabled. Not applicable when "++
           ":ghc-flag:`-XPartialTypesignatures` is not enabled, in which case "++
           "errors are generated for such holes. See "++
           ":ref:`partial-type-signatures`."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-partial-type-signatures"
         }
  , flag { flagName = "-Wderiving-typeable"
         , flagDescription =
           "warn when encountering a request to derive an instance of class "++
           "``Typeable``. As of GHC 7.10, such declarations are unnecessary "++
           "and are ignored by the compiler because GHC has a custom solver "++
           "for discharging this type of constraint."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-deriving-typeable"
         }
  , flag { flagName = "-Wmissing-home-modules"
         , flagDescription =
           "warn when encountering a home module imported, but not listed "++
           "on the command line. Useful for cabal to ensure GHC won't pick "++
           "up modules, not listed neither in ``exposed-modules``, nor in "++
           "``other-modules``."
         , flagType = DynamicFlag
         , flagReverse = "-Wno-missing-home-modules"
         }
  ]

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
  , flag { flagName = "-Werror"
         , flagDescription = "make warnings fatal"
         , flagType = DynamicFlag
         , flagReverse = "-Wwarn"
         }
  , flag { flagName = "-Wwarn"
         , flagDescription = "make warnings non-fatal"
         , flagType = DynamicFlag
         , flagReverse = "-Werror"
         }
  , flag { flagName = "-fdefer-type-errors"
         , flagDescription =
           "Turn type errors into warnings, :ref:`deferring the error until "++
           "runtime <defer-type-errors>`. Implies ``-fdefer-typed-holes``. "++
           "See also ``-fwarn-deferred-type-errors``"
         , flagType = DynamicFlag
         , flagReverse = "-fno-defer-type-errors"
         }
  , flag { flagName = "-fdefer-typed-holes"
         , flagDescription =
           "Convert :ref:`typed hole <typed-holes>` errors into warnings, "++
           ":ref:`deferring the error until runtime <defer-type-errors>`. "++
           "Implied by ``-fdefer-type-errors``. "++
           "See also ``-fwarn-typed-holes``."
         , flagType = DynamicFlag
         , flagReverse = "-fno-defer-typed-holes"
         }
  , flag { flagName = "-fhelpful-errors"
         , flagDescription = "Make suggestions for mis-spelled names."
         , flagType = DynamicFlag
         , flagReverse = "-fno-helpful-errors"
         }
  , flag { flagName = "-fwarn-deprecated-flags"
         , flagDescription =
           "warn about uses of commandline flags that are deprecated"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-deprecated-flags"
         }
  , flag { flagName = "-fwarn-duplicate-constraints"
         , flagDescription =
           "warn when a constraint appears duplicated in a type signature"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-duplicate-constraints"
         }
  , flag { flagName = "-fwarn-duplicate-exports"
         , flagDescription = "warn when an entity is exported multiple times"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-duplicate-exports"
         }
  , flag { flagName = "-fwarn-hi-shadowing"
         , flagDescription =
           "warn when a ``.hi`` file in the current directory shadows a library"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-hi-shadowing"
         }
  , flag { flagName = "-fwarn-identities"
         , flagDescription =
           "warn about uses of Prelude numeric conversions that are probably "++
           "the identity (and hence could be omitted)"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-identities"
         }
  , flag { flagName = "-fwarn-implicit-prelude"
         , flagDescription = "warn when the Prelude is implicitly imported"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-implicit-prelude"
         }
  , flag { flagName = "-fwarn-incomplete-patterns"
         , flagDescription = "warn when a pattern match could fail"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-incomplete-patterns"
         }
  , flag { flagName = "-fwarn-incomplete-uni-patterns"
         , flagDescription =
           "warn when a pattern match in a lambda expression or "++
           "pattern binding could fail"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-incomplete-uni-patterns"
         }
  , flag { flagName = "-fwarn-incomplete-record-updates"
         , flagDescription = "warn when a record update could fail"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-incomplete-record-updates"
         }
  , flag { flagName = "-fwarn-lazy-unlifted-bindings"
         , flagDescription =
           "*(deprecated)* warn when a pattern binding looks lazy but "++
           "must be strict"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-lazy-unlifted-bindings"
         }
  , flag { flagName = "-fwarn-missing-fields"
         , flagDescription = "warn when fields of a record are uninitialised"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-missing-fields"
         }
  , flag { flagName = "-fwarn-missing-import-lists"
         , flagDescription =
           "warn when an import declaration does not explicitly list all the"++
           "names brought into scope"
         , flagType = DynamicFlag
         , flagReverse = "-fnowarn-missing-import-lists"
         }
  , flag { flagName = "-fwarn-missing-methods"
         , flagDescription = "warn when class methods are undefined"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-missing-methods"
         }
  , flag { flagName = "-fwarn-missing-signatures"
         , flagDescription = "warn about top-level functions without signatures"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-missing-signatures"
         }
  , flag { flagName = "-fwarn-missing-exported-sigs"
         , flagDescription =
           "warn about top-level functions without signatures, only if they "++
           "are exported. takes precedence over -fwarn-missing-signatures"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-missing-exported-sigs"
         }
  , flag { flagName = "-fwarn-missing-local-sigs"
         , flagDescription =
           "warn about polymorphic local bindings without signatures"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-missing-local-sigs"
         }
  , flag { flagName = "-fwarn-monomorphism-restriction"
         , flagDescription = "warn when the Monomorphism Restriction is applied"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-monomorphism-restriction"
         }
  , flag { flagName = "-fwarn-name-shadowing"
         , flagDescription = "warn when names are shadowed"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-name-shadowing"
         }
  , flag { flagName = "-fwarn-orphans"
         , flagDescription =
           "warn when the module contains :ref:`orphan instance declarations "++
           "or rewrite rules <orphan-modules>`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-orphans"
         }
  , flag { flagName = "-fwarn-overlapping-patterns"
         , flagDescription = "warn about overlapping patterns"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-overlapping-patterns"
         }
  , flag { flagName = "-fwarn-tabs"
         , flagDescription = "warn if there are tabs in the source file"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-tabs"
         }
  , flag { flagName = "-fwarn-type-defaults"
         , flagDescription = "warn when defaulting happens"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-type-defaults"
         }
  , flag { flagName = "-fwarn-unrecognised-pragmas"
         , flagDescription =
           "warn about uses of pragmas that GHC doesn't recognise"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unrecognised-pragmas"
         }
  , flag { flagName = "-fwarn-unticked-promoted-constructors"
         , flagDescription = "warn if promoted constructors are not ticked"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unticked-promoted-constructors"
         }
  , flag { flagName = "-fwarn-unused-binds"
         , flagDescription =
           "warn about bindings that are unused. Alias for "++
           "``-fwarn-unused-top-binds``, ``-fwarn-unused-local-binds`` and "++
           "``-fwarn-unused-pattern-binds``"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unused-binds"
         }
  , flag { flagName = "-fwarn-unused-top-binds"
         , flagDescription = "warn about top-level bindings that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unused-top-binds"
         }
  , flag { flagName = "-fwarn-unused-local-binds"
         , flagDescription = "warn about local bindings that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unused-local-binds"
         }
  , flag { flagName = "-fwarn-unused-pattern-binds"
         , flagDescription = "warn about pattern match bindings that are unused"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unused-pattern-binds"
         }
  , flag { flagName = "-fwarn-unused-imports"
         , flagDescription = "warn about unnecessary imports"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unused-imports"
         }
  , flag { flagName = "-fwarn-unused-matches"
         , flagDescription = "warn about variables in patterns that aren't used"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unused-matches"
         }
  , flag { flagName = "-fwarn-unused-do-bind"
         , flagDescription =
           "warn about do bindings that appear to throw away values of types "++
           "other than ``()``"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unused-do-bind"
         }
  , flag { flagName = "-fwarn-wrong-do-bind"
         , flagDescription =
           "warn about do bindings that appear to throw away monadic values "++
           "that you should have bound instead"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-wrong-do-bind"
         }
  , flag { flagName = "-fwarn-unsafe"
         , flagDescription =
           "warn if the module being compiled is regarded to be unsafe. "++
           "Should be used to check the safety status of modules when using "++
           "safe inference. Works on all module types, even those using "++
           "explicit :ref:`Safe Haskell <safe-haskell>` modes (such as "++
           "``-XTrustworthy``) and so can be used to have the compiler check "++
           "any assumptions made."
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-unsafe"
         }
  , flag { flagName = "-fwarn-safe"
         , flagDescription =
           "warn if the module being compiled is regarded to be safe. Should "++
           "be used to check the safety status of modules when using safe "++
           "inference. Works on all module types, even those using explicit "++
           ":ref:`Safe Haskell <safe-haskell>` modes (such as "++
           "``-XTrustworthy``) and so can be used to have the compiler check "++
           "any assumptions made."
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-safe"
         }
  , flag { flagName = "-fwarn-trustworthy-safe"
         , flagDescription =
           "warn if the module being compiled is marked as ``-XTrustworthy`` "++
           "but it could instead be marked as ``-XSafe``, a more informative "++
           "bound. Can be used to detect once a Safe Haskell bound can be "++
           "improved as dependencies are updated."
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-safe"
         }
  , flag { flagName = "-fwarn-warnings-deprecations"
         , flagDescription =
           "warn about uses of functions & types that have warnings or "++
           "deprecated pragmas"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-warnings-deprecations"
         }
  , flag { flagName = "-fwarn-amp"
         , flagDescription =
           "*(deprecated)* warn on definitions conflicting with the "++
           "Applicative-Monad Proposal (AMP)"
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-amp"
         }
  , flag { flagName = "-fwarn-deferred-type-errors"
         , flagDescription =
           "Report warnings when :ref:`deferred type errors "++
           "<defer-type-errors>` are enabled. This option is enabled by "++
           "default. See ``-fdefer-type-errors``."
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-deferred-type-errors"
         }
  , flag { flagName = "-fwarn-typed-holes"
         , flagDescription =
           "Report warnings when :ref:`typed hole <typed-holes>` errors are "++
           ":ref:`deferred until runtime <defer-type-errors>`. See "++
           "``-fdefer-typed-holes``."
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-typed-holes"
         }
  , flag { flagName = "-fwarn-partial-type-signatures"
         , flagDescription =
           "warn about holes in partial type signatures when "++
           "``-XPartialTypeSignatures`` is enabled. Not applicable when "++
           "``-XPartialTypesignatures`` is not enabled, in which case errors "++
           "are generated for such holes. See :ref:`partial-type-signatures`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-partial-type-signatures"
         }
  , flag { flagName = "-fwarn-deriving-typeable"
         , flagDescription =
           "warn when encountering a request to derive an instance of class "++
           "``Typeable``. As of GHC 7.10, such declarations are unnecessary "++
           "and are ignored by the compiler because GHC has a custom solver "++
           "for discharging this type of constraint."
         , flagType = DynamicFlag
         , flagReverse = "-fno-warn-deriving-typeable"
         }
  ]

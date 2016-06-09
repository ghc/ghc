module Options.Verbosity where

import Types

verbosityOptions :: [Flag]
verbosityOptions =
  [ flag { flagName = "-v"
         , flagDescription = "verbose mode (equivalent to ``-v3``)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-v⟨n⟩"
         , flagDescription = "set verbosity level"
         , flagType = DynamicFlag
         , flagReverse = ""
         }
  , flag { flagName = "-fprint-potential-instances"
         , flagDescription =
           "display all available instances in type error messages"
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-potential-instances"
         }
  , flag { flagName = "-fprint-explicit-foralls"
         , flagDescription =
           "Print explicit ``forall`` quantification in types. " ++
           "See also :ghc-flag:`-XExplicitForAll`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-explicit-foralls"
         }
  , flag { flagName = "-fprint-explicit-kinds"
         , flagDescription =
           "Print explicit kind foralls and kind arguments in types. " ++
           "See also :ghc-flag:`-XKindSignature`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-explicit-kinds"
         }
  , flag { flagName = "-fprint-explicit-runtime-reps"
         , flagDescription =
           "Print ``RuntimeRep`` variables in types which are "++
           "runtime-representation polymorphic."
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-explicit-runtime-reps"
         }
  , flag { flagName = "-fprint-unicode-syntax"
         , flagDescription =
           "Use unicode syntax when printing expressions, types and kinds. " ++
           "See also :ghc-flag:`-XUnicodeSyntax`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-unicode-syntax"
         }
  , flag { flagName = "-fprint-expanded-synonyms"
         , flagDescription =
           "In type errors, also print type-synonym-expanded types."
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-expanded-synonyms"
         }
  , flag { flagName = "-fprint-typechecker-elaboration"
         , flagDescription =
           "Print extra information from typechecker."
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-typechecker-elaboration"
         }
  , flag { flagName = "-ferror-spans"
         , flagDescription = "Output full span in error messages"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-Rghc-timing"
         , flagDescription =
           "Summarise timing stats for GHC (same as ``+RTS -tstderr``)."
         , flagType = DynamicFlag
         }
  ]

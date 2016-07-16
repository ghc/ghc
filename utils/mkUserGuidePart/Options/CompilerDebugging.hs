module Options.CompilerDebugging where

import Types

compilerDebuggingOptions :: [Flag]
compilerDebuggingOptions =
  [ flag { flagName = "-dcore-lint"
         , flagDescription = "Turn on internal sanity checking"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-to-file"
         , flagDescription = "Dump to files instead of stdout"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-asm"
         , flagDescription = "Dump assembly"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-bcos"
         , flagDescription = "Dump interpreter byte code"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-cmm-from-stg"
         , flagDescription = "Dump STG-to-C-- output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-cmm-verbose"
         , flagDescription = "Show output from each C-- pipeline pass"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-cmm"
         , flagDescription = "Dump the final C-- output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-core-stats"
         , flagDescription =
           "Print a one-line summary of the size of the Core program at the "++
           "end of the optimisation pipeline"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-cse"
         , flagDescription = "Dump CSE output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-deriv"
         , flagDescription = "Dump deriving output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-ds"
         , flagDescription = "Dump desugarer output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-foreign"
         , flagDescription = "Dump ``foreign export`` stubs"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-hpc"
         , flagDescription = "Dump after instrumentation for program coverage"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-inlinings"
         , flagDescription = "Dump inlining info"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-llvm"
         , flagDescription = "Dump LLVM intermediate code. "++
           "Implies :ghc-flag:`-fllvm`."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-occur-anal"
         , flagDescription = "Dump occurrence analysis output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-opt-cmm"
         , flagDescription = "Dump the results of C-- to C-- optimising passes"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-parsed"
         , flagDescription = "Dump parse tree"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-prep"
         , flagDescription = "Dump prepared core"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-rn"
         , flagDescription = "Dump renamer output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-rule-firings"
         , flagDescription = "Dump rule firing info"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-rule-rewrites"
         , flagDescription = "Dump detailed rule firing info"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-rules"
         , flagDescription = "Dump rules"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-vect"
         , flagDescription = "Dump vectoriser input and output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-simpl"
         , flagDescription = "Dump final simplifier output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-simpl-iterations"
         , flagDescription = "Dump output from each simplifier iteration"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-spec"
         , flagDescription = "Dump specialiser output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-splices"
         , flagDescription =
           "Dump TH spliced expressions, and what they evaluate to"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-stg"
         , flagDescription = "Dump final STG"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-stranal"
         , flagDescription = "Dump strictness analyser output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-str-signatures"
         , flagDescription = "Dump strictness signatures"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-tc"
         , flagDescription = "Dump typechecker output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dth-dec-file"
         , flagDescription =
           "Show evaluated TH declarations in a .th.hs file"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-types"
         , flagDescription = "Dump type signatures"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-worker-wrapper"
         , flagDescription = "Dump worker-wrapper output"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-if-trace"
         , flagDescription = "Trace interface files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-tc-trace"
         , flagDescription = "Trace typechecker"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-vt-trace"
         , flagDescription = "Trace vectoriser"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-rn-trace"
         , flagDescription = "Trace renamer"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-rn-stats"
         , flagDescription = "Renamer stats"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-simpl-stats"
         , flagDescription = "Dump simplifier stats"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dno-debug-output"
         , flagDescription = "Suppress unsolicited debugging output"
         , flagType = StaticFlag
         }
  , flag { flagName = "-dppr-debug"
         , flagDescription = "Turn on debug printing (more verbose)"
         , flagType = StaticFlag
         }
  , flag { flagName = "-dppr-user-length"
         , flagDescription =
           "Set the depth for printing expressions in error msgs"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dppr-cols⟨N⟩"
         , flagDescription =
           "Set the width of debugging output. For example ``-dppr-cols200``"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dppr-case-as-let"
         , flagDescription =
           "Print single alternative case expressions as strict lets."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-all"
         , flagDescription =
           "In core dumps, suppress everything (except for uniques) that is "++
           "suppressible."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-uniques"
         , flagDescription =
           "Suppress the printing of uniques in debug output (easier to use "++
           "``diff``)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-idinfo"
         , flagDescription =
           "Suppress extended information about identifiers where they "++
           "are bound"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-unfoldings"
         , flagDescription =
           "Suppress the printing of the stable unfolding of a variable at "++
           "its binding site"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-module-prefixes"
         , flagDescription =
           "Suppress the printing of module qualification prefixes"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-type-signatures"
         , flagDescription = "Suppress type signatures"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-type-applications"
         , flagDescription = "Suppress type applications"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsuppress-coercions"
         , flagDescription =
           "Suppress the printing of coercions in Core dumps to make them "++
           "shorter"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dsource-stats"
         , flagDescription = "Dump haskell source stats"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dcmm-lint"
         , flagDescription = "C-- pass sanity checking"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dstg-lint"
         , flagDescription = "STG pass sanity checking"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dstg-stats"
         , flagDescription = "Dump STG stats"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dverbose-core2core"
         , flagDescription = "Show output from each core-to-core pass"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dverbose-stg2stg"
         , flagDescription = "Show output from each STG-to-STG pass"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dshow-passes"
         , flagDescription = "Print out each pass name as it happens"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dfaststring-stats"
         , flagDescription =
           "Show statistics for fast string usage when finished"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-frule-check"
         , flagDescription =
           "Report sites with rules that could have fired but didn't. "++
           "Takes a string argument."
         , flagType = DynamicFlag
         }
  ]

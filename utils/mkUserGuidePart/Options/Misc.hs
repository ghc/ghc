module Options.Misc where

import Types

miscOptions :: [Flag]
miscOptions =
  [ flag { flagName = "-jN"
         , flagDescription =
           "When compiling with :ghc-flag:`--make`, compile ⟨N⟩ modules in parallel."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-hi-version-check"
         , flagDescription = "Don't complain about ``.hi`` file mismatches"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fhistory-size"
         , flagDescription = "Set simplification history size"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-ghci-history"
         , flagDescription =
           "Do not use the load/store the GHCi command history from/to "++
           "``ghci_history``."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-ghci-sandbox"
         , flagDescription =
           "Turn off the GHCi sandbox. Means computations are run in "++
           "the main thread, rather than a forked thread."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-freverse-errors"
         , flagDescription =
           "Display errors in GHC/GHCi sorted by reverse order of "++
           "source code line numbers."
         , flagType = DynamicFlag
         , flagReverse = "-fno-reverse-errors"
         }
  , flag { flagName = "-flocal-ghci-history"
         , flagDescription =
           "Use current directory for the GHCi command history "++
           "file ``.ghci-history``."
         , flagType = DynamicFlag
         , flagReverse = "-fno-local-ghci-history"
         }
  ]

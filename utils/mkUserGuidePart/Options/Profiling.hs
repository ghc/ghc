module Options.Profiling where

import Types

profilingOptions :: [Flag]
profilingOptions =
  [ flag { flagName = "-prof"
         , flagDescription = "Turn on profiling"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fprof-auto"
         , flagDescription =
           "Auto-add ``SCC``\\ s to all bindings not marked INLINE"
         , flagType = DynamicFlag
         , flagReverse = "-fno-prof-auto"
         }
  , flag { flagName = "-fprof-auto-top"
         , flagDescription =
           "Auto-add ``SCC``\\ s to all top-level bindings not marked INLINE"
         , flagType = DynamicFlag
         , flagReverse = "-fno-prof-auto"
         }
  , flag { flagName = "-fprof-auto-exported"
         , flagDescription =
           "Auto-add ``SCC``\\ s to all exported bindings not marked INLINE"
         , flagType = DynamicFlag
         , flagReverse = "-fno-prof-auto"
         }
  , flag { flagName = "-fprof-cafs"
         , flagDescription = "Auto-add ``SCC``\\ s to all CAFs"
         , flagType = DynamicFlag
         , flagReverse = "-fno-prof-cafs"
         }
  , flag { flagName = "-fno-prof-count-entries"
         , flagDescription = "Do not collect entry counts"
         , flagType = DynamicFlag
         , flagReverse = "-fprof-count-entries"
         }
  , flag { flagName = "-ticky"
         , flagDescription =
           ":ref:`Turn on ticky-ticky profiling <ticky-ticky>`"
         , flagType = DynamicFlag
         }
  ]

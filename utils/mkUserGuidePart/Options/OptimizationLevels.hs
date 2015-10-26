module Options.OptimizationLevels where

import Types

optimizationLevelsOptions :: [Flag]
optimizationLevelsOptions =
  [ flag { flagName = "-O0"
         , flagDescription = "Disable optimisations (default)"
         , flagType = DynamicFlag
         , flagReverse = "-O"
         }
  , flag { flagName = "-O, -O1"
         , flagDescription = "Enable level 1 optimisations"
         , flagType = DynamicFlag
         , flagReverse = "-O0"
         }
  , flag { flagName = "-O2"
         , flagDescription = "Enable level 2 optimisations"
         , flagType = DynamicFlag
         , flagReverse = "-O0"
         }
  , flag { flagName = "-Odph"
         , flagDescription =
           "Enable level 2 optimisations, set "++
           "``-fmax-simplifier-iterations=20`` "++
           "and ``-fsimplifier-phases=3``."
         , flagType = DynamicFlag
         }
  ]

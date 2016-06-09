module Options.RecompilationChecking where

import Types

recompilationCheckingOptions :: [Flag]
recompilationCheckingOptions =
  [ flag { flagName = "-fforce-recomp"
         , flagDescription =
           "Turn off recompilation checking. This is implied by any " ++
           "``-ddump-X`` option when compiling a single file " ++
           "(i.e. when using :ghc-flag:`-c`)."
         , flagType = DynamicFlag
         , flagReverse = "-fno-force-recomp"
         }
  ]

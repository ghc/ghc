module Options.ProgramCoverage where

import Types

programCoverageOptions :: [Flag]
programCoverageOptions =
  [ flag { flagName = "-fhpc"
         , flagDescription =
           "Turn on Haskell program coverage instrumentation"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-hpcdir ⟨dir⟩"
         , flagDescription =
           "Directory to deposit ``.mix`` files during compilation "++
           "(default is ``.hpc``)"
         , flagType = DynamicFlag
         }
  ]

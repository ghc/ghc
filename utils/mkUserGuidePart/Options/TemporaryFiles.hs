module Options.TemporaryFiles where

import Types

temporaryFilesOptions :: [Flag]
temporaryFilesOptions =
  [ flag { flagName = "-tmpdir ⟨dir⟩"
         , flagDescription = "set the directory for temporary files"
         , flagType = DynamicFlag
         }
  ]

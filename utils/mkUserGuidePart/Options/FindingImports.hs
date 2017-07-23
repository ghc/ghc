module Options.FindingImports where

import Types

findingImportsOptions :: [Flag]
findingImportsOptions =
  [ flag { flagName = "-i⟨dir⟩[:⟨dir⟩]*"
         , flagDescription = "add ⟨dir⟩, ⟨dir2⟩, etc. to import path"
         , flagType = DynamicSettableFlag
         }
  , flag { flagName = "-i"
         , flagDescription = "Empty the import directory list"
         , flagType = DynamicSettableFlag
         }
  ]

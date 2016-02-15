module Options.RedirectingOutput where

import Types

redirectingOutputOptions :: [Flag]
redirectingOutputOptions =
  [ flag { flagName = "-hcsuf ⟨suffix⟩"
         , flagDescription = "set the suffix to use for intermediate C files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-hidir ⟨dir⟩"
         , flagDescription = "set directory for interface files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-hisuf ⟨suffix⟩"
         , flagDescription = "set the suffix to use for interface files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-o ⟨filename⟩"
         , flagDescription = "set output filename"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-odir ⟨dir⟩"
         , flagDescription = "set directory for object files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ohi ⟨filename⟩"
         , flagDescription = "set the filename in which to put the interface"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-osuf ⟨suffix⟩"
         , flagDescription = "set the output file suffix"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-stubdir ⟨dir⟩"
         , flagDescription = "redirect FFI stub files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dumpdir ⟨dir⟩"
         , flagDescription = "redirect dump files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-outputdir ⟨dir⟩"
         , flagDescription = "set output directory"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dyno <filename>"
         , flagDescription = "Set the output filename for dynamic object files (see ``-dynamic-too``)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dynosuf <suffix>"
         , flagDescription = "Set the object suffix for dynamic object files (see ``-dynamic-too``)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dynhisuf <suffix>"
         , flagDescription = "Set the hi suffix for dynamic object files (see ``-dynamic-too``)"
         , flagType = DynamicFlag
         }
  ]

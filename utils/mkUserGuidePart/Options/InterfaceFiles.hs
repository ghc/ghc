module Options.InterfaceFiles where

import Types

interfaceFilesOptions :: [Flag]
interfaceFilesOptions =
  [ flag { flagName = "-ddump-hi"
         , flagDescription = "Dump the new interface to stdout"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-hi-diffs"
         , flagDescription = "Show the differences vs. the old interface"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ddump-minimal-imports"
         , flagDescription = "Dump a minimal set of imports"
         , flagType = DynamicFlag
         }
  , flag { flagName = "--show-iface ⟨file⟩"
         , flagDescription = "See :ref:`modes`."
         , flagType = ModeFlag
         }
  ]

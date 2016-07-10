module Options.Packages where

import Types

packagesOptions :: [Flag]
packagesOptions =
  [ flag { flagName = "-this-unit-id ⟨P⟩"
         , flagDescription = "Compile to be part of unit (i.e. package) ⟨P⟩"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-package ⟨P⟩"
         , flagDescription = "Expose package ⟨P⟩"
         , flagType = DynamicSettableFlag
         }
  , flag { flagName = "-hide-all-packages"
         , flagDescription = "Hide all packages by default"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-hide-package ⟨name⟩"
         , flagDescription = "Hide package ⟨P⟩"
         , flagType = DynamicSettableFlag
         }
  , flag { flagName = "-ignore-package ⟨name⟩"
         , flagDescription = "Ignore package ⟨P⟩"
         , flagType = DynamicSettableFlag
         }
  , flag { flagName = "-package-db ⟨file⟩"
         , flagDescription = "Add ⟨file⟩ to the package db stack."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-clear-package-db"
         , flagDescription = "Clear the package db stack."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-no-global-package-db"
         , flagDescription = "Remove the global package db from the stack."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-global-package-db"
         , flagDescription = "Add the global package db to the stack."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-no-user-package-db"
         , flagDescription = "Remove the user's package db from the stack."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-user-package-db"
         , flagDescription = "Add the user's package db to the stack."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-no-auto-link-packages"
         , flagDescription = "Don't automatically link in the base and rts packages."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-trust ⟨P⟩"
         , flagDescription = "Expose package ⟨P⟩ and set it to be trusted"
         , flagType = DynamicSettableFlag
         }
  , flag { flagName = "-distrust ⟨P⟩"
         , flagDescription = "Expose package ⟨P⟩ and set it to be distrusted"
         , flagType = DynamicSettableFlag
         }
  , flag { flagName = "-distrust-all"
         , flagDescription = "Distrust all packages by default"
         , flagType = DynamicSettableFlag
         }
  , flag { flagName = "-package-env ⟨file⟩|⟨name⟩"
         , flagDescription = "Use the specified package environment."
         , flagType = DynamicFlag
         }
  ]

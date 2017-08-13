module Settings.Install (installPackageDbDirectory) where

import Expression
import UserSettings

-- | In the final installation path specified by "DEST", there is another package.conf.d,
-- different from packageDbDirectory in Settings.Path.
-- It is used by installGhcPkgWrapper
installPackageDbDirectory :: FilePath -> FilePath -> Stage -> FilePath
installPackageDbDirectory _ top Stage0 = top -/- buildRootPath -/- "stage0/bootstrapping.conf"
installPackageDbDirectory libdir _ _   = libdir -/- "package.conf.d"

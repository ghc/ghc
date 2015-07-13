module Settings.TargetDirectory (
    targetDirectory, targetPath
    ) where

import Base
import Package
import UserSettings

-- User can override the default target directory settings given below
targetDirectory :: Stage -> Package -> FilePath
targetDirectory = userTargetDirectory

-- Path to the target directory from GHC source root
targetPath :: Stage -> Package -> FilePath
targetPath stage pkg = pkgPath pkg </> targetDirectory stage pkg

module Settings.TargetDirectory (
    targetDirectory
    ) where

import Base
import Package
import UserSettings

-- User can override the default target directory settings given below
targetDirectory :: Stage -> Package -> FilePath
targetDirectory = userTargetDirectory

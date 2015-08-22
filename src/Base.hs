module Base (
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.Config,
    module Development.Shake.FilePath,
    module Development.Shake.Util,
    shakeFilesPath, configPath, bootPackageConstraints, packageDependencies
    ) where

import Development.Shake hiding (unit)
import Development.Shake.Classes
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util

shakeFilesPath :: FilePath
shakeFilesPath = "_build/"

configPath :: FilePath
configPath = "shake/cfg/"

bootPackageConstraints :: FilePath
bootPackageConstraints = shakeFilesPath ++ "boot-package-constraints"

packageDependencies :: FilePath
packageDependencies = shakeFilesPath ++ "package-dependencies"

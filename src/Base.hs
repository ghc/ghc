module Base (
    shakeFilesPath, configPath,
    bootPackageConstraints, packageDependencies,
    module Development.Shake,
    module Development.Shake.Util,
    module Development.Shake.Config,
    module Development.Shake.Classes,
    module Development.Shake.FilePath
    ) where

import Development.Shake
import Development.Shake.Util
import Development.Shake.Config
import Development.Shake.Classes
import Development.Shake.FilePath

shakeFilesPath :: FilePath
shakeFilesPath = "_build/"

configPath :: FilePath
configPath = "shake/cfg/"

bootPackageConstraints :: FilePath
bootPackageConstraints = shakeFilesPath ++ "boot-package-constraints"

packageDependencies :: FilePath
packageDependencies = shakeFilesPath ++ "package-dependencies"

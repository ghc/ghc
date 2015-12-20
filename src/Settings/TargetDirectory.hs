module Settings.TargetDirectory (
    targetDirectory, targetPath, pkgDataFile, pkgHaddockFile, pkgLibraryFile,
    pkgGhciLibraryFile
    ) where

import Expression
import Settings.User

-- TODO: move to Settings.hs?

-- User can override the default target directory settings given below
targetDirectory :: Stage -> Package -> FilePath
targetDirectory = userTargetDirectory

-- Path to the target directory from GHC source root
targetPath :: Stage -> Package -> FilePath
targetPath stage pkg = pkgPath pkg -/- targetDirectory stage pkg

pkgDataFile :: Stage -> Package -> FilePath
pkgDataFile stage pkg = targetPath stage pkg -/- "package-data.mk"

-- Relative path to a package haddock file, e.g.:
-- "libraries/array/dist-install/doc/html/array/array.haddock"
pkgHaddockFile :: Package -> FilePath
pkgHaddockFile pkg @ (Package name _) =
    targetPath Stage1 pkg -/- "doc/html" -/- name -/- name <.> "haddock"

-- Relative path to a package library file, e.g.:
-- "libraries/array/dist-install/build/libHSarray-0.5.1.0.a"
-- TODO: remove code duplication for computing buildPath
pkgLibraryFile :: Stage -> Package -> String -> Way -> Action FilePath
pkgLibraryFile stage pkg componentId way = do
    extension <- libsuf way
    let buildPath = targetPath stage pkg -/- "build"
    return $ buildPath -/- "libHS" ++ componentId <.> extension

-- Relative path to a package ghci library file, e.g.:
-- "libraries/array/dist-install/build/HSarray-0.5.1.0.o"
pkgGhciLibraryFile :: Stage -> Package -> String -> FilePath
pkgGhciLibraryFile stage pkg componentId =
    targetPath stage pkg -/- "build" -/- "HS" ++ componentId <.> "o"

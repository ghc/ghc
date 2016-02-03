module Settings.Paths (
    targetDirectory, targetPath, pkgDataFile, pkgHaddockFile, pkgLibraryFile,
    pkgLibraryFile0, pkgGhciLibraryFile, gmpBuildPath, gmpLibNameCache,
    packageDbDirectory, pkgConfFile
    ) where

import Base
import Expression
import GHC
import Oracles.PackageData
import Settings.User

-- Path to the target directory from GHC source root
targetPath :: Stage -> Package -> FilePath
targetPath stage pkg = buildRootPath -/- targetDirectory stage pkg -/- pkgPath pkg

pkgDataFile :: Stage -> Package -> FilePath
pkgDataFile stage pkg = targetPath stage pkg -/- "package-data.mk"

-- Relative path to a package haddock file, e.g.:
-- "libraries/array/dist-install/doc/html/array/array.haddock"
pkgHaddockFile :: Package -> FilePath
pkgHaddockFile pkg =
    targetPath Stage1 pkg -/- "doc/html" -/- name -/- name <.> "haddock"
  where name = pkgNameString pkg

-- Relative path to a package library file, e.g.:
-- "libraries/array/stage2/build/libHSarray-0.5.1.0.a"
pkgLibraryFile :: Stage -> Package -> Way -> Action FilePath
pkgLibraryFile stage pkg way = do
    extension <- libsuf way
    pkgFile stage pkg "build/libHS" extension

pkgLibraryFile0 :: Stage -> Package -> Way -> Action FilePath
pkgLibraryFile0 stage pkg way = do
    extension <- libsuf way
    pkgFile stage pkg "build/libHS" ("-0" ++ extension)

-- Relative path to a package ghci library file, e.g.:
-- "libraries/array/dist-install/build/HSarray-0.5.1.0.o"
pkgGhciLibraryFile :: Stage -> Package -> Action FilePath
pkgGhciLibraryFile stage pkg = pkgFile stage pkg "build/HS" ".o"

pkgFile :: Stage -> Package -> String -> String -> Action FilePath
pkgFile stage pkg prefix suffix = do
    let path = targetPath stage pkg
    componentId <- pkgData $ ComponentId path
    return $ path -/- prefix ++ componentId ++ suffix

-- This is the build directory for in-tree GMP library
gmpBuildPath :: FilePath
gmpBuildPath = buildRootPath -/- "stage1/gmp"

-- GMP library names extracted from integer-gmp.buildinfo
gmpLibNameCache :: FilePath
gmpLibNameCache = gmpBuildPath -/- "gmp-lib-names"

-- TODO: move to buildRootPath, see #113
-- StageN, N > 0, share the same packageDbDirectory
packageDbDirectory :: Stage -> FilePath
packageDbDirectory Stage0 = buildRootPath -/- "stage0/bootstrapping.conf"
packageDbDirectory _      = "inplace/lib/package.conf.d"

pkgConfFile :: Stage -> Package -> Action FilePath
pkgConfFile stage pkg = do
    componentId <- pkgData . ComponentId $ targetPath stage pkg
    return $ packageDbDirectory stage -/- componentId <.> "conf"

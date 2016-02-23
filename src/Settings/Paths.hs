module Settings.Paths (
    contextDirectory, contextPath, pkgDataFile, pkgHaddockFile, pkgLibraryFile,
    pkgLibraryFile0, pkgGhciLibraryFile, gmpBuildPath, gmpBuildInfoPath,
    packageDbDirectory, pkgConfFile
    ) where

import Base
import Context
import Expression
import GHC
import Oracles.PackageData
import Settings.User

-- Path to the target directory from GHC source root
contextPath :: Context -> FilePath
contextPath context @ Context {..} =
    buildRootPath -/- contextDirectory context -/- pkgPath package

pkgDataFile :: Context -> FilePath
pkgDataFile context = contextPath context -/- "package-data.mk"

-- Relative path to a package haddock file, e.g.:
-- "libraries/array/dist-install/doc/html/array/array.haddock"
pkgHaddockFile :: Context -> FilePath
pkgHaddockFile context @ Context {..} =
    contextPath context -/- "doc/html" -/- name -/- name <.> "haddock"
  where name = pkgNameString package

-- Relative path to a package library file, e.g.:
-- "libraries/array/stage2/build/libHSarray-0.5.1.0.a"
pkgLibraryFile :: Context -> Action FilePath
pkgLibraryFile context @ Context {..} = do
    extension <- libsuf way
    pkgFile context "build/libHS" extension

pkgLibraryFile0 :: Context -> Action FilePath
pkgLibraryFile0 context @ Context {..} = do
    extension <- libsuf way
    pkgFile context "build/libHS" ("-0" ++ extension)

-- Relative path to a package ghci library file, e.g.:
-- "libraries/array/dist-install/build/HSarray-0.5.1.0.o"
pkgGhciLibraryFile :: Context -> Action FilePath
pkgGhciLibraryFile context = pkgFile context "build/HS" ".o"

pkgFile :: Context -> String -> String -> Action FilePath
pkgFile context prefix suffix = do
    let path = contextPath context
    componentId <- pkgData $ ComponentId path
    return $ path -/- prefix ++ componentId ++ suffix

-- This is the build directory for in-tree GMP library
gmpBuildPath :: FilePath
gmpBuildPath = buildRootPath -/- "stage1/gmp"

-- We extract system gmp library name from this file
gmpBuildInfoPath :: FilePath
gmpBuildInfoPath = pkgPath integerGmp -/- "integer-gmp.buildinfo"

-- TODO: move to buildRootPath, see #113
-- StageN, N > 0, share the same packageDbDirectory
packageDbDirectory :: Stage -> FilePath
packageDbDirectory Stage0 = buildRootPath -/- "stage0/bootstrapping.conf"
packageDbDirectory _      = "inplace/lib/package.conf.d"

pkgConfFile :: Context -> Action FilePath
pkgConfFile context @ Context {..} = do
    componentId <- pkgData . ComponentId $ contextPath context
    return $ packageDbDirectory stage -/- componentId <.> "conf"

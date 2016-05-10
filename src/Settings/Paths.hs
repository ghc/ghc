module Settings.Paths (
    contextDirectory, buildPath, pkgDataFile, pkgHaddockFile, pkgLibraryFile,
    pkgLibraryFile0, pkgGhciLibraryFile, gmpBuildPath, gmpLibrary, gmpObjects,
    gmpLibraryH, gmpBuildInfoPath, libffiBuildPath, shakeFilesPath, pkgConfFile,
    packageDbDirectory, bootPackageConstraints, packageDependencies
    ) where

import Base
import Context
import Expression
import GHC
import Oracles.PackageData
import Settings.User

shakeFilesPath :: FilePath
shakeFilesPath = buildRootPath -/- "hadrian/shake-files"

bootPackageConstraints :: FilePath
bootPackageConstraints = shakeFilesPath -/- "boot-package-constraints"

packageDependencies :: FilePath
packageDependencies = shakeFilesPath -/- "package-dependencies"

-- | Path to the directory containing build artefacts of a given 'Context'.
buildPath :: Context -> FilePath
buildPath context@Context {..} =
    buildRootPath -/- contextDirectory context -/- pkgPath package

-- | Path to the @package-data.mk@ of a given 'Context'.
pkgDataFile :: Context -> FilePath
pkgDataFile context = buildPath context -/- "package-data.mk"

-- | Path to the haddock file of a given 'Context', e.g.:
-- "_build/stage1/libraries/array/doc/html/array/array.haddock".
pkgHaddockFile :: Context -> FilePath
pkgHaddockFile context@Context {..} =
    buildPath context -/- "doc/html" -/- name -/- name <.> "haddock"
  where name = pkgNameString package

-- | Path to the library file of a given 'Context', e.g.:
-- "_build/stage1/libraries/array/build/libHSarray-0.5.1.0.a".
pkgLibraryFile :: Context -> Action FilePath
pkgLibraryFile context@Context {..} = do
    extension <- libsuf way
    pkgFile context "libHS" extension

-- | Path to the auxiliary library file of a given 'Context', e.g.:
-- "_build/stage1/compiler/build/libHSghc-8.1-0.a".
pkgLibraryFile0 :: Context -> Action FilePath
pkgLibraryFile0 context@Context {..} = do
    extension <- libsuf way
    pkgFile context "libHS" ("-0" ++ extension)

-- | Path to the GHCi library file of a given 'Context', e.g.:
-- "_build/stage1/libraries/array/build/HSarray-0.5.1.0.o".
pkgGhciLibraryFile :: Context -> Action FilePath
pkgGhciLibraryFile context = pkgFile context "HS" ".o"

pkgFile :: Context -> String -> String -> Action FilePath
pkgFile context prefix suffix = do
    let path = buildPath context
    componentId <- pkgData $ ComponentId path
    return $ path -/- prefix ++ componentId ++ suffix

-- | Build directory for in-tree GMP library.
gmpBuildPath :: FilePath
gmpBuildPath = buildRootPath -/- "stage1/gmp"

-- | Path to the GMP library.
gmpLibrary :: FilePath
gmpLibrary = gmpBuildPath -/- "libgmp.a"

-- | Path to the GMP library header.
gmpLibraryH :: FilePath
gmpLibraryH = gmpBuildPath -/- "include/ghc-gmp.h"

-- | Path to the GMP library object files.
gmpObjects :: FilePath
gmpObjects = gmpBuildPath -/- "objs"

-- | Path to the GMP library buildinfo file.
gmpBuildInfoPath :: FilePath
gmpBuildInfoPath = pkgPath integerGmp -/- "integer-gmp.buildinfo"

-- | Build directory for in-tree libffi library.
libffiBuildPath :: FilePath
libffiBuildPath = buildRootPath -/- "stage1/libffi"

-- TODO: move to buildRootPath, see #113
-- StageN, N > 0, share the same packageDbDirectory
-- | Path to package database directory of a given 'Stage'.
packageDbDirectory :: Stage -> FilePath
packageDbDirectory Stage0 = buildRootPath -/- "stage0/bootstrapping.conf"
packageDbDirectory _      = "inplace/lib/package.conf.d"

-- | Path to the configuration file of a given 'Context'.
pkgConfFile :: Context -> Action FilePath
pkgConfFile context@Context {..} = do
    componentId <- pkgData . ComponentId $ buildPath context
    return $ packageDbDirectory stage -/- componentId <.> "conf"

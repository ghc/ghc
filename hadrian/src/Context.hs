module Context (
    -- * Context
    Context (..), vanillaContext, stageContext,

    -- * Expressions
    getStage, getPackage, getWay, getStagedSettingList, getBuildPath, getPackageDbLoc,

    -- * Paths
    contextDir, buildPath, buildDir, pkgInplaceConfig, pkgSetupConfigFile, pkgSetupConfigDir,
    pkgHaddockFile, pkgRegisteredLibraryFile, pkgRegisteredLibraryFileName,
    pkgLibraryFile, pkgGhciLibraryFile,
    pkgConfFile, pkgStampFile, resourcePath, objectPath, contextPath, getContextPath, libPath, distDir,
    haddockStatsFilesDir
    ) where

import Base
import Context.Path
import Context.Type
import Hadrian.Expression
import Hadrian.Haskell.Cabal
import Oracles.Setting

-- | Most targets are built only one way, hence the notion of 'vanillaContext'.
vanillaContext :: Stage -> Package -> Context
vanillaContext s p = Context s p vanilla Final

-- | Partial context with undefined 'Package' field. Useful for 'Packages'
-- expressions that only read the environment and current 'Stage'.
stageContext :: Stage -> Context
stageContext s = vanillaContext s $ error "stageContext: package not set"

-- | Get the 'Stage' of the current 'Context'.
getStage :: Expr Context b Stage
getStage = stage <$> getContext

getInplace :: Expr Context b Inplace
getInplace = iplace <$> getContext

getPackageDbLoc :: Expr Context b PackageDbLoc
getPackageDbLoc = PackageDbLoc <$> getStage <*> getInplace

-- | Get the 'Package' of the current 'Context'.
getPackage :: Expr Context b Package
getPackage = package <$> getContext

-- | Get the 'Way' of the current 'Context'.
getWay :: Expr Context b Way
getWay = way <$> getContext

-- | Get a list of configuration settings for the current stage.
getStagedSettingList :: (Stage -> SettingList) -> Args Context b
getStagedSettingList f = getSettingList . f =<< getStage

-- | Path to the directory containing the final artifact in a given 'Context'.
libPath :: Context -> Action FilePath
libPath Context {..} = buildRoot <&> (-/- (stageString stage -/- "lib"))

-- | Get the directory name for binary distribution files
-- @<arch>-<os>-ghc-<version>@.
--
-- We preform some renaming to accommodate Cabal's slightly different naming
-- conventions (see 'cabalOsString' and 'cabalArchString').
distDir :: Stage -> Action FilePath
distDir st = do
    let (os,arch) = case st of
            Stage0 {} -> (HostOs , HostArch)
            _      -> (TargetOs, TargetArch)
    version        <- ghcVersionStage st
    hostOs         <- cabalOsString <$> setting os
    hostArch       <- cabalArchString <$> setting arch
    return $ hostArch ++ "-" ++ hostOs ++ "-ghc-" ++ version

pkgFileName :: Context -> Package -> String -> String -> Action FilePath
pkgFileName context package prefix suffix = do
    pid  <- pkgUnitId (stage context) package
    return $ prefix ++ pid ++ suffix

pkgFile :: Context -> String -> String -> Action FilePath
pkgFile context@Context {..} prefix suffix = do
    path <- buildPath context
    fileName <- pkgFileName context package prefix suffix
    return $ path -/- fileName

-- | Path to inplace package configuration file of a given 'Context'.
pkgInplaceConfig :: Context -> Action FilePath
pkgInplaceConfig context = contextPath context <&> (-/- "inplace-pkg-config")

pkgSetupConfigDir :: Context -> Action FilePath
pkgSetupConfigDir context = contextPath context

-- | Path to the @setup-config@ of a given 'Context'.
pkgSetupConfigFile :: Context -> Action FilePath
pkgSetupConfigFile context = pkgSetupConfigDir context <&> (-/- "setup-config")

-- | Path to the haddock file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/doc/html/array/array.haddock@.
pkgHaddockFile :: Context -> Action FilePath
pkgHaddockFile context@Context {..} = do
    root <- buildRoot
    version <- pkgUnitId stage package
    return $ root -/- "doc/html/libraries" -/- version -/- pkgName package <.> "haddock"

-- | Path to the registered ghc-pkg library file of a given 'Context', e.g.:
-- @_build/stage1/lib/x86_64-linux-ghc-8.9.0/libHSarray-0.5.1.0-ghc8.9.0.so@
-- @_build/stage1/lib/x86_64-linux-ghc-8.9.0/array-0.5.1.0/libHSarray-0.5.4.0.a@
pkgRegisteredLibraryFile :: Context -> Action FilePath
pkgRegisteredLibraryFile context@Context {..} = do
    libDir    <- libPath context
    pkgId     <- pkgUnitId stage package
    fileName  <- pkgRegisteredLibraryFileName context
    distDir   <- distDir stage
    return $ if Dynamic `wayUnit` way
        then libDir -/- distDir -/- fileName
        else libDir -/- distDir -/- pkgId -/- fileName

-- | Just the final filename portion of pkgRegisteredLibraryFile
pkgRegisteredLibraryFileName :: Context -> Action FilePath
pkgRegisteredLibraryFileName context@Context{..} = do
    extension <- libsuf stage way
    pkgFileName context package "libHS" extension


-- | Path to the library file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/build/libHSarray-0.5.1.0.a@.
pkgLibraryFile :: Context -> Action FilePath
pkgLibraryFile context@Context {..} = do
    extension <- libsuf stage way
    pkgFile context "libHS" extension

-- | Path to the GHCi library file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/build/HSarray-0.5.1.0.o@.
pkgGhciLibraryFile :: Context -> Action FilePath
pkgGhciLibraryFile context@Context {..} = do
    let extension = "" <.> osuf way
    pkgFile context "HS" extension

-- | Path to the configuration file of a given 'Context'.
pkgConfFile :: Context -> Action FilePath
pkgConfFile context@Context {..} = do
    pid  <- pkgUnitId stage package
    dbPath <- packageDbPath (PackageDbLoc stage iplace)
    return $ dbPath -/- pid <.> "conf"

-- | Path to the stamp file for a given 'Context'. The stamp file records if
-- we have built all the objects necessary for a certain way or not.
pkgStampFile :: Context -> Action FilePath
pkgStampFile c@Context{..} = do
    let extension = waySuffix way
    pkgFile c "stamp-" extension


-- | Given a 'Context' and a 'FilePath' to a source file, compute the 'FilePath'
-- to its object file. For example:
-- * "Task.c"                              -> "_build/stage1/rts/Task.thr_o"
-- * "_build/stage1/rts/cmm/AutoApply.cmm" -> "_build/stage1/rts/cmm/AutoApply.o"
objectPath :: Context -> FilePath -> Action FilePath
objectPath context@Context {..} src = do
    isGenerated <- isGeneratedSource src
    path        <- buildPath context
    let extension = drop 1 $ takeExtension src
        obj       = src -<.> osuf way
        result | isGenerated          = obj
               | "*hs*" ?== extension = path -/- obj
               | otherwise            = path -/- extension -/- obj
    return result


resourcePath :: Context -> FilePath -> Action FilePath
resourcePath context src = do
    path <- buildPath context
    let extension = drop 1 $ takeExtension src
    return (path -/- extension -/- src)

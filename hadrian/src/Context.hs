module Context (
    -- * Context
    Context (..), vanillaContext, stageContext,

    -- * Expressions
    getStage, getPackage, getWay, getStagedSettingList, getBuildPath,

    -- * Paths
    contextDir, buildPath, buildDir, pkgInplaceConfig, pkgSetupConfigFile,
    pkgHaddockFile, pkgRegisteredLibraryFile, pkgLibraryFile, pkgGhciLibraryFile,
    pkgConfFile, objectPath, contextPath, getContextPath, libPath, distDir,
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
vanillaContext s p = Context s p vanilla

-- | Partial context with undefined 'Package' field. Useful for 'Packages'
-- expressions that only read the environment and current 'Stage'.
stageContext :: Stage -> Context
stageContext s = vanillaContext s $ error "stageContext: package not set"

-- | Get the 'Stage' of the current 'Context'.
getStage :: Expr Context b Stage
getStage = stage <$> getContext

-- | Get the 'Package' of the current 'Context'.
getPackage :: Expr Context b Package
getPackage = package <$> getContext

-- | Get the 'Way' of the current 'Context'.
getWay :: Expr Context b Way
getWay = way <$> getContext

-- | Get a list of configuration settings for the current stage.
getStagedSettingList :: (Stage -> SettingList) -> Args Context b
getStagedSettingList f = getSettingList . f =<< getStage

-- | Path to the directory containg the final artifact in a given 'Context'.
libPath :: Context -> Action FilePath
libPath Context {..} = buildRoot <&> (-/- (stageString stage -/- "lib"))

-- | Get the directory name for binary distribution files
-- @<arch>-<os>-ghc-<version>@.
--
-- We preform some renaming to accommodate Cabal's slightly different naming
-- conventions (see 'cabalOsString' and 'cabalArchString').
distDir :: Stage -> Action FilePath
distDir st = do
    version        <- ghcVersionStage st
    hostOs         <- cabalOsString <$> setting BuildOs
    hostArch       <- cabalArchString <$> setting BuildArch
    return $ hostArch ++ "-" ++ hostOs ++ "-ghc-" ++ version

pkgFileName :: Package -> String -> String -> Action FilePath
pkgFileName package prefix suffix = do
    pid  <- pkgIdentifier package
    return $ prefix ++ pid ++ suffix

pkgFile :: Context -> String -> String -> Action FilePath
pkgFile context@Context {..} prefix suffix = do
    path <- buildPath context
    fileName <- pkgFileName package prefix suffix
    return $ path -/- fileName

-- | Path to inplace package configuration file of a given 'Context'.
pkgInplaceConfig :: Context -> Action FilePath
pkgInplaceConfig context = contextPath context <&> (-/- "inplace-pkg-config")

-- | Path to the @setup-config@ of a given 'Context'.
pkgSetupConfigFile :: Context -> Action FilePath
pkgSetupConfigFile context = contextPath context <&> (-/- "setup-config")

-- | Path to the haddock file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/doc/html/array/array.haddock@.
pkgHaddockFile :: Context -> Action FilePath
pkgHaddockFile Context {..} = do
    root <- buildRoot
    let name = pkgName package
    return $ root -/- "docs/html/libraries" -/- name -/- name <.> "haddock"

-- | Path to the registered ghc-pkg library file of a given 'Context', e.g.:
-- @_build/stage1/lib/x86_64-linux-ghc-8.9.0/libHSarray-0.5.1.0-ghc8.9.0.so@
-- @_build/stage1/lib/x86_64-linux-ghc-8.9.0/array-0.5.1.0/libHSarray-0.5.4.0.a@
pkgRegisteredLibraryFile :: Context -> Action FilePath
pkgRegisteredLibraryFile context@Context {..} = do
    libDir    <- libPath context
    pkgId     <- pkgIdentifier package
    extension <- libsuf stage way
    fileName  <- pkgFileName package "libHS" extension
    distDir   <- distDir stage
    return $ if Dynamic `wayUnit` way
        then libDir -/- distDir -/- fileName
        else libDir -/- distDir -/- pkgId -/- fileName

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
pkgConfFile Context {..} = do
    pid  <- pkgIdentifier package
    dbPath <- packageDbPath stage
    return $ dbPath -/- pid <.> "conf"

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
               | "*hs*" ?== extension = path -/- "hs" -/- obj
               | otherwise            = path -/- extension -/- obj
    return result

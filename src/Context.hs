module Context (
    -- * Context
    Context (..), vanillaContext, stageContext,

    -- * Expressions
    getStage, getPackage, getWay, getStagedSettingList, getBuildPath,
    withHsPackage,

    -- * Paths
    contextDir, buildPath, buildDir, pkgInplaceConfig, pkgSetupConfigFile,
    pkgHaddockFile, pkgLibraryFile, pkgGhciLibraryFile, pkgConfFile, objectPath,
    contextPath, getContextPath, libDir, libPath
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

-- | Construct an expression that depends on the Cabal file of the current
-- package and is empty in a non-Haskell context.
withHsPackage :: (Monoid a, Semigroup a) => (Context -> Expr Context b a) -> Expr Context b a
withHsPackage expr = do
    pkg <- getPackage
    ctx <- getContext
    case pkgCabalFile pkg of
        Just _  -> expr ctx
        Nothing -> mempty

pkgId :: Context -> Action FilePath
pkgId ctx@Context {..} = case pkgCabalFile package of
    Just _  -> pkgIdentifier ctx
    Nothing -> return (pkgName package) -- Non-Haskell packages, e.g. rts

libDir :: Context -> FilePath
libDir Context {..} = stageString stage -/- "lib"

-- | Path to the directory containg the final artifact in a given 'Context'
libPath :: Context -> Action FilePath
libPath context = buildRoot <&> (-/- libDir context)

pkgFile :: Context -> String -> String -> Action FilePath
pkgFile context@Context {..} prefix suffix = do
    path <- buildPath context
    pid  <- pkgId context
    return $ path -/- prefix ++ pid ++ suffix

-- | Path to inplace package configuration file of a given 'Context'.
pkgInplaceConfig :: Context -> Action FilePath
pkgInplaceConfig context = do
    path <- contextPath context
    return $ path -/- "inplace-pkg-config"

-- TODO: Add a @Rules FilePath@ alternative.
-- | Path to the @setup-config@ of a given 'Context'.
pkgSetupConfigFile :: Context -> Action FilePath
pkgSetupConfigFile context = do
    path <- contextPath context
    return $ path -/- "setup-config"

-- | Path to the haddock file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/doc/html/array/array.haddock@.
pkgHaddockFile :: Context -> Action FilePath
pkgHaddockFile Context {..} = do
    root <- buildRoot
    let name = pkgName package
    return $ root -/- "docs/html/libraries" -/- name -/- name <.> "haddock"

-- | Path to the library file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/build/libHSarray-0.5.1.0.a@.
pkgLibraryFile :: Context -> Action FilePath
pkgLibraryFile context@Context {..} = do
    extension <- libsuf way
    pkgFile context "libHS" extension

-- | Path to the GHCi library file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/build/HSarray-0.5.1.0.o@.
pkgGhciLibraryFile :: Context -> Action FilePath
pkgGhciLibraryFile context = pkgFile context "HS" ".o"

-- | Path to the configuration file of a given 'Context'.
pkgConfFile :: Context -> Action FilePath
pkgConfFile ctx@Context {..} = do
    root  <- buildRoot
    pid   <- pkgId ctx
    return $ root -/- relativePackageDbPath stage -/- pid <.> "conf"

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

module Context (
    -- * Context
    Context (..), vanillaContext, stageContext,

    -- * Expressions
    getStage, getPackage, getWay, getStagedSettingList, getBuildPath,
    withHsPackage,

    -- * Paths
    contextDir, buildPath, pkgInplaceConfig, pkgDataFile, pkgSetupConfigFile,
    pkgHaddockFile, pkgLibraryFile, pkgLibraryFile0, pkgGhciLibraryFile,
    pkgConfFile, objectPath
    ) where

import GHC.Generics
import Hadrian.Expression
import Hadrian.Haskell.Cabal

import Base
import Oracles.Setting

-- | Build context for a currently built 'Target'. We generate potentially
-- different build rules for each 'Context'.
data Context = Context
    { stage   :: Stage   -- ^ Currently build Stage
    , package :: Package -- ^ Currently build Package
    , way     :: Way     -- ^ Currently build Way (usually 'vanilla')
    } deriving (Eq, Generic, Show)

instance Binary   Context
instance Hashable Context
instance NFData   Context

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
withHsPackage :: (Monoid a, Semigroup a) => (FilePath -> Expr Context b a) -> Expr Context b a
withHsPackage expr = do
    pkg <- getPackage
    case pkgCabalFile pkg of
        Just file -> expr file
        Nothing   -> mempty

-- | The directory in 'buildRoot' containing build artefacts of a given 'Context'.
contextDir :: Context -> FilePath
contextDir Context {..} = stageString stage -/- pkgPath package

-- | Path to the directory containing build artefacts of a given 'Context'.
buildPath :: Context -> Action FilePath
buildPath context = buildRoot <&> (-/- contextDir context)

-- | Get the build path of the current 'Context'.
getBuildPath :: Expr Context b FilePath
getBuildPath = expr . buildPath =<< getContext

pkgId :: Package -> Action FilePath
pkgId package = case pkgCabalFile package of
    Just file -> pkgIdentifier file
    Nothing   -> return (pkgName package) -- Non-Haskell packages, e.g. rts

pkgFile :: Context -> String -> String -> Action FilePath
pkgFile context@Context {..} prefix suffix = do
    path <- buildPath context
    pid  <- pkgId package
    return $ path -/- prefix ++ pid ++ suffix

-- | Path to inplace package configuration file of a given 'Context'.
pkgInplaceConfig :: Context -> Action FilePath
pkgInplaceConfig context = do
    path <- buildPath context
    return $ path -/- "inplace-pkg-config"

-- | Path to the @package-data.mk@ of a given 'Context'.
pkgDataFile :: Context -> Action FilePath
pkgDataFile context = do
    path <- buildPath context
    return $ path -/- "package-data.mk"

-- | Path to the @setup-config@ of a given 'Context'.
pkgSetupConfigFile :: Context -> Action FilePath
pkgSetupConfigFile context = do
    path <- buildPath context
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

-- | Path to the auxiliary library file of a given 'Context', e.g.:
-- @_build/stage1/compiler/build/libHSghc-8.1-0.a@.
pkgLibraryFile0 :: Context -> Action FilePath
pkgLibraryFile0 context@Context {..} = do
    extension <- libsuf way
    pkgFile context "libHS" ("-0" ++ extension)

-- | Path to the GHCi library file of a given 'Context', e.g.:
-- @_build/stage1/libraries/array/build/HSarray-0.5.1.0.o@.
pkgGhciLibraryFile :: Context -> Action FilePath
pkgGhciLibraryFile context = pkgFile context "HS" ".o"

-- | Path to the configuration file of a given 'Context'.
pkgConfFile :: Context -> Action FilePath
pkgConfFile Context {..} = do
    root  <- buildRoot
    pid   <- pkgId package
    let dbDir | stage == Stage0 = root -/- stage0PackageDbDir
              | otherwise       = inplacePackageDbPath
    return $ dbDir -/- pid <.> "conf"

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

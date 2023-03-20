{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Hadrian.Haskell.Hash (pkgUnitId, pkgHashOracle) where

import Development.Shake

import Hadrian.Haskell.Cabal.Type as C
import Hadrian.Haskell.Cabal
import Hadrian.Oracles.Cabal
import Hadrian.Package

import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Context.Type
import Oracles.Setting
import Hadrian.Target
import Hadrian.Expression
import Builder
import Flavour.Type
import Settings
import Way.Type
import Way
import Packages
import Development.Shake.Classes
import Control.Monad


-- | Read a Cabal file and return the package identifier, e.g. @base-4.10.0.0-abcd@.
-- This needs to be an oracle so it's cached
pkgUnitId :: Context -> Package -> Action String
pkgUnitId ctx' pkg = do
  let ctx = ctx'{package = pkg}
  pid   <- pkgSimpleIdentifier (package ctx)
  phash <- pkgHash ctx
  -- Other boot packages still hardcode their unit-id to just <name>, but we
  -- can have hadrian generate a different unit-id for them just as cabal does
  -- because the boot packages unit-ids are overriden by setting -this-unit-id
  -- in the cabal file
  liftIO $ print $ pid <> "-" <> truncateHash 4 phash
  pure $ pid <> "-" <> truncateHash 4 phash

  where
    truncateHash :: Int -> String -> String
    truncateHash = take

-- | Read a Cabal file and return the package identifier without a hash, e.g. @base-4.10.0.0@.
-- The Cabal file is tracked.
--
-- For an identifier complete with the hash use 'pkgUnitId'
pkgSimpleIdentifier :: Package -> Action String
pkgSimpleIdentifier package = do
    cabal <- readPackageData package
    return $ if null (version cabal)
        then C.name cabal
        else C.name cabal ++ "-" ++ version cabal

data PackageHashInputs = PackageHashInputs {
       pkgHashPkgId         :: String, -- ^ name-version
       pkgHashComponent     :: PackageType,
       pkgHashSourceHash    :: BS.ByteString,
       -- pkgHashPkgConfigDeps :: Set (PkgconfigName, Maybe PkgconfigVersion),
       pkgHashDirectDeps    :: Set.Set String,
       pkgHashOtherConfig   :: PackageHashConfigInputs
     }

-- | Those parts of the package configuration that contribute to the
-- package hash computed by hadrian (which is simpler than cabal's).
--
-- setting in Oracle.setting, which come from system.config
data PackageHashConfigInputs = PackageHashConfigInputs {
       pkgHashCompilerId          :: String,
       pkgHashPlatform            :: String,
       pkgHashFlagAssignment      :: [String], -- complete not partial
       -- pkgHashConfigureScriptArgs :: [String], -- just ./configure for build-type Configure
       pkgHashVanillaLib          :: Bool,
       pkgHashSharedLib           :: Bool,
       pkgHashDynExe              :: Bool,
       pkgHashFullyStaticExe      :: Bool,
       pkgHashGHCiLib             :: Bool,
       pkgHashProfLib             :: Bool,
       pkgHashProfExe             :: Bool,
--       pkgHashProfLibDetail       :: ProfDetailLevel,
--       pkgHashProfExeDetail       :: ProfDetailLevel,
       pkgHashCoverage            :: Bool,
       pkgHashOptimization        :: Int,
       pkgHashSplitObjs           :: Bool,
       pkgHashSplitSections       :: Bool,
       pkgHashStripLibs           :: Bool,
       pkgHashStripExes           :: Bool,
--       pkgHashDebugInfo           :: DebugInfoLevel,
       pkgHashProgramArgs         :: Map String [String],
       pkgHashExtraLibDirs        :: [FilePath],
       pkgHashExtraLibDirsStatic  :: [FilePath],
       pkgHashExtraFrameworkDirs  :: [FilePath],
       pkgHashExtraIncludeDirs    :: [FilePath]
       -- pkgHashProgPrefix          :: Maybe PathTemplate,
       -- pkgHashProgSuffix          :: Maybe PathTemplate,
       -- pkgHashPackageDbs          :: [Maybe PackageDB]
     }
  deriving Show

newtype PkgHashKey = PkgHashKey Context
  deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult PkgHashKey = String

pkgHash :: Context -> Action String
pkgHash = askOracle . PkgHashKey

-- TODO: Needs to be oracle to be cached? Called lots of times
pkgHashOracle :: Rules ()
pkgHashOracle = void $ addOracleCache $ \(PkgHashKey ctx) -> do
  -- RECURSIVE ORACLE: ctx_data <- readContextData ctx
  pkg_data <- readPackageData (package ctx)
  name <- pkgSimpleIdentifier (package ctx)
  let stag = stage ctx
  liftIO $ print ("Package and Package Dependencies", package ctx, packageDependencies pkg_data)
  stagePkgs <- stagePackages stag
  depsHashes <- mapM (\pkg -> pkgHash (ctx { package = pkg })) [pkg | pkg <- packageDependencies pkg_data, pkg `elem` stagePkgs]
  liftIO $ print ("Pkg Deps Hashes", depsHashes)
  flav <- flavour
  let flavourArgs = args flav

  targetOs       <- setting TargetOs
  let pkgHashCompilerId = ""
      pkgHashPlatform = targetOs
  libWays <- interpretInContext ctx (libraryWays flav)
  dyn_ghc <- dynamicGhcPrograms flav
  flags <-  interpret (target ctx (Cabal Flags stag) [] []) flavourArgs
  let pkgHashFlagAssignment = flags
      pkgHashConfigureScriptArgs = ""
      pkgHashVanillaLib = vanilla `Set.member` libWays
      pkgHashSharedLib = dynamic `Set.member` libWays
      pkgHashDynExe = dyn_ghc
      -- TODO: fullyStatic flavour transformer
      pkgHashFullyStaticExe = False
      pkgHashGHCiLib = False
      pkgHashProfLib = profiling `Set.member` libWays
      pkgHashProfExe = package ctx == ghc && ghcProfiled flav stag
      pkgHashCoverage = False -- Can't configure this
      pkgHashOptimization = 0 -- TODO: A bit tricky to configure
      pkgHashSplitObjs = False -- Deprecated
      pkgHashSplitSections = ghcSplitSections flav
      pkgHashStripExes = False
      pkgHashStripLibs = False
      pkgHashDebugInfo = undefined

  liftIO $ print "HI"
  -- ghcArgs <- interpret (target ctx (Cabal Setup stag) [] []) flavourArgs
  liftIO $ print "HI"
  let pkgHashProgramArgs = mempty -- Map.singleton "ghc" ghcArgs
      pkgHashExtraLibDirs = []
      pkgHashExtraLibDirsStatic = []
      pkgHashExtraFrameworkDirs = []
      pkgHashExtraIncludeDirs = []

  let other_config = PackageHashConfigInputs{..}

  return $ BS.unpack $ Base16.encode $ SHA256.hash $
    renderPackageHashInputs $ PackageHashInputs
    {
       pkgHashPkgId       = name
    ,  pkgHashComponent   = pkgType (package ctx)
    ,  pkgHashSourceHash  = ""
    ,  pkgHashDirectDeps  = Set.empty
    ,  pkgHashOtherConfig = other_config
    }

prettyShow, showHashValue :: Show a => a -> String
prettyShow = show
showHashValue = show

renderPackageHashInputs :: PackageHashInputs -> BS.ByteString
renderPackageHashInputs PackageHashInputs{
                          pkgHashPkgId,
                          pkgHashComponent,
                          pkgHashSourceHash,
                          pkgHashDirectDeps,
                          -- pkgHashPkgConfigDeps,
                          pkgHashOtherConfig =
                            PackageHashConfigInputs{..}
                        } =
    -- The purpose of this somewhat laboured rendering (e.g. why not just
    -- use show?) is so that existing package hashes do not change
    -- unnecessarily when new configuration inputs are added into the hash.
    BS.pack $ unlines $ catMaybes $
      [ entry "pkgid"       prettyShow pkgHashPkgId
--      , mentry "component"  show pkgHashComponent
      , entry "src"         showHashValue pkgHashSourceHash
      {-
      , entry "pkg-config-deps"
                            (intercalate ", " . map (\(pn, mb_v) -> prettyShow pn ++
                                                    case mb_v of
                                                        Nothing -> ""
                                                        Just v -> " " ++ prettyShow v)
                                              . Set.toList) pkgHashPkgConfigDeps
                                              -}
      , entry "deps"        (intercalate ", " . map prettyShow
                                              . Set.toList) pkgHashDirectDeps
        -- and then all the config
      , entry "compilerid"  prettyShow pkgHashCompilerId
      , entry "platform" prettyShow pkgHashPlatform
      , opt   "flags" mempty show pkgHashFlagAssignment
--      , opt   "configure-script" [] unwords pkgHashConfigureScriptArgs
      , opt   "vanilla-lib" True  prettyShow pkgHashVanillaLib
      , opt   "shared-lib"  False prettyShow pkgHashSharedLib
      , opt   "dynamic-exe" False prettyShow pkgHashDynExe
      , opt   "fully-static-exe" False prettyShow pkgHashFullyStaticExe
      , opt   "ghci-lib"    False prettyShow pkgHashGHCiLib
      , opt   "prof-lib"    False prettyShow pkgHashProfLib
      , opt   "prof-exe"    False prettyShow pkgHashProfExe
 --     , opt   "prof-lib-detail" ProfDetailDefault showProfDetailLevel pkgHashProfLibDetail
 --     , opt   "prof-exe-detail" ProfDetailDefault showProfDetailLevel pkgHashProfExeDetail
      , opt   "hpc"          False prettyShow pkgHashCoverage
      , opt   "optimisation" 0 (show) pkgHashOptimization
      , opt   "split-objs"   False prettyShow pkgHashSplitObjs
      , opt   "split-sections" False prettyShow pkgHashSplitSections
      , opt   "stripped-lib" False prettyShow pkgHashStripLibs
      , opt   "stripped-exe" True  prettyShow pkgHashStripExes
--      , opt   "debug-info"   NormalDebugInfo (show . fromEnum) pkgHashDebugInfo
      , opt   "extra-lib-dirs"     [] unwords pkgHashExtraLibDirs
      , opt   "extra-lib-dirs-static" [] unwords pkgHashExtraLibDirsStatic
      , opt   "extra-framework-dirs" [] unwords pkgHashExtraFrameworkDirs
      , opt   "extra-include-dirs" [] unwords pkgHashExtraIncludeDirs
--      , opt   "prog-prefix" Nothing (maybe "" fromPathTemplate) pkgHashProgPrefix
--      , opt   "prog-suffix" Nothing (maybe "" fromPathTemplate) pkgHashProgSuffix
--      , opt   "package-dbs" [] (unwords . map show) pkgHashPackageDbs

      ] ++ Map.foldrWithKey (\prog args acc -> opt (prog ++ "-options") [] unwords args : acc) [] pkgHashProgramArgs
  where
    entry key     format value = Just (key ++ ": " ++ format value)
    mentry key    format value = fmap (\v -> key ++ ": " ++ format v) value
    opt   key def format value
         | value == def = Nothing
         | otherwise    = entry key format value

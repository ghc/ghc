{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Hadrian.Haskell.Hash (pkgUnitId, pkgHashOracle) where

import Development.Shake

import Hadrian.Haskell.Cabal.Type
import Hadrian.Haskell.Cabal
import Hadrian.Oracles.Cabal
import Hadrian.Package

import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BS
import Data.ByteString as BS (readFile)
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
import Way
import Packages
import Development.Shake.Classes
import Control.Monad
import Base
import System.Directory.Extra (listFilesRecursive)
import Control.Arrow (first)


-- | Read a Cabal file and return the package identifier, e.g. @base-4.10.0.0-abcd@.
-- This needs to be an oracle so it's cached
pkgUnitId :: Stage -> Package -> Action String
pkgUnitId stg pkg = do
  pid   <- pkgSimpleIdentifier pkg
  use_hash <- hashUnitIds <$> flavour
  if pkgName pkg == "rts"
     -- The unit-id will change depending on the way, we need to treat the rts separately
     then pure pid
     else do
        -- Other boot packages still hardcode their unit-id to just <name>, but we
        -- can have hadrian generate a different unit-id for them just as cabal does
        -- because the boot packages unit-ids are overriden by setting -this-unit-id
        -- in the cabal file
        hash <- if use_hash
                  then do
                    phash <- pkgHash stg pkg
                    return $ truncateHash 4 phash
                  else
                    return "inplace"
        pure $ pid <> "-" <> hash

  where
    truncateHash :: Int -> String -> String
    truncateHash = take

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
       pkgHashVanillaLib          :: Bool,
       pkgHashSharedLib           :: Bool,
       pkgHashDynExe              :: Bool,
       pkgHashGHCiLib             :: Bool,
       pkgHashProfLib             :: Bool,
       pkgHashProfExe             :: Bool,
       pkgHashSplitObjs           :: Bool,
       pkgHashSplitSections       :: Bool,
       pkgHashStripLibs           :: Bool,
       pkgHashStripExes           :: Bool,
       pkgHashProgramArgs         :: Map String [String]
       -- pkgHashProgPrefix          :: Maybe PathTemplate,
       -- pkgHashProgSuffix          :: Maybe PathTemplate,
       -- pkgHashPackageDbs          :: [Maybe PackageDB]
       -- Captured by extraArgs
--       pkgHashDebugInfo           :: DebugInfoLevel,
--       pkgHashCoverage            :: Bool,
--       pkgHashFullyStaticExe      :: Bool,
--       pkgHashProfLibDetail       :: ProfDetailLevel,
--       pkgHashOptimization        :: Int,
--       pkgHashProfExeDetail       :: ProfDetailLevel,
--       pkgHashExtraLibDirs        :: [FilePath],
--       pkgHashExtraLibDirsStatic  :: [FilePath],
--       pkgHashExtraFrameworkDirs  :: [FilePath],
--       pkgHashExtraIncludeDirs    :: [FilePath]
     }
  deriving Show

newtype PkgHashKey = PkgHashKey (Stage, Package)
  deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult PkgHashKey = String

pkgHash :: Stage -> Package -> Action String
pkgHash stg p = askOracle . PkgHashKey $ (stg, p)

-- Needs to be an oracle to be cached. Called lots of times.
pkgHashOracle :: Rules ()
pkgHashOracle = void $ addOracleCache $ \(PkgHashKey (stag, pkg)) -> do
  let vanilla_ctx = vanillaContext stag pkg
  pkg_data <- readPackageData pkg
  name <- pkgSimpleIdentifier pkg
  stagePkgs <- stagePackages stag

  depsHashes <- sequence [pkgHash stag pkg' | pkg' <- packageDependencies pkg_data, pkg' `elem` stagePkgs]

  flav <- flavour
  let flavourArgs = extraArgs flav

  targetOs       <- queryTargetTarget stag queryOS
  projectVersion <- setting ProjectVersion
  let pkgHashCompilerId = "ghc-" ++ projectVersion
      pkgHashPlatform = targetOs

  libWays <- interpretInContext vanilla_ctx (libraryWays flav)
  dyn_ghc <- dynamicGhcPrograms flav stag
  flags <-  interpret (target vanilla_ctx (Cabal Flags stag) [] []) getArgs
  let pkgHashFlagAssignment = flags
      pkgHashVanillaLib = vanilla `Set.member` libWays
      pkgHashSharedLib = dynamic `Set.member` libWays
      pkgHashDynExe = dyn_ghc
      pkgHashGHCiLib = False
      pkgHashProfLib = profiling `Set.member` libWays
      pkgHashProfExe = pkg == ghc && ghcProfiled flav stag
      pkgHashSplitObjs = False -- Deprecated
      pkgHashSplitSections = ghcSplitSections flav
      pkgHashStripExes = False
      pkgHashStripLibs = False

  pkgHashProgramArgs <- Map.unions <$> (forM (Set.toList libWays) $ \lib_way -> do
    let ctx = vanilla_ctx { way = lib_way }
    ghcArgs <- interpret (target ctx (Ghc CompileHs stag) [] []) flavourArgs
    ghcCArgs <- interpret (target ctx (Ghc CompileCWithGhc stag) [] []) flavourArgs
    linkArgs <- interpret (target ctx (Ghc LinkHs stag) [] []) flavourArgs
    ccArgs  <- interpret (target ctx (Cc CompileC stag) [] []) flavourArgs
    hsc2hsArgs <- interpret (target ctx (Hsc2Hs stag) [] []) flavourArgs
    -- TODO: Other arguments for other things (a user could pass extra options to any
    -- builder we know about and we need to enumerate them here)
    return $ Map.fromList (map (first (++ waySuffix lib_way))
                           [("ghc", ghcArgs)
                           ,("ghc-c", ghcCArgs)
                           ,("ghc-link", linkArgs)
                           ,("hsc2hs", hsc2hsArgs)
                           ,("cc", ccArgs) ]))

  let other_config = PackageHashConfigInputs{..}

  files <- allFilesInDirectory (pkgPath pkg)
  need files
  files_hash <- liftIO (SHA256.finalize <$> hashFiles (SHA256.init) files)

  return $ BS.unpack $ Base16.encode $ SHA256.hash $
    renderPackageHashInputs $ PackageHashInputs
    {
       pkgHashPkgId       = name
    ,  pkgHashComponent   = pkgType pkg
    ,  pkgHashSourceHash  = files_hash
    ,  pkgHashDirectDeps  = Set.fromList depsHashes
    ,  pkgHashOtherConfig = other_config
    }

allFilesInDirectory :: FilePath -> Action [FilePath]
allFilesInDirectory dir = liftIO $ listFilesRecursive dir

-- Either use git ls-tree if we are in a git repo, otherwise just get all the
-- files in the given directory.
{- Deb9 toolchain is too old to support git ls-tree properly
  git_tree <- isInGitTree
  if git_tree
    then do
      let gitFiles = filter fileFilter . split (=='\NUL')
          fileFilter file = not (null file) && ((dir ++ "/*") ?== file)
      gitFiles <$> askWithResources [] (target (vanillaContext stage0Boot compiler)  -- This value doesn't matter.
                                               (Git ListFiles) [dir] [])
    else
      liftIO $ listFilesRecursive dir


isInGitTree :: Action Bool
isInGitTree = do
  git_commit <- setting ProjectGitCommitId
  -- git_commit is not set if we are in a source dist
  return $ not ("" == git_commit)
-}



hashFiles :: SHA256.Ctx -> [FilePath] -> IO SHA256.Ctx
hashFiles = foldM hashFile

hashFile :: SHA256.Ctx -> FilePath -> IO SHA256.Ctx
hashFile !ctx fp = do
  contents <- BS.readFile fp
  return $! SHA256.update ctx contents


renderPackageHashInputs :: PackageHashInputs -> BS.ByteString
renderPackageHashInputs PackageHashInputs{
                          pkgHashPkgId,
                          pkgHashComponent,
                          pkgHashSourceHash,
                          pkgHashDirectDeps,
                          pkgHashOtherConfig =
                            PackageHashConfigInputs{..}
                        } =
    -- The purpose of this somewhat laboured rendering (e.g. why not just
    -- use show?) is so that existing package hashes do not change
    -- unnecessarily when new configuration inputs are added into the hash.
    BS.pack $ unlines $ catMaybes $
      [ entry "pkgid"       show pkgHashPkgId
      , entry "component"  show pkgHashComponent
      , entry "src"         show pkgHashSourceHash
      , entry "deps"        (intercalate ", " . map show
                                              . Set.toList) pkgHashDirectDeps
        -- and then all the config
      , entry "compilerid"  show pkgHashCompilerId
      , entry "platform" show pkgHashPlatform
      , opt   "flags" mempty show pkgHashFlagAssignment
      , opt   "vanilla-lib" True  show pkgHashVanillaLib
      , opt   "shared-lib"  False show pkgHashSharedLib
      , opt   "dynamic-exe" False show pkgHashDynExe
      , opt   "ghci-lib"    False show pkgHashGHCiLib
      , opt   "prof-lib"    False show pkgHashProfLib
      , opt   "prof-exe"    False show pkgHashProfExe
      , opt   "split-objs"   False show pkgHashSplitObjs
      , opt   "split-sections" False show pkgHashSplitSections
      , opt   "stripped-lib" False show pkgHashStripLibs
      , opt   "stripped-exe" True  show pkgHashStripExes
      ] ++ Map.foldrWithKey (\prog args acc -> opt (prog ++ "-options") [] unwords args : acc) [] pkgHashProgramArgs
  where
    entry key     format value = Just (key ++ ": " ++ format value)
    opt   key def format value
         | value == def = Nothing
         | otherwise    = entry key format value

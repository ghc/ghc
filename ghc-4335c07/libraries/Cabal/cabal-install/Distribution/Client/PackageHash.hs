{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Functions to calculate nix-style hashes for package ids.
--
-- The basic idea is simple, hash the combination of:
--
--   * the package tarball
--   * the ids of all the direct dependencies
--   * other local configuration (flags, profiling, etc)
--
module Distribution.Client.PackageHash (
    -- * Calculating package hashes
    PackageHashInputs(..),
    PackageHashConfigInputs(..),
    PackageSourceHash,
    hashedInstalledPackageId,
    hashPackageHashInputs,
    renderPackageHashInputs,
    -- ** Platform-specific variations
    hashedInstalledPackageIdLong,
    hashedInstalledPackageIdShort,

    -- * Low level hash choice
    HashValue,
    hashValue,
    showHashValue,
    readFileHashValue,
    hashFromTUF,
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Package
         ( PackageId, PackageIdentifier(..), mkComponentId
         , PkgconfigName )
import Distribution.System
         ( Platform, OS(Windows, OSX), buildOS )
import Distribution.PackageDescription
         ( FlagAssignment, unFlagAssignment, showFlagValue )
import Distribution.Simple.Compiler
         ( CompilerId, OptimisationLevel(..), DebugInfoLevel(..)
         , ProfDetailLevel(..), showProfDetailLevel )
import Distribution.Simple.InstallDirs
         ( PathTemplate, fromPathTemplate )
import Distribution.Text
         ( display )
import Distribution.Version
import Distribution.Client.Types
         ( InstalledPackageId )
import qualified Distribution.Solver.Types.ComponentDeps as CD

import qualified Hackage.Security.Client    as Sec

import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set (Set)

import Data.Function     (on)
import Control.Exception (evaluate)
import System.IO         (withBinaryFile, IOMode(..))


-------------------------------
-- Calculating package hashes
--

-- | Calculate a 'InstalledPackageId' for a package using our nix-style
-- inputs hashing method.
--
-- Note that due to path length limitations on Windows, this function uses
-- a different method on Windows that produces shorted package ids.
-- See 'hashedInstalledPackageIdLong' vs 'hashedInstalledPackageIdShort'.
--
hashedInstalledPackageId :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageId
  | buildOS == Windows = hashedInstalledPackageIdShort
  | buildOS == OSX     = hashedInstalledPackageIdVeryShort
  | otherwise          = hashedInstalledPackageIdLong

-- | Calculate a 'InstalledPackageId' for a package using our nix-style
-- inputs hashing method.
--
-- This produces large ids with big hashes. It is only suitable for systems
-- without significant path length limitations (ie not Windows).
--
hashedInstalledPackageIdLong :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageIdLong pkghashinputs@PackageHashInputs{pkgHashPkgId} =
    mkComponentId $
         display pkgHashPkgId   -- to be a bit user friendly
      ++ "-"
      ++ showHashValue (hashPackageHashInputs pkghashinputs)

-- | On Windows we have serious problems with path lengths. Windows imposes a
-- maximum path length of 260 chars, and even if we can use the windows long
-- path APIs ourselves, we cannot guarantee that ghc, gcc, ld, ar, etc etc all
-- do so too.
--
-- So our only choice is to limit the lengths of the paths, and the only real
-- way to do that is to limit the size of the 'InstalledPackageId's that we
-- generate. We do this by truncating the package names and versions and also
-- by truncating the hash sizes.
--
-- Truncating the package names and versions is technically ok because they are
-- just included for human convenience, the full source package id is included
-- in the hash.
--
-- Truncating the hash size is disappointing but also technically ok. We
-- rely on the hash primarily for collision avoidance not for any security
-- properties (at least for now).
--
hashedInstalledPackageIdShort :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageIdShort pkghashinputs@PackageHashInputs{pkgHashPkgId} =
    mkComponentId $
      intercalate "-"
        -- max length now 64
        [ truncateStr 14 (display name)
        , truncateStr  8 (display version)
        , showHashValue (truncateHash (hashPackageHashInputs pkghashinputs))
        ]
  where
    PackageIdentifier name version = pkgHashPkgId

    -- Truncate a 32 byte SHA256 hash to 160bits, 20 bytes :-(
    -- It'll render as 40 hex chars.
    truncateHash (HashValue h) = HashValue (BS.take 20 h)

    -- Truncate a string, with a visual indication that it is truncated.
    truncateStr n s | length s <= n = s
                    | otherwise     = take (n-1) s ++ "_"

-- | On macOS we shorten the name very aggressively.  The mach-o linker on
-- macOS has a limited load command size, to which the name of the lirbary
-- as well as it's relative path (\@rpath) entry count.  To circumvent this,
-- on macOS the libraries are not stored as
--  @store/<libraryname>/libHS<libraryname>.dylib@
-- where libraryname contains the librarys name, version and abi hash, but in
--  @store/lib/libHS<very short libraryname>.dylib@
-- where the very short library name drops all vowels from the package name,
-- and truncates the hash to 4 bytes.
--
-- We therefore only need one \@rpath entry to @store/lib@ instead of one
-- \@rpath entry for each library. And the reduced library name saves some
-- additional space.
--
-- This however has two major drawbacks:
-- 1) Packages can easier collide due to the shortened hash.
-- 2) The lirbaries are *not* prefix relocateable anymore as they all end up
--    in the same @store/lib@ folder.
--
-- The ultimate solution would have to include generating proxy dynamic
-- libraries on macOS, such that the proxy libraries and the linked libraries
-- stay under the load command limit, and the recursive linker is still able
-- to link all of them.
hashedInstalledPackageIdVeryShort :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageIdVeryShort pkghashinputs@PackageHashInputs{pkgHashPkgId} =
  mkComponentId $
    intercalate "-"
      [ filter (not . flip elem "aeiou") (display name)
      , display version
      , showHashValue (truncateHash (hashPackageHashInputs pkghashinputs))
      ]
  where
    PackageIdentifier name version = pkgHashPkgId
    truncateHash (HashValue h) = HashValue (BS.take 4 h)

-- | All the information that contribues to a package's hash, and thus its
-- 'InstalledPackageId'.
--
data PackageHashInputs = PackageHashInputs {
       pkgHashPkgId         :: PackageId,
       pkgHashComponent     :: Maybe CD.Component,
       pkgHashSourceHash    :: PackageSourceHash,
       pkgHashPkgConfigDeps :: Set (PkgconfigName, Maybe Version),
       pkgHashDirectDeps    :: Set InstalledPackageId,
       pkgHashOtherConfig   :: PackageHashConfigInputs
     }

type PackageSourceHash = HashValue

-- | Those parts of the package configuration that contribute to the
-- package hash.
--
data PackageHashConfigInputs = PackageHashConfigInputs {
       pkgHashCompilerId          :: CompilerId,
       pkgHashPlatform            :: Platform,
       pkgHashFlagAssignment      :: FlagAssignment, -- complete not partial
       pkgHashConfigureScriptArgs :: [String], -- just ./configure for build-type Configure
       pkgHashVanillaLib          :: Bool,
       pkgHashSharedLib           :: Bool,
       pkgHashDynExe              :: Bool,
       pkgHashGHCiLib             :: Bool,
       pkgHashProfLib             :: Bool,
       pkgHashProfExe             :: Bool,
       pkgHashProfLibDetail       :: ProfDetailLevel,
       pkgHashProfExeDetail       :: ProfDetailLevel,
       pkgHashCoverage            :: Bool,
       pkgHashOptimization        :: OptimisationLevel,
       pkgHashSplitObjs           :: Bool,
       pkgHashStripLibs           :: Bool,
       pkgHashStripExes           :: Bool,
       pkgHashDebugInfo           :: DebugInfoLevel,
       pkgHashProgramArgs         :: Map String [String],
       pkgHashExtraLibDirs        :: [FilePath],
       pkgHashExtraFrameworkDirs  :: [FilePath],
       pkgHashExtraIncludeDirs    :: [FilePath],
       pkgHashProgPrefix          :: Maybe PathTemplate,
       pkgHashProgSuffix          :: Maybe PathTemplate

--     TODO: [required eventually] pkgHashToolsVersions     ?
--     TODO: [required eventually] pkgHashToolsExtraOptions ?
--     TODO: [research required] and what about docs?
     }
  deriving Show


-- | Calculate the overall hash to be used for an 'InstalledPackageId'.
--
hashPackageHashInputs :: PackageHashInputs -> HashValue
hashPackageHashInputs = hashValue . renderPackageHashInputs

-- | Render a textual representation of the 'PackageHashInputs'.
--
-- The 'hashValue' of this text is the overall package hash.
--
renderPackageHashInputs :: PackageHashInputs -> LBS.ByteString
renderPackageHashInputs PackageHashInputs{
                          pkgHashPkgId,
                          pkgHashComponent,
                          pkgHashSourceHash,
                          pkgHashDirectDeps,
                          pkgHashPkgConfigDeps,
                          pkgHashOtherConfig =
                            PackageHashConfigInputs{..}
                        } =
    -- The purpose of this somewhat laboured rendering (e.g. why not just
    -- use show?) is so that existing package hashes do not change
    -- unnecessarily when new configuration inputs are added into the hash.

    -- In particular, the assumption is that when a new configuration input
    -- is included into the hash, that existing packages will typically get
    -- the default value for that feature. So if we avoid adding entries with
    -- the default value then most of the time adding new features will not
    -- change the hashes of existing packages and so fewer packages will need
    -- to be rebuilt. 

    --TODO: [nice to have] ultimately we probably want to put this config info
    -- into the ghc-pkg db. At that point this should probably be changed to
    -- use the config file infrastructure so it can be read back in again.
    LBS.pack $ unlines $ catMaybes $
      [ entry "pkgid"       display pkgHashPkgId
      , mentry "component"  show pkgHashComponent
      , entry "src"         showHashValue pkgHashSourceHash
      , entry "pkg-config-deps"
                            (intercalate ", " . map (\(pn, mb_v) -> display pn ++
                                                    case mb_v of
                                                        Nothing -> ""
                                                        Just v -> " " ++ display v)
                                              . Set.toList) pkgHashPkgConfigDeps
      , entry "deps"        (intercalate ", " . map display
                                              . Set.toList) pkgHashDirectDeps
        -- and then all the config
      , entry "compilerid"  display pkgHashCompilerId
      , entry "platform"    display pkgHashPlatform
      , opt   "flags" mempty showFlagAssignment pkgHashFlagAssignment
      , opt   "configure-script" [] unwords pkgHashConfigureScriptArgs
      , opt   "vanilla-lib" True  display pkgHashVanillaLib
      , opt   "shared-lib"  False display pkgHashSharedLib
      , opt   "dynamic-exe" False display pkgHashDynExe
      , opt   "ghci-lib"    False display pkgHashGHCiLib
      , opt   "prof-lib"    False display pkgHashProfLib
      , opt   "prof-exe"    False display pkgHashProfExe
      , opt   "prof-lib-detail" ProfDetailDefault showProfDetailLevel pkgHashProfLibDetail 
      , opt   "prof-exe-detail" ProfDetailDefault showProfDetailLevel pkgHashProfExeDetail 
      , opt   "hpc"          False display pkgHashCoverage
      , opt   "optimisation" NormalOptimisation (show . fromEnum) pkgHashOptimization
      , opt   "split-objs"   False display pkgHashSplitObjs
      , opt   "stripped-lib" False display pkgHashStripLibs
      , opt   "stripped-exe" True  display pkgHashStripExes
      , opt   "debug-info"   NormalDebugInfo (show . fromEnum) pkgHashDebugInfo
      , opt   "extra-lib-dirs"     [] unwords pkgHashExtraLibDirs
      , opt   "extra-framework-dirs" [] unwords pkgHashExtraFrameworkDirs
      , opt   "extra-include-dirs" [] unwords pkgHashExtraIncludeDirs
      , opt   "prog-prefix" Nothing (maybe "" fromPathTemplate) pkgHashProgPrefix
      , opt   "prog-suffix" Nothing (maybe "" fromPathTemplate) pkgHashProgSuffix
      ] ++ Map.foldrWithKey (\prog args acc -> opt (prog ++ "-options") [] unwords args : acc) [] pkgHashProgramArgs
  where
    entry key     format value = Just (key ++ ": " ++ format value)
    mentry key    format value = fmap (\v -> key ++ ": " ++ format v) value
    opt   key def format value
         | value == def = Nothing
         | otherwise    = entry key format value

    showFlagAssignment = unwords . map showFlagValue . sortBy (compare `on` fst) . unFlagAssignment

-----------------------------------------------
-- The specific choice of hash implementation
--

-- Is a crypto hash necessary here? One thing to consider is who controls the
-- inputs and what's the result of a hash collision. Obviously we should not
-- install packages we don't trust because they can run all sorts of code, but
-- if I've checked there's no TH, no custom Setup etc, is there still a
-- problem? If someone provided us a tarball that hashed to the same value as
-- some other package and we installed it, we could end up re-using that
-- installed package in place of another one we wanted. So yes, in general
-- there is some value in preventing intentional hash collisions in installed
-- package ids.

newtype HashValue = HashValue BS.ByteString
  deriving (Eq, Show, Typeable)

instance Binary HashValue where
  put (HashValue digest) = put digest
  get = do
    digest <- get
    -- Cannot do any sensible validation here. Although we use SHA256
    -- for stuff we hash ourselves, we can also get hashes from TUF
    -- and that can in principle use different hash functions in future.
    return (HashValue digest)

-- | Hash some data. Currently uses SHA256.
--
hashValue :: LBS.ByteString -> HashValue
hashValue = HashValue . SHA256.hashlazy

showHashValue :: HashValue -> String
showHashValue (HashValue digest) = BS.unpack (Base16.encode digest)

-- | Hash the content of a file. Uses SHA256.
--
readFileHashValue :: FilePath -> IO HashValue
readFileHashValue tarball =
    withBinaryFile tarball ReadMode $ \hnd ->
      evaluate . hashValue =<< LBS.hGetContents hnd

-- | Convert a hash from TUF metadata into a 'PackageSourceHash'.
--
-- Note that TUF hashes don't neessarily have to be SHA256, since it can
-- support new algorithms in future.
--
hashFromTUF :: Sec.Hash -> HashValue
hashFromTUF (Sec.Hash hashstr) =
    --TODO: [code cleanup] either we should get TUF to use raw bytestrings or
    -- perhaps we should also just use a base16 string as the internal rep.
    case Base16.decode (BS.pack hashstr) of
      (hash, trailing) | not (BS.null hash) && BS.null trailing
        -> HashValue hash
      _ -> error "hashFromTUF: cannot decode base16 hash"


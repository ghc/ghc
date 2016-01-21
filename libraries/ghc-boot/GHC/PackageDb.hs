{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PackageDb
-- Copyright   :  (c) The University of Glasgow 2009, Duncan Coutts 2014
--
-- Maintainer  :  ghc-devs@haskell.org
-- Portability :  portable
--
-- This module provides the view of GHC's database of registered packages that
-- is shared between GHC the compiler\/library, and the ghc-pkg program. It
-- defines the database format that is shared between GHC and ghc-pkg.
--
-- The database format, and this library are constructed so that GHC does not
-- have to depend on the Cabal library. The ghc-pkg program acts as the
-- gateway between the external package format (which is defined by Cabal) and
-- the internal package format which is specialised just for GHC.
--
-- GHC the compiler only needs some of the information which is kept about
-- registerd packages, such as module names, various paths etc. On the other
-- hand ghc-pkg has to keep all the information from Cabal packages and be able
-- to regurgitate it for users and other tools.
--
-- The first trick is that we duplicate some of the information in the package
-- database. We essentially keep two versions of the datbase in one file, one
-- version used only by ghc-pkg which keeps the full information (using the
-- serialised form of the 'InstalledPackageInfo' type defined by the Cabal
-- library); and a second version written by ghc-pkg and read by GHC which has
-- just the subset of information that GHC needs.
--
-- The second trick is that this module only defines in detail the format of
-- the second version -- the bit GHC uses -- and the part managed by ghc-pkg
-- is kept in the file but here we treat it as an opaque blob of data. That way
-- this library avoids depending on Cabal.
--
module GHC.PackageDb (
       InstalledPackageInfo(..),
       ExposedModule(..),
       OriginalModule(..),
       BinaryStringRep(..),
       emptyInstalledPackageInfo,
       readPackageDbForGhc,
       readPackageDbForGhcPkg,
       writePackageDb
  ) where

import Data.Version (Version(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Lazy.Internal as BS.Lazy (defaultChunkSize)
import Data.Binary as Bin
import Data.Binary.Put as Bin
import Data.Binary.Get as Bin
import Control.Exception as Exception
import Control.Monad (when)
import System.FilePath
import System.IO
import System.IO.Error
import GHC.IO.Exception (IOErrorType(InappropriateType))
import System.Directory


-- | This is a subset of Cabal's 'InstalledPackageInfo', with just the bits
-- that GHC is interested in.
--
data InstalledPackageInfo srcpkgid srcpkgname unitid modulename
   = InstalledPackageInfo {
       unitId             :: unitid,
       sourcePackageId    :: srcpkgid,
       packageName        :: srcpkgname,
       packageVersion     :: Version,
       abiHash            :: String,
       depends            :: [unitid],
       importDirs         :: [FilePath],
       hsLibraries        :: [String],
       extraLibraries     :: [String],
       extraGHCiLibraries :: [String],
       libraryDirs        :: [FilePath],
       frameworks         :: [String],
       frameworkDirs      :: [FilePath],
       ldOptions          :: [String],
       ccOptions          :: [String],
       includes           :: [String],
       includeDirs        :: [FilePath],
       haddockInterfaces  :: [FilePath],
       haddockHTMLs       :: [FilePath],
       exposedModules     :: [ExposedModule unitid modulename],
       hiddenModules      :: [modulename],
       exposed            :: Bool,
       trusted            :: Bool
     }
  deriving (Eq, Show)

-- | A convenience constraint synonym for common constraints over parameters
-- to 'InstalledPackageInfo'.
type RepInstalledPackageInfo srcpkgid srcpkgname unitid modulename =
    (BinaryStringRep srcpkgid, BinaryStringRep srcpkgname,
     BinaryStringRep unitid, BinaryStringRep modulename)

-- | An original module is a fully-qualified module name (installed package ID
-- plus module name) representing where a module was *originally* defined
-- (i.e., the 'exposedReexport' field of the original ExposedModule entry should
-- be 'Nothing').  Invariant: an OriginalModule never points to a reexport.
data OriginalModule unitid modulename
   = OriginalModule {
       originalPackageId :: unitid,
       originalModuleName :: modulename
     }
  deriving (Eq, Show)

-- | Represents a module name which is exported by a package, stored in the
-- 'exposedModules' field.  A module export may be a reexport (in which case
-- 'exposedReexport' is filled in with the original source of the module).
-- Thus:
--
--  * @ExposedModule n Nothing@ represents an exposed module @n@ which
--    was defined in this package.
--
--  * @ExposedModule n (Just o)@ represents a reexported module @n@
--    which was originally defined in @o@.
--
-- We use a 'Maybe' data types instead of an ADT with two branches because this
-- representation allows us to treat reexports uniformly.
data ExposedModule unitid modulename
   = ExposedModule {
       exposedName      :: modulename,
       exposedReexport  :: Maybe (OriginalModule unitid modulename)
     }
  deriving (Eq, Show)

class BinaryStringRep a where
  fromStringRep :: BS.ByteString -> a
  toStringRep   :: a -> BS.ByteString

emptyInstalledPackageInfo :: RepInstalledPackageInfo a b c d
                          => InstalledPackageInfo a b c d
emptyInstalledPackageInfo =
  InstalledPackageInfo {
       unitId             = fromStringRep BS.empty,
       sourcePackageId    = fromStringRep BS.empty,
       packageName        = fromStringRep BS.empty,
       packageVersion     = Version [] [],
       abiHash            = "",
       depends            = [],
       importDirs         = [],
       hsLibraries        = [],
       extraLibraries     = [],
       extraGHCiLibraries = [],
       libraryDirs        = [],
       frameworks         = [],
       frameworkDirs      = [],
       ldOptions          = [],
       ccOptions          = [],
       includes           = [],
       includeDirs        = [],
       haddockInterfaces  = [],
       haddockHTMLs       = [],
       exposedModules     = [],
       hiddenModules      = [],
       exposed            = False,
       trusted            = False
  }

-- | Read the part of the package DB that GHC is interested in.
--
readPackageDbForGhc :: RepInstalledPackageInfo a b c d =>
                       FilePath -> IO [InstalledPackageInfo a b c d]
readPackageDbForGhc file =
    decodeFromFile file getDbForGhc
  where
    getDbForGhc = do
      _version    <- getHeader
      _ghcPartLen <- get :: Get Word32
      ghcPart     <- get
      -- the next part is for ghc-pkg, but we stop here.
      return ghcPart

-- | Read the part of the package DB that ghc-pkg is interested in
--
-- Note that the Binary instance for ghc-pkg's representation of packages
-- is not defined in this package. This is because ghc-pkg uses Cabal types
-- (and Binary instances for these) which this package does not depend on.
--
readPackageDbForGhcPkg :: Binary pkgs => FilePath -> IO pkgs
readPackageDbForGhcPkg file =
    decodeFromFile file getDbForGhcPkg
  where
    getDbForGhcPkg = do
      _version    <- getHeader
      -- skip over the ghc part
      ghcPartLen  <- get :: Get Word32
      _ghcPart    <- skip (fromIntegral ghcPartLen)
      -- the next part is for ghc-pkg
      ghcPkgPart  <- get
      return ghcPkgPart

-- | Write the whole of the package DB, both parts.
--
writePackageDb :: (Binary pkgs, RepInstalledPackageInfo a b c d) =>
                  FilePath -> [InstalledPackageInfo a b c d] -> pkgs -> IO ()
writePackageDb file ghcPkgs ghcPkgPart =
    writeFileAtomic file (runPut putDbForGhcPkg)
  where
    putDbForGhcPkg = do
        putHeader
        put               ghcPartLen
        putLazyByteString ghcPart
        put               ghcPkgPart
      where
        ghcPartLen :: Word32
        ghcPartLen = fromIntegral (BS.Lazy.length ghcPart)
        ghcPart    = encode ghcPkgs

getHeader :: Get (Word32, Word32)
getHeader = do
    magic <- getByteString (BS.length headerMagic)
    when (magic /= headerMagic) $
      fail "not a ghc-pkg db file, wrong file magic number"

    majorVersion <- get :: Get Word32
    -- The major version is for incompatible changes

    minorVersion <- get :: Get Word32
    -- The minor version is for compatible extensions

    when (majorVersion /= 1) $
      fail "unsupported ghc-pkg db format version"
    -- If we ever support multiple major versions then we'll have to change
    -- this code

    -- The header can be extended without incrementing the major version,
    -- we ignore fields we don't know about (currently all).
    headerExtraLen <- get :: Get Word32
    skip (fromIntegral headerExtraLen)

    return (majorVersion, minorVersion)

putHeader :: Put
putHeader = do
    putByteString headerMagic
    put majorVersion
    put minorVersion
    put headerExtraLen
  where
    majorVersion   = 1 :: Word32
    minorVersion   = 0 :: Word32
    headerExtraLen = 0 :: Word32

headerMagic :: BS.ByteString
headerMagic = BS.Char8.pack "\0ghcpkg\0"


-- TODO: we may be able to replace the following with utils from the binary
-- package in future.

-- | Feed a 'Get' decoder with data chunks from a file.
--
decodeFromFile :: FilePath -> Get a -> IO a
decodeFromFile file decoder =
    withBinaryFile file ReadMode $ \hnd ->
      feed hnd (runGetIncremental decoder)
  where
    feed hnd (Partial k)  = do chunk <- BS.hGet hnd BS.Lazy.defaultChunkSize
                               if BS.null chunk
                                 then feed hnd (k Nothing)
                                 else feed hnd (k (Just chunk))
    feed _ (Done _ _ res) = return res
    feed _ (Fail _ _ msg) = ioError err
      where
        err = mkIOError InappropriateType loc Nothing (Just file)
              `ioeSetErrorString` msg
        loc = "GHC.PackageDb.readPackageDb"

-- Copied from Cabal's Distribution.Simple.Utils.
writeFileAtomic :: FilePath -> BS.Lazy.ByteString -> IO ()
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  Exception.bracketOnError
    (openBinaryTempFileWithDefaultPermissions targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        BS.Lazy.hPut handle content
        hClose handle
        renameFile tmpPath targetPath)

instance (RepInstalledPackageInfo a b c d) =>
         Binary (InstalledPackageInfo a b c d) where
  put (InstalledPackageInfo
         unitId sourcePackageId
         packageName packageVersion
         abiHash depends importDirs
         hsLibraries extraLibraries extraGHCiLibraries libraryDirs
         frameworks frameworkDirs
         ldOptions ccOptions
         includes includeDirs
         haddockInterfaces haddockHTMLs
         exposedModules hiddenModules
         exposed trusted) = do
    put (toStringRep sourcePackageId)
    put (toStringRep packageName)
    put packageVersion
    put (toStringRep unitId)
    put abiHash
    put (map toStringRep depends)
    put importDirs
    put hsLibraries
    put extraLibraries
    put extraGHCiLibraries
    put libraryDirs
    put frameworks
    put frameworkDirs
    put ldOptions
    put ccOptions
    put includes
    put includeDirs
    put haddockInterfaces
    put haddockHTMLs
    put exposedModules
    put (map toStringRep hiddenModules)
    put exposed
    put trusted

  get = do
    sourcePackageId    <- get
    packageName        <- get
    packageVersion     <- get
    unitId         <- get
    abiHash            <- get
    depends            <- get
    importDirs         <- get
    hsLibraries        <- get
    extraLibraries     <- get
    extraGHCiLibraries <- get
    libraryDirs        <- get
    frameworks         <- get
    frameworkDirs      <- get
    ldOptions          <- get
    ccOptions          <- get
    includes           <- get
    includeDirs        <- get
    haddockInterfaces  <- get
    haddockHTMLs       <- get
    exposedModules     <- get
    hiddenModules      <- get
    exposed            <- get
    trusted            <- get
    return (InstalledPackageInfo
              (fromStringRep unitId)
              (fromStringRep sourcePackageId)
              (fromStringRep packageName) packageVersion
              abiHash
              (map fromStringRep depends)
              importDirs
              hsLibraries extraLibraries extraGHCiLibraries libraryDirs
              frameworks frameworkDirs
              ldOptions ccOptions
              includes includeDirs
              haddockInterfaces haddockHTMLs
              exposedModules
              (map fromStringRep hiddenModules)
              exposed trusted)

instance (BinaryStringRep a, BinaryStringRep b) =>
         Binary (OriginalModule a b) where
  put (OriginalModule originalPackageId originalModuleName) = do
    put (toStringRep originalPackageId)
    put (toStringRep originalModuleName)
  get = do
    originalPackageId <- get
    originalModuleName <- get
    return (OriginalModule (fromStringRep originalPackageId)
                           (fromStringRep originalModuleName))

instance (BinaryStringRep a, BinaryStringRep b) =>
         Binary (ExposedModule a b) where
  put (ExposedModule exposedName exposedReexport) = do
    put (toStringRep exposedName)
    put exposedReexport
  get = do
    exposedName <- get
    exposedReexport <- get
    return (ExposedModule (fromStringRep exposedName)
                          exposedReexport)

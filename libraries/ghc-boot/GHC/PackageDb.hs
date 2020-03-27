{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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
-- registered packages, such as module names, various paths etc. On the other
-- hand ghc-pkg has to keep all the information from Cabal packages and be able
-- to regurgitate it for users and other tools.
--
-- The first trick is that we duplicate some of the information in the package
-- database. We essentially keep two versions of the database in one file, one
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
module GHC.PackageDb
   ( GenericUnitInfo(..)
   , emptyGenericUnitInfo
   -- * Read and write
   , DbMode(..)
   , DbOpenMode(..)
   , isDbOpenReadMode
   , readPackageDbForGhc
   , readPackageDbForGhcPkg
   , writePackageDb
   -- * Locking
   , PackageDbLock
   , lockPackageDb
   , unlockPackageDb
   -- * Misc
   , DbModule(..)
   , DbUnitId(..)
   , BinaryStringRep(..)
   , DbUnitIdModuleRep(..)
   )
where

import Prelude -- See note [Why do we import Prelude here?]
import Data.Version (Version(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Lazy.Internal as BS.Lazy (defaultChunkSize)
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import Data.Binary as Bin
import Data.Binary.Put as Bin
import Data.Binary.Get as Bin
import Control.Exception as Exception
import Control.Monad (when)
import System.FilePath
import System.IO
import System.IO.Error
import GHC.IO.Exception (IOErrorType(InappropriateType))
import GHC.IO.Handle.Lock
import System.Directory


-- | Information about an unit (a unit is an installed module library).
--
-- This is a subset of Cabal's 'InstalledPackageInfo', with just the bits
-- that GHC is interested in.
--
-- Some types are left as parameters to be instantiated differently in ghc-pkg
-- and in ghc itself.
--
data GenericUnitInfo compid srcpkgid srcpkgname instunitid unitid modulename mod = GenericUnitInfo
   { unitId             :: instunitid
      -- ^ Unique unit identifier that is used during compilation (e.g. to
      -- generate symbols).

   , unitInstanceOf     :: compid
      -- ^ Identifier of an indefinite unit (i.e. with module holes) that this
      -- unit is an instance of.

   , unitInstantiations :: [(modulename, mod)]
      -- ^ How this unit instantiates some of its module holes. Map hole module
      -- names to actual module

   , unitPackageId      :: srcpkgid
      -- ^ Source package identifier.
      --
      -- Cabal instantiates this with Distribution.Types.PackageId.PackageId
      -- type which only contains the source package name and version. Notice
      -- that it doesn't contain the Hackage revision, nor any kind of hash.

   , unitPackageName    :: srcpkgname
      -- ^ Source package name

   , unitPackageVersion :: Version
      -- ^ Source package version

   , unitComponentName  :: Maybe srcpkgname
      -- ^ Name of the component.
      --
      -- Cabal supports more than one components (libraries, executables,
      -- testsuites) in the same package. Each component has a name except the
      -- default one (that can only be a library component) for which we use
      -- "Nothing".
      --
      -- GHC only deals with "library" components as they are the only kind of
      -- components that can be registered in a database and used by other
      -- modules.

   , unitAbiHash        :: String
      -- ^ ABI hash used to avoid mixing up units compiled with different
      -- dependencies, compiler, options, etc.

   , unitDepends        :: [instunitid]
      -- ^ Identifiers of the units this one depends on

   , unitAbiDepends     :: [(instunitid, String)]
     -- ^ Like 'unitDepends', but each dependency is annotated with the ABI hash
     -- we expect the dependency to respect.

   , unitImportDirs     :: [FilePath]
      -- ^ Directories containing module interfaces

   , unitLibraries      :: [String]
      -- ^ Names of the Haskell libraries provided by this unit

   , unitExtDepLibsSys  :: [String]
      -- ^ Names of the external system libraries that this unit depends on. See
      -- also `unitExtDepLibsGhc` field.

   , unitExtDepLibsGhc  :: [String]
      -- ^ Because of slight differences between the GHC dynamic linker (in
      -- GHC.Runtime.Linker) and the
      -- native system linker, some packages have to link with a different list
      -- of libraries when using GHC's. Examples include: libs that are actually
      -- gnu ld scripts, and the possibility that the .a libs do not exactly
      -- match the .so/.dll equivalents.
      --
      -- If this field is set, then we use that instead of the
      -- `unitExtDepLibsSys` field.

   , unitLibraryDirs    :: [FilePath]
      -- ^ Directories containing libraries provided by this unit. See also
      -- `unitLibraryDynDirs`.
      --
      -- It seems to be used to store paths to external library dependencies
      -- too.

   , unitLibraryDynDirs :: [FilePath]
      -- ^ Directories containing the dynamic libraries provided by this unit.
      -- See also `unitLibraryDirs`.
      --
      -- It seems to be used to store paths to external dynamic library
      -- dependencies too.

   , unitExtDepFrameworks :: [String]
      -- ^ Names of the external MacOS frameworks that this unit depends on.

   , unitExtDepFrameworkDirs :: [FilePath]
      -- ^ Directories containing MacOS frameworks that this unit depends
      -- on.

   , unitLinkerOptions  :: [String]
      -- ^ Linker (e.g. ld) command line options

   , unitCcOptions      :: [String]
      -- ^ C compiler options that needs to be passed to the C compiler when we
      -- compile some C code against this unit.

   , unitIncludes       :: [String]
      -- ^ C header files that are required by this unit (provided by this unit
      -- or external)

   , unitIncludeDirs    :: [FilePath]
      -- ^ Directories containing C header files that this unit depends
      -- on.

   , unitHaddockInterfaces :: [FilePath]
      -- ^ Paths to Haddock interface files for this unit

   , unitHaddockHTMLs   :: [FilePath]
      -- ^ Paths to Haddock directories containing HTML files

   , unitExposedModules :: [(modulename, Maybe mod)]
      -- ^ Modules exposed by the unit.
      --
      -- A module can be re-exported from another package. In this case, we
      -- indicate the module origin in the second parameter.

   , unitHiddenModules  :: [modulename]
      -- ^ Hidden modules.
      --
      -- These are useful for error reporting (e.g. if a hidden module is
      -- imported)

   , unitIsIndefinite   :: Bool
      -- ^ True if this unit has some module holes that need to be instantiated
      -- with real modules to make the unit usable (a.k.a. Backpack).

   , unitIsExposed      :: Bool
      -- ^ True if the unit is exposed. A unit could be installed in a database
      -- by "disabled" by not being exposed.

   , unitIsTrusted      :: Bool
      -- ^ True if the unit is trusted (cf Safe Haskell)

   }
   deriving (Eq, Show)

-- | A convenience constraint synonym for common constraints over parameters
-- to 'GenericUnitInfo'.
type RepGenericUnitInfo compid srcpkgid srcpkgname instunitid unitid modulename mod =
    (BinaryStringRep srcpkgid, BinaryStringRep srcpkgname,
     BinaryStringRep modulename, BinaryStringRep compid,
     BinaryStringRep instunitid,
     DbUnitIdModuleRep instunitid compid unitid modulename mod)

-- | A type-class for the types which can be converted into 'DbModule'/'DbUnitId'.
-- There is only one type class because these types are mutually recursive.
-- NB: The functional dependency helps out type inference in cases
-- where types would be ambiguous.
class DbUnitIdModuleRep instunitid compid unitid modulename mod
    | mod -> unitid, unitid -> mod, mod -> modulename, unitid -> compid, unitid -> instunitid
    where
  fromDbModule :: DbModule instunitid compid unitid modulename mod -> mod
  toDbModule :: mod -> DbModule instunitid compid unitid modulename mod
  fromDbUnitId :: DbUnitId instunitid compid unitid modulename mod -> unitid
  toDbUnitId :: unitid -> DbUnitId instunitid compid unitid modulename mod

-- | @ghc-boot@'s copy of 'Module', i.e. what is serialized to the database.
-- Use 'DbUnitIdModuleRep' to convert it into an actual 'Module'.
-- It has phantom type parameters as this is the most convenient way
-- to avoid undecidable instances.
data DbModule instunitid compid unitid modulename mod
   = DbModule {
       dbModuleUnitId :: unitid,
       dbModuleName :: modulename
     }
   | DbModuleVar {
       dbModuleVarName :: modulename
     }
  deriving (Eq, Show)

-- | @ghc-boot@'s copy of 'UnitId', i.e. what is serialized to the database.
-- Use 'DbUnitIdModuleRep' to convert it into an actual 'UnitId'.
-- It has phantom type parameters as this is the most convenient way
-- to avoid undecidable instances.
data DbUnitId instunitid compid unitid modulename mod
   = DbUnitId compid [(modulename, mod)]
   | DbInstalledUnitId instunitid
  deriving (Eq, Show)

class BinaryStringRep a where
  fromStringRep :: BS.ByteString -> a
  toStringRep   :: a -> BS.ByteString

emptyGenericUnitInfo :: RepGenericUnitInfo a b c d e f g
                          => GenericUnitInfo a b c d e f g
emptyGenericUnitInfo =
  GenericUnitInfo {
       unitId                  = fromStringRep BS.empty,
       unitInstanceOf          = fromStringRep BS.empty,
       unitInstantiations      = [],
       unitPackageId           = fromStringRep BS.empty,
       unitPackageName         = fromStringRep BS.empty,
       unitPackageVersion      = Version [] [],
       unitComponentName       = Nothing,
       unitAbiHash             = "",
       unitDepends             = [],
       unitAbiDepends          = [],
       unitImportDirs          = [],
       unitLibraries           = [],
       unitExtDepLibsSys       = [],
       unitExtDepLibsGhc       = [],
       unitLibraryDirs         = [],
       unitLibraryDynDirs      = [],
       unitExtDepFrameworks    = [],
       unitExtDepFrameworkDirs = [],
       unitLinkerOptions       = [],
       unitCcOptions           = [],
       unitIncludes            = [],
       unitIncludeDirs         = [],
       unitHaddockInterfaces   = [],
       unitHaddockHTMLs        = [],
       unitExposedModules      = [],
       unitHiddenModules       = [],
       unitIsIndefinite        = False,
       unitIsExposed           = False,
       unitIsTrusted           = False
  }

-- | Represents a lock of a package db.
newtype PackageDbLock = PackageDbLock Handle

-- | Acquire an exclusive lock related to package DB under given location.
lockPackageDb :: FilePath -> IO PackageDbLock

-- | Release the lock related to package DB.
unlockPackageDb :: PackageDbLock -> IO ()

-- | Acquire a lock of given type related to package DB under given location.
lockPackageDbWith :: LockMode -> FilePath -> IO PackageDbLock
lockPackageDbWith mode file = do
  -- We are trying to open the lock file and then lock it. Thus the lock file
  -- needs to either exist or we need to be able to create it. Ideally we
  -- would not assume that the lock file always exists in advance. When we are
  -- dealing with a package DB where we have write access then if the lock
  -- file does not exist then we can create it by opening the file in
  -- read/write mode. On the other hand if we are dealing with a package DB
  -- where we do not have write access (e.g. a global DB) then we can only
  -- open in read mode, and the lock file had better exist already or we're in
  -- trouble. So for global read-only DBs on platforms where we must lock the
  -- DB for reading then we will require that the installer/packaging has
  -- included the lock file.
  --
  -- Thus the logic here is to first try opening in read-write mode
  -- and if that fails we try read-only (to handle global read-only DBs).
  -- If either succeed then lock the file. IO exceptions (other than the first
  -- open attempt failing due to the file not existing) simply propagate.
  --
  -- Note that there is a complexity here which was discovered in #13945: some
  -- filesystems (e.g. NFS) will only allow exclusive locking if the fd was
  -- opened for write access. We would previously try opening the lockfile for
  -- read-only access first, however this failed when run on such filesystems.
  -- Consequently, we now try read-write access first, falling back to read-only
  -- if we are denied permission (e.g. in the case of a global database).
  catchJust
    (\e -> if isPermissionError e then Just () else Nothing)
    (lockFileOpenIn ReadWriteMode)
    (const $ lockFileOpenIn ReadMode)
  where
    lock = file <.> "lock"

    lockFileOpenIn io_mode = bracketOnError
      (openBinaryFile lock io_mode)
      hClose
      -- If file locking support is not available, ignore the error and proceed
      -- normally. Without it the only thing we lose on non-Windows platforms is
      -- the ability to safely issue concurrent updates to the same package db.
      $ \hnd -> do hLock hnd mode `catch` \FileLockingNotSupported -> return ()
                   return $ PackageDbLock hnd

lockPackageDb = lockPackageDbWith ExclusiveLock
unlockPackageDb (PackageDbLock hnd) = do
    hUnlock hnd
    hClose hnd

-- | Mode to open a package db in.
data DbMode = DbReadOnly | DbReadWrite

-- | 'DbOpenMode' holds a value of type @t@ but only in 'DbReadWrite' mode.  So
-- it is like 'Maybe' but with a type argument for the mode to enforce that the
-- mode is used consistently.
data DbOpenMode (mode :: DbMode) t where
  DbOpenReadOnly  ::      DbOpenMode 'DbReadOnly t
  DbOpenReadWrite :: t -> DbOpenMode 'DbReadWrite t

deriving instance Functor (DbOpenMode mode)
deriving instance F.Foldable (DbOpenMode mode)
deriving instance F.Traversable (DbOpenMode mode)

isDbOpenReadMode :: DbOpenMode mode t -> Bool
isDbOpenReadMode = \case
  DbOpenReadOnly    -> True
  DbOpenReadWrite{} -> False

-- | Read the part of the package DB that GHC is interested in.
--
readPackageDbForGhc :: RepGenericUnitInfo a b c d e f g =>
                       FilePath -> IO [GenericUnitInfo a b c d e f g]
readPackageDbForGhc file =
  decodeFromFile file DbOpenReadOnly getDbForGhc >>= \case
    (pkgs, DbOpenReadOnly) -> return pkgs
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
-- If we open the package db in read only mode, we get its contents. Otherwise
-- we additionally receive a PackageDbLock that represents a lock on the
-- database, so that we can safely update it later.
--
readPackageDbForGhcPkg :: Binary pkgs => FilePath -> DbOpenMode mode t ->
                          IO (pkgs, DbOpenMode mode PackageDbLock)
readPackageDbForGhcPkg file mode =
    decodeFromFile file mode getDbForGhcPkg
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
writePackageDb :: (Binary pkgs, RepGenericUnitInfo a b c d e f g) =>
                  FilePath -> [GenericUnitInfo a b c d e f g] ->
                  pkgs -> IO ()
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
decodeFromFile :: FilePath -> DbOpenMode mode t -> Get pkgs ->
                  IO (pkgs, DbOpenMode mode PackageDbLock)
decodeFromFile file mode decoder = case mode of
  DbOpenReadOnly -> do
  -- Note [Locking package database on Windows]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- When we open the package db in read only mode, there is no need to acquire
  -- shared lock on non-Windows platform because we update the database with an
  -- atomic rename, so readers will always see the database in a consistent
  -- state.
#if defined(mingw32_HOST_OS)
    bracket (lockPackageDbWith SharedLock file) unlockPackageDb $ \_ -> do
#endif
      (, DbOpenReadOnly) <$> decodeFileContents
  DbOpenReadWrite{} -> do
    -- When we open the package db in read/write mode, acquire an exclusive lock
    -- on the database and return it so we can keep it for the duration of the
    -- update.
    bracketOnError (lockPackageDb file) unlockPackageDb $ \lock -> do
      (, DbOpenReadWrite lock) <$> decodeFileContents
  where
    decodeFileContents = withBinaryFile file ReadMode $ \hnd ->
      feed hnd (runGetIncremental decoder)

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

instance (RepGenericUnitInfo a b c d e f g) =>
         Binary (GenericUnitInfo a b c d e f g) where
  put (GenericUnitInfo
         unitId unitInstanceOf unitInstantiations
         unitPackageId
         unitPackageName unitPackageVersion
         unitComponentName
         unitAbiHash unitDepends unitAbiDepends unitImportDirs
         unitLibraries unitExtDepLibsSys unitExtDepLibsGhc
         unitLibraryDirs unitLibraryDynDirs
         unitExtDepFrameworks unitExtDepFrameworkDirs
         unitLinkerOptions unitCcOptions
         unitIncludes unitIncludeDirs
         unitHaddockInterfaces unitHaddockHTMLs
         unitExposedModules unitHiddenModules
         unitIsIndefinite unitIsExposed unitIsTrusted) = do
    put (toStringRep unitPackageId)
    put (toStringRep unitPackageName)
    put unitPackageVersion
    put (fmap toStringRep unitComponentName)
    put (toStringRep unitId)
    put (toStringRep unitInstanceOf)
    put (map (\(mod_name, mod) -> (toStringRep mod_name, toDbModule mod))
             unitInstantiations)
    put unitAbiHash
    put (map toStringRep unitDepends)
    put (map (\(k,v) -> (toStringRep k, v)) unitAbiDepends)
    put unitImportDirs
    put unitLibraries
    put unitExtDepLibsSys
    put unitExtDepLibsGhc
    put unitLibraryDirs
    put unitLibraryDynDirs
    put unitExtDepFrameworks
    put unitExtDepFrameworkDirs
    put unitLinkerOptions
    put unitCcOptions
    put unitIncludes
    put unitIncludeDirs
    put unitHaddockInterfaces
    put unitHaddockHTMLs
    put (map (\(mod_name, mb_mod) -> (toStringRep mod_name, fmap toDbModule mb_mod))
             unitExposedModules)
    put (map toStringRep unitHiddenModules)
    put unitIsIndefinite
    put unitIsExposed
    put unitIsTrusted

  get = do
    unitPackageId      <- get
    unitPackageName    <- get
    unitPackageVersion <- get
    unitComponentName  <- get
    unitId             <- get
    unitInstanceOf     <- get
    unitInstantiations <- get
    unitAbiHash        <- get
    unitDepends        <- get
    unitAbiDepends     <- get
    unitImportDirs     <- get
    unitLibraries      <- get
    unitExtDepLibsSys  <- get
    unitExtDepLibsGhc  <- get
    libraryDirs        <- get
    libraryDynDirs     <- get
    frameworks         <- get
    frameworkDirs      <- get
    unitLinkerOptions  <- get
    unitCcOptions      <- get
    unitIncludes       <- get
    unitIncludeDirs    <- get
    unitHaddockInterfaces <- get
    unitHaddockHTMLs   <- get
    unitExposedModules <- get
    unitHiddenModules  <- get
    unitIsIndefinite   <- get
    unitIsExposed      <- get
    unitIsTrusted      <- get
    return (GenericUnitInfo
              (fromStringRep unitId)
              (fromStringRep unitInstanceOf)
              (map (\(mod_name, mod) -> (fromStringRep mod_name, fromDbModule mod))
                unitInstantiations)
              (fromStringRep unitPackageId)
              (fromStringRep unitPackageName) unitPackageVersion
              (fmap fromStringRep unitComponentName)
              unitAbiHash
              (map fromStringRep unitDepends)
              (map (\(k,v) -> (fromStringRep k, v)) unitAbiDepends)
              unitImportDirs
              unitLibraries unitExtDepLibsSys unitExtDepLibsGhc
              libraryDirs libraryDynDirs
              frameworks frameworkDirs
              unitLinkerOptions unitCcOptions
              unitIncludes unitIncludeDirs
              unitHaddockInterfaces unitHaddockHTMLs
              (map (\(mod_name, mb_mod) ->
                        (fromStringRep mod_name, fmap fromDbModule mb_mod))
                   unitExposedModules)
              (map fromStringRep unitHiddenModules)
              unitIsIndefinite unitIsExposed unitIsTrusted)

instance (BinaryStringRep modulename, BinaryStringRep compid,
          BinaryStringRep instunitid,
          DbUnitIdModuleRep instunitid compid unitid modulename mod) =>
         Binary (DbModule instunitid compid unitid modulename mod) where
  put (DbModule dbModuleUnitId dbModuleName) = do
    putWord8 0
    put (toDbUnitId dbModuleUnitId)
    put (toStringRep dbModuleName)
  put (DbModuleVar dbModuleVarName) = do
    putWord8 1
    put (toStringRep dbModuleVarName)
  get = do
    b <- getWord8
    case b of
      0 -> do dbModuleUnitId <- get
              dbModuleName <- get
              return (DbModule (fromDbUnitId dbModuleUnitId)
                               (fromStringRep dbModuleName))
      _ -> do dbModuleVarName <- get
              return (DbModuleVar (fromStringRep dbModuleVarName))

instance (BinaryStringRep modulename, BinaryStringRep compid,
          BinaryStringRep instunitid,
          DbUnitIdModuleRep instunitid compid unitid modulename mod) =>
         Binary (DbUnitId instunitid compid unitid modulename mod) where
  put (DbInstalledUnitId instunitid) = do
    putWord8 0
    put (toStringRep instunitid)
  put (DbUnitId dbUnitIdComponentId dbUnitIdInsts) = do
    putWord8 1
    put (toStringRep dbUnitIdComponentId)
    put (map (\(mod_name, mod) -> (toStringRep mod_name, toDbModule mod)) dbUnitIdInsts)
  get = do
    b <- getWord8
    case b of
      0 -> do
        instunitid <- get
        return (DbInstalledUnitId (fromStringRep instunitid))
      _ -> do
        dbUnitIdComponentId <- get
        dbUnitIdInsts <- get
        return (DbUnitId
            (fromStringRep dbUnitIdComponentId)
            (map (\(mod_name, mod) -> ( fromStringRep mod_name
                                      , fromDbModule mod))
                 dbUnitIdInsts))

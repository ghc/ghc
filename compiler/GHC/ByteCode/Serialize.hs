{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
-- Orphans are here since the Binary instances use an ad-hoc means of serialising
-- names which we don't want to pollute the rest of the codebase with.
{- | This module implements the serialization of bytecode objects to and from disk.
-}
module GHC.ByteCode.Serialize
  ( writeBinByteCode, readBinByteCode, readBinByteCodeHash, readOnDiskModuleByteCode
  , ModuleByteCode(..)
  , BytecodeLibX(..)
  , BytecodeLib
  , OnDiskBytecodeLib
  , InterpreterLibrary(..)
  , InterpreterLibraryContents(..)
  , writeBytecodeLib
  , readBytecodeLib
  , readBytecodeLibInputsHash
  , fingerprintBytecodeLibInputs
  , mkModuleByteCode
  , fingerprintModuleByteCodeContents
  , decodeOnDiskModuleByteCode
  , decodeOnDiskBytecodeLib
  )
where

import GHC.Prelude

import GHC.ByteCode.Binary
import GHC.ByteCode.Recomp.Binary (computeFingerprint)
import GHC.ByteCode.Types
import GHC.Data.Maybe (rightToMaybe)
import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Iface.Binary
import GHC.Iface.Recomp.Binary (putNameLiterally)
import GHC.Linker.Types
import GHC.Unit.Types
import GHC.Utils.Binary
import GHC.Utils.Exception (tryIO)
import GHC.Utils.Fingerprint (Fingerprint(..), fingerprintFingerprints)
import GHC.Utils.Logger
import GHC.Utils.Panic
import GHC.Utils.TmpFs
import GHC.Version (cProjectVersionInt, cProjectPatchLevel1, cProjectPatchLevel2)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.List (intercalate, sort)
import Data.Traversable
import Data.Word
import System.Directory
import System.FilePath

{- Note [Overview of persistent bytecode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, when using the interpreter, a Haskell module is first compiled to
bytecode (which lives in memory) and then executed by the RTS interpreter.
However, when dealing with many modules compiling to bytecode from scratch every
time is expensive. This is especially relevant for interpreter-heavy workflows
on large projects where changes are incremental or non-existent (e.g. running
the project in the debugger).

In light of this, GHC can produce `.gbc` files, which contain a
serialized representation of the bytecode for a Haskell module. These
files are written by enabling the flag `-fwrite-byte-code` when using the
interpreter.

The driver will always look for both the interface and the `.gbc` file and load
those to avoid unnecessary recompilation. This can save a lot of time if you
have many modules. Even compared to `-fwrite-if-simplified-core`.

.gbc files are standalone, in the sense that they can be loaded into the interpreter
without having the interface file or source files available. In the future you could
create a "bytecode executable", which just contained bytecode objects, a simple wrapper
and the runtime, which would load the bytecode objects and execute main.

.gbc files also contain the contents of object files which arise from foreign files
and other stubs (such as info table map, foreign files added by TH, CApiFFI
etc). In the normal compilation pipeline, these are merged into the final object
by object merging to produce a single .o file. Bytecode objects are not "normal
objects", so they are stored alongside the 'CompiledByteCode' and written to
temporary files when needed.

The ticket where bytecode objects were dicussed is #26298

See Note [-fwrite-byte-code is not the default]
See Note [Recompilation avoidance with bytecode objects]
See Note [Persistent bytecode file headers]
See Note [Hash of bytecode libs]

Note [Persistent bytecode file headers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Persistent bytecode files (`.gbc`) and bytecode libraries (`.bytecodelib`)
are version-specific binary formats. Without a small file-level header, stale
or corrupt files are only discovered once we start deserialising the payload,
which can lead to confusing failures.

To make these failures explicit, we write a header ahead of the binary payload:

  * a file-kind-specific magic word,
  * the version of the compiler (`PersistentBytecodeVersion`),
  * a hash: the `gbc_hash` of a bytecode file, or the inputs hash of a
    bytecode library (see Note [Hash of bytecode libs]).

We ensure everthing has a fixed length encoding (see
`persistentBytecodeHeaderSize`, so that `readPersistentBytecodeHash` can read
the hash without reading the rest of the file.

Note [Hash of bytecode libs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A bytecode library records the combined hash of its constituents, so
that relinking can be skipped when they haven't changed.

We can't hash the library file itself: we would have to build the
library to know which hash to expect. So we compare the inputs.

The hash is part of the file header, see Note [Persistent bytecode file
headers]. The constituent hashes are sorted before they are combined, so
the order of the modules doesn't matter.
-}

-- | The 'bytecodeLibInputsHash' of a bytecode library, given the hash of each
-- constituent.
-- See Note [Hash of bytecode libs]
fingerprintBytecodeLibInputs :: [Fingerprint] -> Fingerprint
fingerprintBytecodeLibInputs = fingerprintFingerprints . sort

writeBytecodeLib :: BytecodeLib -> FilePath -> IO ()
writeBytecodeLib lib path = do
  odbco <- encodeBytecodeLib lib
  createDirectoryIfMissing True (takeDirectory path)
  bh' <- openBinMem initBinMemSize
  bh <- addBinNameWriter bh'
  writePersistentBytecodeHeader BytecodeLibraryFile (bytecodeLibInputsHash lib) bh
  putWithUserData QuietBinIFace NormalCompression bh odbco
  writeBinMem bh path

readBytecodeLib :: HscEnv -> FilePath -> IO OnDiskBytecodeLib
readBytecodeLib hsc_env path = do
  bh' <- readBinMem path
  _inputs_hash <- readPersistentBytecodeHeader BytecodeLibraryFile path bh'
  bh <- addBinNameReader (hsc_NC hsc_env) bh'
  res <- getWithUserData (hsc_NC hsc_env) bh
  pure res

-- | 'Nothing' if the file can't be read or its header is not valid.
-- See Note [Hash of bytecode libs]
readBytecodeLibInputsHash :: FilePath -> IO (Maybe Fingerprint)
readBytecodeLibInputsHash = readPersistentBytecodeHash BytecodeLibraryFile

-- | Convert an 'OnDiskModuleByteCode' to an 'ModuleByteCode'.
-- 'OnDiskModuleByteCode' is the representation which we read from a file,
-- the 'ModuleByteCode' is the representation which is manipulated by program logic.
--
-- This notably writes the object files to temporary files.
-- They are written to temporary files so that the normal object file loading
-- code paths (which expect object files to exist as on-disk files) can be used
-- in the loader.
decodeOnDiskModuleByteCode :: HscEnv -> OnDiskModuleByteCode -> IO ModuleByteCode
decodeOnDiskModuleByteCode hsc_env odbco = do
  foreign_files <- writeObjectFiles (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (tmpDir (hsc_dflags hsc_env)) (odgbc_foreign odbco)
  pure $ ModuleByteCode {
    gbc_module = odgbc_module odbco,
    gbc_compiled_byte_code = odgbc_compiled_byte_code odbco,
    gbc_foreign_files = foreign_files,
    gbc_hash = odgbc_hash odbco
   }

decodeOnDiskBytecodeLib :: HscEnv -> OnDiskBytecodeLib -> IO BytecodeLib
decodeOnDiskBytecodeLib hsc_env odbco = do
  foreign_contents <- traverse (writeInterpreterLibraryFile (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (tmpDir (hsc_dflags hsc_env)))  (bytecodeLibForeign odbco)
  pure $ BytecodeLib {
    bytecodeLibUnitId = bytecodeLibUnitId odbco,
    bytecodeLibInputsHash = bytecodeLibInputsHash odbco,
    bytecodeLibFiles = bytecodeLibFiles odbco,
    bytecodeLibForeign = foreign_contents
   }

encodeBytecodeLib :: BytecodeLib -> IO OnDiskBytecodeLib
encodeBytecodeLib (BytecodeLib {..}) = do
  foreign_contents <- traverse readInterpreterLibraryFile bytecodeLibForeign
  pure $ BytecodeLib {
    bytecodeLibUnitId = bytecodeLibUnitId,
    bytecodeLibInputsHash = bytecodeLibInputsHash,
    bytecodeLibFiles = bytecodeLibFiles,
    bytecodeLibForeign = foreign_contents
   }

readObjectFile :: FilePath -> IO ByteString
readObjectFile f = BS.readFile f

readObjectFiles :: [FilePath] -> IO [ByteString]
readObjectFiles fs = mapM readObjectFile fs

-- | Write a list of bytestrings, representing object files, to a temporary files.
writeObjectFiles ::  Logger -> TmpFs -> TempDir -> [ByteString] -> IO [FilePath]
writeObjectFiles logger tmpfs tmp_dir files =
  for files $ \file -> do
    f <- newTempName logger tmpfs tmp_dir TFL_GhcSession "o"
    BS.writeFile f file
    pure f

writeInterpreterLibraryFile :: Logger -> TmpFs -> TempDir -> InterpreterLibraryContents -> IO InterpreterLibrary
writeInterpreterLibraryFile logger tmpfs tmp_dir (InterpreterLibrarySharedContents contents) = do
  (soFile, libdir, libname)  <- newTempLibName logger tmpfs tmp_dir TFL_GhcSession "so"
  BS.writeFile soFile contents
  pure (InterpreterSharedObject soFile libdir libname)
writeInterpreterLibraryFile logger tmpfs tmp_dir (InterpreterLibraryStaticContents contents) = do
  object_files <- writeObjectFiles logger tmpfs tmp_dir contents
  pure (InterpreterStaticObjects object_files)

readInterpreterLibraryFile :: InterpreterLibrary -> IO InterpreterLibraryContents
readInterpreterLibraryFile (InterpreterSharedObject path _ _) = do
  contents <- BS.readFile path
  pure (InterpreterLibrarySharedContents contents)
readInterpreterLibraryFile (InterpreterStaticObjects paths) = do
  contents <- readObjectFiles paths
  pure (InterpreterLibraryStaticContents contents)

-- | Prepare an in-memory 'ModuleByteCode' for writing to disk.
encodeOnDiskModuleByteCode :: ModuleByteCode -> IO OnDiskModuleByteCode
encodeOnDiskModuleByteCode bco = do
  foreign_contents <- readObjectFiles (gbc_foreign_files bco)
  pure $ OnDiskModuleByteCode {
    odgbc_module = gbc_module bco,
    odgbc_compiled_byte_code = gbc_compiled_byte_code bco,
    odgbc_foreign = foreign_contents,
    odgbc_hash = gbc_hash bco
   }

-- | Read a 'ModuleByteCode' from a file.
readBinByteCode :: HscEnv -> FilePath -> IO ModuleByteCode
readBinByteCode hsc_env f = do
  odbco <- readOnDiskModuleByteCode hsc_env f
  decodeOnDiskModuleByteCode hsc_env odbco

-- | Read the hash of a bytecode file, without reading the rest of the file.
-- 'Nothing' if the file can't be read or its header is not valid.
readBinByteCodeHash :: FilePath -> IO (Maybe Fingerprint)
readBinByteCodeHash = readPersistentBytecodeHash ModuleByteCodeFile

readOnDiskModuleByteCode :: HscEnv -> FilePath -> IO OnDiskModuleByteCode
readOnDiskModuleByteCode hsc_env f = do
  bh' <- readBinMem f
  _hash <- readPersistentBytecodeHeader ModuleByteCodeFile f bh'
  bh <- addBinNameReader (hsc_NC hsc_env) bh'
  getWithUserData (hsc_NC hsc_env) bh

-- | Write a 'ModuleByteCode' to a file.
writeBinByteCode :: FilePath -> ModuleByteCode -> IO ()
writeBinByteCode f cbc = do
  createDirectoryIfMissing True (takeDirectory f)
  bh' <- openBinMem initBinMemSize
  bh <- addBinNameWriter bh'
  odbco <- encodeOnDiskModuleByteCode cbc
  writePersistentBytecodeHeader ModuleByteCodeFile (gbc_hash cbc) bh
  putWithUserData QuietBinIFace NormalCompression bh odbco
  writeBinMem bh f

mkModuleByteCode :: Module -> CompiledByteCode -> [FilePath] -> IO ModuleByteCode
mkModuleByteCode modl cbc foreign_files = do
  !bcos_hash <- fingerprintModuleByteCodeContents modl cbc foreign_files
  return $! ModuleByteCode modl cbc foreign_files bcos_hash

-- | Generate a 'Fingerprint' for the 'ModuleByteCode' contents.
--
-- Note, this will serialise the contents of the 'ModuleByteCode' separately
-- to 'writeBytecodeLib'.
-- This means, if the 'ModuleByteCode' is written to disk, it will be
-- serialised twice.
fingerprintModuleByteCodeContents :: Module -> CompiledByteCode -> [FilePath] -> IO Fingerprint
fingerprintModuleByteCodeContents modl cbc foreign_files = do
  foreign_contents <- readObjectFiles foreign_files
  pure $ computeFingerprint putNameLiterally (modl, cbc, foreign_contents)

-- ----------------------------------------------------------------------------
-- ByteCode module and library magic header.
-- ----------------------------------------------------------------------------

data PersistentBytecodeFile
  = ModuleByteCodeFile
  | BytecodeLibraryFile

-- | The version of the compiler: major, minor, patch1, patch2
data PersistentBytecodeVersion
  = PersistentBytecodeVersion !Word32 !Word32 !Word32 !Word32
  deriving Eq

instance Binary PersistentBytecodeVersion where
  put_ bh (PersistentBytecodeVersion a b c d) =
    mapM_ (put_ bh . FixedLengthEncoding) [a, b, c, d]
  get bh = PersistentBytecodeVersion <$> field <*> field <*> field <*> field
    where field = unFixedLength <$> get bh

persistentBytecodeVersion :: PersistentBytecodeVersion
persistentBytecodeVersion =
  PersistentBytecodeVersion
    (version_int `div` 100) (version_int `mod` 100)
    (read cProjectPatchLevel1) (read cProjectPatchLevel2)
  where
    version_int = read cProjectVersionInt

showPersistentBytecodeVersion :: PersistentBytecodeVersion -> String
showPersistentBytecodeVersion (PersistentBytecodeVersion a b c d) =
  intercalate "." (map show [a, b, c, d])

-- | The size in bytes of the header: the magic word, the version and the hash.
-- See Note [Persistent bytecode file headers]
persistentBytecodeHeaderSize :: Int
persistentBytecodeHeaderSize = 4 + 4 * 4 + 2 * 8

-- See Note [Persistent bytecode file headers]
writePersistentBytecodeHeader :: PersistentBytecodeFile -> Fingerprint -> WriteBinHandle -> IO ()
writePersistentBytecodeHeader file_kind (Fingerprint w1 w2) bh = do
  put_ bh (persistentBytecodeMagic file_kind)
  put_ bh persistentBytecodeVersion
  put_ bh (FixedLengthEncoding w1)
  put_ bh (FixedLengthEncoding w2)

-- | Read only the header of a persistent bytecode file.
getPersistentBytecodeHeader :: PersistentBytecodeFile -> ReadBinHandle -> IO (Either String Fingerprint)
getPersistentBytecodeHeader file_kind bh = do
  magic <- get bh
  if unFixedLength magic /= unFixedLength expected_magic
    then mismatch "magic" (show $ unFixedLength expected_magic) (show $ unFixedLength magic)
    else do
      version <- get bh
      if version /= persistentBytecodeVersion
        then mismatch "version"
               (showPersistentBytecodeVersion persistentBytecodeVersion)
               (showPersistentBytecodeVersion version)
        else do
          w1 <- get bh
          w2 <- get bh
          pure $ Right (Fingerprint (unFixedLength w1) (unFixedLength w2))
  where
    expected_magic = persistentBytecodeMagic file_kind
    mismatch what expected actual =
      pure $ Left $ what ++ " (expected " ++ expected ++ ", got " ++ actual ++ ")"

-- | Validate the header of a persistent bytecode file, and return its hash.
readPersistentBytecodeHeader :: PersistentBytecodeFile -> FilePath -> ReadBinHandle -> IO Fingerprint
readPersistentBytecodeHeader file_kind path bh = do
  r <- getPersistentBytecodeHeader file_kind bh
  case r of
    Right hash -> pure hash
    Left what -> throwGhcExceptionIO $ ProgramError $
      persistentBytecodeFileDescription file_kind ++ " header mismatch in " ++ path ++
      ": " ++ what

-- | Read the hash from the header of a persistent bytecode file, without
-- reading the rest of the file. 'Nothing' if the file can't be read or its
-- header is not valid.
readPersistentBytecodeHash :: PersistentBytecodeFile -> FilePath -> IO (Maybe Fingerprint)
readPersistentBytecodeHash file_kind path = do
  r <- tryIO $ readBinMemN persistentBytecodeHeaderSize path
  case r of
    Right (Just bh) -> rightToMaybe <$> getPersistentBytecodeHeader file_kind bh
    _ -> pure Nothing

persistentBytecodeFileDescription :: PersistentBytecodeFile -> String
persistentBytecodeFileDescription ModuleByteCodeFile = "bytecode file"
persistentBytecodeFileDescription BytecodeLibraryFile = "bytecode library"

persistentBytecodeMagic :: PersistentBytecodeFile -> FixedLengthEncoding Word32
persistentBytecodeMagic file_kind =
  case file_kind of
    ModuleByteCodeFile -> asciiWord32 "gbc0"
    BytecodeLibraryFile -> asciiWord32 "bcl0"

-- | Encode a 4-letter word into a single Word32.
asciiWord32 :: String -> FixedLengthEncoding Word32
asciiWord32 [a, b, c, d] =
  FixedLengthEncoding $
    (fromIntegral (ord a) `shiftL` 24) .|.
    (fromIntegral (ord b) `shiftL` 16) .|.
    (fromIntegral (ord c) `shiftL` 8)  .|.
    fromIntegral (ord d)
asciiWord32 _ = error "asciiWord32: expected exactly four ASCII characters"

-- ----------------------------------------------------------------------------
-- Constants and utils
-- ----------------------------------------------------------------------------

-- | Initial ram buffer to allocate for writing .gbc and .bytecodelib files.
initBinMemSize :: Int
initBinMemSize = 1024 * 1024 -- 1 MB

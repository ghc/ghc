{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
-- Orphans are here since the Binary instances use an ad-hoc means of serialising
-- names which we don't want to pollute the rest of the codebase with.
{-# OPTIONS_GHC -Wno-orphans #-}
{- | This module implements the serialization of bytecode objects to and from disk.
-}
module GHC.ByteCode.Serialize
  ( writeBinByteCode, readBinByteCode, ModuleByteCode(..)
  , BytecodeLibX(..)
  , BytecodeLib
  , OnDiskBytecodeLib
  , InterpreterLibrary(..)
  , InterpreterLibraryContents(..)
  , writeBytecodeLib
  , readBytecodeLib
  , decodeOnDiskModuleByteCode
  , decodeOnDiskBytecodeLib
  )
where

import Control.Monad
import Data.Binary qualified as Binary
import Data.Foldable
import Data.IORef
import Data.Proxy
import Data.Word
import GHC.ByteCode.Types
import GHC.Data.FastString
import GHC.Driver.Env
import GHC.Iface.Binary
import GHC.Prelude
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Utils.Binary
import GHC.Utils.Exception
import GHC.Utils.Panic
import GHC.Utils.TmpFs
import System.FilePath
import GHC.Unit.Types
import GHC.Driver.DynFlags
import System.Directory
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Traversable
import GHC.Utils.Logger
import GHC.Linker.Types
import System.IO.Unsafe (unsafeInterleaveIO)
import GHC.Utils.Outputable

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

-}

-- | The on-disk representation of a bytecode object for a specific module.
--
-- This is the representation which we serialise and write to disk.
-- The difference from 'ModuleByteCode' is that the contents of the object files
-- contained by 'ModuleByteCode' are stored in-memory rather than as file paths to
-- temporary files.
data OnDiskModuleByteCode = OnDiskModuleByteCode { odgbc_module :: Module
                                                 , odgbc_compiled_byte_code :: CompiledByteCode
                                                 , odgbc_foreign :: [ByteString]  -- ^ Contents of object files
                                                 }

type OnDiskBytecodeLib = BytecodeLibX (Maybe InterpreterLibraryContents)

instance Outputable a => Outputable (BytecodeLibX a) where
  ppr (BytecodeLib {..}) = vcat [
    (text "BytecodeLib" <+> ppr bytecodeLibUnitId),
    (text "Files" <+> ppr bytecodeLibFiles),
    (text "Foreign" <+> ppr bytecodeLibForeign) ]

type BytecodeLib = BytecodeLibX (Maybe InterpreterLibrary)

-- | A bytecode library is a collection of CompiledByteCode objects and a .so file containing the combination of foreign stubs
data BytecodeLibX a = BytecodeLib {
    bytecodeLibUnitId :: UnitId,
    bytecodeLibFiles :: [CompiledByteCode],
    bytecodeLibForeign :: a -- A library file containing the combination of foreign stubs. (Ie arising from CApiFFI)
}

data InterpreterLibrary = InterpreterSharedObject { getSharedObjectFilePath :: FilePath, getSharedObjectDir :: FilePath, getSharedObjectLibName :: String }
                         | InterpreterStaticObjects { getStaticObjects :: [FilePath] }


instance Outputable InterpreterLibrary where
  ppr (InterpreterSharedObject path dir name) = text "SharedObject" <+> text path <+> text dir <+> text name
  ppr (InterpreterStaticObjects paths) = text "StaticObjects" <+> text (show paths)


data InterpreterLibraryContents = InterpreterLibrarySharedContents { interpreterLibraryContents :: ByteString }
                                | InterpreterLibraryStaticContents { interpreterLibraryStaticContents :: [ByteString] }

instance Binary InterpreterLibraryContents where
  get bh = do
    t <- getByte bh
    case t of
      0 -> InterpreterLibrarySharedContents <$> get bh
      1 -> InterpreterLibraryStaticContents <$> get bh
      _ -> panic "Binary InterpreterLibraryContents: invalid byte"
  put_ bh (InterpreterLibrarySharedContents contents) = do
    putByte bh 0
    put_ bh contents
  put_ bh (InterpreterLibraryStaticContents contents) = do
    putByte bh 1
    put_ bh contents

instance Binary OnDiskBytecodeLib where
  get bh = do
    bytecodeLibUnitId <- get bh
    bytecodeLibFiles <- get bh
    bytecodeLibForeign <- get bh
    pure BytecodeLib {..}

  put_ bh BytecodeLib {..} = do
    put_ bh bytecodeLibUnitId
    put_ bh bytecodeLibFiles
    put_ bh bytecodeLibForeign



writeBytecodeLib :: BytecodeLib -> FilePath -> IO ()
writeBytecodeLib lib path = do
  odbco <- encodeBytecodeLib lib
  createDirectoryIfMissing True (takeDirectory path)
  bh' <- openBinMem (1024 * 1024)
  bh <- addBinNameWriter bh'
  putWithUserData QuietBinIFace NormalCompression bh odbco
  writeBinMem bh path

readBytecodeLib :: HscEnv -> FilePath -> IO OnDiskBytecodeLib
readBytecodeLib hsc_env path = do
  bh' <- readBinMem path
  bh <- addBinNameReader hsc_env bh'
  res <- getWithUserData (hsc_NC hsc_env) bh
  pure res

instance Binary OnDiskModuleByteCode where
  get bh = do
    odgbc_module <- get bh
    odgbc_compiled_byte_code <- get bh
    odgbc_foreign <- get bh
    pure OnDiskModuleByteCode {..}

  put_ bh OnDiskModuleByteCode {..} = do
    put_ bh odgbc_module
    put_ bh odgbc_compiled_byte_code
    put_ bh odgbc_foreign

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
    gbc_foreign_files = foreign_files
   }

decodeOnDiskBytecodeLib :: HscEnv -> OnDiskBytecodeLib -> IO BytecodeLib
decodeOnDiskBytecodeLib hsc_env odbco = do
  foreign_contents <- traverse (writeInterpreterLibraryFile (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (tmpDir (hsc_dflags hsc_env)))  (bytecodeLibForeign odbco)
  pure $ BytecodeLib {
    bytecodeLibUnitId = bytecodeLibUnitId odbco,
    bytecodeLibFiles = bytecodeLibFiles odbco,
    bytecodeLibForeign = foreign_contents
   }

encodeBytecodeLib :: BytecodeLib -> IO OnDiskBytecodeLib
encodeBytecodeLib (BytecodeLib {..}) = do
  foreign_contents <- traverse readInterpreterLibraryFile bytecodeLibForeign
  pure $ BytecodeLib {
    bytecodeLibUnitId = bytecodeLibUnitId,
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
    odgbc_foreign = foreign_contents
   }

-- | Read a 'ModuleByteCode' from a file.
readBinByteCode :: HscEnv -> FilePath -> IO ModuleByteCode
readBinByteCode hsc_env f = do
  odbco <- readOnDiskModuleByteCode hsc_env f
  decodeOnDiskModuleByteCode hsc_env odbco

readOnDiskModuleByteCode :: HscEnv -> FilePath -> IO OnDiskModuleByteCode
readOnDiskModuleByteCode hsc_env f = do
  bh' <- readBinMem f
  bh <- addBinNameReader hsc_env bh'
  getWithUserData (hsc_NC hsc_env) bh

-- | Write a 'ModuleByteCode' to a file.
writeBinByteCode :: FilePath -> ModuleByteCode -> IO ()
writeBinByteCode f cbc = do
  createDirectoryIfMissing True (takeDirectory f)
  bh' <- openBinMem (1024 * 1024)
  bh <- addBinNameWriter bh'
  odbco <- encodeOnDiskModuleByteCode cbc
  putWithUserData QuietBinIFace NormalCompression bh odbco
  writeBinMem bh f

instance Binary CompiledByteCode where
  get bh = do
    bc_bcos <- get bh
    bc_itbls_len <- get bh
    bc_itbls <- replicateM bc_itbls_len $ do
      nm <- getViaBinName bh
      itbl <- get bh
      pure (nm, itbl)
    bc_strs_len <- get bh
    bc_strs <-
      replicateM bc_strs_len $ (,) <$> getViaBinName bh <*> get bh
    bc_breaks <- get bh
    bc_spt_entries <- get bh
    return $
      CompiledByteCode
        { bc_bcos,
          bc_itbls,
          bc_strs,
          bc_breaks,
          bc_spt_entries
        }

  put_ bh CompiledByteCode {..} = do
    put_ bh bc_bcos
    put_ bh $ length bc_itbls
    for_ bc_itbls $ \(nm, itbl) -> do
      putViaBinName bh nm
      put_ bh itbl
    put_ bh $ length bc_strs
    for_ bc_strs $ \(nm, str) -> putViaBinName bh nm *> put_ bh str
    put_ bh bc_breaks
    put_ bh bc_spt_entries

instance Binary UnlinkedBCO where
  get bh =
    UnlinkedBCO
      <$> getViaBinName bh
      <*> get bh
      <*> (Binary.decode <$> get bh)
      <*> (Binary.decode <$> get bh)
      <*> get bh
      <*> get bh

  put_ bh UnlinkedBCO {..} = do
    putViaBinName bh unlinkedBCOName
    put_ bh unlinkedBCOArity
    put_ bh $ Binary.encode unlinkedBCOInstrs
    put_ bh $ Binary.encode unlinkedBCOBitmap
    put_ bh unlinkedBCOLits
    put_ bh unlinkedBCOPtrs

instance Binary BCOPtr where
  get bh = do
    t <- getByte bh
    case t of
      0 -> BCOPtrName <$> getViaBinName bh
      1 -> BCOPtrPrimOp <$> get bh
      2 -> BCOPtrBCO <$> get bh
      3 -> BCOPtrBreakArray <$> get bh
      _ -> panic "Binary BCOPtr: invalid byte"

  put_ bh ptr = case ptr of
    BCOPtrName nm -> putByte bh 0 *> putViaBinName bh nm
    BCOPtrPrimOp op -> putByte bh 1 *> put_ bh op
    BCOPtrBCO bco -> putByte bh 2 *> put_ bh bco
    BCOPtrBreakArray info_mod -> putByte bh 3 *> put_ bh info_mod

instance Binary BCONPtr where
  get bh = do
    t <- getByte bh
    case t of
      0 -> BCONPtrWord . fromIntegral <$> (get bh :: IO Word64)
      1 -> BCONPtrLbl <$> get bh
      2 -> BCONPtrItbl <$> getViaBinName bh
      3 -> BCONPtrAddr <$> getViaBinName bh
      4 -> BCONPtrStr <$> get bh
      5 -> BCONPtrFS <$> get bh
      6 -> BCONPtrFFIInfo <$> get bh
      7 -> BCONPtrCostCentre <$> get bh
      _ -> panic "Binary BCONPtr: invalid byte"

  put_ bh ptr = case ptr of
    BCONPtrWord lit -> putByte bh 0 *> put_ bh (fromIntegral lit :: Word64)
    BCONPtrLbl sym -> putByte bh 1 *> put_ bh sym
    BCONPtrItbl nm -> putByte bh 2 *> putViaBinName bh nm
    BCONPtrAddr nm -> putByte bh 3 *> putViaBinName bh nm
    BCONPtrStr str -> putByte bh 4 *> put_ bh str
    BCONPtrFS fs -> putByte bh 5 *> put_ bh fs
    BCONPtrFFIInfo ffi -> putByte bh 6 *> put_ bh ffi
    BCONPtrCostCentre ibi -> putByte bh 7 *> put_ bh ibi

newtype BinName = BinName {unBinName :: Name}

getViaBinName :: ReadBinHandle -> IO Name
getViaBinName bh = case findUserDataReader Proxy bh of
  BinaryReader f -> unBinName <$> f bh

putViaBinName :: WriteBinHandle -> Name -> IO ()
putViaBinName bh nm = case findUserDataWriter Proxy bh of
  BinaryWriter f -> f bh $ BinName nm

addBinNameWriter :: WriteBinHandle -> IO WriteBinHandle
addBinNameWriter bh' =
  evaluate
    $ flip addWriterToUserData bh'
    $ BinaryWriter
    $ \bh (BinName nm) ->
      if
        | isExternalName nm -> do
            putByte bh 0
            put_ bh nm
        | otherwise -> do
            putByte bh 1
            put_ bh
              $ occNameFS (occName nm)
              `appendFS` mkFastString
                (show $ nameUnique nm)

addBinNameReader :: HscEnv -> ReadBinHandle -> IO ReadBinHandle
addBinNameReader HscEnv {..} bh' = do
  env_ref <- newIORef emptyOccEnv
  pure $ flip addReaderToUserData bh' $ BinaryReader $ \bh -> do
    t <- getByte bh
    case t of
      0 -> do
        nm <- get bh
        pure $ BinName nm
      1 -> do
        occ <- mkVarOccFS <$> get bh
        -- We don't want to get a new unique from the NameCache each time we
        -- see a name.
        nm' <- unsafeInterleaveIO $ do
          u <- takeUniqFromNameCache hsc_NC
          evaluate $ mkInternalName u occ noSrcSpan
        fmap BinName $ atomicModifyIORef' env_ref $ \env ->
          case lookupOccEnv env occ of
            Just nm -> (env, nm)
            _ -> nm' `seq` (extendOccEnv env occ nm', nm')
      _ -> panic "Binary BinName: invalid byte"

-- Note [Serializing Names in bytecode]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- NOTE: This approach means that bytecode objects are not deterministic.
-- We need to revisit this in order to make the output deterministic.
--
-- The bytecode related types contain various Names which we need to
-- serialize. Unfortunately, we can't directly use the Binary instance
-- of Name: it is only meant to be used for serializing external Names
-- in BinIface logic, but bytecode does contain internal Names.
--
-- We also need to maintain the invariant that: any pair of internal
-- Names with equal/different uniques must also be deserialized to
-- have the same equality. So normally uniques aren't supposed to be
-- serialized, but for this invariant to work, we do append uniques to
-- OccNames of internal Names, so that they can be uniquely identified
-- by OccName alone. When deserializing, we check a global cached
-- mapping from OccName to Unique, and create the real Name with the
-- right Unique if it's already deserialized at least once.

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
-- Orphans are here since the Binary instances use an ad-hoc means of serialising
-- names which we don't want to pollute the rest of the codebase with.
{-# OPTIONS_GHC -Wno-orphans #-}
{- | This module implements the serialization of bytecode objects to and from disk.
-}
module GHC.ByteCode.Serialize
  ( writeBinByteCode, readBinByteCode, ByteCodeObject(..)
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

{- Note [Overview of persistent bytecode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, when using the interpreter, a Haskell module is first compiled to
bytecode (which lives in memory) and then executed by the RTS interpreter.
However, when dealing with many modules compiling to bytecode from scratch every
time is expensive. This is especially relevant for interpreter-heavy workflows
on large projects where changes are incremental or non-existent (e.g. running
the project in the debugger).

In light of this, GHC can produce `.gbc` files, which contain a
serialized representation of the bytecode objects in a Haskell module. These
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

-- | The on-disk representation of a bytecode object
--
-- This is the representation which we serialise and write to disk.
-- The difference from 'ByteCodeObject' is that the contents of the object files
-- contained by 'ByteCodeObject' are stored in-memory rather than as file paths to
-- temporary files.
data OnDiskByteCodeObject = OnDiskByteCodeObject { odbco_module :: Module
                                                 , odbco_compiled_byte_code :: CompiledByteCode
                                                 , odbco_foreign :: [ByteString]  -- ^ Contents of object files
                                                 }


instance Binary OnDiskByteCodeObject where
  get bh = do
    odbco_module <- get bh
    odbco_compiled_byte_code <- get bh
    odbco_foreign <- get bh
    pure OnDiskByteCodeObject {..}

  put_ bh OnDiskByteCodeObject {..} = do
    put_ bh odbco_module
    put_ bh odbco_compiled_byte_code
    put_ bh odbco_foreign

-- | Convert an 'OnDiskByteCodeObject' to an 'ByteCodeObject'.
-- 'OnDiskByteCodeObject' is the representation which we read from a file,
-- the 'ByteCodeObject' is the representation which is manipulated by program logic.
--
-- This notably writes the object files to temporary files.
-- They are written to temporary files so that the normal object file loading
-- code paths (which expect object files to exist as on-disk files) can be used
-- in the loader.
decodeOnDiskByteCodeObject :: HscEnv -> OnDiskByteCodeObject -> IO ByteCodeObject
decodeOnDiskByteCodeObject hsc_env odbco = do
  foreign_files <- writeObjectFiles (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (tmpDir (hsc_dflags hsc_env)) (odbco_foreign odbco)
  pure $ ByteCodeObject {
    bco_module = odbco_module odbco,
    bco_compiled_byte_code = odbco_compiled_byte_code odbco,
    bco_foreign_files = foreign_files
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

-- | Prepare an in-memory 'ByteCodeObject' for writing to disk.
encodeOnDiskByteCodeObject :: ByteCodeObject -> IO OnDiskByteCodeObject
encodeOnDiskByteCodeObject bco = do
  foreign_contents <- readObjectFiles (bco_foreign_files bco)
  pure $ OnDiskByteCodeObject {
    odbco_module = bco_module bco,
    odbco_compiled_byte_code = bco_compiled_byte_code bco,
    odbco_foreign = foreign_contents
   }

-- | Read a 'ByteCodeObject' from a file.
readBinByteCode :: HscEnv -> FilePath -> IO ByteCodeObject
readBinByteCode hsc_env f = do
  bh' <- readBinMem f
  bh <- addBinNameReader hsc_env bh'
  odbco <- getWithUserData (hsc_NC hsc_env) bh
  decodeOnDiskByteCodeObject hsc_env odbco

-- | Write a 'ByteCodeObject' to a file.
writeBinByteCode :: FilePath -> ByteCodeObject -> IO ()
writeBinByteCode f cbc = do
  createDirectoryIfMissing True (takeDirectory f)
  bh' <- openBinMem (1024 * 1024)
  bh <- addBinNameWriter bh'
  odbco <- encodeOnDiskByteCodeObject cbc
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

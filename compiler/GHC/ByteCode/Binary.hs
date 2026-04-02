{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.ByteCode.Binary (
  -- * ByteCode objects on disk and intermediate representations
  OnDiskModuleByteCode(..),
  BytecodeLibX(..),
  BytecodeLib,
  OnDiskBytecodeLib,
  InterpreterLibrary(..),
  InterpreterLibraryContents(..),
  -- * Binary 'Name' serializers
  BytecodeNameEnv(..),
  addBinNameWriter,
  addBinNameReader,
) where

import GHC.Prelude

import GHC.ByteCode.Types
import GHC.Data.FastString
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Unit.Types
import GHC.Utils.Binary
import GHC.Utils.Exception
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Utils.Fingerprint (Fingerprint)

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString(..))
import Data.Foldable
import Data.IORef
import Data.Proxy
import Data.Word
import System.IO.Unsafe (unsafeInterleaveIO)

-- | The on-disk representation of a bytecode object for a specific module.
--
-- This is the representation which we serialise and write to disk.
-- The difference from 'ModuleByteCode' is that the contents of the object files
-- contained by 'ModuleByteCode' are stored in-memory rather than as file paths to
-- temporary files.
data OnDiskModuleByteCode = OnDiskModuleByteCode { odgbc_module :: Module
                                                 , odgbc_hash :: Fingerprint
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

instance Binary OnDiskModuleByteCode where
  get bh = do
    odgbc_hash <- get bh
    odgbc_module <- get bh
    odgbc_compiled_byte_code <- lazyGet bh
    odgbc_foreign <- lazyGet bh
    pure OnDiskModuleByteCode {..}

  put_ bh OnDiskModuleByteCode {..} = do
    put_ bh odgbc_hash
    put_ bh odgbc_module
    lazyPut bh odgbc_compiled_byte_code
    lazyPut bh odgbc_foreign

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
    bc_hpc_info <- get bh
    return $
      CompiledByteCode
        { bc_bcos,
          bc_itbls,
          bc_strs,
          bc_breaks,
          bc_spt_entries,
          bc_hpc_info
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
    put_ bh bc_hpc_info

instance Binary ByteCodeHpcInfo where
  put_ bh ByteCodeHpcInfo{bchi_tick_count,bchi_hash,bchi_tickbox_name,bchi_module_name} = do
    put_ bh bchi_module_name
    put_ bh bchi_tickbox_name
    put_ bh bchi_tick_count
    put_ bh bchi_hash

  get bh = do
    bchi_module_name <- get bh
    bchi_tickbox_name <- get bh
    bchi_tick_count <- get bh
    bchi_hash <- get bh
    pure ByteCodeHpcInfo
      { bchi_tick_count
      , bchi_hash
      , bchi_tickbox_name
      , bchi_module_name
      }

instance Binary UnlinkedBCO where
  get bh = do
    t <- getByte bh
    case t of
      0 -> UnlinkedBCO
        <$> getViaBinName bh
        <*> get bh
        <*> get bh
        <*> get bh
        <*> get bh
        <*> get bh
      1 -> UnlinkedStaticCon
        <$> getViaBinName bh
        <*> getViaBinName bh
        <*> get bh
        <*> get bh
        <*> get bh
      _ -> panic "Binary UnlinkedBCO: invalid byte"

  put_ bh UnlinkedBCO {..} = do
    putByte bh 0
    putViaBinName bh unlinkedBCOName
    put_ bh unlinkedBCOArity
    put_ bh unlinkedBCOInstrs
    put_ bh unlinkedBCOBitmap
    put_ bh unlinkedBCOLits
    put_ bh unlinkedBCOPtrs
  put_ bh UnlinkedStaticCon {..} = do
    putByte bh 1
    putViaBinName bh unlinkedStaticConName
    putViaBinName bh unlinkedStaticConDataConName
    put_ bh unlinkedStaticConLits
    put_ bh unlinkedStaticConPtrs
    put_ bh unlinkedStaticConIsUnlifted

-- Also see Note [BCOByteArray serialization]. This instance is unlike
-- the `Binary` instances in `ghci`, which are for the `Binary` class
-- in `binary` and are used across host/target platforms; here this
-- instance is only used on the host for bytecode object serialization
-- and doesn't cross host/target boundary. Therefore it's safe to
-- serialize the underlying buffer directly.
instance Binary (BCOByteArray a) where
  put_ bh (BCOByteArray ba#) = put_ bh $ SBS ba#

  get bh = (\(SBS ba#) -> BCOByteArray ba#) <$> get bh

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

data BytecodeNameEnv = ByteCodeNameEnv { _bytecode_next_id :: !Word64
                                       , _bytecode_name_subst :: NameEnv Word64
                                       }

addBinNameWriter :: WriteBinHandle -> IO WriteBinHandle
addBinNameWriter bh' = do
  env_ref <- newIORef (ByteCodeNameEnv 0 emptyNameEnv)
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
            key <- getBinNameKey env_ref nm
            -- Delimit the OccName from the deterministic counter to keep the
            -- encoding injective, avoiding collisions like "foo1" vs "foo#1".
            put_ bh (occNameFS (occName nm) `appendFS` mkFastString ('#' : show key))
  where
    -- Find a deterministic key for local names. This
    getBinNameKey ref name = do
      atomicModifyIORef ref (\b@(ByteCodeNameEnv next subst) ->
        case lookupNameEnv subst name of
          Just idx -> (b, idx)
          Nothing  -> (ByteCodeNameEnv (next + 1) (extendNameEnv subst name next), next))

addBinNameReader :: NameCache -> ReadBinHandle -> IO ReadBinHandle
addBinNameReader nc bh' = do
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
          u <- takeUniqFromNameCache nc
          evaluate $ mkInternalName u occ noSrcSpan
        fmap BinName $ atomicModifyIORef' env_ref $ \env ->
          case lookupOccEnv env occ of
            Just nm -> (env, nm)
            _ -> nm' `seq` (extendOccEnv env occ nm', nm')
      _ -> panic "Binary BinName: invalid byte"

-- Note [Serializing Names in bytecode]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The bytecode related types contain various Names which we need to
-- serialize. Unfortunately, we can't directly use the Binary instance
-- of Name: it is only meant to be used for serializing external Names
-- in BinIface logic, but bytecode does contain internal Names.
--
-- We also need to maintain the invariant that: any pair of internal
-- Names with equal/different uniques must also be deserialized to
-- have the same equality. Therefore when we write the names to the interface, we
-- use an incrementing counter to give each local name it's own unique number. A substitution
-- is maintained to give each occurence of the Name the same unique key. When the interface
-- is read, a reverse mapping is used from these unique keys to a Name.
--

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

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
import qualified GHC.Data.Word64Map.Strict as Word64Map
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

import Data.ByteString (ByteString)
import Data.IORef
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
            put_ bh $ occNameFS $ occName nm
            put_ bh key
  where
    -- Find a deterministic key for local names. This
    getBinNameKey ref name = do
      atomicModifyIORef ref (\b@(ByteCodeNameEnv next subst) ->
        case lookupNameEnv subst name of
          Just idx -> (b, idx)
          Nothing  -> (ByteCodeNameEnv (next + 1) (extendNameEnv subst name next), next))

addBinNameReader :: NameCache -> ReadBinHandle -> IO ReadBinHandle
addBinNameReader nc bh' = do
  env_ref <- newIORef Word64Map.empty
  pure $ flip addReaderToUserData bh' $ BinaryReader $ \bh -> do
    t <- getByte bh
    case t of
      0 -> do
        nm <- get bh
        pure $ BinName nm
      1 -> do
        occ <- mkVarOccFS <$> get bh
        key <- get bh
        -- We don't want to get a new unique from the NameCache each time we
        -- see a name.
        nm' <- unsafeInterleaveIO $ do
          u <- takeUniqFromNameCache nc
          evaluate $ mkInternalName u occ noSrcSpan
        fmap BinName $ atomicModifyIORef' env_ref $ \env ->
          case Word64Map.lookup key env of
            Just nm -> (env, nm)
            _ -> nm' `seq` (Word64Map.insert key nm' env, nm')
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

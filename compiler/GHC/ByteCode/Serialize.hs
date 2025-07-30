{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.ByteCode.Serialize
  ( testBinByteCode, writeBinByteCode, readBinByteCode, ByteCodeObject(..)
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
import GHC.Unit.Module.WholeCoreBindings
import GHC.Types.ForeignStubs
import GHC.ForeignSrcLang
import GHC.Driver.DynFlags
import System.Directory

-- | The on-disk representation of a bytecode object
data OnDiskByteCodeObject = OnDiskByteCodeObject { odbco_module :: Module
                                                 , odbco_compiled_byte_code :: CompiledByteCode
                                                 , odbco_foreign :: IfaceForeign }

-- | The in-memory representation of a bytecode object
data ByteCodeObject = ByteCodeObject { bco_module :: Module
                                      , bco_compiled_byte_code :: CompiledByteCode
                                      , bco_foreign_sources :: [(ForeignSrcLang, FilePath)]
                                      , bco_foreign_stubs :: ForeignStubs }

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

decodeOnDiskByteCodeObject :: HscEnv -> OnDiskByteCodeObject -> IO ByteCodeObject
decodeOnDiskByteCodeObject hsc_env odbco = do
  (a,b) <- decodeIfaceForeign (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (tmpDir (hsc_dflags hsc_env)) (odbco_foreign odbco)
  pure $ ByteCodeObject {
    bco_module = odbco_module odbco,
    bco_compiled_byte_code = odbco_compiled_byte_code odbco,
    bco_foreign_stubs = a,
    bco_foreign_sources = b
   }

encodeOnDiskByteCodeObject :: HscEnv -> ByteCodeObject -> IO OnDiskByteCodeObject
encodeOnDiskByteCodeObject hsc_env bco = do
  iface_foreign <- encodeIfaceForeign (hsc_logger hsc_env) (hsc_dflags hsc_env) (bco_foreign_stubs bco) (bco_foreign_sources bco)
  pure $ OnDiskByteCodeObject {
    odbco_module = bco_module bco,
    odbco_compiled_byte_code = bco_compiled_byte_code bco,
    odbco_foreign = iface_foreign
   }

testBinByteCode :: HscEnv -> ByteCodeObject -> IO ByteCodeObject
testBinByteCode hsc_env cbc = withSystemTempDirectory "ghc-bbc" $ \tmpdir -> do
  let f = tmpdir </> "ghc-bbc"
  roundtripBinByteCode hsc_env f cbc

roundtripBinByteCode ::
  HscEnv -> FilePath -> ByteCodeObject -> IO ByteCodeObject
roundtripBinByteCode hsc_env f cbc = do
  writeBinByteCode hsc_env f cbc
  readBinByteCode hsc_env f

readBinByteCode :: HscEnv -> FilePath -> IO ByteCodeObject
readBinByteCode hsc_env f = do
  bh' <- readBinMem f
  bh <- addBinNameReader hsc_env bh'
  odbco <- getWithUserData (hsc_NC hsc_env) bh
  decodeOnDiskByteCodeObject hsc_env odbco


writeBinByteCode :: HscEnv -> FilePath -> ByteCodeObject -> IO ()
writeBinByteCode hsc_env f cbc = do
  createDirectoryIfMissing True (takeDirectory f)
  bh' <- openBinMem (1024 * 1024)
  bh <- addBinNameWriter bh'
  odbco <- encodeOnDiskByteCodeObject hsc_env cbc
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
    evaluate
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
        u <- takeUniqFromNameCache hsc_NC
        nm' <- evaluate $ mkInternalName u occ noSrcSpan
        fmap BinName $ atomicModifyIORef' env_ref $ \env ->
          case lookupOccEnv env occ of
            Just nm -> (env, nm)
            _ -> (extendOccEnv env occ nm', nm')
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
-- have the same equality. So normally uniques aren't supposed to be
-- serialized, but for this invariant to work, we do append uniques to
-- OccNames of internal Names, so that they can be uniquely identified
-- by OccName alone. When deserializing, we check a global cached
-- mapping from OccName to Unique, and create the real Name with the
-- right Unique if it's already deserialized at least once.

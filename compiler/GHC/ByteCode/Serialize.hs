{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.ByteCode.Serialize where

import Control.Exception
import Control.Monad
import Data.Binary qualified as Binary
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable
import Data.IORef
import Data.Proxy
import Data.Word
import GHC.Builtin.PrimOps
import GHC.ByteCode.Types
import GHC.Data.FlatBag as FlatBag
import GHC.Driver.Env.Types
import GHC.Iface.Binary
import GHC.Prelude
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.SptEntry
import GHC.Types.SrcLoc
import GHC.Utils.Binary
import GHC.Utils.Panic
import GHCi.FFI
import GHCi.Message

import System.FilePath
import GHC.Utils.TmpFs

unsupportedBCO :: UnlinkedBCO -> Bool
unsupportedBCO UnlinkedBCO {..} = any w unlinkedBCOPtrs
  where
    w BCOPtrBreakArray {} = True
    w (BCOPtrBCO bco) = unsupportedBCO bco
    w _ = False

testBinByteCode :: HscEnv -> CompiledByteCode -> IO CompiledByteCode
testBinByteCode hsc_env cbc@CompiledByteCode {..}
  | Just _ <- bc_breaks = evaluate cbc
  | any unsupportedBCO bc_bcos = evaluate cbc
  | otherwise = withSystemTempDirectory "ghc-bbc" $ \tmpdir -> do
  appendFile "/tmp/ghc-bbc.log" "1"
  let f = tmpdir </> "ghc-bbc"
  roundtripBinByteCode hsc_env f cbc

roundtripBinByteCode :: HscEnv -> FilePath -> CompiledByteCode -> IO CompiledByteCode
roundtripBinByteCode hsc_env f cbc = do
  writeBinByteCode f cbc
  readBinByteCode hsc_env f

readBinByteCode :: HscEnv -> FilePath -> IO CompiledByteCode
readBinByteCode hsc_env f = do
  bh' <- readBinMem f
  bh <- addSerializableNameReader hsc_env bh'
  getWithUserData (hsc_NC hsc_env) bh

writeBinByteCode :: FilePath -> CompiledByteCode -> IO ()
writeBinByteCode f cbc = do
  bh' <- openBinMem (1024*1024)
  bh <- addSerializableNameWriter bh'
  putWithUserData QuietBinIFace NormalCompression bh cbc
  writeBinMem bh f

instance Binary CompiledByteCode where
  get bh = do
    bc_bcos <- get bh
    bc_itbls_len <- get bh
    bc_itbls <- replicateM bc_itbls_len $ do
      nm <- getViaSerializableName bh
      msg <-
        MkConInfoTable
          <$> get bh
          <*> get bh
          <*> get bh
          <*> get bh
          <*> get bh
          <*> get bh
      evaluate (nm, msg)
    bc_strs_len <- get bh
    bc_strs <-
      replicateM bc_strs_len
        $ (,)
        <$> getViaSerializableName bh
        <*> get bh
    bc_spt_entries <- get bh
    evaluate
      CompiledByteCode
        { bc_bcos,
          bc_itbls,
          bc_strs,
          bc_breaks = Nothing,
          bc_spt_entries
        }

  put_ bh CompiledByteCode {..} = do
    put_ bh bc_bcos
    put_ bh $ length bc_itbls
    for_ bc_itbls
      $ \(nm, MkConInfoTable tntc ptrs nptrs conNo tag descr) -> do
        putViaSerializableName bh nm
        put_ bh tntc
        put_ bh ptrs
        put_ bh nptrs
        put_ bh conNo
        put_ bh tag
        put_ bh descr
    put_ bh $ length bc_strs
    for_ bc_strs
      $ \(nm, str) -> putViaSerializableName bh nm *> put_ bh str
    put_ bh bc_spt_entries

instance Binary UnlinkedBCO where
  get bh =
    UnlinkedBCO
      <$> getViaSerializableName bh
      <*> get bh
      <*> (Binary.decode . LBS.fromStrict <$> get bh)
      <*> (Binary.decode . LBS.fromStrict <$> get bh)
      <*> get bh
      <*> get bh

  put_ bh UnlinkedBCO {..} = do
    putViaSerializableName bh unlinkedBCOName
    put_ bh unlinkedBCOArity
    put_ bh $ LBS.toStrict $ Binary.encode unlinkedBCOInstrs
    put_ bh $ LBS.toStrict $ Binary.encode unlinkedBCOBitmap
    put_ bh unlinkedBCOLits
    put_ bh unlinkedBCOPtrs

instance Binary BCOPtr where
  get bh = do
    t <- getByte bh
    case t of
      0 -> BCOPtrName <$> getViaSerializableName bh
      1 -> BCOPtrPrimOp <$> get bh
      2 -> BCOPtrBCO <$> get bh
      _ -> panic "GHC.ByteCode.Serialize.BCOPtr.get"

  put_ bh ptr = case ptr of
    BCOPtrName nm -> putByte bh 0 *> putViaSerializableName bh nm
    BCOPtrPrimOp op -> putByte bh 1 *> put_ bh op
    BCOPtrBCO bco -> putByte bh 2 *> put_ bh bco
    BCOPtrBreakArray {} -> panic "GHC.ByteCode.Serialize.BCOPtr.put"

instance Binary BCONPtr where
  get bh = do
    t <- getByte bh
    case t of
      0 -> BCONPtrWord . fromIntegral <$> (get bh :: IO Word64)
      1 -> BCONPtrLbl <$> get bh
      2 -> BCONPtrItbl <$> getViaSerializableName bh
      3 -> BCONPtrAddr <$> getViaSerializableName bh
      4 -> BCONPtrStr <$> get bh
      5 -> BCONPtrFFIInfo <$> get bh
      _ -> panic "GHC.ByteCode.Serialize.BCONPtr.get"

  put_ bh ptr = case ptr of
    BCONPtrWord lit -> putByte bh 0 *> put_ bh (fromIntegral lit :: Word64)
    BCONPtrLbl sym -> putByte bh 1 *> put_ bh sym
    BCONPtrItbl nm -> putByte bh 2 *> putViaSerializableName bh nm
    BCONPtrAddr nm -> putByte bh 3 *> putViaSerializableName bh nm
    BCONPtrStr str -> putByte bh 4 *> put_ bh str
    BCONPtrFFIInfo ffi -> putByte bh 5 *> put_ bh ffi

instance Binary SptEntry where
  get bh = SptEntry <$> getViaSerializableName bh <*> get bh

  put_ bh (SptEntry nm fp) = putViaSerializableName bh nm *> put_ bh fp

newtype SerializableName = SerializableName {unSerializableName :: Name}

getViaSerializableName :: ReadBinHandle -> IO Name
getViaSerializableName bh = case findUserDataReader Proxy bh of
  BinaryReader f -> unSerializableName <$> f bh

putViaSerializableName :: WriteBinHandle -> Name -> IO ()
putViaSerializableName bh nm = case findUserDataWriter Proxy bh of
  BinaryWriter f -> f bh $ SerializableName nm

addSerializableNameWriter :: WriteBinHandle -> IO WriteBinHandle
addSerializableNameWriter bh' =
  evaluate
    $ flip addWriterToUserData bh'
    $ BinaryWriter
    $ \bh
       ( SerializableName
           nm
         ) ->
        if
          | isExternalName nm -> do
              putByte bh 0
              put_ bh nm
          | otherwise -> do
              putByte bh 1
              put_ bh $ occName nm

addSerializableNameReader :: HscEnv -> ReadBinHandle -> IO ReadBinHandle
addSerializableNameReader HscEnv {..} bh' = do
  nc <- evaluate hsc_NC
  env_ref <- newIORef emptyOccEnv
  evaluate $ flip addReaderToUserData bh' $ BinaryReader $ \bh -> do
    t <- getByte bh
    case t of
      0 -> do
        nm <- get bh
        evaluate $ SerializableName nm
      1 -> do
        occ <- get bh
        u <- takeUniqFromNameCache nc
        nm' <- evaluate $ mkInternalName u occ noSrcSpan
        fmap SerializableName
          $ atomicModifyIORef' env_ref
          $ \env -> case lookupOccEnv env occ of
            Just nm -> (env, nm)
            _ -> (extendOccEnv env occ nm', nm')
      _ -> panic "GHC.ByteCode.Serialize.addSerializableNameReader"

instance Binary PrimOp where
  get bh = (allThePrimOps !!) <$> get bh

  put_ bh = put_ bh . primOpTag

instance Binary FFIInfo where
  get bh = FFIInfo <$> get bh <*> get bh

  put_ bh FFIInfo {..} = put_ bh ffiInfoArgs *> put_ bh ffiInfoRet

instance Binary FFIType where
  get bh = do
    t <- getByte bh
    evaluate $ case t of
      0 -> FFIVoid
      1 -> FFIPointer
      2 -> FFIFloat
      3 -> FFIDouble
      4 -> FFISInt8
      5 -> FFISInt16
      6 -> FFISInt32
      7 -> FFISInt64
      8 -> FFIUInt8
      9 -> FFIUInt16
      10 -> FFIUInt32
      11 -> FFIUInt64
      _ -> panic "GHC.ByteCode.Serialize.FFIType.get"

  put_ bh t = putByte bh $ case t of
    FFIVoid -> 0
    FFIPointer -> 1
    FFIFloat -> 2
    FFIDouble -> 3
    FFISInt8 -> 4
    FFISInt16 -> 5
    FFISInt32 -> 6
    FFISInt64 -> 7
    FFIUInt8 -> 8
    FFIUInt16 -> 9
    FFIUInt32 -> 10
    FFIUInt64 -> 11

instance (Binary a) => Binary (FlatBag a) where
  get bh = do
    xs <- get bh
    evaluate $ FlatBag.fromList (fromIntegral $ length xs) xs

  put_ bh = put_ bh . FlatBag.elemsFlatBag

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.ByteCode.Serialize (testBinByteCode) where

import GHC.Prelude
import GHC.Utils.Binary
import GHC.ByteCode.Breakpoints
import GHC.ByteCode.Types
import GHC.Utils.Exception
import GHC.Data.FlatBag as FlatBag
import GHCi.FFI
import GHC.Builtin.PrimOps
import GHC.Driver.Env
import GHC.Types.Name
import Data.Proxy
import GHC.Data.FastString
import Data.IORef
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.SptEntry
import GHC.Builtin.Types
import GHC.Types.Id
import Data.Word
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as Binary
import GHCi.Message
import Data.Foldable
import Control.Monad
import GHC.Iface.Binary
import GHC.Utils.TmpFs
import System.FilePath

testBinByteCode :: HscEnv -> CompiledByteCode -> IO CompiledByteCode
testBinByteCode hsc_env cbc = withSystemTempDirectory "ghc-bbc" $ \tmpdir -> do
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
      itbl <- get bh
      pure (nm, itbl)
    bc_strs_len <- get bh
    bc_strs <-
      replicateM bc_strs_len
        $ (,)
        <$> getViaSerializableName bh
        <*> get bh
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
    for_ bc_itbls
      $ \(nm, itbl) -> do
        putViaSerializableName bh nm
        put_ bh itbl
    put_ bh $ length bc_strs
    for_ bc_strs
      $ \(nm, str) -> putViaSerializableName bh nm *> put_ bh str
    put_ bh bc_breaks
    put_ bh bc_spt_entries

instance Binary InternalModBreaks where
  get bh = InternalModBreaks <$> get bh <*> get bh

  put_ bh InternalModBreaks {..} = put_ bh imodBreaks_breakInfo *> put_ bh imodBreaks_modBreaks

instance Binary ModBreaks where
  get bh = ModBreaks <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh

  put_ bh ModBreaks {..} = put_ bh modBreaks_locs *> put_ bh modBreaks_vars *> put_ bh modBreaks_decls *> put_ bh modBreaks_ccs *> put_ bh modBreaks_module

instance Binary SrcSpan where
  get bh = unBinSrcSpan <$> get bh

  put_ bh = put_ bh . BinSrcSpan

instance Binary CgBreakInfo where
  put_ bh CgBreakInfo {..} = put_ bh cgb_tyvars *> put_ bh cgb_vars *> put_ bh cgb_resty *> put_ bh cgb_tick_id

  get bh = CgBreakInfo <$> get bh <*> get bh <*> get bh <*> get bh

instance Binary ConInfoTable where
  get bh = Binary.decode . LBS.fromStrict <$> get bh

  put_ bh = put_ bh . LBS.toStrict . Binary.encode


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
      _ -> BCOPtrBreakArray <$> get bh

  put_ bh ptr = case ptr of
    BCOPtrName nm -> putByte bh 0 *> putViaSerializableName bh nm
    BCOPtrPrimOp op -> putByte bh 1 *> put_ bh op
    BCOPtrBCO bco -> putByte bh 2 *> put_ bh bco
    BCOPtrBreakArray info_mod -> putByte bh 3 *> put_ bh info_mod

instance Binary BCONPtr where
  get bh = do
    t <- getByte bh
    case t of
      0 -> BCONPtrWord . fromIntegral <$> (get bh :: IO Word64)
      1 -> BCONPtrLbl <$> get bh
      2 -> BCONPtrItbl <$> getViaSerializableName bh
      3 -> BCONPtrAddr <$> getViaSerializableName bh
      4 -> BCONPtrStr <$> get bh
      5 -> BCONPtrFS <$> get bh
      6 -> BCONPtrFFIInfo <$> get bh
      _ -> BCONPtrCostCentre <$> get bh

  put_ bh ptr = case ptr of
    BCONPtrWord lit -> putByte bh 0 *> put_ bh (fromIntegral lit :: Word64)
    BCONPtrLbl sym -> putByte bh 1 *> put_ bh sym
    BCONPtrItbl nm -> putByte bh 2 *> putViaSerializableName bh nm
    BCONPtrAddr nm -> putByte bh 3 *> putViaSerializableName bh nm
    BCONPtrStr str -> putByte bh 4 *> put_ bh str
    BCONPtrFS fs -> putByte bh 5 *> put_ bh fs
    BCONPtrFFIInfo ffi -> putByte bh 6 *> put_ bh ffi
    BCONPtrCostCentre ibi -> putByte bh 7 *> put_ bh ibi

instance Binary InternalBreakLoc where
  get bh = InternalBreakLoc <$> get bh

  put_ bh InternalBreakLoc {..} = put_ bh internalBreakLoc

instance Binary BreakpointId where
  get bh = BreakpointId <$> get bh <*> get bh

  put_ bh BreakpointId {..} = put_ bh bi_tick_mod *> put_ bh bi_tick_index

instance Binary InternalBreakpointId where
  get bh = InternalBreakpointId <$> get bh <*> get bh

  put_ bh InternalBreakpointId {..} = put_ bh ibi_info_mod *> put_ bh ibi_info_index

instance Binary SptEntry where
  get bh = do
    nm <- getViaSerializableName bh
    fp <- get bh
    pure $ SptEntry (mkVanillaGlobal nm anyTy) fp

  put_ bh (SptEntry nm fp) = putViaSerializableName bh (getName nm) *> put_ bh fp


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
              put_ bh $ occNameFS (occName nm) `appendFS` mkFastString (show $ nameUnique nm)

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
      _ -> do
        occ <- mkVarOccFS <$> get bh
        u <- takeUniqFromNameCache nc
        nm' <- evaluate $ mkInternalName u occ noSrcSpan
        fmap SerializableName
          $ atomicModifyIORef' env_ref
          $ \env -> case lookupOccEnv env occ of
            Just nm -> (env, nm)
            _ -> (extendOccEnv env occ nm', nm')

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
      _ -> FFIUInt64

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

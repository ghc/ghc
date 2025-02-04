{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans -fdefer-typed-holes -Wno-typed-holes #-}

module GHC.ByteCode.Serialize where

import GHC.Prelude
import GHC.ByteCode.Types
import GHC.Utils.Binary
import GHC.Data.FlatBag as FlatBag
import GHC.Builtin.PrimOps
import Data.Word
import GHC.Utils.Panic
import qualified Data.Binary as Binary
import GHCi.ResolvedBCO ()
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import GHC.Types.Name.Env
import Control.Monad
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import Data.Proxy

instance Binary CompiledByteCode where
  get bh = do
    bc_bcos <- get bh
    bc_strs_len <- get bh
    bc_strs <- replicateM bc_strs_len $ (,) <$> getViaSerializableName bh <*> get bh
    pure CompiledByteCode {bc_bcos, bc_itbls = emptyNameEnv, bc_ffis = [], bc_strs, bc_breaks = Nothing, bc_spt_entries = []}

  put_ bh CompiledByteCode {..} = do
    put_ bh bc_bcos
    put_ bh $ length bc_strs
    for_ bc_strs $ \(nm, str) -> putViaSerializableName bh nm *> put_ bh str

instance Binary UnlinkedBCO where
  get bh = UnlinkedBCO <$> getViaSerializableName bh <*> get bh <*> (Binary.decode . LBS.fromStrict <$> get bh) <*> (Binary.decode . LBS.fromStrict <$> get bh) <*> get bh <*> get bh

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
      _ -> panic "GHC.ByteCode.Serialize.BCONPtr.get"

  put_ bh ptr = case ptr of
    BCONPtrWord lit -> putByte bh 0 *> put_ bh (fromIntegral lit :: Word64)
    BCONPtrLbl sym -> putByte bh 1 *> put_ bh sym
    BCONPtrItbl nm -> putByte bh 2 *> putViaSerializableName bh nm
    BCONPtrAddr nm -> putByte bh 3 *> putViaSerializableName bh nm
    BCONPtrStr str -> putByte bh 4 *> put_ bh str

newtype SerializableName = SerializableName { unSerializableName :: Name }

instance Binary SerializableName where
  get bh =
    case findUserDataReader Proxy bh of
      BinaryReader f -> f bh

  put_ bh nm =
    case findUserDataWriter Proxy bh of
      BinaryWriter f -> f bh nm

getViaSerializableName :: ReadBinHandle -> IO Name
getViaSerializableName bh = unSerializableName <$> get bh

putViaSerializableName :: WriteBinHandle -> Name -> IO ()
putViaSerializableName bh nm = put_ bh $ SerializableName nm

addSerializableNameWriter :: WriteBinHandle -> IO WriteBinHandle
addSerializableNameWriter = undefined

instance Binary PrimOp where
  get bh = (allThePrimOps !!) <$> get bh
  put_ bh = put_ bh . primOpTag

instance Binary a => Binary (FlatBag a) where
  get bh = do
    xs <- get bh
    pure $ FlatBag.fromList (fromIntegral $ length xs) xs

  put_ bh = put_ bh . FlatBag.elemsFlatBag

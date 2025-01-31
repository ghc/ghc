{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans -fdefer-typed-holes -Wno-typed-holes #-}

module GHC.ByteCode.Serialize where

import GHC.Prelude
import GHC.ByteCode.Types
import GHC.Utils.Binary
import GHC.Data.FlatBag as FlatBag
import GHC.Builtin.PrimOps
import Data.Word
import GHC.Iface.Syntax
import GHC.Utils.Panic
import qualified Data.Binary as Binary
import GHCi.ResolvedBCO ()
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import GHC.Types.Name.Env
import Control.Monad

instance Binary CompiledByteCode where
  get bh = do
    bc_bcos <- get bh
    bc_strs_len <- get bh
    bc_strs <- replicateM bc_strs_len $ (,) <$> getIfaceTopBndr bh <*> get bh
    pure CompiledByteCode {bc_bcos, bc_itbls = emptyNameEnv, bc_ffis = [], bc_strs, bc_breaks = Nothing, bc_spt_entries = []}

  put_ bh CompiledByteCode {..} = do
    put_ bh bc_bcos
    put_ bh $ length bc_strs
    for_ bc_strs $ \(nm, str) -> putIfaceTopBndr bh nm *> put_ bh str

instance Binary UnlinkedBCO where
  get bh = UnlinkedBCO <$> getIfaceTopBndr bh <*> get bh <*> (Binary.decode . LBS.fromStrict <$> get bh) <*> (Binary.decode . LBS.fromStrict <$> get bh) <*> get bh <*> get bh

  put_ bh UnlinkedBCO {..} = do
    putIfaceTopBndr bh unlinkedBCOName
    put_ bh unlinkedBCOArity
    put_ bh $ LBS.toStrict $ Binary.encode unlinkedBCOInstrs
    put_ bh $ LBS.toStrict $ Binary.encode unlinkedBCOBitmap
    put_ bh unlinkedBCOLits
    put_ bh unlinkedBCOPtrs

instance Binary BCOPtr where
  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> BCOPtrName <$> getIfaceTopBndr bh
      1 -> BCOPtrPrimOp <$> get bh
      2 -> BCOPtrBCO <$> get bh
      _ -> panic "GHC.ByteCode.Serialize.BCOPtr.get"
  put_ bh ptr = case ptr of
    BCOPtrName nm -> put_ bh (0 :: Word8) *> putIfaceTopBndr bh nm
    BCOPtrPrimOp op -> put_ bh (1 :: Word8) *> put_ bh op
    BCOPtrBCO bco -> put_ bh (2 :: Word8) *> put_ bh bco
    BCOPtrBreakArray {} -> panic "GHC.ByteCode.Serialize.BCOPtr.put"

instance Binary BCONPtr where
  get bh = do
    (t :: Word8) <- get bh
    case t of
      0 -> BCONPtrWord . fromIntegral <$> (get bh :: IO Word64)
      1 -> BCONPtrLbl <$> get bh
      2 -> BCONPtrItbl <$> getIfaceTopBndr bh
      3 -> BCONPtrAddr <$> getIfaceTopBndr bh
      4 -> BCONPtrStr <$> get bh
      _ -> panic "GHC.ByteCode.Serialize.BCONPtr.get"

  put_ bh ptr = case ptr of
    BCONPtrWord lit -> put_ bh (0 :: Word8) *> put_ bh (fromIntegral lit :: Word64)
    BCONPtrLbl sym -> put_ bh (1 :: Word8) *> put_ bh sym
    BCONPtrItbl nm -> put_ bh (2 :: Word8) *> putIfaceTopBndr bh nm
    BCONPtrAddr nm -> put_ bh (3 :: Word8) *> putIfaceTopBndr bh nm
    BCONPtrStr str -> put_ bh (4 :: Word8) *> put_ bh str

instance Binary PrimOp where
  get bh = (allThePrimOps !!) <$> get bh
  put_ bh = put_ bh . primOpTag

instance Binary a => Binary (FlatBag a) where
  get bh = do
    xs <- get bh
    pure $ FlatBag.fromList (fromIntegral $ length xs) xs

  put_ bh = put_ bh . FlatBag.elemsFlatBag

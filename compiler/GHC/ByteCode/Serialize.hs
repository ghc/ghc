{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.ByteCode.Serialize where

import Data.Word
import GHC.ByteCode.Types
import GHC.HsToCore.Ticks
import GHC.Unit.Types
import GHC.Utils.Binary
import GHC.Utils.Panic
import GHC.Prelude
import GHC.Types.SrcLoc
import Trace.Hpc.Mix

data MkModBreaks = MkModBreaks {
  mkModBreaksModule :: !Module,
  mkModBreaksTicks :: ![Tick]
}

instance Binary CgBreakInfo where
  put_ bh CgBreakInfo {..} = do
    put_ bh cgb_tyvars
    put_ bh cgb_vars
    put_ bh cgb_resty

  get bh = CgBreakInfo <$> get bh <*> get bh <*> get bh

instance Binary Word where
  put_ bh i = put_ bh (fromIntegral i :: Word64)

  get bh = fromIntegral <$> (get bh :: IO Word64)

instance Binary MkModBreaks where
  put_ bh MkModBreaks {..} = do
    put_ bh mkModBreaksModule
    put_ bh mkModBreaksTicks

  get bh = MkModBreaks <$> get bh <*> get bh

instance Binary Tick where
  put_ bh Tick {..} = do
    put_ bh tick_loc
    put_ bh tick_path
    put_ bh tick_ids
    put_ bh tick_label

  get bh = Tick <$> get bh <*> get bh <*> get bh <*> get bh

instance Binary SrcSpan where
  put_ bh (RealSrcSpan s b) = do
    putByte bh 0
    put_ bh s
    put_ bh b
  put_ bh (UnhelpfulSpan s) = do
    putByte bh 1
    put_ bh s

  get bh = do
    t <- getByte bh
    case t of
      0 -> RealSrcSpan <$> get bh <*> get bh
      1 -> UnhelpfulSpan <$> get bh
      _ -> panic "GHC.ByteCode.Serialize.SrcSpan.get"

instance Binary RealSrcSpan where
  put_ bh (RealSrcSpan' {..}) = do
    put_ bh srcSpanFile
    put_ bh srcSpanSLine
    put_ bh srcSpanSCol
    put_ bh srcSpanELine
    put_ bh srcSpanECol

  get bh = RealSrcSpan' <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh

instance Binary BufSpan where
  put_ bh (BufSpan {..}) = do
    put_ bh bufSpanStart
    put_ bh bufSpanEnd

  get bh = BufSpan <$> get bh <*> get bh

instance Binary BufPos where
  put_ bh (BufPos p) = put_ bh p

  get bh = BufPos <$> get bh

instance Binary BoxLabel where
  put_ bh = put_ bh . show

  get bh = read <$> get bh

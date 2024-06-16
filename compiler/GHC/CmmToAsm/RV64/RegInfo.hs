-- | Minimum viable implementation of jump short-cutting: No short-cutting.
--
-- The functions here simply implement the no-short-cutting case. Implementing
-- the real behaviour would be a great optimization in future.
module GHC.CmmToAsm.RV64.RegInfo
  ( getJumpDestBlockId,
    canShortcut,
    shortcutStatics,
    shortcutJump,
  )
where

import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.CmmToAsm.RV64.Instr
import GHC.Prelude
import GHC.Utils.Outputable

newtype JumpDest = DestBlockId BlockId

instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid

-- | Extract BlockId
--
-- Never `Nothing` for Riscv64 NCG.
getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid

-- No `Instr`s can bet shortcut (for now)
canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

-- Identity of the provided `RawCmmStatics`
shortcutStatics :: (BlockId -> Maybe JumpDest) -> RawCmmStatics -> RawCmmStatics
shortcutStatics _ other_static = other_static

-- Identity of the provided `Instr`
shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other

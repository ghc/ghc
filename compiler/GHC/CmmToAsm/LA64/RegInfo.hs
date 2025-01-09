-- Here maybe have something to be optimized in future?
module GHC.CmmToAsm.LA64.RegInfo where

import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.CmmToAsm.LA64.Instr
import GHC.Prelude
import GHC.Utils.Outputable

newtype JumpDest = DestBlockId BlockId

instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid

getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutStatics :: (BlockId -> Maybe JumpDest) -> RawCmmStatics -> RawCmmStatics
shortcutStatics _ other_static = other_static

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other

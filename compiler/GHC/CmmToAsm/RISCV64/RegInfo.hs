module GHC.CmmToAsm.RISCV64.RegInfo where
import Prelude
import GHC.Cmm.BlockId
import GHC.CmmToAsm.RISCV64.Instr
import GHC.Cmm
import GHC.Utils.Outputable

data JumpDest = DestBlockId BlockId

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId _) = text "TODO"

getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId _ = error "TODO: getJumpDestBlockId"

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = error "TODO: canShortcut"

shortcutStatics :: (BlockId -> Maybe JumpDest) -> RawCmmStatics -> RawCmmStatics
shortcutStatics _ _ = error "TODO: shortcutStatics"

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ _ = error "TODO: shortcutJump"

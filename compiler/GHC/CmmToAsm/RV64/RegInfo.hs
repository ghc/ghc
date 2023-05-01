module GHC.CmmToAsm.RV64.RegInfo where

import GHC.Prelude

import GHC.CmmToAsm.RV64.Instr
import GHC.Cmm.BlockId
import GHC.Cmm

import GHC.Utils.Outputable

data JumpDest = DestBlockId BlockId

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid

-- TODO: documen what this does. See Ticket 19914
getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid

-- TODO: document what this does. See Ticket 19914
canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

-- TODO: document what this does. See Ticket 19914
shortcutStatics :: (BlockId -> Maybe JumpDest) -> RawCmmStatics -> RawCmmStatics
shortcutStatics _ other_static = other_static

-- TODO: document what this does. See Ticket 19914
shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other
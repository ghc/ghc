{-# language CPP #-}
module GHC.CmmToAsm.AArch64.RegInfo where

#include "HsVersions.h"

import GHC.Prelude

import GHC.CmmToAsm.AArch64.Instr
import GHC.Cmm.BlockId
import GHC.Cmm

import GHC.Utils.Outputable

data JumpDest = DestBlockId BlockId

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid

-- XXX: documen what this does.
getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid

-- XXX: document what this does.
canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

-- XXX: document what this does
shortcutStatics :: (BlockId -> Maybe JumpDest) -> RawCmmStatics -> RawCmmStatics
shortcutStatics _ other_static = other_static

-- XXX: document what this does.
shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other

{-# LANGUAGE CPP #-}
module GHC.CmmToAsm.ARM.RegInfo
  ( JumpDest( DestBlockId )
  , getJumpDestBlockId
  , canShortcut
  , shortcutJump
  , shortcutStatics
  ) where

#include "HsVersions.h"

import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.CmmToAsm.ARM.Instr
import GHC.Prelude
import GHC.Types.Unique
import GHC.Utils.Outputable   (ppr, text, Outputable)

data JumpDest = DestBlockId BlockId

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId _) = text "TODO: implement me"

getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other

-- Here because it knows about JumpDest
shortcutStatics :: (BlockId -> Maybe JumpDest) -> RawCmmStatics -> RawCmmStatics
shortcutStatics fn (CmmStaticsRaw lbl statics)
  = CmmStaticsRaw lbl $ map (shortcutStatic fn) statics
  -- we need to get the jump tables, so apply the mapping to the entries
  -- of a CmmData too.

shortcutLabel :: (BlockId -> Maybe JumpDest) -> CLabel -> CLabel
shortcutLabel fn lab
  | Just blkId <- maybeLocalBlockLabel lab = shortBlockId fn blkId
  | otherwise                              = lab

shortcutStatic :: (BlockId -> Maybe JumpDest) -> CmmStatic -> CmmStatic
shortcutStatic fn (CmmStaticLit (CmmLabel lab))
  = CmmStaticLit (CmmLabel (shortcutLabel fn lab))
shortcutStatic fn (CmmStaticLit (CmmLabelDiffOff lbl1 lbl2 off w))
  = CmmStaticLit (CmmLabelDiffOff (shortcutLabel fn lbl1) lbl2 off w)
        -- slightly dodgy, we're ignoring the second label, but this
        -- works with the way we use CmmLabelDiffOff for jump tables now.
shortcutStatic _ other_static
        = other_static

shortBlockId
        :: (BlockId -> Maybe JumpDest)
        -> BlockId
        -> CLabel

shortBlockId fn blockid =
   case fn blockid of
      Nothing -> mkLocalBlockLabel uq
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
   where uq = getUnique blockid

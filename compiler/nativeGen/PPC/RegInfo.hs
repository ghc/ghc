{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------
module PPC.RegInfo (
        JumpDest( DestBlockId ), getJumpDestBlockId,
        canShortcut,
        shortcutJump,

        shortcutStatics
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import GhcPrelude

import PPC.Instr

import BlockId
import Cmm
import CLabel

import Unique
import Outputable (ppr, text, Outputable, (<>))

data JumpDest = DestBlockId BlockId

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid

getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other


-- Here because it knows about JumpDest
shortcutStatics :: (BlockId -> Maybe JumpDest) -> CmmStatics -> CmmStatics
shortcutStatics fn (Statics lbl statics)
  = Statics lbl $ map (shortcutStatic fn) statics
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

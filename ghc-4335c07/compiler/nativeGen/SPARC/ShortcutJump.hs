module SPARC.ShortcutJump (
        JumpDest(..), getJumpDestBlockId,
        canShortcut,
        shortcutJump,
        shortcutStatics,
        shortBlockId
)

where

import GhcPrelude

import SPARC.Instr
import SPARC.Imm

import CLabel
import BlockId
import Cmm

import Panic


data JumpDest
        = DestBlockId BlockId
        | DestImm Imm

getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid
getJumpDestBlockId _                 = Nothing


canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing


shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other



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
shortcutStatic fn (CmmStaticLit (CmmLabelDiffOff lbl1 lbl2 off))
        = CmmStaticLit (CmmLabelDiffOff (shortcutLabel fn lbl1) lbl2 off)
-- slightly dodgy, we're ignoring the second label, but this
-- works with the way we use CmmLabelDiffOff for jump tables now.
shortcutStatic _ other_static
        = other_static


shortBlockId :: (BlockId -> Maybe JumpDest) -> BlockId -> CLabel
shortBlockId fn blockid =
   case fn blockid of
      Nothing -> blockLbl blockid
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
      Just (DestImm (ImmCLbl lbl)) -> lbl
      _other -> panic "shortBlockId"

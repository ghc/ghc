
module SPARC.ShortcutJump (
	JumpDest(..),
	canShortcut,
	shortcutJump,
	shortcutStatic,
	shortBlockId
)

where

import SPARC.Instr
import SPARC.Imm

import CLabel
import BlockId
import Cmm

import Panic



data JumpDest 
	= DestBlockId BlockId 
	| DestImm Imm


canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing


shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other


shortcutStatic :: (BlockId -> Maybe JumpDest) -> CmmStatic -> CmmStatic

shortcutStatic fn (CmmStaticLit (CmmLabel lab))
	| Just uq <- maybeAsmTemp lab 
	= CmmStaticLit (CmmLabel (shortBlockId fn (BlockId uq)))

shortcutStatic fn (CmmStaticLit (CmmLabelDiffOff lbl1 lbl2 off))
	| Just uq <- maybeAsmTemp lbl1
	= CmmStaticLit (CmmLabelDiffOff (shortBlockId fn (BlockId uq)) lbl2 off)

-- slightly dodgy, we're ignoring the second label, but this
-- works with the way we use CmmLabelDiffOff for jump tables now.
shortcutStatic _ other_static
        = other_static


shortBlockId :: (BlockId -> Maybe JumpDest) -> BlockId -> CLabel
shortBlockId fn blockid@(BlockId uq) =
   case fn blockid of
      Nothing -> mkAsmTempLabel uq
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
      Just (DestImm (ImmCLbl lbl)) -> lbl
      _other -> panic "shortBlockId"




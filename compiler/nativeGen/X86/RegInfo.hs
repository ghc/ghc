
module X86.RegInfo (
	mkVReg,

        JumpDest, 
	canShortcut, 
	shortcutJump, 
	
	shortcutStatic,
	regDotColor
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import X86.Instr
import X86.Cond
import X86.Regs
import Size
import Reg

import Cmm
import CLabel
import BlockId
import Outputable
import Unique

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
import UniqFM
#endif


mkVReg :: Unique -> Size -> Reg
mkVReg u size
   | not (isFloatSize size) = VirtualRegI u
   | otherwise
   = case size of
        FF32	-> VirtualRegD u
        FF64	-> VirtualRegD u
	_	-> panic "mkVReg"


data JumpDest = DestBlockId BlockId | DestImm Imm


canShortcut :: Instr -> Maybe JumpDest
canShortcut (JXX ALWAYS id) 	= Just (DestBlockId id)
canShortcut (JMP (OpImm imm)) 	= Just (DestImm imm)
canShortcut _ 			= Nothing


-- The helper ensures that we don't follow cycles.
shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump fn insn = shortcutJump' fn emptyBlockSet insn
  where shortcutJump' fn seen insn@(JXX cc id) =
          if elemBlockSet id seen then insn
          else case fn id of
                 Nothing                -> insn
                 Just (DestBlockId id') -> shortcutJump' fn seen' (JXX cc id')
                 Just (DestImm imm)     -> shortcutJump' fn seen' (JXX_GBL cc imm)
               where seen' = extendBlockSet seen id
        shortcutJump' _ _ other = other


-- Here because it knows about JumpDest
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

shortBlockId 
	:: (BlockId -> Maybe JumpDest)
	-> BlockId
	-> CLabel

shortBlockId fn blockid@(BlockId uq) =
   case fn blockid of
      Nothing -> mkAsmTempLabel uq
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
      Just (DestImm (ImmCLbl lbl)) -> lbl
      _other -> panic "shortBlockId"



-- reg colors for x86
#if i386_TARGET_ARCH
regDotColor :: Reg -> SDoc
regDotColor reg
 = let	Just	str	= lookupUFM regColors reg
   in	text str

regColors :: UniqFM [Char]
regColors
 = listToUFM
 $  	[ (eax,	"#00ff00")
	, (ebx,	"#0000ff")
	, (ecx,	"#00ffff")
	, (edx,	"#0080ff")

	, (fake0, "#ff00ff")
	, (fake1, "#ff00aa")
	, (fake2, "#aa00ff")
	, (fake3, "#aa00aa")
	, (fake4, "#ff0055")
	, (fake5, "#5500ff") ]


-- reg colors for x86_64
#elif x86_64_TARGET_ARCH
regDotColor :: Reg -> SDoc
regDotColor reg
 = let	Just	str	= lookupUFM regColors reg
   in	text str

regColors :: UniqFM [Char]
regColors
 = listToUFM
 $	[ (rax, "#00ff00"), (eax, "#00ff00")
	, (rbx,	"#0000ff"), (ebx, "#0000ff")
	, (rcx,	"#00ffff"), (ecx, "#00ffff")
	, (rdx,	"#0080ff"), (edx, "#00ffff")
	, (r8,  "#00ff80")
	, (r9,  "#008080")
	, (r10, "#0040ff")
	, (r11, "#00ff40")
	, (r12, "#008040")
	, (r13, "#004080")
	, (r14, "#004040")
	, (r15, "#002080") ]

	++ zip (map RealReg [16..31]) (repeat "red")
#else
regDotColor :: Reg -> SDoc
regDotColor	= panic "not defined"
#endif

-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

module PPC.RegInfo (
	RegUsage(..),
	noUsage,
	regUsage,
	patchRegs,
	jumpDests,
	isJumpish,
	patchJump,
	isRegRegMove,

        JumpDest(..), 
	canShortcut, 
	shortcutJump, 

	mkSpillInstr,
	mkLoadInstr,
	mkRegRegMoveInstr,
	mkBranchInstr,

	spillSlotSize,
	maxSpillSlots,
	spillSlotToOffset		
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import BlockId
import RegsBase
import PPC.Regs
import PPC.Instr
import Outputable
import Constants	( rESERVED_C_STACK_BYTES )
import FastBool

data RegUsage = RU [Reg] [Reg]

noUsage :: RegUsage
noUsage  = RU [] []

regUsage :: Instr -> RegUsage
regUsage instr = case instr of
    SPILL  reg _	-> usage ([reg], [])
    RELOAD _ reg	-> usage ([], [reg])

    LD    _ reg addr  	-> usage (regAddr addr, [reg])
    LA    _ reg addr  	-> usage (regAddr addr, [reg])
    ST    _ reg addr  	-> usage (reg : regAddr addr, [])
    STU    _ reg addr  -> usage (reg : regAddr addr, [])
    LIS   reg _		-> usage ([], [reg])
    LI    reg _		-> usage ([], [reg])
    MR	  reg1 reg2     -> usage ([reg2], [reg1])
    CMP   _ reg ri	-> usage (reg : regRI ri,[])
    CMPL  _ reg ri	-> usage (reg : regRI ri,[])
    BCC	   _ _		-> noUsage
    BCCFAR _ _		-> noUsage
    MTCTR reg		-> usage ([reg],[])
    BCTR  _		-> noUsage
    BL    _  params	-> usage (params, callClobberedRegs)
    BCTRL params	-> usage (params, callClobberedRegs)
    ADD	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    ADDC  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    ADDE  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    ADDIS reg1 reg2 _   -> usage ([reg2], [reg1])
    SUBF  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    MULLW reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    DIVW  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    DIVWU reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    MULLW_MayOflo reg1 reg2 reg3        
                        -> usage ([reg2,reg3], [reg1])
    AND	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    OR	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    XOR	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    XORIS reg1 reg2 _   -> usage ([reg2], [reg1])
    EXTS  _   reg1 reg2 -> usage ([reg2], [reg1])
    NEG	  reg1 reg2	-> usage ([reg2], [reg1])
    NOT	  reg1 reg2	-> usage ([reg2], [reg1])
    SLW	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SRW	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SRAW  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    RLWINM reg1 reg2 _ _ _
                        -> usage ([reg2], [reg1])
    FADD  _ r1 r2 r3   -> usage ([r2,r3], [r1])
    FSUB  _ r1 r2 r3   -> usage ([r2,r3], [r1])
    FMUL  _ r1 r2 r3   -> usage ([r2,r3], [r1])
    FDIV  _ r1 r2 r3   -> usage ([r2,r3], [r1])
    FNEG  r1 r2		-> usage ([r2], [r1])
    FCMP  r1 r2		-> usage ([r1,r2], [])
    FCTIWZ r1 r2	-> usage ([r2], [r1])
    FRSP r1 r2		-> usage ([r2], [r1])
    MFCR reg            -> usage ([], [reg])
    MFLR reg            -> usage ([], [reg])
    FETCHPC reg         -> usage ([], [reg])
    _ 	    	    	-> noUsage
  where
    usage (src, dst) = RU (filter interesting src)
    	    	    	  (filter interesting dst)
    regAddr (AddrRegReg r1 r2) = [r1, r2]
    regAddr (AddrRegImm r1 _)  = [r1]

    regRI (RIReg r) = [r]
    regRI  _	= []

interesting :: Reg -> Bool
interesting (VirtualRegI  _)  = True
interesting (VirtualRegHi _)  = True
interesting (VirtualRegF  _)  = True
interesting (VirtualRegD  _)  = True
interesting (RealReg i)       = isFastTrue (freeReg i)


-- -----------------------------------------------------------------------------
-- 'patchRegs' function

-- 'patchRegs' takes an instruction and applies the given mapping to
-- all the register references.

patchRegs :: Instr -> (Reg -> Reg) -> Instr
patchRegs instr env = case instr of
    SPILL reg slot	-> SPILL (env reg) slot
    RELOAD slot reg	-> RELOAD slot (env reg)

    LD    sz reg addr   -> LD sz (env reg) (fixAddr addr)
    LA    sz reg addr   -> LA sz (env reg) (fixAddr addr)
    ST    sz reg addr   -> ST sz (env reg) (fixAddr addr)
    STU    sz reg addr  -> STU sz (env reg) (fixAddr addr)
    LIS   reg imm	-> LIS (env reg) imm
    LI    reg imm	-> LI (env reg) imm
    MR	  reg1 reg2     -> MR (env reg1) (env reg2)
    CMP	  sz reg ri	-> CMP sz (env reg) (fixRI ri)
    CMPL  sz reg ri	-> CMPL sz (env reg) (fixRI ri)
    BCC	  cond lbl	-> BCC cond lbl
    BCCFAR cond lbl	-> BCCFAR cond lbl
    MTCTR reg		-> MTCTR (env reg)
    BCTR  targets	-> BCTR targets
    BL    imm argRegs	-> BL imm argRegs	-- argument regs
    BCTRL argRegs	-> BCTRL argRegs 	-- cannot be remapped
    ADD	  reg1 reg2 ri	-> ADD (env reg1) (env reg2) (fixRI ri)
    ADDC  reg1 reg2 reg3-> ADDC (env reg1) (env reg2) (env reg3)
    ADDE  reg1 reg2 reg3-> ADDE (env reg1) (env reg2) (env reg3)
    ADDIS reg1 reg2 imm -> ADDIS (env reg1) (env reg2) imm
    SUBF  reg1 reg2 reg3-> SUBF (env reg1) (env reg2) (env reg3)
    MULLW reg1 reg2 ri	-> MULLW (env reg1) (env reg2) (fixRI ri)
    DIVW  reg1 reg2 reg3-> DIVW (env reg1) (env reg2) (env reg3)
    DIVWU reg1 reg2 reg3-> DIVWU (env reg1) (env reg2) (env reg3)
    MULLW_MayOflo reg1 reg2 reg3
                        -> MULLW_MayOflo (env reg1) (env reg2) (env reg3)
    AND	  reg1 reg2 ri	-> AND (env reg1) (env reg2) (fixRI ri)
    OR 	  reg1 reg2 ri	-> OR  (env reg1) (env reg2) (fixRI ri)
    XOR	  reg1 reg2 ri	-> XOR (env reg1) (env reg2) (fixRI ri)
    XORIS reg1 reg2 imm -> XORIS (env reg1) (env reg2) imm
    EXTS  sz reg1 reg2 -> EXTS sz (env reg1) (env reg2)
    NEG	  reg1 reg2	-> NEG (env reg1) (env reg2)
    NOT	  reg1 reg2	-> NOT (env reg1) (env reg2)
    SLW	  reg1 reg2 ri	-> SLW (env reg1) (env reg2) (fixRI ri)
    SRW	  reg1 reg2 ri	-> SRW (env reg1) (env reg2) (fixRI ri)
    SRAW  reg1 reg2 ri	-> SRAW (env reg1) (env reg2) (fixRI ri)
    RLWINM reg1 reg2 sh mb me
                        -> RLWINM (env reg1) (env reg2) sh mb me
    FADD  sz r1 r2 r3   -> FADD sz (env r1) (env r2) (env r3)
    FSUB  sz r1 r2 r3   -> FSUB sz (env r1) (env r2) (env r3)
    FMUL  sz r1 r2 r3   -> FMUL sz (env r1) (env r2) (env r3)
    FDIV  sz r1 r2 r3   -> FDIV sz (env r1) (env r2) (env r3)
    FNEG  r1 r2		-> FNEG (env r1) (env r2)
    FCMP  r1 r2		-> FCMP (env r1) (env r2)
    FCTIWZ r1 r2	-> FCTIWZ (env r1) (env r2)
    FRSP r1 r2		-> FRSP (env r1) (env r2)
    MFCR reg            -> MFCR (env reg)
    MFLR reg            -> MFLR (env reg)
    FETCHPC reg         -> FETCHPC (env reg)
    _ -> instr
  where
    fixAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other



jumpDests :: Instr -> [BlockId] -> [BlockId]
jumpDests insn acc
  = case insn of
        BCC _ id        -> id : acc
        BCCFAR _ id     -> id : acc
        BCTR targets    -> targets ++ acc
	_		-> acc
	
	
-- | Check whether a particular instruction is a jump, branch or call instruction (jumpish)
--	We can't just use jumpDests above because the jump might take its arg,
--	so the instr won't contain a blockid.
--
isJumpish :: Instr -> Bool
isJumpish instr
 = case instr of
	BCC{}		-> True
	BCCFAR{}	-> True
        BCTR{}		-> True
        BCTRL{}		-> True
	BL{}		-> True
	JMP{}		-> True
	_		-> False

-- | Change the destination of this jump instruction
--	Used in joinToTargets in the linear allocator, when emitting fixup code
--	for join points.
patchJump :: Instr -> BlockId -> BlockId -> Instr
patchJump insn old new
  = case insn of
        BCC cc id 
	 | id == old 	-> BCC cc new

        BCCFAR cc id 
	 | id == old 	-> BCCFAR cc new

        BCTR _	 	-> error "Cannot patch BCTR"

	_		-> insn


isRegRegMove :: Instr -> Maybe (Reg,Reg)
isRegRegMove (MR dst src) = Just (src,dst)
isRegRegMove _  = Nothing


data JumpDest = DestBlockId BlockId | DestImm Imm

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other




-- -----------------------------------------------------------------------------
-- Generating spill instructions

mkSpillInstr
   :: Reg		-- register to spill
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr
mkSpillInstr reg delta slot
  = let	off     = spillSlotToOffset slot
    in
    let sz = case regClass reg of
                RcInteger -> II32
                RcDouble  -> FF64
		RcFloat	  -> panic "PPC.RegInfo.mkSpillInstr: no match"
    in ST sz reg (AddrRegImm sp (ImmInt (off-delta)))


mkLoadInstr
   :: Reg		-- register to load
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr
mkLoadInstr reg delta slot
  = let off     = spillSlotToOffset slot
    in
    let sz = case regClass reg of
                RcInteger -> II32
                RcDouble  -> FF64
		RcFloat	  -> panic "PPC.RegInfo.mkSpillInstr: no match"
    in LD sz reg (AddrRegImm sp (ImmInt (off-delta)))


mkRegRegMoveInstr
    :: Reg
    -> Reg
    -> Instr
mkRegRegMoveInstr src dst
    = MR dst src


mkBranchInstr
    :: BlockId
    -> [Instr]

mkBranchInstr id = [BCC ALWAYS id]



spillSlotSize :: Int
spillSlotSize = 8

maxSpillSlots :: Int
maxSpillSlots = ((rESERVED_C_STACK_BYTES - 64) `div` spillSlotSize) - 1

-- convert a spill slot number to a *byte* offset, with no sign:
-- decide on a per arch basis whether you are spilling above or below
-- the C stack pointer.
spillSlotToOffset :: Int -> Int
spillSlotToOffset slot
   | slot >= 0 && slot < maxSpillSlots
   = 64 + spillSlotSize * slot
   | otherwise
   = pprPanic "spillSlotToOffset:" 
              (   text "invalid spill location: " <> int slot
	      $$  text "maxSpillSlots:          " <> int maxSpillSlots)

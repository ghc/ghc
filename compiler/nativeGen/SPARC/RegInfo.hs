
-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

module SPARC.RegInfo (
	-- machine specific 
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

import SPARC.Instr
import SPARC.Regs
import RegsBase

import BlockId
import Outputable
import Constants	( rESERVED_C_STACK_BYTES )
import FastBool


-- | Represents what regs are read and written to in an instruction.
--	
data RegUsage 
	= RU 	[Reg] 	-- regs read from
		[Reg]	-- regs written to


-- | No regs read or written to.
noUsage :: RegUsage
noUsage  = RU [] []


-- | regUsage returns the sets of src and destination registers used
-- 	by a particular instruction.  Machine registers that are
-- 	pre-allocated to stgRegs are filtered out, because they are
-- 	uninteresting from a register allocation standpoint.  (We wouldn't
-- 	want them to end up on the free list!)  As far as we are concerned,
-- 	the fixed registers simply don't exist (for allocation purposes,
-- 	anyway).

-- 	regUsage doesn't need to do any trickery for jumps and such.  Just
-- 	state precisely the regs read and written by that insn.  The
-- 	consequences of control flow transfers, as far as register
-- 	allocation goes, are taken care of by the register allocator.
--
regUsage :: Instr -> RegUsage
regUsage instr 
 = case instr of
    SPILL  reg _		-> usage ([reg], [])
    RELOAD _   reg		-> usage ([], [reg])

    LD    _ addr reg  		-> usage (regAddr addr, 	[reg])
    ST    _ reg addr  		-> usage (reg : regAddr addr, 	[])
    ADD   _ _ r1 ar r2		-> usage (r1 : regRI ar, 	[r2])
    SUB   _ _ r1 ar r2		-> usage (r1 : regRI ar, 	[r2])
    UMUL    _ r1 ar r2		-> usage (r1 : regRI ar, 	[r2])
    SMUL    _ r1 ar r2		-> usage (r1 : regRI ar, 	[r2])
    UDIV    _ r1 ar r2		-> usage (r1 : regRI ar, 	[r2])
    SDIV    _ r1 ar r2		-> usage (r1 : regRI ar, 	[r2])
    RDY       rd           	-> usage ([], 		 	[rd])
    WRY       r1 r2        	-> usage ([r1, r2], 	 	[])
    AND     _ r1 ar r2  	-> usage (r1 : regRI ar, 	[r2])
    ANDN    _ r1 ar r2 		-> usage (r1 : regRI ar, 	[r2])
    OR      _ r1 ar r2   	-> usage (r1 : regRI ar, 	[r2])
    ORN     _ r1 ar r2  	-> usage (r1 : regRI ar, 	[r2])
    XOR     _ r1 ar r2  	-> usage (r1 : regRI ar, 	[r2])
    XNOR    _ r1 ar r2 		-> usage (r1 : regRI ar, 	[r2])
    SLL       r1 ar r2    	-> usage (r1 : regRI ar, 	[r2])
    SRL       r1 ar r2    	-> usage (r1 : regRI ar,	[r2])
    SRA       r1 ar r2    	-> usage (r1 : regRI ar, 	[r2])
    SETHI   _ reg   		-> usage ([], 			[reg])
    FABS    _ r1 r2    		-> usage ([r1], 		[r2])
    FADD    _ r1 r2 r3 		-> usage ([r1, r2],		[r3])
    FCMP    _ _  r1 r2  	-> usage ([r1, r2], 		[])
    FDIV    _ r1 r2 r3 		-> usage ([r1, r2], 		[r3])
    FMOV    _ r1 r2    		-> usage ([r1],			[r2])
    FMUL    _ r1 r2 r3 		-> usage ([r1, r2],	 	[r3])
    FNEG    _ r1 r2    		-> usage ([r1], 		[r2])
    FSQRT   _ r1 r2   		-> usage ([r1], 		[r2])
    FSUB    _ r1 r2 r3 		-> usage ([r1, r2], 		[r3])
    FxTOy   _ _  r1 r2 		-> usage ([r1], 		[r2])

    JMP     addr 		-> usage (regAddr addr, [])
    JMP_TBL addr _      	-> usage (regAddr addr, [])

    CALL  (Left _  )  _ True  	-> noUsage
    CALL  (Left _  )  n False 	-> usage (argRegs n, callClobberedRegs)
    CALL  (Right reg) _ True  	-> usage ([reg], [])
    CALL  (Right reg) n False 	-> usage (reg : (argRegs n), callClobberedRegs)
    _ 	    	    		-> noUsage

  where
    usage (src, dst) 
     = RU (filter interesting src) (filter interesting dst)

    regAddr (AddrRegReg r1 r2)	= [r1, r2]
    regAddr (AddrRegImm r1 _)	= [r1]

    regRI (RIReg r)		= [r]
    regRI  _			= []


-- | Interesting regs are virtuals, or ones that are allocatable 
--	by the register allocator.
interesting :: Reg -> Bool
interesting reg
 = case reg of
 	VirtualRegI  _	-> True
	VirtualRegHi _	-> True
	VirtualRegF  _	-> True
	VirtualRegD  _	-> True
	RealReg i	-> isFastTrue (freeReg i)



-- | Apply a given mapping to tall the register references in this instruction.

patchRegs :: Instr -> (Reg -> Reg) -> Instr
patchRegs instr env = case instr of
    SPILL reg slot		-> SPILL  (env reg) slot
    RELOAD slot reg		-> RELOAD slot (env reg)

    LD    sz addr reg   	-> LD sz (fixAddr addr) (env reg)
    ST    sz reg addr   	-> ST sz (env reg) (fixAddr addr)

    ADD   x cc r1 ar r2 	-> ADD   x cc  (env r1) (fixRI ar) (env r2)
    SUB   x cc r1 ar r2	 	-> SUB   x cc  (env r1) (fixRI ar) (env r2)
    UMUL    cc r1 ar r2		-> UMUL    cc  (env r1) (fixRI ar) (env r2)
    SMUL    cc r1 ar r2		-> SMUL    cc  (env r1) (fixRI ar) (env r2)
    UDIV    cc r1 ar r2		-> UDIV    cc  (env r1) (fixRI ar) (env r2)
    SDIV    cc r1 ar r2		-> SDIV    cc  (env r1) (fixRI ar) (env r2)
    RDY   rd            	-> RDY         (env rd)
    WRY   r1 r2			-> WRY         (env r1) (env r2)
    AND   b r1 ar r2   	 	-> AND   b     (env r1) (fixRI ar) (env r2)
    ANDN  b r1 ar r2    	-> ANDN  b     (env r1) (fixRI ar) (env r2)
    OR    b r1 ar r2   		-> OR    b     (env r1) (fixRI ar) (env r2)
    ORN   b r1 ar r2    	-> ORN   b     (env r1) (fixRI ar) (env r2)
    XOR   b r1 ar r2    	-> XOR   b     (env r1) (fixRI ar) (env r2)
    XNOR  b r1 ar r2    	-> XNOR  b     (env r1) (fixRI ar) (env r2)
    SLL   r1 ar r2      	-> SLL         (env r1) (fixRI ar) (env r2)
    SRL   r1 ar r2      	-> SRL         (env r1) (fixRI ar) (env r2)
    SRA   r1 ar r2      	-> SRA         (env r1) (fixRI ar) (env r2)

    SETHI imm reg       	-> SETHI imm (env reg)

    FABS  s r1 r2       	-> FABS    s   (env r1) (env r2)
    FADD  s r1 r2 r3    	-> FADD    s   (env r1) (env r2) (env r3)
    FCMP  e s r1 r2     	-> FCMP e  s   (env r1) (env r2)
    FDIV  s r1 r2 r3    	-> FDIV    s   (env r1) (env r2) (env r3)
    FMOV  s r1 r2       	-> FMOV    s   (env r1) (env r2)
    FMUL  s r1 r2 r3    	-> FMUL    s   (env r1) (env r2) (env r3)
    FNEG  s r1 r2       	-> FNEG    s   (env r1) (env r2)
    FSQRT s r1 r2       	-> FSQRT   s   (env r1) (env r2)
    FSUB  s r1 r2 r3   	 	-> FSUB    s   (env r1) (env r2) (env r3)
    FxTOy s1 s2 r1 r2   	-> FxTOy s1 s2 (env r1) (env r2)

    JMP     addr        	-> JMP     (fixAddr addr)
    JMP_TBL addr ids    	-> JMP_TBL (fixAddr addr) ids

    CALL  (Left i) n t  	-> CALL (Left i) n t
    CALL  (Right r) n t 	-> CALL (Right (env r)) n t
    _ 				-> instr

  where
    fixAddr (AddrRegReg r1 r2) 	= AddrRegReg   (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  	= AddrRegImm   (env r1) i

    fixRI (RIReg r) 		= RIReg (env r)
    fixRI other			= other


-- -----------------------------------------------------------------------------
-- Determine the possible destinations from the current instruction.

-- (we always assume that the next instruction is also a valid destination;
-- if this isn't the case then the jump should be at the end of the basic
-- block).

jumpDests :: Instr -> [BlockId] -> [BlockId]
jumpDests insn acc
  = case insn of
	BI   _ _ id	-> id : acc
	BF   _ _ id	-> id : acc
	JMP_TBL _ ids	-> ids ++ acc
	_other		-> acc


-- | Check whether a particular instruction is a jump, branch or call instruction (jumpish)
--	We can't just use jumpDests above because the jump might take its arg,
--	so the instr won't contain a blockid.
--
isJumpish :: Instr -> Bool
isJumpish instr
 = case instr of
	BI{}		-> True
	BF{}		-> True
	JMP{}		-> True
	JMP_TBL{}	-> True
	CALL{}		-> True
	_		-> False


-- | Change the destination of this jump instruction
--	Used in joinToTargets in the linear allocator, when emitting fixup code
--	for join points.
patchJump :: Instr -> BlockId -> BlockId -> Instr
patchJump insn old new
  = case insn of
	BI cc annul id
	 | id == old	-> BI cc annul new
	 
	BF cc annul id
	 | id == old	-> BF cc annul new

	_other		-> insn




data JumpDest = DestBlockId BlockId | DestImm Imm

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other



-- | Make a spill instruction.
-- 	On SPARC we spill below frame pointer leaving 2 words/spill
mkSpillInstr
	:: Reg		-- ^ register to spill
	-> Int		-- ^ current stack delta
	-> Int		-- ^ spill slot to use
	-> Instr

mkSpillInstr reg _ slot
 = let	off     = spillSlotToOffset slot
        off_w	= 1 + (off `div` 4)
        sz 	= case regClass reg of
			RcInteger -> II32
			RcFloat   -> FF32
			RcDouble  -> FF64
		
    in ST sz reg (fpRel (negate off_w))


-- | Make a spill reload instruction.
mkLoadInstr
	:: Reg		-- ^ register to load
	-> Int		-- ^ current stack delta
	-> Int		-- ^ spill slot to use
	-> Instr

mkLoadInstr reg _ slot
  = let off     = spillSlotToOffset slot
	off_w	= 1 + (off `div` 4)
        sz	= case regClass reg of
			RcInteger -> II32
			RcFloat   -> FF32
			RcDouble  -> FF64

        in LD sz (fpRel (- off_w)) reg


-- | Make a reg-reg move instruction.
--	On SPARC v8 there are no instructions to move directly between
--	floating point and integer regs. If we need to do that then we
--	have to go via memory.
--
mkRegRegMoveInstr
	:: Reg
	-> Reg
	-> Instr

mkRegRegMoveInstr src dst
 = case regClass src of
	RcInteger -> ADD  False False src (RIReg g0) dst
	RcDouble  -> FMOV FF64 src dst
	RcFloat   -> FMOV FF32 src dst


-- | Check whether an instruction represents a reg-reg move.
-- 	The register allocator attempts to eliminate reg->reg moves whenever it can,
-- 	by assigning the src and dest temporaries to the same real register.
--
isRegRegMove :: Instr -> Maybe (Reg,Reg)
isRegRegMove instr
 = case instr of
 	ADD False False src (RIReg src2) dst
	 | g0 == src2		-> Just (src, dst)

	FMOV FF64 src dst	-> Just (src, dst)
	FMOV FF32  src dst	-> Just (src, dst)
	_			-> Nothing


-- | Make an unconditional branch instruction.
mkBranchInstr
	:: BlockId
	-> [Instr]

mkBranchInstr id 
 = 	 [BI ALWAYS False id
	, NOP]			-- fill the branch delay slot.


-- | TODO: Why do we need 8 bytes per slot?? -BL 2009/02
spillSlotSize :: Int
spillSlotSize = 8


-- | The maximum number of spill slots available on the C stack.
--	If we use up all of the slots, then we're screwed.
maxSpillSlots :: Int
maxSpillSlots = ((rESERVED_C_STACK_BYTES - 64) `div` spillSlotSize) - 1


-- | Convert a spill slot number to a *byte* offset, with no sign.
--
spillSlotToOffset :: Int -> Int
spillSlotToOffset slot
	| slot >= 0 && slot < maxSpillSlots
	= 64 + spillSlotSize * slot

	| otherwise
	= pprPanic "spillSlotToOffset:" 
	              (   text "invalid spill location: " <> int slot
		      $$  text "maxSpillSlots:          " <> int maxSpillSlots)


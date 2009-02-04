-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

#include "HsVersions.h"
#include "nativeGen/NCG.h"

module PPC.Instr (
	Cond(..),
	condNegate,
	RI(..),
	Instr(..)
)

where

import BlockId
import PPC.Regs
import RegsBase
import Cmm
import Outputable
import FastString
import CLabel

data Cond
	= ALWAYS
	| EQQ
	| GE
	| GEU
	| GTT
	| GU
	| LE
	| LEU
	| LTT
	| LU
	| NE
	deriving Eq


condNegate :: Cond -> Cond
condNegate ALWAYS  = panic "condNegate: ALWAYS"
condNegate EQQ     = NE
condNegate GE      = LTT
condNegate GEU     = LU
condNegate GTT     = LE
condNegate GU      = LEU
condNegate LE      = GTT
condNegate LEU     = GU
condNegate LTT     = GE
condNegate LU      = GEU
condNegate NE      = EQQ


-- -----------------------------------------------------------------------------
-- Machine's assembly language

-- We have a few common "instructions" (nearly all the pseudo-ops) but
-- mostly all of 'Instr' is machine-specific.

-- Register or immediate
data RI 
	= RIReg Reg
	| RIImm Imm

data Instr
	-- comment pseudo-op
	= COMMENT FastString		

	-- some static data spat out during code
	-- generation.  Will be extracted before
	-- pretty-printing.
	| LDATA   Section [CmmStatic]	

	-- start a new basic block.  Useful during
	-- codegen, removed later.  Preceding 
	-- instruction should be a jump, as per the
	-- invariants for a BasicBlock (see Cmm).
	| NEWBLOCK BlockId		

	-- specify current stack offset for
        -- benefit of subsequent passes
	| DELTA   Int

	-- | spill this reg to a stack slot
	| SPILL   Reg Int

	-- | reload this reg from a stack slot
	| RELOAD  Int Reg

	-- Loads and stores.
	| LD	Size Reg AddrMode 	-- Load size, dst, src
	| LA      Size Reg AddrMode 	-- Load arithmetic size, dst, src
	| ST	Size Reg AddrMode 	-- Store size, src, dst 
	| STU	Size Reg AddrMode 	-- Store with Update size, src, dst 
	| LIS	Reg Imm 		-- Load Immediate Shifted dst, src
	| LI	Reg Imm 		-- Load Immediate dst, src
	| MR	Reg Reg 		-- Move Register dst, src -- also for fmr
	      
	| CMP     Size Reg RI 		--- size, src1, src2
	| CMPL    Size Reg RI 		--- size, src1, src2
	      
	| BCC     Cond BlockId
	| BCCFAR  Cond BlockId
	| JMP     CLabel          	-- same as branch,
                                        -- but with CLabel instead of block ID
	| MTCTR	Reg
	| BCTR    [BlockId]       	-- with list of local destinations
	| BL	CLabel [Reg]		-- with list of argument regs
	| BCTRL	[Reg]
	      
	| ADD     Reg Reg RI 		-- dst, src1, src2
	| ADDC    Reg Reg Reg 		-- (carrying) dst, src1, src2
	| ADDE    Reg Reg Reg 		-- (extend) dst, src1, src2
	| ADDIS   Reg Reg Imm 		-- Add Immediate Shifted dst, src1, src2
	| SUBF    Reg Reg Reg 		-- dst, src1, src2 ; dst = src2 - src1  
	| MULLW	Reg Reg RI
	| DIVW	Reg Reg Reg
	| DIVWU	Reg Reg Reg

	| MULLW_MayOflo Reg Reg Reg
                        		-- dst = 1 if src1 * src2 overflows
                        		-- pseudo-instruction; pretty-printed as:
                        		-- mullwo. dst, src1, src2
                        		-- mfxer dst
                        		-- rlwinm dst, dst, 2, 31,31
	      
	| AND	Reg Reg RI 		-- dst, src1, src2
	| OR	Reg Reg RI 		-- dst, src1, src2
	| XOR	Reg Reg RI 		-- dst, src1, src2
	| XORIS	Reg Reg Imm 		-- XOR Immediate Shifted dst, src1, src2
	      
	| EXTS    Size Reg Reg
		  
	| NEG	Reg Reg
	| NOT	Reg Reg
	      
	| SLW	Reg Reg RI		-- shift left word
	| SRW	Reg Reg RI		-- shift right word
	| SRAW	Reg Reg RI		-- shift right arithmetic word
	      
        	        		-- Rotate Left Word Immediate then AND with Mask
	| RLWINM  Reg Reg Int Int Int
	      
	| FADD	Size Reg Reg Reg
	| FSUB	Size Reg Reg Reg
	| FMUL	Size Reg Reg Reg
	| FDIV	Size Reg Reg Reg
	| FNEG	Reg Reg	 		-- negate is the same for single and double prec.
	      
	| FCMP	Reg Reg
	      
	| FCTIWZ	Reg Reg		-- convert to integer word
	| FRSP		Reg Reg		-- reduce to single precision
					-- (but destination is a FP register)
	      
	| CRNOR   Int Int Int    	-- condition register nor
	| MFCR    Reg            	-- move from condition register
	      
	| MFLR    Reg            	-- move from link register
	| FETCHPC Reg          	  	-- pseudo-instruction:
	                         	-- bcl to next insn, mflr reg
	      
	| LWSYNC -- memory barrier

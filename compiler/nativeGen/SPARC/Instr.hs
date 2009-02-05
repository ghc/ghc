-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

#include "HsVersions.h"
#include "nativeGen/NCG.h"

module SPARC.Instr (
	Cond(..),
	RI(..),
	Instr(..),
	riZero,
	fpRelEA,
	moveSp,
	fPair,
)

where

import BlockId
import RegsBase
import SPARC.Regs
import Cmm
import Outputable
import Constants	( wORD_SIZE )
import FastString

import GHC.Exts


-- | Branch condition codes.
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
	| NEG
	| NEVER
	| POS
	| VC
	| VS
	deriving Eq


-- | Register or immediate
data RI 
	= RIReg Reg
	| RIImm Imm


-- | SPARC isntruction set.
data Instr

	-- meta ops --------------------------------------------------
	-- comment pseudo-op
	= COMMENT FastString		

	-- some static data spat out during code generation.
	-- Will be extracted before pretty-printing.
	| LDATA   Section [CmmStatic]	

	-- Start a new basic block.  Useful during codegen, removed later.
	-- Preceding instruction should be a jump, as per the invariants
	-- for a BasicBlock (see Cmm).
	| NEWBLOCK BlockId		

	-- specify current stack offset for benefit of subsequent passes.
	| DELTA   Int

	-- | spill this reg to a stack slot
	| SPILL   Reg Int

	-- | reload this reg from a stack slot
	| RELOAD  Int Reg

	-- real instrs -----------------------------------------------
	-- Loads and stores.
	| LD		Size AddrMode Reg 		-- size, src, dst
	| ST		Size Reg AddrMode 		-- size, src, dst

	-- Int Arithmetic.
	| ADD		Bool Bool Reg RI Reg 		-- x?, cc?, src1, src2, dst
	| SUB		Bool Bool Reg RI Reg 		-- x?, cc?, src1, src2, dst

	| UMUL		Bool Reg RI Reg 		--     cc?, src1, src2, dst
	| SMUL	        Bool Reg RI Reg 		--     cc?, src1, src2, dst


	-- The SPARC divide instructions perform 64bit by 32bit division
	--   The Y register is xored into the first operand.

	--   On _some implementations_ the Y register is overwritten by
	--   the remainder, so we have to make sure it is 0 each time.

	--   dst <- ((Y `shiftL` 32) `or` src1) `div` src2
	| UDIV          Bool Reg RI Reg 		--     cc?, src1, src2, dst
	| SDIV          Bool Reg RI Reg 		--     cc?, src1, src2, dst

 	| RDY           Reg                  		-- move contents of Y register to reg
	| WRY           Reg  Reg             		-- Y <- src1 `xor` src2
	
	-- Logic operations.
	| AND		Bool Reg RI Reg 		-- cc?, src1, src2, dst
	| ANDN		Bool Reg RI Reg 		-- cc?, src1, src2, dst
	| OR		Bool Reg RI Reg 		-- cc?, src1, src2, dst
	| ORN		Bool Reg RI Reg 		-- cc?, src1, src2, dst
	| XOR		Bool Reg RI Reg 		-- cc?, src1, src2, dst
	| XNOR		Bool Reg RI Reg 		-- cc?, src1, src2, dst
	| SLL		Reg RI Reg 			-- src1, src2, dst
	| SRL		Reg RI Reg 			-- src1, src2, dst
	| SRA		Reg RI Reg 			-- src1, src2, dst

	-- Load immediates.
	| SETHI		Imm Reg 			-- src, dst

	-- Do nothing.
	-- Implemented by the assembler as SETHI 0, %g0, but worth an alias
	| NOP						

	-- Float Arithmetic.
	-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single
	-- instructions right up until we spit them out.
	--
	| FABS		Size Reg Reg	   		-- src dst
	| FADD		Size Reg Reg Reg  		-- src1, src2, dst
	| FCMP		Bool Size Reg Reg 		-- exception?, src1, src2, dst
	| FDIV		Size Reg Reg Reg 		-- src1, src2, dst
	| FMOV		Size Reg Reg     		-- src, dst
	| FMUL		Size Reg Reg Reg 		-- src1, src2, dst
 	| FNEG		Size Reg Reg     		-- src, dst
	| FSQRT		Size Reg Reg     		-- src, dst
	| FSUB		Size Reg Reg Reg 		-- src1, src2, dst
	| FxTOy		Size Size Reg Reg 		-- src, dst

	-- Jumping around.
	| BI		Cond Bool BlockId 		-- cond, annul?, target
	| BF		Cond Bool BlockId 		-- cond, annul?, target

	| JMP		AddrMode     			-- target

	-- With a tabled jump we know all the possible destinations.
	-- We also need this info so we can work out what regs are live across the jump.
	-- 
	| JMP_TBL	AddrMode [BlockId]

	| CALL		(Either Imm Reg) Int Bool 	-- target, args, terminal


-- | Check if a RI represents a zero value.
--  	- a literal zero
--	- register %g0, which is always zero.
--
riZero :: RI -> Bool
riZero (RIImm (ImmInt 0))	    = True
riZero (RIImm (ImmInteger 0))	    = True
riZero (RIReg (RealReg 0))          = True
riZero _			    = False


-- | Calculate the effective address which would be used by the
-- 	corresponding fpRel sequence.  fpRel is in MachRegs.lhs,
-- 	alas -- can't have fpRelEA here because of module dependencies.
fpRelEA :: Int -> Reg -> Instr
fpRelEA n dst
   = ADD False False fp (RIImm (ImmInt (n * wORD_SIZE))) dst


-- | Code to shift the stack pointer by n words.
moveSp :: Int -> Instr
moveSp n
   = ADD False False sp (RIImm (ImmInt (n * wORD_SIZE))) sp


-- | Produce the second-half-of-a-double register given the first half.
fPair :: Reg -> Maybe Reg
fPair (RealReg n) 
	| n >= 32 && n `mod` 2 == 0  = Just (RealReg (n+1))

fPair (VirtualRegD u)
	= Just (VirtualRegHi u)

fPair _
	= trace ("MachInstrs.fPair: can't get high half of supposed double reg ") 
		Nothing

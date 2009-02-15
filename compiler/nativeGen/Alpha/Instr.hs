-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

#include "HsVersions.h"
#include "nativeGen/NCG.h"

module Alpha.Instr (
--	Cond(..),
--	Instr(..),
--	RI(..)
)

where

{-
import BlockId
import Regs
import Cmm
import FastString
import CLabel

data Cond
	= ALWAYS	-- For BI (same as BR)
	| EQQ		-- For CMP and BI (NB: "EQ" is a 1.3 Prelude name)
	| GE		-- For BI only
	| GTT		-- For BI only (NB: "GT" is a 1.3 Prelude name)
	| LE		-- For CMP and BI
	| LTT		-- For CMP and BI (NB: "LT" is a 1.3 Prelude name)
	| NE		-- For BI only
	| NEVER		-- For BI (null instruction)
	| ULE		-- For CMP only
	| ULT		-- For CMP only
	deriving Eq
	

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
	| LD	      Size Reg AddrMode		-- size, dst, src
	| LDA	      Reg AddrMode		-- dst, src
	| LDAH	      Reg AddrMode		-- dst, src
	| LDGP	      Reg AddrMode		-- dst, src
	| LDI	      Size Reg Imm     		-- size, dst, src
	| ST	      Size Reg AddrMode 	-- size, src, dst

	-- Int Arithmetic.
	| CLR	      Reg		    	-- dst
	| ABS	      Size RI Reg	    	-- size, src, dst
	| NEG	      Size Bool RI Reg	   	-- size, overflow, src, dst
	| ADD	      Size Bool Reg RI Reg 	-- size, overflow, src, src, dst
	| SADD	      Size Size Reg RI Reg 	-- size, scale, src, src, dst
	| SUB	      Size Bool Reg RI Reg 	-- size, overflow, src, src, dst
	| SSUB	      Size Size Reg RI Reg 	-- size, scale, src, src, dst
	| MUL	      Size Bool Reg RI Reg 	-- size, overflow, src, src, dst
	| DIV	      Size Bool Reg RI Reg 	-- size, unsigned, src, src, dst
	| REM	      Size Bool Reg RI Reg 	-- size, unsigned, src, src, dst

	-- Simple bit-twiddling.
	| NOT	      RI Reg
	| AND	      Reg RI Reg
	| ANDNOT      Reg RI Reg
	| OR	      Reg RI Reg
	| ORNOT	      Reg RI Reg
	| XOR	      Reg RI Reg
	| XORNOT      Reg RI Reg
	| SLL	      Reg RI Reg
	| SRL	      Reg RI Reg
	| SRA	      Reg RI Reg

	| ZAP	      Reg RI Reg
	| ZAPNOT      Reg RI Reg

	| NOP

	-- Comparison
	| CMP	      Cond Reg RI Reg

	-- Float Arithmetic.
	| FCLR	      Reg
	| FABS	      Reg Reg
	| FNEG	      Size Reg Reg
	| FADD	      Size Reg Reg Reg
	| FDIV	      Size Reg Reg Reg
	| FMUL	      Size Reg Reg Reg
	| FSUB	      Size Reg Reg Reg
	| CVTxy	      Size Size Reg Reg
	| FCMP	      Size Cond Reg Reg Reg
	| FMOV	      Reg Reg

	-- Jumping around.
	| BI	      Cond Reg Imm
	| BF	      Cond Reg Imm
	| BR	      Imm
	| JMP	      Reg AddrMode Int
	| BSR	      Imm Int
	| JSR	      Reg AddrMode Int

	-- Alpha-specific pseudo-ops.
	| FUNBEGIN CLabel
	| FUNEND CLabel


-}

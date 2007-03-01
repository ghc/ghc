-----------------------------------------------------------------------------
--
-- Cmm data types
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module Cmm ( 
	GenCmm(..), Cmm,
	GenCmmTop(..), CmmTop,
	GenBasicBlock(..), CmmBasicBlock, blockId, blockStmts,
	CmmStmt(..),  
	CmmCallTarget(..),
	CmmStatic(..), Section(..),
	CmmExpr(..), cmmExprRep, 
	CmmReg(..), cmmRegRep,
	CmmLit(..), cmmLitRep,
	LocalReg(..), localRegRep,
	BlockId(..),
	GlobalReg(..), globalRegRep,

	node, nodeReg, spReg, hpReg,
  ) where

#include "HsVersions.h"

import MachOp
import CLabel
import ForeignCall
import Unique
import FastString

import Data.Word

-----------------------------------------------------------------------------
--		Cmm, CmmTop, CmmBasicBlock
-----------------------------------------------------------------------------

-- A file is a list of top-level chunks.  These may be arbitrarily
-- re-orderd during code generation.

-- GenCmm is abstracted over
--   (a) the type of static data elements
--   (b) the contents of a basic block.
-- We expect there to be two main instances of this type:
--   (a) Plain C--, i.e. populated with CmmLit and CmmExpr respectively,
--   (b) Native code, populated with instructions
--
newtype GenCmm d i = Cmm [GenCmmTop d i]

type Cmm = GenCmm CmmStatic CmmStmt

-- A top-level chunk, abstracted over the type of the contents of
-- the basic blocks (Cmm or instructions are the likely instantiations).
data GenCmmTop d i
  = CmmProc
     [d]	       -- Info table, may be empty
     CLabel            -- Used to generate both info & entry labels
     [LocalReg]        -- Argument locals live on entry (C-- procedure params)
     [GenBasicBlock i] -- Code, may be empty.  The first block is
                       -- the entry point.  The order is otherwise initially 
                       -- unimportant, but at some point the code gen will
                       -- fix the order.

		       -- the BlockId of the first block does not give rise
		       -- to a label.  To jump to the first block in a Proc,
		       -- use the appropriate CLabel.

  -- some static data.
  | CmmData Section [d]	-- constant values only

type CmmTop = GenCmmTop CmmStatic CmmStmt

-- A basic block containing a single label, at the beginning.
-- The list of basic blocks in a top-level code block may be re-ordered.
-- Fall-through is not allowed: there must be an explicit jump at the
-- end of each basic block, but the code generator might rearrange basic
-- blocks in order to turn some jumps into fallthroughs.

data GenBasicBlock i = BasicBlock BlockId [i]
  -- ToDo: Julian suggests that we might need to annotate this type
  -- with the out & in edges in the graph, i.e. two * [BlockId].  This
  -- information can be derived from the contents, but it might be
  -- helpful to cache it here.

type CmmBasicBlock = GenBasicBlock CmmStmt

blockId :: GenBasicBlock i -> BlockId
-- The branch block id is that of the first block in 
-- the branch, which is that branch's entry point
blockId (BasicBlock blk_id _ ) = blk_id

blockStmts :: GenBasicBlock i -> [i]
blockStmts (BasicBlock _ stmts) = stmts


-----------------------------------------------------------------------------
--		CmmStmt
-- A "statement".  Note that all branches are explicit: there are no
-- control transfers to computed addresses, except when transfering
-- control to a new function.
-----------------------------------------------------------------------------

data CmmStmt
  = CmmNop
  | CmmComment FastString

  | CmmAssign CmmReg CmmExpr	 -- Assign to register

  | CmmStore CmmExpr CmmExpr     -- Assign to memory location.  Size is
                                 -- given by cmmExprRep of the rhs.

  | CmmCall	 		 -- A foreign call, with 
     CmmCallTarget
     [(CmmReg,MachHint)]	 -- zero or more results
     [(CmmExpr,MachHint)]	 -- zero or more arguments
     (Maybe [GlobalReg])	 -- Global regs that may need to be saved
				 -- if they will be clobbered by the call.
				 -- Nothing <=> save *all* globals that
				 -- might be clobbered.

  | CmmBranch BlockId             -- branch to another BB in this fn

  | CmmCondBranch CmmExpr BlockId -- conditional branch

  | CmmSwitch CmmExpr [Maybe BlockId]   -- Table branch
	-- The scrutinee is zero-based; 
	--	zero -> first block
	--	one  -> second block etc
	-- Undefined outside range, and when there's a Nothing

  | CmmJump CmmExpr [LocalReg]    -- Jump to another function, with these 
				  -- parameters.

{-
Discussion
~~~~~~~~~~

One possible problem with the above type is that the only way to do a
non-local conditional jump is to encode it as a branch to a block that
contains a single jump.  This leads to inefficient code in the back end.

One possible way to fix this would be:

data CmmStat = 
  ...
  | CmmJump CmmBranchDest
  | CmmCondJump CmmExpr CmmBranchDest
  ...

data CmmBranchDest
  = Local BlockId
  | NonLocal CmmExpr [LocalReg]

In favour:

+ one fewer constructors in CmmStmt
+ allows both cond branch and switch to jump to non-local destinations

Against:

- not strictly necessary: can already encode as branch+jump
- not always possible to implement any better in the back end
- could do the optimisation in the back end (but then plat-specific?)
- C-- doesn't have it
- back-end optimisation might be more general (jump shortcutting)

So we'll stick with the way it is, and add the optimisation to the NCG.
-}

-----------------------------------------------------------------------------
--		CmmCallTarget
--
-- The target of a CmmCall.
-----------------------------------------------------------------------------

data CmmCallTarget
  = CmmForeignCall		-- Call to a foreign function
	CmmExpr 		-- literal label <=> static call
				-- other expression <=> dynamic call
	CCallConv		-- The calling convention

  | CmmPrim			-- Call to a "primitive" (eg. sin, cos)
	CallishMachOp		-- These might be implemented as inline
				-- code by the backend.

-----------------------------------------------------------------------------
--		CmmExpr
-- An expression.  Expressions have no side effects.
-----------------------------------------------------------------------------

data CmmExpr
  = CmmLit CmmLit               -- Literal
  | CmmLoad CmmExpr MachRep     -- Read memory location
  | CmmReg CmmReg		-- Contents of register
  | CmmMachOp MachOp [CmmExpr]  -- Machine operation (+, -, *, etc.)
  | CmmRegOff CmmReg Int	
	-- CmmRegOff reg i
	--        ** is shorthand only, meaning **
	-- CmmMachOp (MO_S_Add rep (CmmReg reg) (CmmLit (CmmInt i rep)))
	--	where rep = cmmRegRep reg
  deriving Eq

cmmExprRep :: CmmExpr -> MachRep
cmmExprRep (CmmLit lit)      = cmmLitRep lit
cmmExprRep (CmmLoad _ rep)   = rep
cmmExprRep (CmmReg reg)      = cmmRegRep reg
cmmExprRep (CmmMachOp op _)  = resultRepOfMachOp op
cmmExprRep (CmmRegOff reg _) = cmmRegRep reg

data CmmReg 
  = CmmLocal  LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq )

cmmRegRep :: CmmReg -> MachRep
cmmRegRep (CmmLocal  reg) 	= localRegRep reg
cmmRegRep (CmmGlobal reg)	= globalRegRep reg

data LocalReg
  = LocalReg !Unique MachRep

instance Eq LocalReg where
  (LocalReg u1 _) == (LocalReg u2 _) = u1 == u2

instance Uniquable LocalReg where
  getUnique (LocalReg uniq _) = uniq

localRegRep :: LocalReg -> MachRep
localRegRep (LocalReg _ rep) = rep

data CmmLit
  = CmmInt Integer  MachRep
	-- Interpretation: the 2's complement representation of the value
	-- is truncated to the specified size.  This is easier than trying
	-- to keep the value within range, because we don't know whether
	-- it will be used as a signed or unsigned value (the MachRep doesn't
	-- distinguish between signed & unsigned).
  | CmmFloat  Rational MachRep
  | CmmLabel    CLabel			-- Address of label
  | CmmLabelOff CLabel Int		-- Address of label + byte offset
  
        -- Due to limitations in the C backend, the following
        -- MUST ONLY be used inside the info table indicated by label2
        -- (label2 must be the info label), and label1 must be an
        -- SRT, a slow entrypoint or a large bitmap (see the Mangler)
        -- Don't use it at all unless tablesNextToCode.
        -- It is also used inside the NCG during when generating
        -- position-independent code. 
  | CmmLabelDiffOff CLabel CLabel Int   -- label1 - label2 + offset
  deriving Eq

cmmLitRep :: CmmLit -> MachRep
cmmLitRep (CmmInt _ rep)    = rep
cmmLitRep (CmmFloat _ rep)  = rep
cmmLitRep (CmmLabel _)      = wordRep
cmmLitRep (CmmLabelOff _ _) = wordRep
cmmLitRep (CmmLabelDiffOff _ _ _) = wordRep

-----------------------------------------------------------------------------
-- A local label.

-- Local labels must be unique within a single compilation unit.

newtype BlockId = BlockId Unique
  deriving (Eq,Ord)

instance Uniquable BlockId where
  getUnique (BlockId u) = u

-----------------------------------------------------------------------------
--		Static Data
-----------------------------------------------------------------------------

data Section
  = Text
  | Data
  | ReadOnlyData
  | RelocatableReadOnlyData
  | UninitialisedData
  | ReadOnlyData16	-- .rodata.cst16 on x86_64, 16-byte aligned
  | OtherSection String

data CmmStatic
  = CmmStaticLit CmmLit	
	-- a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
	-- uninitialised data, N bytes long
  | CmmAlign Int
	-- align to next N-byte boundary (N must be a power of 2).
  | CmmDataLabel CLabel
	-- label the current position in this section.
  | CmmString [Word8]
	-- string of 8-bit values only, not zero terminated.

-----------------------------------------------------------------------------
--		Global STG registers
-----------------------------------------------------------------------------

data GlobalReg
  -- Argument and return registers
  = VanillaReg			-- pointers, unboxed ints and chars
	{-# UNPACK #-} !Int	-- its number

  | FloatReg		-- single-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | DoubleReg		-- double-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | LongReg	        -- long int registers (64-bit, really)
	{-# UNPACK #-} !Int	-- its number

  -- STG registers
  | Sp			-- Stack ptr; points to last occupied stack location.
  | SpLim		-- Stack limit
  | Hp			-- Heap ptr; points to last occupied heap location.
  | HpLim		-- Heap limit register
  | CurrentTSO		-- pointer to current thread's TSO
  | CurrentNursery	-- pointer to allocation area
  | HpAlloc		-- allocation count for heap check failure

		-- We keep the address of some commonly-called 
		-- functions in the register table, to keep code
		-- size down:
  | GCEnter1		-- stg_gc_enter_1
  | GCFun		-- stg_gc_fun

  -- Base offset for the register table, used for accessing registers
  -- which do not have real registers assigned to them.  This register
  -- will only appear after we have expanded GlobalReg into memory accesses
  -- (where necessary) in the native code generator.
  | BaseReg

  -- Base Register for PIC (position-independent code) calculations
  -- Only used inside the native code generator. It's exact meaning differs
  -- from platform to platform (see module PositionIndependentCode).
  | PicBaseReg

  deriving( Eq
#ifdef DEBUG
	, Show
#endif
	 )

-- convenient aliases
spReg, hpReg, nodeReg :: CmmReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
nodeReg = CmmGlobal node

node :: GlobalReg
node = VanillaReg 1

globalRegRep :: GlobalReg -> MachRep
globalRegRep (VanillaReg _) 	= wordRep
globalRegRep (FloatReg _) 	= F32
globalRegRep (DoubleReg _) 	= F64
globalRegRep (LongReg _) 	= I64
globalRegRep _			= wordRep

-----------------------------------------------------------------------------
--
-- Generating machine code (instruction selection)
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module SPARC.CodeGen ( 
	cmmTopCodeGen, 
	generateJumpTableForInstr,
	InstrBlock 
) 

where

#include "HsVersions.h"
#include "nativeGen/NCG.h"
#include "../includes/MachDeps.h"

-- NCG stuff:
import SPARC.CodeGen.Sanity
import SPARC.CodeGen.Amode
import SPARC.CodeGen.CondCode
import SPARC.CodeGen.Gen64
import SPARC.CodeGen.Gen32
import SPARC.CodeGen.CCall
import SPARC.CodeGen.Base
import SPARC.Ppr	()
import SPARC.Instr
import SPARC.Imm
import SPARC.AddrMode
import SPARC.Regs
import Instruction
import Size
import NCGMonad

-- Our intermediate code:
import BlockId
import OldCmm
import CLabel

-- The rest:
import DynFlags
import StaticFlags	( opt_PIC )
import OrdList
import Outputable
import Platform
import Unique

import Control.Monad	( mapAndUnzipM )

-- | Top level code generation
cmmTopCodeGen :: RawCmmDecl
              -> NatM [NatCmmDecl CmmStatics Instr]

cmmTopCodeGen (CmmProc info lab (ListGraph blocks))
 = do
      dflags <- getDynFlagsNat
      let platform = targetPlatform dflags
      (nat_blocks,statics) <- mapAndUnzipM (basicBlockCodeGen platform) blocks

      let proc = CmmProc info lab (ListGraph $ concat nat_blocks)
      let tops = proc : concat statics

      return tops

cmmTopCodeGen (CmmData sec dat) = do
  return [CmmData sec dat]  -- no translation, we just use CmmStatic


-- | Do code generation on a single block of CMM code.
--	code generation may introduce new basic block boundaries, which
-- 	are indicated by the NEWBLOCK instruction.  We must split up the
-- 	instruction stream into basic blocks again.  Also, we extract
-- 	LDATAs here too.
basicBlockCodeGen :: Platform
                  -> CmmBasicBlock
                  -> NatM ( [NatBasicBlock Instr]
                          , [NatCmmDecl CmmStatics Instr])

basicBlockCodeGen platform cmm@(BasicBlock id stmts) = do
  instrs <- stmtsToInstrs stmts
  let
	(top,other_blocks,statics) 
		= foldrOL mkBlocks ([],[],[]) instrs
	
	mkBlocks (NEWBLOCK id) (instrs,blocks,statics) 
	  = ([], BasicBlock id instrs : blocks, statics)

	mkBlocks (LDATA sec dat) (instrs,blocks,statics) 
	  = (instrs, blocks, CmmData sec dat:statics)

	mkBlocks instr (instrs,blocks,statics)
	  = (instr:instrs, blocks, statics)

	-- do intra-block sanity checking
	blocksChecked
	  	= map (checkBlock platform cmm)
	  	$ BasicBlock id top : other_blocks

  return (blocksChecked, statics)


-- | Convert some Cmm statements to SPARC instructions.
stmtsToInstrs :: [CmmStmt] -> NatM InstrBlock
stmtsToInstrs stmts
   = do instrss <- mapM stmtToInstrs stmts
        return (concatOL instrss)


stmtToInstrs :: CmmStmt -> NatM InstrBlock
stmtToInstrs stmt = case stmt of
    CmmNop	   -> return nilOL
    CmmComment s   -> return (unitOL (COMMENT s))

    CmmAssign reg src
      | isFloatType ty	-> assignReg_FltCode size reg src
      | isWord64 ty	-> assignReg_I64Code      reg src
      | otherwise	-> assignReg_IntCode size reg src
	where ty = cmmRegType reg
	      size = cmmTypeSize ty

    CmmStore addr src
      | isFloatType ty	-> assignMem_FltCode size addr src
      | isWord64 ty	-> assignMem_I64Code      addr src
      | otherwise	-> assignMem_IntCode size addr src
	where ty = cmmExprType src
	      size = cmmTypeSize ty

    CmmCall target result_regs args _
       -> genCCall target result_regs args

    CmmBranch	id		-> genBranch id
    CmmCondBranch arg id	-> genCondJump id arg
    CmmSwitch	arg ids		-> genSwitch arg ids
    CmmJump	arg _		-> genJump arg

    CmmReturn	_		
     -> panic "stmtToInstrs: return statement should have been cps'd away"


{-
Now, given a tree (the argument to an CmmLoad) that references memory,
produce a suitable addressing mode.

A Rule of the Game (tm) for Amodes: use of the addr bit must
immediately follow use of the code part, since the code part puts
values in registers which the addr then refers to.  So you can't put
anything in between, lest it overwrite some of those registers.  If
you need to do some other computation between the code part and use of
the addr bit, first store the effective address from the amode in a
temporary, then do the other computation, and then use the temporary:

    code
    LEA amode, tmp
    ... other computation ...
    ... (tmp) ...
-}



-- | Convert a BlockId to some CmmStatic data
jumpTableEntry :: Maybe BlockId -> CmmStatic
jumpTableEntry Nothing = CmmStaticLit (CmmInt 0 wordWidth)
jumpTableEntry (Just blockid) = CmmStaticLit (CmmLabel blockLabel)
    where blockLabel = mkAsmTempLabel (getUnique blockid)



-- -----------------------------------------------------------------------------
-- Generating assignments

-- Assignments are really at the heart of the whole code generation
-- business.  Almost all top-level nodes of any real importance are
-- assignments, which correspond to loads, stores, or register
-- transfers.  If we're really lucky, some of the register transfers
-- will go away, because we can use the destination register to
-- complete the code generation for the right hand side.  This only
-- fails when the right hand side is forced into a fixed register
-- (e.g. the result of a call).

assignMem_IntCode :: Size -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignMem_IntCode pk addr src = do
    (srcReg, code) <- getSomeReg src
    Amode dstAddr addr_code <- getAmode addr
    return $ code `appOL` addr_code `snocOL` ST pk srcReg dstAddr


assignReg_IntCode :: Size -> CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_IntCode _ reg src = do
    r <- getRegister src
    return $ case r of
	Any _ code         -> code dst
	Fixed _ freg fcode -> fcode `snocOL` OR False g0 (RIReg freg) dst
    where
      dst = getRegisterReg reg



-- Floating point assignment to memory
assignMem_FltCode :: Size -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignMem_FltCode pk addr src = do
    Amode dst__2 code1 <- getAmode addr
    (src__2, code2) <- getSomeReg src
    tmp1 <- getNewRegNat pk
    let
    	pk__2   = cmmExprType src
    	code__2 = code1 `appOL` code2 `appOL`
	    if   sizeToWidth pk == typeWidth pk__2 
            then unitOL (ST pk src__2 dst__2)
	    else toOL 	[ FxTOy (cmmTypeSize pk__2) pk src__2 tmp1
	    		, ST    pk tmp1 dst__2]
    return code__2

-- Floating point assignment to a register/temporary
assignReg_FltCode :: Size -> CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_FltCode pk dstCmmReg srcCmmExpr = do
    srcRegister <- getRegister srcCmmExpr
    let dstReg	= getRegisterReg dstCmmReg

    return $ case srcRegister of
        Any _ code         	    -> code dstReg
	Fixed _ srcFixedReg srcCode -> srcCode `snocOL` FMOV pk srcFixedReg dstReg




genJump :: CmmExpr{-the branch target-} -> NatM InstrBlock

genJump (CmmLit (CmmLabel lbl))
  = return (toOL [CALL (Left target) 0 True, NOP])
  where
    target = ImmCLbl lbl

genJump tree
  = do
        (target, code) <- getSomeReg tree
	return (code `snocOL` JMP (AddrRegReg target g0)  `snocOL` NOP)

-- -----------------------------------------------------------------------------
--  Unconditional branches

genBranch :: BlockId -> NatM InstrBlock
genBranch = return . toOL . mkJumpInstr


-- -----------------------------------------------------------------------------
--  Conditional jumps

{-
Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

SPARC: First, we have to ensure that the condition codes are set
according to the supplied comparison operation.  We generate slightly
different code for floating point comparisons, because a floating
point operation cannot directly precede a @BF@.  We assume the worst
and fill that slot with a @NOP@.

SPARC: Do not fill the delay slots here; you will confuse the register
allocator.
-}


genCondJump
    :: BlockId	    -- the branch target
    -> CmmExpr      -- the condition on which to branch
    -> NatM InstrBlock



genCondJump bid bool = do
  CondCode is_float cond code <- getCondCode bool
  return (
       code `appOL` 
       toOL (
         if   is_float
         then [NOP, BF cond False bid, NOP]
         else [BI cond False bid, NOP]
       )
    )



-- -----------------------------------------------------------------------------
-- Generating a table-branch

genSwitch :: CmmExpr -> [Maybe BlockId] -> NatM InstrBlock
genSwitch expr ids
	| opt_PIC
	= error "MachCodeGen: sparc genSwitch PIC not finished\n"
  
	| otherwise
	= do	(e_reg, e_code) <- getSomeReg expr

		base_reg	<- getNewRegNat II32
		offset_reg	<- getNewRegNat II32
		dst		<- getNewRegNat II32

		label 		<- getNewLabelNat

		return $ e_code `appOL`
		 toOL	
			[ -- load base of jump table
			  SETHI (HI (ImmCLbl label)) base_reg
			, OR    False base_reg (RIImm $ LO $ ImmCLbl label) base_reg
			
			-- the addrs in the table are 32 bits wide..
			, SLL   e_reg (RIImm $ ImmInt 2) offset_reg

			-- load and jump to the destination
			, LD 	  II32 (AddrRegReg base_reg offset_reg) dst
			, JMP_TBL (AddrRegImm dst (ImmInt 0)) ids label
			, NOP ]

generateJumpTableForInstr :: Instr -> Maybe (NatCmmDecl CmmStatics Instr)
generateJumpTableForInstr (JMP_TBL _ ids label) =
	let jumpTable = map jumpTableEntry ids
	in Just (CmmData ReadOnlyData (Statics label jumpTable))
generateJumpTableForInstr _ = Nothing

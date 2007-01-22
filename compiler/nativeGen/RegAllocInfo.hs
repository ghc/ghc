-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

#include "nativeGen/NCG.h"

module RegAllocInfo (
	RegUsage(..),
	noUsage,
	regUsage,
	patchRegs,
	jumpDests,
	patchJump,
	isRegRegMove,

        JumpDest, canShortcut, shortcutJump, shortcutStatic,

	maxSpillSlots,
	mkSpillInstr,
	mkLoadInstr,
	mkRegRegMoveInstr,
	mkBranchInstr
    ) where

#include "HsVersions.h"

import Cmm
import CLabel
import MachOp           ( MachRep(..), wordRep )
import MachInstrs
import MachRegs
import Outputable
import Constants	( rESERVED_C_STACK_BYTES )
import FastTypes

-- -----------------------------------------------------------------------------
-- RegUsage type

-- @regUsage@ returns the sets of src and destination registers used
-- by a particular instruction.  Machine registers that are
-- pre-allocated to stgRegs are filtered out, because they are
-- uninteresting from a register allocation standpoint.  (We wouldn't
-- want them to end up on the free list!)  As far as we are concerned,
-- the fixed registers simply don't exist (for allocation purposes,
-- anyway).

-- regUsage doesn't need to do any trickery for jumps and such.  Just
-- state precisely the regs read and written by that insn.  The
-- consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.

data RegUsage = RU [Reg] [Reg]

noUsage :: RegUsage
noUsage  = RU [] []

regUsage :: Instr -> RegUsage

interesting (VirtualRegI  _)  = True
interesting (VirtualRegHi _)  = True
interesting (VirtualRegF  _)  = True
interesting (VirtualRegD  _)  = True
interesting (RealReg i)       = isFastTrue (freeReg i)


#if alpha_TARGET_ARCH
regUsage instr = case instr of
    LD B reg addr	-> usage (regAddr addr, [reg, t9])
    LD Bu reg addr	-> usage (regAddr addr, [reg, t9])
--  LD W reg addr	-> usage (regAddr addr, [reg, t9]) : UNUSED
--  LD Wu reg addr	-> usage (regAddr addr, [reg, t9]) : UNUSED
    LD sz reg addr	-> usage (regAddr addr, [reg])
    LDA reg addr	-> usage (regAddr addr, [reg])
    LDAH reg addr	-> usage (regAddr addr, [reg])
    LDGP reg addr	-> usage (regAddr addr, [reg])
    LDI sz reg imm	-> usage ([], [reg])
    ST B reg addr	-> usage (reg : regAddr addr, [t9, t10])
--  ST W reg addr	-> usage (reg : regAddr addr, [t9, t10]) : UNUSED
    ST sz reg addr	-> usage (reg : regAddr addr, [])
    CLR reg		-> usage ([], [reg])
    ABS sz ri reg	-> usage (regRI ri, [reg])
    NEG sz ov ri reg	-> usage (regRI ri, [reg])
    ADD sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SADD sz sc r1 ar r2 -> usage (r1 : regRI ar, [r2])
    SUB sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SSUB sz sc r1 ar r2 -> usage (r1 : regRI ar, [r2])
    MUL sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    DIV sz un r1 ar r2	-> usage (r1 : regRI ar, [r2, t9, t10, t11, t12])
    REM sz un r1 ar r2	-> usage (r1 : regRI ar, [r2, t9, t10, t11, t12])
    NOT ri reg		-> usage (regRI ri, [reg])
    AND r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ANDNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    OR r1 ar r2		-> usage (r1 : regRI ar, [r2])
    ORNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    XOR r1 ar r2	-> usage (r1 : regRI ar, [r2])
    XORNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SLL r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SRL r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SRA r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ZAP r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ZAPNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    CMP co r1 ar r2	-> usage (r1 : regRI ar, [r2])
    FCLR reg		-> usage ([], [reg])
    FABS r1 r2		-> usage ([r1], [r2])
    FNEG sz r1 r2	-> usage ([r1], [r2])
    FADD sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FDIV sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FMUL sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FSUB sz r1 r2 r3	-> usage ([r1, r2], [r3])
    CVTxy sz1 sz2 r1 r2 -> usage ([r1], [r2])
    FCMP sz co r1 r2 r3 -> usage ([r1, r2], [r3])
    FMOV r1 r2		-> usage ([r1], [r2])


    -- We assume that all local jumps will be BI/BF/BR.	 JMP must be out-of-line.
    BI cond reg lbl	-> usage ([reg], [])
    BF cond reg lbl	-> usage ([reg], [])
    JMP reg addr hint	-> RU (mkRegSet (filter interesting (regAddr addr))) freeRegSet

    BSR _ n		-> RU (argRegSet n) callClobberedRegSet
    JSR reg addr n	-> RU (argRegSet n) callClobberedRegSet

    _			-> noUsage

  where
    usage (src, dst) = RU (mkRegSet (filter interesting src))
			  (mkRegSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

    regAddr (AddrReg r1)      = [r1]
    regAddr (AddrRegImm r1 _) = [r1]
    regAddr (AddrImm _)	      = []

    regRI (RIReg r) = [r]
    regRI  _	= []

#endif /* alpha_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH

regUsage instr = case instr of
    MOV    sz src dst	-> usageRW src dst
    MOVZxL sz src dst	-> usageRW src dst
    MOVSxL sz src dst	-> usageRW src dst
    LEA    sz src dst	-> usageRW src dst
    ADD    sz src dst	-> usageRM src dst
    ADC    sz src dst	-> usageRM src dst
    SUB    sz src dst	-> usageRM src dst
    IMUL   sz src dst	-> usageRM src dst
    IMUL2  sz src       -> mkRU (eax:use_R src) [eax,edx]
    MUL    sz src dst	-> usageRM src dst
    DIV    sz op	-> mkRU (eax:edx:use_R op) [eax,edx]
    IDIV   sz op	-> mkRU (eax:edx:use_R op) [eax,edx]
    AND    sz src dst	-> usageRM src dst
    OR     sz src dst	-> usageRM src dst
    XOR    sz (OpReg src) (OpReg dst)
        | src == dst    -> mkRU [] [dst]
    XOR    sz src dst	-> usageRM src dst
    NOT    sz op	-> usageM op
    NEGI   sz op	-> usageM op
    SHL    sz imm dst	-> usageRM imm dst
    SAR    sz imm dst	-> usageRM imm dst
    SHR    sz imm dst	-> usageRM imm dst
    BT     sz imm src	-> mkRUR (use_R src)

    PUSH   sz op	-> mkRUR (use_R op)
    POP    sz op	-> mkRU [] (def_W op)
    TEST   sz src dst	-> mkRUR (use_R src ++ use_R dst)
    CMP    sz src dst	-> mkRUR (use_R src ++ use_R dst)
    SETCC  cond op	-> mkRU [] (def_W op)
    JXX    cond lbl	-> mkRU [] []
    JXX_GBL cond lbl	-> mkRU [] []
    JMP    op		-> mkRUR (use_R op)
    JMP_TBL op ids      -> mkRUR (use_R op)
    CALL (Left imm)  params -> mkRU params callClobberedRegs
    CALL (Right reg) params -> mkRU (reg:params) callClobberedRegs
    CLTD   sz		-> mkRU [eax] [edx]
    NOP			-> mkRU [] []

#if i386_TARGET_ARCH
    GMOV   src dst	-> mkRU [src] [dst]
    GLD    sz src dst	-> mkRU (use_EA src) [dst]
    GST    sz src dst	-> mkRUR (src : use_EA dst)

    GLDZ   dst		-> mkRU [] [dst]
    GLD1   dst		-> mkRU [] [dst]

    GFTOI  src dst	-> mkRU [src] [dst]
    GDTOI  src dst	-> mkRU [src] [dst]

    GITOF  src dst	-> mkRU [src] [dst]
    GITOD  src dst	-> mkRU [src] [dst]

    GADD   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GSUB   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GMUL   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GDIV   sz s1 s2 dst	-> mkRU [s1,s2] [dst]

    GCMP   sz src1 src2	-> mkRUR [src1,src2]
    GABS   sz src dst	-> mkRU [src] [dst]
    GNEG   sz src dst	-> mkRU [src] [dst]
    GSQRT  sz src dst	-> mkRU [src] [dst]
    GSIN   sz src dst	-> mkRU [src] [dst]
    GCOS   sz src dst	-> mkRU [src] [dst]
    GTAN   sz src dst	-> mkRU [src] [dst]
#endif

#if x86_64_TARGET_ARCH
    CVTSS2SD src dst	-> mkRU [src] [dst]
    CVTSD2SS src dst	-> mkRU [src] [dst]
    CVTSS2SI src dst	-> mkRU (use_R src) [dst]
    CVTSD2SI src dst	-> mkRU (use_R src) [dst]
    CVTSI2SS src dst	-> mkRU (use_R src) [dst]
    CVTSI2SD src dst	-> mkRU (use_R src) [dst]
    FDIV sz src dst     -> usageRM src dst
#endif    

    FETCHGOT reg        -> mkRU [] [reg]
    FETCHPC  reg        -> mkRU [] [reg]

    COMMENT _		-> noUsage
    DELTA   _           -> noUsage

    _other		-> panic "regUsage: unrecognised instr"

 where
    -- 2 operand form; first operand Read; second Written
    usageRW :: Operand -> Operand -> RegUsage
    usageRW op (OpReg reg) = mkRU (use_R op) [reg]
    usageRW op (OpAddr ea) = mkRUR (use_R op ++ use_EA ea)

    -- 2 operand form; first operand Read; second Modified
    usageRM :: Operand -> Operand -> RegUsage
    usageRM op (OpReg reg) = mkRU (use_R op ++ [reg]) [reg]
    usageRM op (OpAddr ea) = mkRUR (use_R op ++ use_EA ea)

    -- 1 operand form; operand Modified
    usageM :: Operand -> RegUsage
    usageM (OpReg reg)    = mkRU [reg] [reg]
    usageM (OpAddr ea)    = mkRUR (use_EA ea)

    -- Registers defd when an operand is written.
    def_W (OpReg reg)  = [reg]
    def_W (OpAddr ea)  = []

    -- Registers used when an operand is read.
    use_R (OpReg reg)  = [reg]
    use_R (OpImm imm)  = []
    use_R (OpAddr ea)  = use_EA ea

    -- Registers used to compute an effective address.
    use_EA (ImmAddr _ _) = []
    use_EA (AddrBaseIndex base index _) = 
	use_base base $! use_index index
	where use_base (EABaseReg r) x = r : x
	      use_base _ x             = x
	      use_index EAIndexNone   = []
	      use_index (EAIndex i _) = [i]

    mkRUR src = src' `seq` RU src' []
	where src' = filter interesting src

    mkRU src dst = src' `seq` dst' `seq` RU src' dst'
	where src' = filter interesting src
	      dst' = filter interesting dst

#endif /* i386_TARGET_ARCH || x86_64_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

regUsage instr = case instr of
    LD    sz addr reg  	-> usage (regAddr addr, [reg])
    ST    sz reg addr  	-> usage (reg : regAddr addr, [])
    ADD   x cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SUB   x cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    UMUL    cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SMUL    cc r1 ar r2	-> usage (r1 : regRI ar, [r2])
    RDY   rd            -> usage ([], [rd])
    AND   b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    ANDN  b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    OR    b r1 ar r2   	-> usage (r1 : regRI ar, [r2])
    ORN   b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XOR   b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XNOR  b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    SLL   r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRL   r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRA   r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SETHI imm reg   	-> usage ([], [reg])
    FABS  s r1 r2    	-> usage ([r1], [r2])
    FADD  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FCMP  e s r1 r2  	-> usage ([r1, r2], [])
    FDIV  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FMOV  s r1 r2    	-> usage ([r1], [r2])
    FMUL  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FNEG  s r1 r2    	-> usage ([r1], [r2])
    FSQRT s r1 r2   	-> usage ([r1], [r2])
    FSUB  s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FxTOy s1 s2 r1 r2 	-> usage ([r1], [r2])

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.
    JMP   addr 	        -> usage (regAddr addr, [])

    CALL  (Left imm)  n True  -> noUsage
    CALL  (Left imm)  n False -> usage (argRegs n, callClobberedRegs)
    CALL  (Right reg) n True  -> usage ([reg], [])
    CALL  (Right reg) n False -> usage (reg : (argRegs n), callClobberedRegs)

    _ 	    	    	-> noUsage
  where
    usage (src, dst) = RU (filter interesting src)
    	    	    	 (filter interesting dst)

    regAddr (AddrRegReg r1 r2) = [r1, r2]
    regAddr (AddrRegImm r1 _)  = [r1]

    regRI (RIReg r) = [r]
    regRI  _	= []

#endif /* sparc_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if powerpc_TARGET_ARCH

regUsage instr = case instr of
    LD    sz reg addr  	-> usage (regAddr addr, [reg])
    LA    sz reg addr  	-> usage (regAddr addr, [reg])
    ST    sz reg addr  	-> usage (reg : regAddr addr, [])
    STU    sz reg addr  -> usage (reg : regAddr addr, [])
    LIS   reg imm	-> usage ([], [reg])
    LI    reg imm	-> usage ([], [reg])
    MR	  reg1 reg2     -> usage ([reg2], [reg1])
    CMP   sz reg ri	-> usage (reg : regRI ri,[])
    CMPL  sz reg ri	-> usage (reg : regRI ri,[])
    BCC	  cond lbl	-> noUsage
    BCCFAR cond lbl	-> noUsage
    MTCTR reg		-> usage ([reg],[])
    BCTR  targets	-> noUsage
    BL    imm params	-> usage (params, callClobberedRegs)
    BCTRL params	-> usage (params, callClobberedRegs)
    ADD	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    ADDC  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    ADDE  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    ADDIS reg1 reg2 imm -> usage ([reg2], [reg1])
    SUBF  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    MULLW reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    DIVW  reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    DIVWU reg1 reg2 reg3-> usage ([reg2,reg3], [reg1])
    MULLW_MayOflo reg1 reg2 reg3        
                        -> usage ([reg2,reg3], [reg1])
    AND	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    OR	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    XOR	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    XORIS reg1 reg2 imm -> usage ([reg2], [reg1])
    EXTS  siz reg1 reg2 -> usage ([reg2], [reg1])
    NEG	  reg1 reg2	-> usage ([reg2], [reg1])
    NOT	  reg1 reg2	-> usage ([reg2], [reg1])
    SLW	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SRW	  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SRAW  reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    RLWINM reg1 reg2 sh mb me
                        -> usage ([reg2], [reg1])
    FADD  sz r1 r2 r3   -> usage ([r2,r3], [r1])
    FSUB  sz r1 r2 r3   -> usage ([r2,r3], [r1])
    FMUL  sz r1 r2 r3   -> usage ([r2,r3], [r1])
    FDIV  sz r1 r2 r3   -> usage ([r2,r3], [r1])
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
#endif /* powerpc_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- Determine the possible destinations from the current instruction.

-- (we always assume that the next instruction is also a valid destination;
-- if this isn't the case then the jump should be at the end of the basic
-- block).

jumpDests :: Instr -> [BlockId] -> [BlockId]
jumpDests insn acc
  = case insn of
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	JXX _ id	-> id : acc
	JMP_TBL _ ids	-> ids ++ acc
#elif powerpc_TARGET_ARCH
        BCC _ id        -> id : acc
        BCCFAR _ id     -> id : acc
        BCTR targets    -> targets ++ acc
#endif
	_other		-> acc

patchJump :: Instr -> BlockId -> BlockId -> Instr

patchJump insn old new
  = case insn of
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	JXX cc id | id == old -> JXX cc new
	JMP_TBL op ids -> error "Cannot patch JMP_TBL"
#elif powerpc_TARGET_ARCH
        BCC cc id | id == old -> BCC cc new
        BCCFAR cc id | id == old -> BCCFAR cc new
        BCTR targets -> error "Cannot patch BCTR"
#endif
	_other		-> insn

data JumpDest = DestBlockId BlockId | DestImm Imm

canShortcut :: Instr -> Maybe JumpDest
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
canShortcut (JXX ALWAYS id) = Just (DestBlockId id)
canShortcut (JMP (OpImm imm)) = Just (DestImm imm)
#endif
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
shortcutJump fn insn@(JXX cc id) = 
  case fn id of
    Nothing                -> insn
    Just (DestBlockId id') -> shortcutJump fn (JXX cc id')
    Just (DestImm imm)     -> shortcutJump fn (JXX_GBL cc imm)
#endif
shortcutJump fn other = other

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
shortcutStatic fn other_static
        = other_static

shortBlockId fn blockid@(BlockId uq) =
   case fn blockid of
      Nothing -> mkAsmTempLabel uq
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
      Just (DestImm (ImmCLbl lbl)) -> lbl
      _other -> panic "shortBlockId"

-- -----------------------------------------------------------------------------
-- 'patchRegs' function

-- 'patchRegs' takes an instruction and applies the given mapping to
-- all the register references.

patchRegs :: Instr -> (Reg -> Reg) -> Instr

#if alpha_TARGET_ARCH

patchRegs instr env = case instr of
    LD sz reg addr -> LD sz (env reg) (fixAddr addr)
    LDA reg addr -> LDA (env reg) (fixAddr addr)
    LDAH reg addr -> LDAH (env reg) (fixAddr addr)
    LDGP reg addr -> LDGP (env reg) (fixAddr addr)
    LDI sz reg imm -> LDI sz (env reg) imm
    ST sz reg addr -> ST sz (env reg) (fixAddr addr)
    CLR reg -> CLR (env reg)
    ABS sz ar reg -> ABS sz (fixRI ar) (env reg)
    NEG sz ov ar reg -> NEG sz ov (fixRI ar) (env reg)
    ADD sz ov r1 ar r2 -> ADD sz ov (env r1) (fixRI ar) (env r2)
    SADD sz sc r1 ar r2 -> SADD sz sc (env r1) (fixRI ar) (env r2)
    SUB sz ov r1 ar r2 -> SUB sz ov (env r1) (fixRI ar) (env r2)
    SSUB sz sc r1 ar r2 -> SSUB sz sc (env r1) (fixRI ar) (env r2)
    MUL sz ov r1 ar r2 -> MUL sz ov (env r1) (fixRI ar) (env r2)
    DIV sz un r1 ar r2 -> DIV sz un (env r1) (fixRI ar) (env r2)
    REM sz un r1 ar r2 -> REM sz un (env r1) (fixRI ar) (env r2)
    NOT ar reg -> NOT (fixRI ar) (env reg)
    AND r1 ar r2 -> AND (env r1) (fixRI ar) (env r2)
    ANDNOT r1 ar r2 -> ANDNOT (env r1) (fixRI ar) (env r2)
    OR r1 ar r2 -> OR (env r1) (fixRI ar) (env r2)
    ORNOT r1 ar r2 -> ORNOT (env r1) (fixRI ar) (env r2)
    XOR r1 ar r2 -> XOR (env r1) (fixRI ar) (env r2)
    XORNOT r1 ar r2 -> XORNOT (env r1) (fixRI ar) (env r2)
    SLL r1 ar r2 -> SLL (env r1) (fixRI ar) (env r2)
    SRL r1 ar r2 -> SRL (env r1) (fixRI ar) (env r2)
    SRA r1 ar r2 -> SRA (env r1) (fixRI ar) (env r2)
    ZAP r1 ar r2 -> ZAP (env r1) (fixRI ar) (env r2)
    ZAPNOT r1 ar r2 -> ZAPNOT (env r1) (fixRI ar) (env r2)
    CMP co r1 ar r2 -> CMP co (env r1) (fixRI ar) (env r2)
    FCLR reg -> FCLR (env reg)
    FABS r1 r2 -> FABS (env r1) (env r2)
    FNEG s r1 r2 -> FNEG s (env r1) (env r2)
    FADD s r1 r2 r3 -> FADD s (env r1) (env r2) (env r3)
    FDIV s r1 r2 r3 -> FDIV s (env r1) (env r2) (env r3)
    FMUL s r1 r2 r3 -> FMUL s (env r1) (env r2) (env r3)
    FSUB s r1 r2 r3 -> FSUB s (env r1) (env r2) (env r3)
    CVTxy s1 s2 r1 r2 -> CVTxy s1 s2 (env r1) (env r2)
    FCMP s co r1 r2 r3 -> FCMP s co (env r1) (env r2) (env r3)
    FMOV r1 r2 -> FMOV (env r1) (env r2)
    BI cond reg lbl -> BI cond (env reg) lbl
    BF cond reg lbl -> BF cond (env reg) lbl
    JMP reg addr hint -> JMP (env reg) (fixAddr addr) hint
    JSR reg addr i -> JSR (env reg) (fixAddr addr) i
    _ -> instr
  where
    fixAddr (AddrReg r1)       = AddrReg (env r1)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i
    fixAddr other	       = other

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other

#endif /* alpha_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH

patchRegs instr env = case instr of
    MOV  sz src dst	-> patch2 (MOV  sz) src dst
    MOVZxL sz src dst	-> patch2 (MOVZxL sz) src dst
    MOVSxL sz src dst	-> patch2 (MOVSxL sz) src dst
    LEA  sz src dst	-> patch2 (LEA  sz) src dst
    ADD  sz src dst	-> patch2 (ADD  sz) src dst
    ADC  sz src dst	-> patch2 (ADC  sz) src dst
    SUB  sz src dst	-> patch2 (SUB  sz) src dst
    IMUL sz src dst 	-> patch2 (IMUL sz) src dst
    IMUL2 sz src        -> patch1 (IMUL2 sz) src
    MUL sz src dst 	-> patch2 (MUL sz) src dst
    IDIV sz op		-> patch1 (IDIV sz) op
    DIV sz op		-> patch1 (DIV sz) op
    AND  sz src dst	-> patch2 (AND  sz) src dst
    OR   sz src dst	-> patch2 (OR   sz) src dst
    XOR  sz src dst	-> patch2 (XOR  sz) src dst
    NOT  sz op 		-> patch1 (NOT  sz) op
    NEGI sz op		-> patch1 (NEGI sz) op
    SHL  sz imm dst 	-> patch1 (SHL sz imm) dst
    SAR  sz imm dst 	-> patch1 (SAR sz imm) dst
    SHR  sz imm dst 	-> patch1 (SHR sz imm) dst
    BT   sz imm src     -> patch1 (BT  sz imm) src
    TEST sz src dst	-> patch2 (TEST sz) src dst
    CMP  sz src dst	-> patch2 (CMP  sz) src dst
    PUSH sz op		-> patch1 (PUSH sz) op
    POP  sz op		-> patch1 (POP  sz) op
    SETCC cond op	-> patch1 (SETCC cond) op
    JMP op		-> patch1 JMP op
    JMP_TBL op ids      -> patch1 JMP_TBL op $ ids

#if i386_TARGET_ARCH
    GMOV src dst	-> GMOV (env src) (env dst)
    GLD sz src dst	-> GLD sz (lookupAddr src) (env dst)
    GST sz src dst	-> GST sz (env src) (lookupAddr dst)

    GLDZ dst		-> GLDZ (env dst)
    GLD1 dst		-> GLD1 (env dst)

    GFTOI src dst	-> GFTOI (env src) (env dst)
    GDTOI src dst	-> GDTOI (env src) (env dst)

    GITOF src dst	-> GITOF (env src) (env dst)
    GITOD src dst	-> GITOD (env src) (env dst)

    GADD sz s1 s2 dst	-> GADD sz (env s1) (env s2) (env dst)
    GSUB sz s1 s2 dst	-> GSUB sz (env s1) (env s2) (env dst)
    GMUL sz s1 s2 dst	-> GMUL sz (env s1) (env s2) (env dst)
    GDIV sz s1 s2 dst	-> GDIV sz (env s1) (env s2) (env dst)

    GCMP sz src1 src2	-> GCMP sz (env src1) (env src2)
    GABS sz src dst	-> GABS sz (env src) (env dst)
    GNEG sz src dst	-> GNEG sz (env src) (env dst)
    GSQRT sz src dst	-> GSQRT sz (env src) (env dst)
    GSIN sz src dst	-> GSIN sz (env src) (env dst)
    GCOS sz src dst	-> GCOS sz (env src) (env dst)
    GTAN sz src dst	-> GTAN sz (env src) (env dst)
#endif

#if x86_64_TARGET_ARCH
    CVTSS2SD src dst	-> CVTSS2SD (env src) (env dst)
    CVTSD2SS src dst	-> CVTSD2SS (env src) (env dst)
    CVTSS2SI src dst	-> CVTSS2SI (patchOp src) (env dst)
    CVTSD2SI src dst	-> CVTSD2SI (patchOp src) (env dst)
    CVTSI2SS src dst	-> CVTSI2SS (patchOp src) (env dst)
    CVTSI2SD src dst	-> CVTSI2SD (patchOp src) (env dst)
    FDIV sz src dst	-> FDIV sz (patchOp src) (patchOp dst)
#endif    

    CALL (Left imm)  _	-> instr
    CALL (Right reg) p	-> CALL (Right (env reg)) p
    
    FETCHGOT reg        -> FETCHGOT (env reg)
    FETCHPC  reg        -> FETCHPC  (env reg)
   
    NOP			-> instr
    COMMENT _		-> instr
    DELTA _ 		-> instr
    JXX _ _		-> instr
    JXX_GBL _ _		-> instr
    CLTD _		-> instr

    _other		-> panic "patchRegs: unrecognised instr"

  where
    patch1 insn op      = insn $! patchOp op
    patch2 insn src dst = (insn $! patchOp src) $! patchOp dst

    patchOp (OpReg  reg) = OpReg $! env reg
    patchOp (OpImm  imm) = OpImm imm
    patchOp (OpAddr ea)  = OpAddr $! lookupAddr ea

    lookupAddr (ImmAddr imm off) = ImmAddr imm off
    lookupAddr (AddrBaseIndex base index disp)
      = ((AddrBaseIndex $! lookupBase base) $! lookupIndex index) disp
      where
	lookupBase EABaseNone       = EABaseNone
	lookupBase EABaseRip        = EABaseRip
	lookupBase (EABaseReg r)    = EABaseReg (env r)
				 
	lookupIndex EAIndexNone     = EAIndexNone
	lookupIndex (EAIndex r i)   = EAIndex (env r) i

#endif /* i386_TARGET_ARCH || x86_64_TARGET_ARCH*/
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

patchRegs instr env = case instr of
    LD    sz addr reg   -> LD sz (fixAddr addr) (env reg)
    ST    sz reg addr   -> ST sz (env reg) (fixAddr addr)
    ADD   x cc r1 ar r2 -> ADD x cc (env r1) (fixRI ar) (env r2)
    SUB   x cc r1 ar r2 -> SUB x cc (env r1) (fixRI ar) (env r2)
    UMUL    cc r1 ar r2	-> UMUL cc (env r1) (fixRI ar) (env r2)
    SMUL    cc r1 ar r2	-> SMUL cc (env r1) (fixRI ar) (env r2)
    RDY   rd            -> RDY (env rd)
    AND   b r1 ar r2    -> AND b (env r1) (fixRI ar) (env r2)
    ANDN  b r1 ar r2    -> ANDN b (env r1) (fixRI ar) (env r2)
    OR    b r1 ar r2    -> OR b (env r1) (fixRI ar) (env r2)
    ORN   b r1 ar r2    -> ORN b (env r1) (fixRI ar) (env r2)
    XOR   b r1 ar r2    -> XOR b (env r1) (fixRI ar) (env r2)
    XNOR  b r1 ar r2    -> XNOR b (env r1) (fixRI ar) (env r2)
    SLL   r1 ar r2      -> SLL (env r1) (fixRI ar) (env r2)
    SRL   r1 ar r2      -> SRL (env r1) (fixRI ar) (env r2)
    SRA   r1 ar r2      -> SRA (env r1) (fixRI ar) (env r2)
    SETHI imm reg       -> SETHI imm (env reg)
    FABS  s r1 r2       -> FABS s (env r1) (env r2)
    FADD  s r1 r2 r3    -> FADD s (env r1) (env r2) (env r3)
    FCMP  e s r1 r2     -> FCMP e s (env r1) (env r2)
    FDIV  s r1 r2 r3    -> FDIV s (env r1) (env r2) (env r3)
    FMOV  s r1 r2       -> FMOV s (env r1) (env r2)
    FMUL  s r1 r2 r3    -> FMUL s (env r1) (env r2) (env r3)
    FNEG  s r1 r2       -> FNEG s (env r1) (env r2)
    FSQRT s r1 r2       -> FSQRT s (env r1) (env r2)
    FSUB  s r1 r2 r3    -> FSUB s (env r1) (env r2) (env r3)
    FxTOy s1 s2 r1 r2   -> FxTOy s1 s2 (env r1) (env r2)
    JMP   addr          -> JMP (fixAddr addr)
    CALL  (Left i) n t  -> CALL (Left i) n t
    CALL  (Right r) n t -> CALL (Right (env r)) n t
    _ -> instr
  where
    fixAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other

#endif /* sparc_TARGET_ARCH */
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if powerpc_TARGET_ARCH

patchRegs instr env = case instr of
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
#endif /* powerpc_TARGET_ARCH */

-- -----------------------------------------------------------------------------
-- Detecting reg->reg moves

-- The register allocator attempts to eliminate reg->reg moves whenever it can,
-- by assigning the src and dest temporaries to the same real register.

isRegRegMove :: Instr -> Maybe (Reg,Reg)
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
-- TMP:
isRegRegMove (MOV _ (OpReg r1) (OpReg r2)) = Just (r1,r2)
#elif powerpc_TARGET_ARCH
isRegRegMove (MR dst src) = Just (src,dst)
#else
#warning ToDo: isRegRegMove
#endif
isRegRegMove _ = Nothing

-- -----------------------------------------------------------------------------
-- Generating spill instructions

mkSpillInstr
   :: Reg		-- register to spill (should be a real)
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr
mkSpillInstr reg delta slot
  = ASSERT(isRealReg reg)
    let	
        off     = spillSlotToOffset slot
    in
#ifdef alpha_TARGET_ARCH
    {-Alpha: spill below the stack pointer (?)-}
    ST sz dyn (spRel (- (off `div` 8)))
#endif
#ifdef i386_TARGET_ARCH
    let off_w = (off-delta) `div` 4
    in case regClass reg of
	   RcInteger -> MOV I32 (OpReg reg) (OpAddr (spRel off_w))
	   _         -> GST F80 reg (spRel off_w) {- RcFloat/RcDouble -}
#endif
#ifdef x86_64_TARGET_ARCH
    let off_w = (off-delta) `div` 8
    in case regClass reg of
	   RcInteger -> MOV I64 (OpReg reg) (OpAddr (spRel off_w))
	   RcDouble  -> MOV F64 (OpReg reg) (OpAddr (spRel off_w))
		-- ToDo: will it work to always spill as a double?
		-- does that cause a stall if the data was a float?
#endif
#ifdef sparc_TARGET_ARCH
	{-SPARC: spill below frame pointer leaving 2 words/spill-}
                        let{off_w = 1 + (off `div` 4);
                            sz = case regClass reg of {
                                    RcInteger -> I32;
				    RcFloat   -> F32;
                                    RcDouble  -> F64}}
                        in ST sz reg (fpRel (- off_w))
#endif
#ifdef powerpc_TARGET_ARCH
    let sz = case regClass reg of
                RcInteger -> I32
                RcDouble -> F64
    in ST sz reg (AddrRegImm sp (ImmInt (off-delta)))
#endif


mkLoadInstr
   :: Reg		-- register to load (should be a real)
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr
mkLoadInstr reg delta slot
  = ASSERT(isRealReg reg)
    let
        off     = spillSlotToOffset slot
    in
#if alpha_TARGET_ARCH
	 LD  sz dyn (spRel (- (off `div` 8)))
#endif
#if i386_TARGET_ARCH
	let off_w = (off-delta) `div` 4
        in case regClass reg of {
              RcInteger -> MOV I32 (OpAddr (spRel off_w)) (OpReg reg);
              _         -> GLD F80 (spRel off_w) reg} {- RcFloat/RcDouble -}
#endif
#if x86_64_TARGET_ARCH
	let off_w = (off-delta) `div` 8
        in case regClass reg of
              RcInteger -> MOV I64 (OpAddr (spRel off_w)) (OpReg reg)
              _         -> MOV F64 (OpAddr (spRel off_w)) (OpReg reg)
#endif
#if sparc_TARGET_ARCH
        let{off_w = 1 + (off `div` 4);
            sz = case regClass reg of {
                   RcInteger -> I32;
		   RcFloat   -> F32;
                   RcDouble  -> F64}}
        in LD sz (fpRel (- off_w)) reg
#endif
#if powerpc_TARGET_ARCH
    let sz = case regClass reg of
                RcInteger -> I32
                RcDouble -> F64
    in LD sz reg (AddrRegImm sp (ImmInt (off-delta)))
#endif

mkRegRegMoveInstr
    :: Reg
    -> Reg
    -> Instr
mkRegRegMoveInstr src dst
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
    = case regClass src of
        RcInteger -> MOV wordRep (OpReg src) (OpReg dst)
#if i386_TARGET_ARCH
        RcDouble  -> GMOV src dst
#else
        RcDouble  -> MOV F64 (OpReg src) (OpReg dst)
#endif
#elif powerpc_TARGET_ARCH
    = MR dst src
#endif

mkBranchInstr
    :: BlockId
    -> [Instr]
#if alpha_TARGET_ARCH
mkBranchInstr id = [BR id]
#endif

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
mkBranchInstr id = [JXX ALWAYS id]
#endif

#if sparc_TARGET_ARCH
mkBranchInstr (BlockId id) = [BI ALWAYS False (ImmCLbl (mkAsmTempLabel id)), NOP]
#endif

#if powerpc_TARGET_ARCH
mkBranchInstr id = [BCC ALWAYS id]
#endif


spillSlotSize :: Int
spillSlotSize = IF_ARCH_i386(12, 8)

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
              (text "invalid spill location: " <> int slot)


-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

module Alpha.RegInfo (
{-
	RegUsage(..),
	noUsage,
	regUsage,
	patchRegs,
	jumpDests,
	isJumpish,
	patchJump,
	isRegRegMove,

        JumpDest, canShortcut, shortcutJump, shortcutStatic,

	maxSpillSlots,
	mkSpillInstr,
	mkLoadInstr,
	mkRegRegMoveInstr,
	mkBranchInstr
-}
)

where

{-
#include "nativeGen/NCG.h"
#include "HsVersions.h"


import BlockId
import Cmm
import CLabel
import Instrs
import Regs
import Outputable
import Constants	( rESERVED_C_STACK_BYTES )
import FastBool

data RegUsage = RU [Reg] [Reg]

noUsage :: RegUsage
noUsage  = RU [] []

regUsage :: Instr -> RegUsage

regUsage instr = case instr of
    SPILL  reg slot	-> usage ([reg], [])
    RELOAD slot reg	-> usage ([], [reg])
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


patchRegs :: Instr -> (Reg -> Reg) -> Instr
patchRegs instr env = case instr of
    SPILL  reg slot	-> SPILL (env reg) slot
    RELOAD slot reg	-> RELOAD slot (env reg)
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


mkSpillInstr
   :: Reg		-- register to spill
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr

mkSpillInstr reg delta slot
  = let	off     = spillSlotToOffset slot
    in
    -- Alpha: spill below the stack pointer (?)
    ST sz dyn (spRel (- (off `div` 8)))


mkLoadInstr
   :: Reg		-- register to load
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr
mkLoadInstr reg delta slot
  = let off     = spillSlotToOffset slot
    in
	 LD  sz dyn (spRel (- (off `div` 8)))


mkBranchInstr
    :: BlockId
    -> [Instr]

mkBranchInstr id = [BR id]

-}





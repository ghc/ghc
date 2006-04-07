-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

-- We start with the @pprXXX@s with some cross-platform commonality
-- (e.g., 'pprReg'); we conclude with the no-commonality monster,
-- 'pprInstr'.

#include "nativeGen/NCG.h"

module PprMach ( 
	pprNatCmmTop, pprBasicBlock,
	pprInstr, pprSize, pprUserReg,
  ) where


#include "HsVersions.h"

import Cmm
import MachOp		( MachRep(..), wordRep, isFloatingRep )
import MachRegs		-- may differ per-platform
import MachInstrs

import CLabel		( CLabel, pprCLabel, externallyVisibleCLabel,
			  labelDynamic, mkAsmTempLabel, entryLblToInfoLbl )
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
import CLabel       ( mkDeadStripPreventer )
#endif

import Panic		( panic )
import Unique		( pprUnique )
import Pretty
import FastString
import qualified Outputable

import StaticFlags      ( opt_PIC, opt_Static )

#if __GLASGOW_HASKELL__ >= 504
import Data.Array.ST
import Data.Word	( Word8 )
#else
import MutableArray
#endif

import MONAD_ST
import Char		( chr, ord )
import Maybe            ( isJust )

#if powerpc_TARGET_ARCH || darwin_TARGET_OS
import DATA_WORD(Word32)
import DATA_BITS
#endif

-- -----------------------------------------------------------------------------
-- Printing this stuff out

asmSDoc d = Outputable.withPprStyleDoc (
	      Outputable.mkCodeStyle Outputable.AsmStyle) d
pprCLabel_asm l = asmSDoc (pprCLabel l)

pprNatCmmTop :: NatCmmTop -> Doc
pprNatCmmTop (CmmData section dats) = 
  pprSectionHeader section $$ vcat (map pprData dats)

 -- special case for split markers:
pprNatCmmTop (CmmProc [] lbl _ []) = pprLabel lbl

pprNatCmmTop (CmmProc info lbl params blocks) = 
  pprSectionHeader Text $$
  (if not (null info)
	then
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
            pprCLabel_asm (mkDeadStripPreventer $ entryLblToInfoLbl lbl)
                <> char ':' $$
#endif
            vcat (map pprData info) $$
            pprLabel (entryLblToInfoLbl lbl)
	else empty) $$
  (case blocks of
	[] -> empty
	(BasicBlock _ instrs : rest) -> 
		(if null info then pprLabel lbl else empty) $$
		-- the first block doesn't get a label:
		vcat (map pprInstr instrs) $$
		vcat (map pprBasicBlock rest)
  )
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
        -- If we are using the .subsections_via_symbols directive
        -- (available on recent versions of Darwin),
        -- we have to make sure that there is some kind of reference
        -- from the entry code to a label on the _top_ of of the info table,
        -- so that the linker will not think it is unreferenced and dead-strip
        -- it. That's why the label is called a DeadStripPreventer (_dsp).
  $$ if not (null info)
		    then text "\t.long "
		      <+> pprCLabel_asm (entryLblToInfoLbl lbl)
		      <+> char '-'
		      <+> pprCLabel_asm (mkDeadStripPreventer $ entryLblToInfoLbl lbl)
		    else empty
#endif


pprBasicBlock :: NatBasicBlock -> Doc
pprBasicBlock (BasicBlock (BlockId id) instrs) =
  pprLabel (mkAsmTempLabel id) $$
  vcat (map pprInstr instrs)

-- -----------------------------------------------------------------------------
-- pprReg: print a 'Reg'

-- For x86, the way we print a register name depends
-- on which bit of it we care about.  Yurgh.

pprUserReg :: Reg -> Doc
pprUserReg = pprReg IF_ARCH_i386(I32,) IF_ARCH_x86_64(I64,)

pprReg :: IF_ARCH_i386(MachRep ->,) IF_ARCH_x86_64(MachRep ->,) Reg -> Doc

pprReg IF_ARCH_i386(s,) IF_ARCH_x86_64(s,) r
  = case r of
      RealReg i      -> ppr_reg_no IF_ARCH_i386(s,) IF_ARCH_x86_64(s,) i
      VirtualRegI  u  -> text "%vI_" <> asmSDoc (pprUnique u)
      VirtualRegHi u  -> text "%vHi_" <> asmSDoc (pprUnique u)
      VirtualRegF  u  -> text "%vF_" <> asmSDoc (pprUnique u)
      VirtualRegD  u  -> text "%vD_" <> asmSDoc (pprUnique u)
  where
#if alpha_TARGET_ARCH
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> SLIT("$0");    1 -> SLIT("$1");
	 2 -> SLIT("$2");    3 -> SLIT("$3");
	 4 -> SLIT("$4");    5 -> SLIT("$5");
	 6 -> SLIT("$6");    7 -> SLIT("$7");
	 8 -> SLIT("$8");    9 -> SLIT("$9");
	10 -> SLIT("$10");  11 -> SLIT("$11");
	12 -> SLIT("$12");  13 -> SLIT("$13");
	14 -> SLIT("$14");  15 -> SLIT("$15");
	16 -> SLIT("$16");  17 -> SLIT("$17");
	18 -> SLIT("$18");  19 -> SLIT("$19");
	20 -> SLIT("$20");  21 -> SLIT("$21");
	22 -> SLIT("$22");  23 -> SLIT("$23");
	24 -> SLIT("$24");  25 -> SLIT("$25");
	26 -> SLIT("$26");  27 -> SLIT("$27");
	28 -> SLIT("$28");  29 -> SLIT("$29");
	30 -> SLIT("$30");  31 -> SLIT("$31");
	32 -> SLIT("$f0");  33 -> SLIT("$f1");
	34 -> SLIT("$f2");  35 -> SLIT("$f3");
	36 -> SLIT("$f4");  37 -> SLIT("$f5");
	38 -> SLIT("$f6");  39 -> SLIT("$f7");
	40 -> SLIT("$f8");  41 -> SLIT("$f9");
	42 -> SLIT("$f10"); 43 -> SLIT("$f11");
	44 -> SLIT("$f12"); 45 -> SLIT("$f13");
	46 -> SLIT("$f14"); 47 -> SLIT("$f15");
	48 -> SLIT("$f16"); 49 -> SLIT("$f17");
	50 -> SLIT("$f18"); 51 -> SLIT("$f19");
	52 -> SLIT("$f20"); 53 -> SLIT("$f21");
	54 -> SLIT("$f22"); 55 -> SLIT("$f23");
	56 -> SLIT("$f24"); 57 -> SLIT("$f25");
	58 -> SLIT("$f26"); 59 -> SLIT("$f27");
	60 -> SLIT("$f28"); 61 -> SLIT("$f29");
	62 -> SLIT("$f30"); 63 -> SLIT("$f31");
	_  -> SLIT("very naughty alpha register")
      })
#endif
#if i386_TARGET_ARCH
    ppr_reg_no :: MachRep -> Int -> Doc
    ppr_reg_no I8   = ppr_reg_byte
    ppr_reg_no I16  = ppr_reg_word
    ppr_reg_no _    = ppr_reg_long

    ppr_reg_byte i = ptext
      (case i of {
	 0 -> SLIT("%al");     1 -> SLIT("%bl");
	 2 -> SLIT("%cl");     3 -> SLIT("%dl");
	_  -> SLIT("very naughty I386 byte register")
      })

    ppr_reg_word i = ptext
      (case i of {
	 0 -> SLIT("%ax");     1 -> SLIT("%bx");
	 2 -> SLIT("%cx");     3 -> SLIT("%dx");
	 4 -> SLIT("%si");     5 -> SLIT("%di");
	 6 -> SLIT("%bp");     7 -> SLIT("%sp");
	_  -> SLIT("very naughty I386 word register")
      })

    ppr_reg_long i = ptext
      (case i of {
	 0 -> SLIT("%eax");    1 -> SLIT("%ebx");
	 2 -> SLIT("%ecx");    3 -> SLIT("%edx");
	 4 -> SLIT("%esi");    5 -> SLIT("%edi");
	 6 -> SLIT("%ebp");    7 -> SLIT("%esp");
	 8 -> SLIT("%fake0");  9 -> SLIT("%fake1");
	10 -> SLIT("%fake2"); 11 -> SLIT("%fake3");
	12 -> SLIT("%fake4"); 13 -> SLIT("%fake5");
	_  -> SLIT("very naughty I386 register")
      })
#endif

#if x86_64_TARGET_ARCH
    ppr_reg_no :: MachRep -> Int -> Doc
    ppr_reg_no I8   = ppr_reg_byte
    ppr_reg_no I16  = ppr_reg_word
    ppr_reg_no I32  = ppr_reg_long
    ppr_reg_no _    = ppr_reg_quad

    ppr_reg_byte i = ptext
      (case i of {
	 0 -> SLIT("%al");     1 -> SLIT("%bl");
	 2 -> SLIT("%cl");     3 -> SLIT("%dl");
	 4 -> SLIT("%sil");    5 -> SLIT("%dil"); -- new 8-bit regs!
	 6 -> SLIT("%bpl");    7 -> SLIT("%spl");
	 8 -> SLIT("%r8b");    9  -> SLIT("%r9b");
	10 -> SLIT("%r10b");   11 -> SLIT("%r11b");
	12 -> SLIT("%r12b");   13 -> SLIT("%r13b");
	14 -> SLIT("%r14b");   15 -> SLIT("%r15b");
	_  -> SLIT("very naughty x86_64 byte register")
      })

    ppr_reg_word i = ptext
      (case i of {
	 0 -> SLIT("%ax");     1 -> SLIT("%bx");
	 2 -> SLIT("%cx");     3 -> SLIT("%dx");
	 4 -> SLIT("%si");     5 -> SLIT("%di");
	 6 -> SLIT("%bp");     7 -> SLIT("%sp");
	 8 -> SLIT("%r8w");    9  -> SLIT("%r9w");
	10 -> SLIT("%r10w");   11 -> SLIT("%r11w");
	12 -> SLIT("%r12w");   13 -> SLIT("%r13w");
	14 -> SLIT("%r14w");   15 -> SLIT("%r15w");
	_  -> SLIT("very naughty x86_64 word register")
      })

    ppr_reg_long i = ptext
      (case i of {
	 0 -> SLIT("%eax");    1  -> SLIT("%ebx");
	 2 -> SLIT("%ecx");    3  -> SLIT("%edx");
	 4 -> SLIT("%esi");    5  -> SLIT("%edi");
	 6 -> SLIT("%ebp");    7  -> SLIT("%esp");
	 8 -> SLIT("%r8d");    9  -> SLIT("%r9d");
	10 -> SLIT("%r10d");   11 -> SLIT("%r11d");
	12 -> SLIT("%r12d");   13 -> SLIT("%r13d");
	14 -> SLIT("%r14d");   15 -> SLIT("%r15d");
	_  -> SLIT("very naughty x86_64 register")
      })

    ppr_reg_quad i = ptext
      (case i of {
	 0 -> SLIT("%rax");	1 -> SLIT("%rbx");
	 2 -> SLIT("%rcx");	3 -> SLIT("%rdx");
	 4 -> SLIT("%rsi");	5 -> SLIT("%rdi");
	 6 -> SLIT("%rbp");	7 -> SLIT("%rsp");
	 8 -> SLIT("%r8");  	9 -> SLIT("%r9");
	10 -> SLIT("%r10");    11 -> SLIT("%r11");
	12 -> SLIT("%r12");    13 -> SLIT("%r13");
	14 -> SLIT("%r14");    15 -> SLIT("%r15");
	16 -> SLIT("%xmm0");   17 -> SLIT("%xmm1");
	18 -> SLIT("%xmm2");   19 -> SLIT("%xmm3");
	20 -> SLIT("%xmm4");   21 -> SLIT("%xmm5");
	22 -> SLIT("%xmm6");   23 -> SLIT("%xmm7");
	24 -> SLIT("%xmm8");   25 -> SLIT("%xmm9");
	26 -> SLIT("%xmm10");  27 -> SLIT("%xmm11");
	28 -> SLIT("%xmm12");  29 -> SLIT("%xmm13");
	30 -> SLIT("%xmm14");  31 -> SLIT("%xmm15");
	_  -> SLIT("very naughty x86_64 register")
      })
#endif

#if sparc_TARGET_ARCH
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> SLIT("%g0");   1 -> SLIT("%g1");
	 2 -> SLIT("%g2");   3 -> SLIT("%g3");
	 4 -> SLIT("%g4");   5 -> SLIT("%g5");
	 6 -> SLIT("%g6");   7 -> SLIT("%g7");
	 8 -> SLIT("%o0");   9 -> SLIT("%o1");
	10 -> SLIT("%o2");  11 -> SLIT("%o3");
	12 -> SLIT("%o4");  13 -> SLIT("%o5");
	14 -> SLIT("%o6");  15 -> SLIT("%o7");
	16 -> SLIT("%l0");  17 -> SLIT("%l1");
	18 -> SLIT("%l2");  19 -> SLIT("%l3");
	20 -> SLIT("%l4");  21 -> SLIT("%l5");
	22 -> SLIT("%l6");  23 -> SLIT("%l7");
	24 -> SLIT("%i0");  25 -> SLIT("%i1");
	26 -> SLIT("%i2");  27 -> SLIT("%i3");
	28 -> SLIT("%i4");  29 -> SLIT("%i5");
	30 -> SLIT("%i6");  31 -> SLIT("%i7");
	32 -> SLIT("%f0");  33 -> SLIT("%f1");
	34 -> SLIT("%f2");  35 -> SLIT("%f3");
	36 -> SLIT("%f4");  37 -> SLIT("%f5");
	38 -> SLIT("%f6");  39 -> SLIT("%f7");
	40 -> SLIT("%f8");  41 -> SLIT("%f9");
	42 -> SLIT("%f10"); 43 -> SLIT("%f11");
	44 -> SLIT("%f12"); 45 -> SLIT("%f13");
	46 -> SLIT("%f14"); 47 -> SLIT("%f15");
	48 -> SLIT("%f16"); 49 -> SLIT("%f17");
	50 -> SLIT("%f18"); 51 -> SLIT("%f19");
	52 -> SLIT("%f20"); 53 -> SLIT("%f21");
	54 -> SLIT("%f22"); 55 -> SLIT("%f23");
	56 -> SLIT("%f24"); 57 -> SLIT("%f25");
	58 -> SLIT("%f26"); 59 -> SLIT("%f27");
	60 -> SLIT("%f28"); 61 -> SLIT("%f29");
	62 -> SLIT("%f30"); 63 -> SLIT("%f31");
	_  -> SLIT("very naughty sparc register")
      })
#endif
#if powerpc_TARGET_ARCH
#if darwin_TARGET_OS
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> SLIT("r0");   1 -> SLIT("r1");
	 2 -> SLIT("r2");   3 -> SLIT("r3");
	 4 -> SLIT("r4");   5 -> SLIT("r5");
	 6 -> SLIT("r6");   7 -> SLIT("r7");
	 8 -> SLIT("r8");   9 -> SLIT("r9");
	10 -> SLIT("r10");  11 -> SLIT("r11");
	12 -> SLIT("r12");  13 -> SLIT("r13");
	14 -> SLIT("r14");  15 -> SLIT("r15");
	16 -> SLIT("r16");  17 -> SLIT("r17");
	18 -> SLIT("r18");  19 -> SLIT("r19");
	20 -> SLIT("r20");  21 -> SLIT("r21");
	22 -> SLIT("r22");  23 -> SLIT("r23");
	24 -> SLIT("r24");  25 -> SLIT("r25");
	26 -> SLIT("r26");  27 -> SLIT("r27");
	28 -> SLIT("r28");  29 -> SLIT("r29");
	30 -> SLIT("r30");  31 -> SLIT("r31");
	32 -> SLIT("f0");  33 -> SLIT("f1");
	34 -> SLIT("f2");  35 -> SLIT("f3");
	36 -> SLIT("f4");  37 -> SLIT("f5");
	38 -> SLIT("f6");  39 -> SLIT("f7");
	40 -> SLIT("f8");  41 -> SLIT("f9");
	42 -> SLIT("f10"); 43 -> SLIT("f11");
	44 -> SLIT("f12"); 45 -> SLIT("f13");
	46 -> SLIT("f14"); 47 -> SLIT("f15");
	48 -> SLIT("f16"); 49 -> SLIT("f17");
	50 -> SLIT("f18"); 51 -> SLIT("f19");
	52 -> SLIT("f20"); 53 -> SLIT("f21");
	54 -> SLIT("f22"); 55 -> SLIT("f23");
	56 -> SLIT("f24"); 57 -> SLIT("f25");
	58 -> SLIT("f26"); 59 -> SLIT("f27");
	60 -> SLIT("f28"); 61 -> SLIT("f29");
	62 -> SLIT("f30"); 63 -> SLIT("f31");
	_  -> SLIT("very naughty powerpc register")
      })
#else
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i | i <= 31 = int i	-- GPRs
                 | i <= 63 = int (i-32) -- FPRs
		 | otherwise = ptext SLIT("very naughty powerpc register")
#endif
#endif


-- -----------------------------------------------------------------------------
-- pprSize: print a 'Size'

#if powerpc_TARGET_ARCH || i386_TARGET_ARCH || x86_64_TARGET_ARCH || sparc_TARGET_ARCH
pprSize :: MachRep -> Doc
#else
pprSize :: Size -> Doc
#endif

pprSize x = ptext (case x of
#if alpha_TARGET_ARCH
	 B  -> SLIT("b")
	 Bu -> SLIT("bu")
--	 W  -> SLIT("w") UNUSED
--	 Wu -> SLIT("wu") UNUSED
	 L  -> SLIT("l")
	 Q  -> SLIT("q")
--	 FF -> SLIT("f") UNUSED
--	 DF -> SLIT("d") UNUSED
--	 GF -> SLIT("g") UNUSED
--	 SF -> SLIT("s") UNUSED
	 TF -> SLIT("t")
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	I8   -> SLIT("b")
	I16  -> SLIT("w")
	I32  -> SLIT("l")
	I64  -> SLIT("q")
#endif
#if i386_TARGET_ARCH
	F32  -> SLIT("s")
	F64  -> SLIT("l")
	F80  -> SLIT("t")
#endif
#if x86_64_TARGET_ARCH
	F32  -> SLIT("ss")	-- "scalar single-precision float" (SSE2)
	F64  -> SLIT("sd")	-- "scalar double-precision float" (SSE2)
#endif
#if sparc_TARGET_ARCH
	I8   -> SLIT("sb")
        I16   -> SLIT("sh")
	I32   -> SLIT("")
	F32   -> SLIT("")
	F64  -> SLIT("d")
    )
pprStSize :: MachRep -> Doc
pprStSize x = ptext (case x of
	I8   -> SLIT("b")
	I16  -> SLIT("h")
	I32  -> SLIT("")
	F32  -> SLIT("")
	F64  -> SLIT("d")
#endif
#if powerpc_TARGET_ARCH
	I8   -> SLIT("b")
        I16  -> SLIT("h")
	I32  -> SLIT("w")
	F32  -> SLIT("fs")
	F64  -> SLIT("fd")
#endif
    )

-- -----------------------------------------------------------------------------
-- pprCond: print a 'Cond'

pprCond :: Cond -> Doc

pprCond c = ptext (case c of {
#if alpha_TARGET_ARCH
	EQQ  -> SLIT("eq");
	LTT  -> SLIT("lt");
	LE  -> SLIT("le");
	ULT -> SLIT("ult");
	ULE -> SLIT("ule");
	NE  -> SLIT("ne");
	GTT  -> SLIT("gt");
	GE  -> SLIT("ge")
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	GEU	-> SLIT("ae");	LU    -> SLIT("b");
	EQQ	-> SLIT("e");	GTT   -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("a");
	LTT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("be");	NE    -> SLIT("ne");
	NEG	-> SLIT("s");	POS   -> SLIT("ns");
        CARRY   -> SLIT("c");   OFLO  -> SLIT("o");
	PARITY  -> SLIT("p");   NOTPARITY -> SLIT("np");
	ALWAYS	-> SLIT("mp")	-- hack
#endif
#if sparc_TARGET_ARCH
	ALWAYS	-> SLIT("");	NEVER -> SLIT("n");
	GEU	-> SLIT("geu");	LU    -> SLIT("lu");
	EQQ	-> SLIT("e");	GTT   -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("gu");
	LTT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("leu");	NE    -> SLIT("ne");
	NEG	-> SLIT("neg");	POS   -> SLIT("pos");
	VC	-> SLIT("vc");	VS    -> SLIT("vs")
#endif
#if powerpc_TARGET_ARCH
	ALWAYS  -> SLIT("");
	EQQ	-> SLIT("eq");	NE    -> SLIT("ne");
	LTT     -> SLIT("lt");  GE    -> SLIT("ge");
	GTT     -> SLIT("gt");  LE    -> SLIT("le");
	LU      -> SLIT("lt");  GEU   -> SLIT("ge");
	GU      -> SLIT("gt");  LEU   -> SLIT("le");
#endif
    })


-- -----------------------------------------------------------------------------
-- pprImm: print an 'Imm'

pprImm :: Imm -> Doc

pprImm (ImmInt i)     = int i
pprImm (ImmInteger i) = integer i
pprImm (ImmCLbl l)    = pprCLabel_asm l
pprImm (ImmIndex l i) = pprCLabel_asm l <> char '+' <> int i
pprImm (ImmLit s)     = s

pprImm (ImmFloat _) = ptext SLIT("naughty float immediate")
pprImm (ImmDouble _) = ptext SLIT("naughty double immediate")

pprImm (ImmConstantSum a b) = pprImm a <> char '+' <> pprImm b
#if sparc_TARGET_ARCH
-- ToDo: This should really be fixed in the PIC support, but only
-- print a for now.
pprImm (ImmConstantDiff a b) = pprImm a 
#else
pprImm (ImmConstantDiff a b) = pprImm a <> char '-'
                            <> lparen <> pprImm b <> rparen
#endif

#if sparc_TARGET_ARCH
pprImm (LO i)
  = hcat [ pp_lo, pprImm i, rparen ]
  where
    pp_lo = text "%lo("

pprImm (HI i)
  = hcat [ pp_hi, pprImm i, rparen ]
  where
    pp_hi = text "%hi("
#endif
#if powerpc_TARGET_ARCH
#if darwin_TARGET_OS
pprImm (LO i)
  = hcat [ pp_lo, pprImm i, rparen ]
  where
    pp_lo = text "lo16("

pprImm (HI i)
  = hcat [ pp_hi, pprImm i, rparen ]
  where
    pp_hi = text "hi16("

pprImm (HA i)
  = hcat [ pp_ha, pprImm i, rparen ]
  where
    pp_ha = text "ha16("
    
#else
pprImm (LO i)
  = pprImm i <> text "@l"

pprImm (HI i)
  = pprImm i <> text "@h"

pprImm (HA i)
  = pprImm i <> text "@ha"
#endif
#endif


-- -----------------------------------------------------------------------------
-- @pprAddr: print an 'AddrMode'

pprAddr :: AddrMode -> Doc

#if alpha_TARGET_ARCH
pprAddr (AddrReg r) = parens (pprReg r)
pprAddr (AddrImm i) = pprImm i
pprAddr (AddrRegImm r1 i)
  = (<>) (pprImm i) (parens (pprReg r1))
#endif

-------------------

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
pprAddr (ImmAddr imm off)
  = let	pp_imm = pprImm imm
    in
    if (off == 0) then
	pp_imm
    else if (off < 0) then
	pp_imm <> int off
    else
	pp_imm <> char '+' <> int off

pprAddr (AddrBaseIndex base index displacement)
  = let
	pp_disp  = ppr_disp displacement
	pp_off p = pp_disp <> char '(' <> p <> char ')'
	pp_reg r = pprReg wordRep r
    in
    case (base,index) of
      (EABaseNone,  EAIndexNone) -> pp_disp
      (EABaseReg b, EAIndexNone) -> pp_off (pp_reg b)
      (EABaseRip,   EAIndexNone) -> pp_off (ptext SLIT("%rip"))
      (EABaseNone,  EAIndex r i) -> pp_off (comma <> pp_reg r <> comma <> int i)
      (EABaseReg b, EAIndex r i) -> pp_off (pp_reg b <> comma <> pp_reg r 
                                       <> comma <> int i)
  where
    ppr_disp (ImmInt 0) = empty
    ppr_disp imm        = pprImm imm
#endif

-------------------

#if sparc_TARGET_ARCH
pprAddr (AddrRegReg r1 (RealReg 0)) = pprReg r1

pprAddr (AddrRegReg r1 r2)
  = hcat [ pprReg r1, char '+', pprReg r2 ]

pprAddr (AddrRegImm r1 (ImmInt i))
  | i == 0 = pprReg r1
  | not (fits13Bits i) = largeOffsetError i
  | otherwise = hcat [ pprReg r1, pp_sign, int i ]
  where
    pp_sign = if i > 0 then char '+' else empty

pprAddr (AddrRegImm r1 (ImmInteger i))
  | i == 0 = pprReg r1
  | not (fits13Bits i) = largeOffsetError i
  | otherwise  = hcat [ pprReg r1, pp_sign, integer i ]
  where
    pp_sign = if i > 0 then char '+' else empty

pprAddr (AddrRegImm r1 imm)
  = hcat [ pprReg r1, char '+', pprImm imm ]
#endif

-------------------

#if powerpc_TARGET_ARCH
pprAddr (AddrRegReg r1 r2)
  = pprReg r1 <+> ptext SLIT(", ") <+> pprReg r2

pprAddr (AddrRegImm r1 (ImmInt i)) = hcat [ int i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 (ImmInteger i)) = hcat [ integer i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 imm) = hcat [ pprImm imm, char '(', pprReg r1, char ')' ]
#endif


-- -----------------------------------------------------------------------------
-- pprData: print a 'CmmStatic'

pprSectionHeader Text
    = ptext
	IF_ARCH_alpha(SLIT("\t.text\n\t.align 3") {-word boundary-}
       ,IF_ARCH_sparc(SLIT(".text\n\t.align 4") {-word boundary-}
       ,IF_ARCH_i386(IF_OS_darwin(SLIT(".text\n\t.align 2"),
                                  SLIT(".text\n\t.align 4,0x90"))
                                  {-needs per-OS variation!-}
       ,IF_ARCH_x86_64(SLIT(".text\n\t.align 8") {-needs per-OS variation!-}
       ,IF_ARCH_powerpc(SLIT(".text\n.align 2")
       ,)))))
pprSectionHeader Data
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 3")
	,IF_ARCH_sparc(SLIT(".data\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(SLIT(".data\n\t.align 2"),
                                   SLIT(".data\n\t.align 4"))
	,IF_ARCH_x86_64(SLIT(".data\n\t.align 8")
        ,IF_ARCH_powerpc(SLIT(".data\n.align 2")
	,)))))
pprSectionHeader ReadOnlyData
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 3")
	,IF_ARCH_sparc(SLIT(".data\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(SLIT(".const\n.align 2"),
                                   SLIT(".section .rodata\n\t.align 4"))
	,IF_ARCH_x86_64(SLIT(".section .rodata\n\t.align 8")
        ,IF_ARCH_powerpc(IF_OS_darwin(SLIT(".const\n.align 2"),
                                      SLIT(".section .rodata\n\t.align 2"))
	,)))))
pprSectionHeader RelocatableReadOnlyData
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 3")
	,IF_ARCH_sparc(SLIT(".data\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(SLIT(".const_data\n.align 2"),
                                   SLIT(".section .rodata\n\t.align 4"))
	,IF_ARCH_x86_64(SLIT(".section .rodata\n\t.align 8")
        ,IF_ARCH_powerpc(IF_OS_darwin(SLIT(".const_data\n.align 2"),
                                      SLIT(".data\n\t.align 2"))
	,)))))
pprSectionHeader UninitialisedData
    = ptext
	 IF_ARCH_alpha(SLIT("\t.bss\n\t.align 3")
	,IF_ARCH_sparc(SLIT(".bss\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(SLIT(".const_data\n\t.align 2"),
                                   SLIT(".section .bss\n\t.align 4"))
	,IF_ARCH_x86_64(SLIT(".section .bss\n\t.align 8")
        ,IF_ARCH_powerpc(IF_OS_darwin(SLIT(".const_data\n.align 2"),
                                      SLIT(".section .bss\n\t.align 2"))
	,)))))
pprSectionHeader ReadOnlyData16
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 4")
	,IF_ARCH_sparc(SLIT(".data\n\t.align 16")
	,IF_ARCH_i386(IF_OS_darwin(SLIT(".const\n.align 4"),
                                   SLIT(".section .rodata\n\t.align 16"))
	,IF_ARCH_x86_64(SLIT(".section .rodata.cst16\n\t.align 16")
        ,IF_ARCH_powerpc(IF_OS_darwin(SLIT(".const\n.align 4"),
                                      SLIT(".section .rodata\n\t.align 4"))
	,)))))

pprSectionHeader (OtherSection sec)
    = panic "PprMach.pprSectionHeader: unknown section"

pprData :: CmmStatic -> Doc
pprData (CmmAlign bytes)         = pprAlign bytes
pprData (CmmDataLabel lbl)       = pprLabel lbl
pprData (CmmString str)          = pprASCII str
pprData (CmmUninitialised bytes) = ptext SLIT(".space ") <> int bytes
pprData (CmmStaticLit lit)       = pprDataItem lit

pprGloblDecl :: CLabel -> Doc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext IF_ARCH_sparc(SLIT(".global "), 
				    SLIT(".globl ")) <>
		pprCLabel_asm lbl

pprLabel :: CLabel -> Doc
pprLabel lbl = pprGloblDecl lbl $$ (pprCLabel_asm lbl <> char ':')


pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> Doc
       do1 w = ptext SLIT("\t.byte\t") <> int (fromIntegral w)

pprAlign bytes =
	IF_ARCH_alpha(ptextSLIT(".align ") <> int pow2,
	IF_ARCH_i386(ptext SLIT(".align ") <> int IF_OS_darwin(pow2,bytes),
	IF_ARCH_x86_64(ptext SLIT(".align ") <> int bytes,
	IF_ARCH_sparc(ptext SLIT(".align ") <> int bytes,
	IF_ARCH_powerpc(ptext SLIT(".align ") <> int pow2,)))))
  where
	pow2 = log2 bytes
	
	log2 :: Int -> Int  -- cache the common ones
	log2 1 = 0 
	log2 2 = 1
	log2 4 = 2
	log2 8 = 3
	log2 n = 1 + log2 (n `quot` 2)


pprDataItem :: CmmLit -> Doc
pprDataItem lit
  = vcat (ppr_item (cmmLitRep lit) lit)
    where
	imm = litToImm lit

	-- These seem to be common:
	ppr_item I8   x = [ptext SLIT("\t.byte\t") <> pprImm imm]
	ppr_item I32  x = [ptext SLIT("\t.long\t") <> pprImm imm]
	ppr_item F32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs
    	ppr_item F64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs

#if sparc_TARGET_ARCH
        -- copy n paste of x86 version
	ppr_item I16  x = [ptext SLIT("\t.short\t") <> pprImm imm]
	ppr_item I64  x = [ptext SLIT("\t.quad\t") <> pprImm imm]
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	ppr_item I16  x = [ptext SLIT("\t.word\t") <> pprImm imm]
#endif
#if i386_TARGET_ARCH && darwin_TARGET_OS
        ppr_item I64 (CmmInt x _)  =
                [ptext SLIT("\t.long\t")
                    <> int (fromIntegral (fromIntegral x :: Word32)),
                 ptext SLIT("\t.long\t")
                    <> int (fromIntegral
                        (fromIntegral (x `shiftR` 32) :: Word32))]
#endif
#if i386_TARGET_ARCH
	ppr_item I64  x = [ptext SLIT("\t.quad\t") <> pprImm imm]
#endif
#if x86_64_TARGET_ARCH
	-- x86_64: binutils can't handle the R_X86_64_PC64 relocation
	-- type, which means we can't do pc-relative 64-bit addresses.
	-- Fortunately we're assuming the small memory model, in which
	-- all such offsets will fit into 32 bits, so we have to stick
	-- to 32-bit offset fields and modify the RTS appropriately
	-- (see InfoTables.h).
	-- 
	ppr_item I64  x 
	   | isRelativeReloc x =
		[ptext SLIT("\t.long\t") <> pprImm imm,
		 ptext SLIT("\t.long\t0")]
	   | otherwise =
		[ptext SLIT("\t.quad\t") <> pprImm imm]
	   where
		isRelativeReloc (CmmLabelOff _ _)       = True
		isRelativeReloc (CmmLabelDiffOff _ _ _) = True
		isRelativeReloc _ = False
#endif
#if powerpc_TARGET_ARCH
	ppr_item I16 x = [ptext SLIT("\t.short\t") <> pprImm imm]
        ppr_item I64 (CmmInt x _)  =
                [ptext SLIT("\t.long\t")
                    <> int (fromIntegral 
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 ptext SLIT("\t.long\t")
                    <> int (fromIntegral (fromIntegral x :: Word32))]
#endif

-- fall through to rest of (machine-specific) pprInstr...

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

pprInstr :: Instr -> Doc

--pprInstr (COMMENT s) = empty -- nuke 'em
pprInstr (COMMENT s)
   =  IF_ARCH_alpha( ((<>) (ptext SLIT("\t# ")) (ftext s))
     ,IF_ARCH_sparc( ((<>) (ptext SLIT("! "))   (ftext s))
     ,IF_ARCH_i386( ((<>) (ptext SLIT("# "))   (ftext s))
     ,IF_ARCH_x86_64( ((<>) (ptext SLIT("# "))   (ftext s))
     ,IF_ARCH_powerpc( IF_OS_linux(
        ((<>) (ptext SLIT("# ")) (ftext s)),
        ((<>) (ptext SLIT("; ")) (ftext s)))
     ,)))))

pprInstr (DELTA d)
   = pprInstr (COMMENT (mkFastString ("\tdelta = " ++ show d)))

pprInstr (NEWBLOCK _)
   = panic "PprMach.pprInstr: NEWBLOCK"

pprInstr (LDATA _ _)
   = panic "PprMach.pprInstr: LDATA"

-- -----------------------------------------------------------------------------
-- pprInstr for an Alpha

#if alpha_TARGET_ARCH

pprInstr (LD size reg addr)
  = hcat [
	ptext SLIT("\tld"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDA reg addr)
  = hcat [
	ptext SLIT("\tlda\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDAH reg addr)
  = hcat [
	ptext SLIT("\tldah\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDGP reg addr)
  = hcat [
	ptext SLIT("\tldgp\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDI size reg imm)
  = hcat [
	ptext SLIT("\tldi"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprImm imm
    ]

pprInstr (ST size reg addr)
  = hcat [
	ptext SLIT("\tst"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (CLR reg)
  = hcat [
	ptext SLIT("\tclr\t"),
	pprReg reg
    ]

pprInstr (ABS size ri reg)
  = hcat [
	ptext SLIT("\tabs"),
	pprSize size,
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (NEG size ov ri reg)
  = hcat [
	ptext SLIT("\tneg"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (ADD size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tadd"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SADD size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> SLIT("\ts4");-} Q -> SLIT("\ts8")}),
	ptext SLIT("add"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SUB size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tsub"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SSUB size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> SLIT("\ts4");-} Q -> SLIT("\ts8")}),
	ptext SLIT("sub"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (MUL size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tmul"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (DIV size uns reg1 ri reg2)
  = hcat [
	ptext SLIT("\tdiv"),
	pprSize size,
	if uns then ptext SLIT("u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (REM size uns reg1 ri reg2)
  = hcat [
	ptext SLIT("\trem"),
	pprSize size,
	if uns then ptext SLIT("u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (NOT ri reg)
  = hcat [
	ptext SLIT("\tnot"),
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (AND reg1 ri reg2) = pprRegRIReg SLIT("and") reg1 ri reg2
pprInstr (ANDNOT reg1 ri reg2) = pprRegRIReg SLIT("andnot") reg1 ri reg2
pprInstr (OR reg1 ri reg2) = pprRegRIReg SLIT("or") reg1 ri reg2
pprInstr (ORNOT reg1 ri reg2) = pprRegRIReg SLIT("ornot") reg1 ri reg2
pprInstr (XOR reg1 ri reg2) = pprRegRIReg SLIT("xor") reg1 ri reg2
pprInstr (XORNOT reg1 ri reg2) = pprRegRIReg SLIT("xornot") reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg SLIT("sll") reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg SLIT("srl") reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg SLIT("sra") reg1 ri reg2

pprInstr (ZAP reg1 ri reg2) = pprRegRIReg SLIT("zap") reg1 ri reg2
pprInstr (ZAPNOT reg1 ri reg2) = pprRegRIReg SLIT("zapnot") reg1 ri reg2

pprInstr (NOP) = ptext SLIT("\tnop")

pprInstr (CMP cond reg1 ri reg2)
  = hcat [
	ptext SLIT("\tcmp"),
	pprCond cond,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (FCLR reg)
  = hcat [
	ptext SLIT("\tfclr\t"),
	pprReg reg
    ]

pprInstr (FABS reg1 reg2)
  = hcat [
	ptext SLIT("\tfabs\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FNEG size reg1 reg2)
  = hcat [
	ptext SLIT("\tneg"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FADD size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("add") size reg1 reg2 reg3
pprInstr (FDIV size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("div") size reg1 reg2 reg3
pprInstr (FMUL size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("mul") size reg1 reg2 reg3
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("sub") size reg1 reg2 reg3

pprInstr (CVTxy size1 size2 reg1 reg2)
  = hcat [
	ptext SLIT("\tcvt"),
	pprSize size1,
	case size2 of {Q -> ptext SLIT("qc"); _ -> pprSize size2},
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FCMP size cond reg1 reg2 reg3)
  = hcat [
	ptext SLIT("\tcmp"),
	pprSize size,
	pprCond cond,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

pprInstr (FMOV reg1 reg2)
  = hcat [
	ptext SLIT("\tfmov\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (BI ALWAYS reg lab) = pprInstr (BR lab)

pprInstr (BI NEVER reg lab) = empty

pprInstr (BI cond reg lab)
  = hcat [
	ptext SLIT("\tb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BF cond reg lab)
  = hcat [
	ptext SLIT("\tfb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BR lab)
  = (<>) (ptext SLIT("\tbr\t")) (pprImm lab)

pprInstr (JMP reg addr hint)
  = hcat [
	ptext SLIT("\tjmp\t"),
	pprReg reg,
	comma,
	pprAddr addr,
	comma,
	int hint
    ]

pprInstr (BSR imm n)
  = (<>) (ptext SLIT("\tbsr\t")) (pprImm imm)

pprInstr (JSR reg addr n)
  = hcat [
	ptext SLIT("\tjsr\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (FUNBEGIN clab)
  = hcat [
	if (externallyVisibleCLabel clab) then
	    hcat [ptext SLIT("\t.globl\t"), pp_lab, char '\n']
	else
	    empty,
	ptext SLIT("\t.ent "),
	pp_lab,
	char '\n',
	pp_lab,
	pp_ldgp,
	pp_lab,
	pp_frame
    ]
    where
	pp_lab = pprCLabel_asm clab

        -- NEVER use commas within those string literals, cpp will ruin your day
	pp_ldgp  = hcat [ ptext SLIT(":\n\tldgp $29"), char ',', ptext SLIT("0($27)\n") ]
	pp_frame = hcat [ ptext SLIT("..ng:\n\t.frame $30"), char ',',
                          ptext SLIT("4240"), char ',',
                          ptext SLIT("$26"), char ',',
                          ptext SLIT("0\n\t.prologue 1") ]

pprInstr (FUNEND clab)
  = (<>) (ptext SLIT("\t.align 4\n\t.end ")) (pprCLabel_asm clab)
\end{code}

Continue with Alpha-only printing bits and bobs:
\begin{code}
pprRI :: RI -> Doc

pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprRegRIReg :: LitString -> Reg -> RI -> Reg -> Doc
pprRegRIReg name reg1 ri reg2
  = hcat [
 	char '\t',
	ptext name,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprSizeRegRegReg :: LitString -> Size -> Reg -> Reg -> Reg -> Doc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
	char '\t',
	ptext name,
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

#endif /* alpha_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- pprInstr for an x86

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH

pprInstr v@(MOV size s@(OpReg src) d@(OpReg dst)) -- hack
  | src == dst
  =
#if 0 /* #ifdef DEBUG */
    (<>) (ptext SLIT("# warning: ")) (pprSizeOpOp SLIT("mov") size s d)
#else
    empty
#endif

pprInstr (MOV size src dst)
  = pprSizeOpOp SLIT("mov") size src dst

pprInstr (MOVZxL I32 src dst) = pprSizeOpOp SLIT("mov") I32 src dst
	-- 32-to-64 bit zero extension on x86_64 is accomplished by a simple
	-- movl.  But we represent it as a MOVZxL instruction, because
	-- the reg alloc would tend to throw away a plain reg-to-reg
	-- move, and we still want it to do that.

pprInstr (MOVZxL sizes src dst) = pprSizeOpOpCoerce SLIT("movz") sizes I32 src dst
	-- zero-extension only needs to extend to 32 bits: on x86_64, 
	-- the remaining zero-extension to 64 bits is automatic, and the 32-bit
	-- instruction is shorter.

pprInstr (MOVSxL sizes src dst) = pprSizeOpOpCoerce SLIT("movs") sizes wordRep src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg1 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg2) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg2 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg1) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(EABaseReg reg1) EAIndexNone displ)) dst@(OpReg reg3))
  | reg1 == reg3
  = pprInstr (ADD size (OpImm displ) dst)
pprInstr (LEA size src dst) = pprSizeOpOp SLIT("lea") size src dst

pprInstr (ADD size (OpImm (ImmInt (-1))) dst)
  = pprSizeOp SLIT("dec") size dst
pprInstr (ADD size (OpImm (ImmInt 1)) dst)
  = pprSizeOp SLIT("inc") size dst
pprInstr (ADD size src dst)
  = pprSizeOpOp SLIT("add") size src dst
pprInstr (ADC size src dst)
  = pprSizeOpOp SLIT("adc") size src dst
pprInstr (SUB size src dst) = pprSizeOpOp SLIT("sub") size src dst
pprInstr (IMUL size op1 op2) = pprSizeOpOp SLIT("imul") size op1 op2

{- A hack.  The Intel documentation says that "The two and three
   operand forms [of IMUL] may also be used with unsigned operands
   because the lower half of the product is the same regardless if
   (sic) the operands are signed or unsigned.  The CF and OF flags,
   however, cannot be used to determine if the upper half of the
   result is non-zero."  So there.  
-} 
pprInstr (AND size src dst) = pprSizeOpOp SLIT("and") size src dst
pprInstr (OR  size src dst) = pprSizeOpOp SLIT("or")  size src dst

pprInstr (XOR F32 src dst)  = pprOpOp SLIT("xorps") F32 src dst
pprInstr (XOR F64 src dst)  = pprOpOp SLIT("xorpd") F64 src dst
pprInstr (XOR size src dst) = pprSizeOpOp SLIT("xor")  size src dst

pprInstr (NOT size op) = pprSizeOp SLIT("not") size op
pprInstr (NEGI size op) = pprSizeOp SLIT("neg") size op

pprInstr (SHL size src dst) = pprShift SLIT("shl") size src dst
pprInstr (SAR size src dst) = pprShift SLIT("sar") size src dst
pprInstr (SHR size src dst) = pprShift SLIT("shr") size src dst

pprInstr (BT  size imm src) = pprSizeImmOp SLIT("bt") size imm src

pprInstr (CMP size src dst) 
  | isFloatingRep size =  pprSizeOpOp SLIT("ucomi")  size src dst -- SSE2
  | otherwise          =  pprSizeOpOp SLIT("cmp")  size src dst

pprInstr (TEST size src dst) = pprSizeOpOp SLIT("test")  size src dst
pprInstr (PUSH size op) = pprSizeOp SLIT("push") size op
pprInstr (POP size op) = pprSizeOp SLIT("pop") size op

-- both unused (SDM):
-- pprInstr PUSHA = ptext SLIT("\tpushal")
-- pprInstr POPA = ptext SLIT("\tpopal")

pprInstr NOP = ptext SLIT("\tnop")
pprInstr (CLTD I32) = ptext SLIT("\tcltd")
pprInstr (CLTD I64) = ptext SLIT("\tcqto")

pprInstr (SETCC cond op) = pprCondInstr SLIT("set") cond (pprOperand I8 op)

pprInstr (JXX cond (BlockId id)) 
  = pprCondInstr SLIT("j") cond (pprCLabel_asm lab)
  where lab = mkAsmTempLabel id

pprInstr (JMP (OpImm imm)) = (<>) (ptext SLIT("\tjmp ")) (pprImm imm)
pprInstr (JMP op)          = (<>) (ptext SLIT("\tjmp *")) (pprOperand wordRep op)
pprInstr (JMP_TBL op ids)  = pprInstr (JMP op)
pprInstr (CALL (Left imm) _)    = (<>) (ptext SLIT("\tcall ")) (pprImm imm)
pprInstr (CALL (Right reg) _)   = (<>) (ptext SLIT("\tcall *")) (pprReg wordRep reg)

pprInstr (IDIV sz op)	= pprSizeOp SLIT("idiv") sz op
pprInstr (DIV sz op)    = pprSizeOp SLIT("div")  sz op
pprInstr (IMUL2 sz op)  = pprSizeOp SLIT("imul") sz op

#if x86_64_TARGET_ARCH
pprInstr (MUL size op1 op2) = pprSizeOpOp SLIT("mul") size op1 op2

pprInstr (FDIV size op1 op2) = pprSizeOpOp SLIT("div") size op1 op2

pprInstr (CVTSS2SD from to) = pprRegReg SLIT("cvtss2sd") from to
pprInstr (CVTSD2SS from to) = pprRegReg SLIT("cvtsd2ss") from to
pprInstr (CVTSS2SI from to) = pprOpReg  SLIT("cvtss2si") from to
pprInstr (CVTSD2SI from to) = pprOpReg  SLIT("cvtsd2si") from to
pprInstr (CVTSI2SS from to) = pprOpReg  SLIT("cvtsi2ss") from to
pprInstr (CVTSI2SD from to) = pprOpReg  SLIT("cvtsi2sd") from to
#endif

    -- FETCHGOT for PIC on ELF platforms
pprInstr (FETCHGOT reg)
   = vcat [ ptext SLIT("\tcall 1f"),
            hcat [ ptext SLIT("1:\tpopl\t"), pprReg I32 reg ],
            hcat [ ptext SLIT("\taddl\t$_GLOBAL_OFFSET_TABLE_+(.-1b), "),
                   pprReg I32 reg ]
          ]

    -- FETCHPC for PIC on Darwin/x86
    -- get the instruction pointer into a register
    -- (Terminology note: the IP is called Program Counter on PPC,
    --  and it's a good thing to use the same name on both platforms)
pprInstr (FETCHPC reg)
   = vcat [ ptext SLIT("\tcall 1f"),
            hcat [ ptext SLIT("1:\tpopl\t"), pprReg I32 reg ]
          ]



#endif

-- -----------------------------------------------------------------------------
-- i386 floating-point

#if i386_TARGET_ARCH
-- Simulating a flat register set on the x86 FP stack is tricky.
-- you have to free %st(7) before pushing anything on the FP reg stack
-- so as to preclude the possibility of a FP stack overflow exception.
pprInstr g@(GMOV src dst)
   | src == dst
   = empty
   | otherwise 
   = pprG g (hcat [gtab, gpush src 0, gsemi, gpop dst 1])

-- GLD sz addr dst ==> FFREE %st(7) ; FLDsz addr ; FSTP (dst+1)
pprInstr g@(GLD sz addr dst)
 = pprG g (hcat [gtab, text "ffree %st(7) ; fld", pprSize sz, gsp, 
                 pprAddr addr, gsemi, gpop dst 1])

-- GST sz src addr ==> FFREE %st(7) ; FLD dst ; FSTPsz addr
pprInstr g@(GST sz src addr)
 = pprG g (hcat [gtab, gpush src 0, gsemi, 
                 text "fstp", pprSize sz, gsp, pprAddr addr])

pprInstr g@(GLDZ dst)
 = pprG g (hcat [gtab, text "ffree %st(7) ; fldz ; ", gpop dst 1])
pprInstr g@(GLD1 dst)
 = pprG g (hcat [gtab, text "ffree %st(7) ; fld1 ; ", gpop dst 1])

pprInstr g@(GFTOI src dst) 
   = pprInstr (GDTOI src dst)
pprInstr g@(GDTOI src dst) 
   = pprG g (hcat [gtab, text "subl $4, %esp ; ", 
                   gpush src 0, gsemi, text "fistpl 0(%esp) ; popl ", 
                   pprReg I32 dst])

pprInstr g@(GITOF src dst) 
   = pprInstr (GITOD src dst)
pprInstr g@(GITOD src dst) 
   = pprG g (hcat [gtab, text "pushl ", pprReg I32 src, 
                   text " ; ffree %st(7); fildl (%esp) ; ",
                   gpop dst 1, text " ; addl $4,%esp"])

{- Gruesome swamp follows.  If you're unfortunate enough to have ventured
   this far into the jungle AND you give a Rat's Ass (tm) what's going
   on, here's the deal.  Generate code to do a floating point comparison
   of src1 and src2, of kind cond, and set the Zero flag if true.

   The complications are to do with handling NaNs correctly.  We want the
   property that if either argument is NaN, then the result of the
   comparison is False ... except if we're comparing for inequality,
   in which case the answer is True.

   Here's how the general (non-inequality) case works.  As an
   example, consider generating the an equality test:

     pushl %eax		-- we need to mess with this
     <get src1 to top of FPU stack>
     fcomp <src2 location in FPU stack> and pop pushed src1
		-- Result of comparison is in FPU Status Register bits
		-- C3 C2 and C0
     fstsw %ax	-- Move FPU Status Reg to %ax
     sahf	-- move C3 C2 C0 from %ax to integer flag reg
     -- now the serious magic begins
     setpo %ah	   -- %ah = if comparable(neither arg was NaN) then 1 else 0
     sete  %al     -- %al = if arg1 == arg2 then 1 else 0
     andb %ah,%al  -- %al &= %ah
                   -- so %al == 1 iff (comparable && same); else it holds 0
     decb %al	   -- %al == 0, ZeroFlag=1  iff (comparable && same); 
                      else %al == 0xFF, ZeroFlag=0
     -- the zero flag is now set as we desire.
     popl %eax

   The special case of inequality differs thusly:

     setpe %ah     -- %ah = if incomparable(either arg was NaN) then 1 else 0
     setne %al     -- %al = if arg1 /= arg2 then 1 else 0
     orb %ah,%al   -- %al = if (incomparable || different) then 1 else 0
     decb %al      -- if (incomparable || different) then (%al == 0, ZF=1)
                                                     else (%al == 0xFF, ZF=0)
-}
pprInstr g@(GCMP cond src1 src2) 
   | case cond of { NE -> True; other -> False }
   = pprG g (vcat [
        hcat [gtab, text "pushl %eax ; ",gpush src1 0],
        hcat [gtab, text "fcomp ", greg src2 1, 
                    text "; fstsw %ax ; sahf ;  setpe %ah"],
        hcat [gtab, text "setne %al ;  ",
              text "orb %ah,%al ;  decb %al ;  popl %eax"]
    ])
   | otherwise
   = pprG g (vcat [
        hcat [gtab, text "pushl %eax ; ",gpush src1 0],
        hcat [gtab, text "fcomp ", greg src2 1, 
                    text "; fstsw %ax ; sahf ;  setpo %ah"],
        hcat [gtab, text "set", pprCond (fix_FP_cond cond), text " %al ;  ",
              text "andb %ah,%al ;  decb %al ;  popl %eax"]
    ])
    where
        {- On the 486, the flags set by FP compare are the unsigned ones!
           (This looks like a HACK to me.  WDP 96/03)
        -}
        fix_FP_cond :: Cond -> Cond
        fix_FP_cond GE   = GEU
        fix_FP_cond GTT  = GU
        fix_FP_cond LTT  = LU
        fix_FP_cond LE   = LEU
        fix_FP_cond EQQ  = EQQ
        fix_FP_cond NE   = NE
        -- there should be no others


pprInstr g@(GABS sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fabs ; ", gpop dst 1])
pprInstr g@(GNEG sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fchs ; ", gpop dst 1])

pprInstr g@(GSQRT sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fsqrt"] $$ 
             hcat [gtab, gcoerceto sz, gpop dst 1])
pprInstr g@(GSIN sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fsin"] $$ 
             hcat [gtab, gcoerceto sz, gpop dst 1])
pprInstr g@(GCOS sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fcos"] $$ 
             hcat [gtab, gcoerceto sz, gpop dst 1])
pprInstr g@(GTAN sz src dst)
   = pprG g (hcat [gtab, text "ffree %st(6) ; ",
                   gpush src 0, text " ; fptan ; ", 
                   text " fstp %st(0)"] $$
             hcat [gtab, gcoerceto sz, gpop dst 1])

-- In the translations for GADD, GMUL, GSUB and GDIV,
-- the first two cases are mere optimisations.  The otherwise clause
-- generates correct code under all circumstances.

pprInstr g@(GADD sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GADD-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; faddp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GADD-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; faddp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fadd ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GMUL sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GMUL-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; fmulp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GMUL-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; fmulp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fmul ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GSUB sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GSUB-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; fsubrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GSUB-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; fsubp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fsub ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GDIV sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GDIV-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; fdivrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GDIV-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; fdivp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fdiv ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr GFREE 
   = vcat [ ptext SLIT("\tffree %st(0) ;ffree %st(1) ;ffree %st(2) ;ffree %st(3)"),
            ptext SLIT("\tffree %st(4) ;ffree %st(5) ;ffree %st(6) ;ffree %st(7)") 
          ]

--------------------------

-- coerce %st(0) to the specified size
gcoerceto F64 = empty
gcoerceto F32 = empty --text "subl $4,%esp ; fstps (%esp) ; flds (%esp) ; addl $4,%esp ; "

gpush reg offset
   = hcat [text "ffree %st(7) ; fld ", greg reg offset]
gpop reg offset
   = hcat [text "fstp ", greg reg offset]

greg reg offset = text "%st(" <> int (gregno reg - 8+offset) <> char ')'
gsemi = text " ; "
gtab  = char '\t'
gsp   = char ' '

gregno (RealReg i) = i
gregno other       = --pprPanic "gregno" (ppr other)
                     999   -- bogus; only needed for debug printing

pprG :: Instr -> Doc -> Doc
pprG fake actual
   = (char '#' <> pprGInstr fake) $$ actual

pprGInstr (GMOV src dst)   = pprSizeRegReg SLIT("gmov") F64 src dst
pprGInstr (GLD sz src dst) = pprSizeAddrReg SLIT("gld") sz src dst
pprGInstr (GST sz src dst) = pprSizeRegAddr SLIT("gst") sz src dst

pprGInstr (GLDZ dst) = pprSizeReg SLIT("gldz") F64 dst
pprGInstr (GLD1 dst) = pprSizeReg SLIT("gld1") F64 dst

pprGInstr (GFTOI src dst) = pprSizeSizeRegReg SLIT("gftoi") F32 I32  src dst
pprGInstr (GDTOI src dst) = pprSizeSizeRegReg SLIT("gdtoi") F64 I32 src dst

pprGInstr (GITOF src dst) = pprSizeSizeRegReg SLIT("gitof") I32 F32  src dst
pprGInstr (GITOD src dst) = pprSizeSizeRegReg SLIT("gitod") I32 F64 src dst

pprGInstr (GCMP co src dst) = pprCondRegReg SLIT("gcmp_") F64 co src dst
pprGInstr (GABS sz src dst) = pprSizeRegReg SLIT("gabs") sz src dst
pprGInstr (GNEG sz src dst) = pprSizeRegReg SLIT("gneg") sz src dst
pprGInstr (GSQRT sz src dst) = pprSizeRegReg SLIT("gsqrt") sz src dst
pprGInstr (GSIN sz src dst) = pprSizeRegReg SLIT("gsin") sz src dst
pprGInstr (GCOS sz src dst) = pprSizeRegReg SLIT("gcos") sz src dst
pprGInstr (GTAN sz src dst) = pprSizeRegReg SLIT("gtan") sz src dst

pprGInstr (GADD sz src1 src2 dst) = pprSizeRegRegReg SLIT("gadd") sz src1 src2 dst
pprGInstr (GSUB sz src1 src2 dst) = pprSizeRegRegReg SLIT("gsub") sz src1 src2 dst
pprGInstr (GMUL sz src1 src2 dst) = pprSizeRegRegReg SLIT("gmul") sz src1 src2 dst
pprGInstr (GDIV sz src1 src2 dst) = pprSizeRegRegReg SLIT("gdiv") sz src1 src2 dst
#endif

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH

-- Continue with I386-only printing bits and bobs:

pprDollImm :: Imm -> Doc

pprDollImm i =  ptext SLIT("$") <> pprImm i

pprOperand :: MachRep -> Operand -> Doc
pprOperand s (OpReg r)   = pprReg s r
pprOperand s (OpImm i)   = pprDollImm i
pprOperand s (OpAddr ea) = pprAddr ea

pprMnemonic_  :: LitString -> Doc
pprMnemonic_ name = 
   char '\t' <> ptext name <> space

pprMnemonic  :: LitString -> MachRep -> Doc
pprMnemonic name size = 
   char '\t' <> ptext name <> pprSize size <> space

pprSizeImmOp :: LitString -> MachRep -> Imm -> Operand -> Doc
pprSizeImmOp name size imm op1
  = hcat [
	pprMnemonic name size,
	char '$',
	pprImm imm,
	comma,
	pprOperand size op1
    ]
	
pprSizeOp :: LitString -> MachRep -> Operand -> Doc
pprSizeOp name size op1
  = hcat [
	pprMnemonic name size,
	pprOperand size op1
    ]

pprSizeOpOp :: LitString -> MachRep -> Operand -> Operand -> Doc
pprSizeOpOp name size op1 op2
  = hcat [
	pprMnemonic name size,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprOpOp :: LitString -> MachRep -> Operand -> Operand -> Doc
pprOpOp name size op1 op2
  = hcat [
	pprMnemonic_ name,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprSizeReg :: LitString -> MachRep -> Reg -> Doc
pprSizeReg name size reg1
  = hcat [
	pprMnemonic name size,
	pprReg size reg1
    ]

pprSizeRegReg :: LitString -> MachRep -> Reg -> Reg -> Doc
pprSizeRegReg name size reg1 reg2
  = hcat [
	pprMnemonic name size,
	pprReg size reg1,
        comma,
        pprReg size reg2
    ]

pprRegReg :: LitString -> Reg -> Reg -> Doc
pprRegReg name reg1 reg2
  = hcat [
	pprMnemonic_ name,
	pprReg wordRep reg1,
        comma,
        pprReg wordRep reg2
    ]

pprOpReg :: LitString -> Operand -> Reg -> Doc
pprOpReg name op1 reg2
  = hcat [
	pprMnemonic_ name,
	pprOperand wordRep op1,
        comma,
        pprReg wordRep reg2
    ]

pprCondRegReg :: LitString -> MachRep -> Cond -> Reg -> Reg -> Doc
pprCondRegReg name size cond reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	pprCond cond,
	space,
	pprReg size reg1,
        comma,
        pprReg size reg2
    ]

pprSizeSizeRegReg :: LitString -> MachRep -> MachRep -> Reg -> Reg -> Doc
pprSizeSizeRegReg name size1 size2 reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size1,
        pprSize size2,
	space,
	pprReg size1 reg1,

        comma,
        pprReg size2 reg2
    ]

pprSizeRegRegReg :: LitString -> MachRep -> Reg -> Reg -> Reg -> Doc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
	pprMnemonic name size,
	pprReg size reg1,
        comma,
        pprReg size reg2,
        comma,
        pprReg size reg3
    ]

pprSizeAddrReg :: LitString -> MachRep -> AddrMode -> Reg -> Doc
pprSizeAddrReg name size op dst
  = hcat [
	pprMnemonic name size,
	pprAddr op,
	comma,
	pprReg size dst
    ]

pprSizeRegAddr :: LitString -> MachRep -> Reg -> AddrMode -> Doc
pprSizeRegAddr name size src op
  = hcat [
	pprMnemonic name size,
	pprReg size src,
	comma,
	pprAddr op
    ]

pprShift :: LitString -> MachRep -> Operand -> Operand -> Doc
pprShift name size src dest
  = hcat [
	pprMnemonic name size,
	pprOperand I8 src,  -- src is 8-bit sized
	comma,
	pprOperand size dest
    ]

pprSizeOpOpCoerce :: LitString -> MachRep -> MachRep -> Operand -> Operand -> Doc
pprSizeOpOpCoerce name size1 size2 op1 op2
  = hcat [ char '\t', ptext name, pprSize size1, pprSize size2, space,
	pprOperand size1 op1,
	comma,
	pprOperand size2 op2
    ]

pprCondInstr :: LitString -> Cond -> Doc -> Doc
pprCondInstr name cond arg
  = hcat [ char '\t', ptext name, pprCond cond, space, arg]

#endif /* i386_TARGET_ARCH */


-- ------------------------------------------------------------------------------- pprInstr for a SPARC

#if sparc_TARGET_ARCH

-- a clumsy hack for now, to handle possible double alignment problems

-- even clumsier, to allow for RegReg regs that show when doing indexed
-- reads (bytearrays).
--

-- Translate to the following:
--    add g1,g2,g1
--    ld  [g1],%fn
--    ld  [g1+4],%f(n+1)
--    sub g1,g2,g1           -- to restore g1

pprInstr (LD F64 (AddrRegReg g1 g2) reg)
  = vcat [
       hcat [ptext SLIT("\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1],
       hcat [pp_ld_lbracket, pprReg g1, pp_rbracket_comma, pprReg reg],
       hcat [pp_ld_lbracket, pprReg g1, ptext SLIT("+4]"), comma, pprReg (fPair reg)],
       hcat [ptext SLIT("\tsub\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1]
    ]

-- Translate to
--    ld  [addr],%fn
--    ld  [addr+4],%f(n+1)
pprInstr (LD F64 addr reg) | isJust off_addr
  = vcat [
       hcat [pp_ld_lbracket, pprAddr addr, pp_rbracket_comma, pprReg reg],
       hcat [pp_ld_lbracket, pprAddr addr2, pp_rbracket_comma,pprReg (fPair reg)]
    ]
  where
    off_addr = addrOffset addr 4
    addr2 = case off_addr of Just x -> x


pprInstr (LD size addr reg)
  = hcat [
       ptext SLIT("\tld"),
       pprSize size,
       char '\t',
       lbrack,
       pprAddr addr,
       pp_rbracket_comma,
       pprReg reg
    ]

-- The same clumsy hack as above

-- Translate to the following:
--    add g1,g2,g1
--    st  %fn,[g1]
--    st  %f(n+1),[g1+4]
--    sub g1,g2,g1           -- to restore g1
pprInstr (ST F64 reg (AddrRegReg g1 g2))
 = vcat [
       hcat [ptext SLIT("\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1],
       hcat [ptext SLIT("\tst\t"), pprReg reg, pp_comma_lbracket, 
             pprReg g1,	rbrack],
       hcat [ptext SLIT("\tst\t"), pprReg (fPair reg), pp_comma_lbracket,
             pprReg g1, ptext SLIT("+4]")],
       hcat [ptext SLIT("\tsub\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1]
    ]

-- Translate to
--    st  %fn,[addr]
--    st  %f(n+1),[addr+4]
pprInstr (ST F64 reg addr) | isJust off_addr 
 = vcat [
      hcat [ptext SLIT("\tst\t"), pprReg reg, pp_comma_lbracket, 
            pprAddr addr, rbrack],
      hcat [ptext SLIT("\tst\t"), pprReg (fPair reg), pp_comma_lbracket,
            pprAddr addr2, rbrack]
    ]
  where
    off_addr = addrOffset addr 4
    addr2 = case off_addr of Just x -> x

-- no distinction is made between signed and unsigned bytes on stores for the
-- Sparc opcodes (at least I cannot see any, and gas is nagging me --SOF),
-- so we call a special-purpose pprSize for ST..

pprInstr (ST size reg addr)
  = hcat [
       ptext SLIT("\tst"),
       pprStSize size,
       char '\t',
       pprReg reg,
       pp_comma_lbracket,
       pprAddr addr,
       rbrack
    ]

pprInstr (ADD x cc reg1 ri reg2)
  | not x && not cc && riZero ri
  = hcat [ ptext SLIT("\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then SLIT("addx") else SLIT("add")) cc reg1 ri reg2

pprInstr (SUB x cc reg1 ri reg2)
  | not x && cc && reg2 == g0
  = hcat [ ptext SLIT("\tcmp\t"), pprReg reg1, comma, pprRI ri ]
  | not x && not cc && riZero ri
  = hcat [ ptext SLIT("\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then SLIT("subx") else SLIT("sub")) cc reg1 ri reg2

pprInstr (AND  b reg1 ri reg2) = pprRegRIReg SLIT("and")  b reg1 ri reg2
pprInstr (ANDN b reg1 ri reg2) = pprRegRIReg SLIT("andn") b reg1 ri reg2

pprInstr (OR b reg1 ri reg2)
  | not b && reg1 == g0
  = let doit = hcat [ ptext SLIT("\tmov\t"), pprRI ri, comma, pprReg reg2 ]
    in  case ri of
           RIReg rrr | rrr == reg2 -> empty
           other                   -> doit
  | otherwise
  = pprRegRIReg SLIT("or") b reg1 ri reg2

pprInstr (ORN b reg1 ri reg2) = pprRegRIReg SLIT("orn") b reg1 ri reg2

pprInstr (XOR  b reg1 ri reg2) = pprRegRIReg SLIT("xor")  b reg1 ri reg2
pprInstr (XNOR b reg1 ri reg2) = pprRegRIReg SLIT("xnor") b reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg SLIT("sll") False reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg SLIT("srl") False reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg SLIT("sra") False reg1 ri reg2

pprInstr (RDY rd) = ptext SLIT("\trd\t%y,") <> pprReg rd
pprInstr (SMUL b reg1 ri reg2) = pprRegRIReg SLIT("smul")  b reg1 ri reg2
pprInstr (UMUL b reg1 ri reg2) = pprRegRIReg SLIT("umul")  b reg1 ri reg2

pprInstr (SETHI imm reg)
  = hcat [
	ptext SLIT("\tsethi\t"),
	pprImm imm,
	comma,
	pprReg reg
    ]

pprInstr NOP = ptext SLIT("\tnop")

pprInstr (FABS F32 reg1 reg2) = pprSizeRegReg SLIT("fabs") F32 reg1 reg2
pprInstr (FABS F64 reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fabs") F32 reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F32 (fPair reg1) (fPair reg2)))

pprInstr (FADD size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fadd") size reg1 reg2 reg3
pprInstr (FCMP e size reg1 reg2)
  = pprSizeRegReg (if e then SLIT("fcmpe") else SLIT("fcmp")) size reg1 reg2
pprInstr (FDIV size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fdiv") size reg1 reg2 reg3

pprInstr (FMOV F32 reg1 reg2) = pprSizeRegReg SLIT("fmov") F32 reg1 reg2
pprInstr (FMOV F64 reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fmov") F32 reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F32 (fPair reg1) (fPair reg2)))

pprInstr (FMUL size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fmul") size reg1 reg2 reg3

pprInstr (FNEG F32 reg1 reg2) = pprSizeRegReg SLIT("fneg") F32 reg1 reg2
pprInstr (FNEG F64 reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fneg") F32 reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F32 (fPair reg1) (fPair reg2)))

pprInstr (FSQRT size reg1 reg2)     = pprSizeRegReg SLIT("fsqrt") size reg1 reg2
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fsub") size reg1 reg2 reg3
pprInstr (FxTOy size1 size2 reg1 reg2)
  = hcat [
    	ptext SLIT("\tf"),
	ptext
    	(case size1 of
    	    I32  -> SLIT("ito")
    	    F32  -> SLIT("sto")
    	    F64  -> SLIT("dto")),
	ptext
    	(case size2 of
    	    I32  -> SLIT("i\t")
    	    F32  -> SLIT("s\t")
    	    F64  -> SLIT("d\t")),
	pprReg reg1, comma, pprReg reg2
    ]


pprInstr (BI cond b lab)
  = hcat [
	ptext SLIT("\tb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (BF cond b lab)
  = hcat [
	ptext SLIT("\tfb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (JMP addr) = (<>) (ptext SLIT("\tjmp\t")) (pprAddr addr)

pprInstr (CALL (Left imm) n _)
  = hcat [ ptext SLIT("\tcall\t"), pprImm imm, comma, int n ]
pprInstr (CALL (Right reg) n _)
  = hcat [ ptext SLIT("\tcall\t"), pprReg reg, comma, int n ]

pprRI :: RI -> Doc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprSizeRegReg :: LitString -> MachRep -> Reg -> Reg -> Doc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F32  -> ptext SLIT("s\t")
    	    F64 -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprSizeRegRegReg :: LitString -> MachRep -> Reg -> Reg -> Reg -> Doc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F32  -> ptext SLIT("s\t")
    	    F64  -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

pprRegRIReg :: LitString -> Bool -> Reg -> RI -> Reg -> Doc
pprRegRIReg name b reg1 ri reg2
  = hcat [
	char '\t',
	ptext name,
	if b then ptext SLIT("cc\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprRIReg :: LitString -> Bool -> RI -> Reg -> Doc
pprRIReg name b ri reg1
  = hcat [
	char '\t',
	ptext name,
	if b then ptext SLIT("cc\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg1
    ]

pp_ld_lbracket    = ptext SLIT("\tld\t[")
pp_rbracket_comma = text "],"
pp_comma_lbracket = text ",["
pp_comma_a	  = text ",a"

#endif /* sparc_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- pprInstr for PowerPC

#if powerpc_TARGET_ARCH
pprInstr (LD sz reg addr) = hcat [
	char '\t',
	ptext SLIT("l"),
	ptext (case sz of
	    I8  -> SLIT("bz")
	    I16 -> SLIT("hz")
	    I32 -> SLIT("wz")
	    F32 -> SLIT("fs")
	    F64 -> SLIT("fd")),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprAddr addr
    ]
pprInstr (LA sz reg addr) = hcat [
	char '\t',
	ptext SLIT("l"),
	ptext (case sz of
	    I8  -> SLIT("ba")
	    I16 -> SLIT("ha")
	    I32 -> SLIT("wa")
	    F32 -> SLIT("fs")
	    F64 -> SLIT("fd")),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprAddr addr
    ]
pprInstr (ST sz reg addr) = hcat [
	char '\t',
	ptext SLIT("st"),
	pprSize sz,
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprAddr addr
    ]
pprInstr (STU sz reg addr) = hcat [
	char '\t',
	ptext SLIT("st"),
	pprSize sz,
	ptext SLIT("u\t"),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	pprReg reg,
	ptext SLIT(", "),
	pprAddr addr
    ]
pprInstr (LIS reg imm) = hcat [
	char '\t',
	ptext SLIT("lis"),
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprImm imm
    ]
pprInstr (LI reg imm) = hcat [
	char '\t',
	ptext SLIT("li"),
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprImm imm
    ]
pprInstr (MR reg1 reg2) 
    | reg1 == reg2 = empty
    | otherwise = hcat [
	char '\t',
	case regClass reg1 of
	    RcInteger -> ptext SLIT("mr")
	    _ -> ptext SLIT("fmr"),
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2
    ]
pprInstr (CMP sz reg ri) = hcat [
	char '\t',
	op,
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprRI ri
    ]
    where
	op = hcat [
		ptext SLIT("cmp"),
		pprSize sz,
		case ri of
		    RIReg _ -> empty
		    RIImm _ -> char 'i'
	    ]
pprInstr (CMPL sz reg ri) = hcat [
	char '\t',
	op,
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprRI ri
    ]
    where
	op = hcat [
		ptext SLIT("cmpl"),
		pprSize sz,
		case ri of
		    RIReg _ -> empty
		    RIImm _ -> char 'i'
	    ]
pprInstr (BCC cond (BlockId id)) = hcat [
	char '\t',
	ptext SLIT("b"),
	pprCond cond,
	char '\t',
	pprCLabel_asm lbl
    ]
    where lbl = mkAsmTempLabel id

pprInstr (JMP lbl) = hcat [ -- an alias for b that takes a CLabel
	char '\t',
	ptext SLIT("b"),
	char '\t',
	pprCLabel_asm lbl
    ]

pprInstr (MTCTR reg) = hcat [
	char '\t',
	ptext SLIT("mtctr"),
	char '\t',
	pprReg reg
    ]
pprInstr (BCTR _) = hcat [
	char '\t',
	ptext SLIT("bctr")
    ]
pprInstr (BL lbl _) = hcat [
	ptext SLIT("\tbl\t"),
        pprCLabel_asm lbl
    ]
pprInstr (BCTRL _) = hcat [
	char '\t',
	ptext SLIT("bctrl")
    ]
pprInstr (ADD reg1 reg2 ri) = pprLogic SLIT("add") reg1 reg2 ri
pprInstr (ADDIS reg1 reg2 imm) = hcat [
	char '\t',
	ptext SLIT("addis"),
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprImm imm
    ]

pprInstr (ADDC reg1 reg2 reg3) = pprLogic SLIT("addc") reg1 reg2 (RIReg reg3)
pprInstr (ADDE reg1 reg2 reg3) = pprLogic SLIT("adde") reg1 reg2 (RIReg reg3)
pprInstr (SUBF reg1 reg2 reg3) = pprLogic SLIT("subf") reg1 reg2 (RIReg reg3)
pprInstr (MULLW reg1 reg2 ri@(RIReg _)) = pprLogic SLIT("mullw") reg1 reg2 ri
pprInstr (MULLW reg1 reg2 ri@(RIImm _)) = pprLogic SLIT("mull") reg1 reg2 ri
pprInstr (DIVW reg1 reg2 reg3) = pprLogic SLIT("divw") reg1 reg2 (RIReg reg3)
pprInstr (DIVWU reg1 reg2 reg3) = pprLogic SLIT("divwu") reg1 reg2 (RIReg reg3)

pprInstr (MULLW_MayOflo reg1 reg2 reg3) = vcat [
         hcat [ ptext SLIT("\tmullwo\t"), pprReg reg1, ptext SLIT(", "),
                                          pprReg reg2, ptext SLIT(", "),
                                          pprReg reg3 ],
         hcat [ ptext SLIT("\tmfxer\t"),  pprReg reg1 ],
         hcat [ ptext SLIT("\trlwinm\t"), pprReg reg1, ptext SLIT(", "),
                                          pprReg reg1, ptext SLIT(", "),
                                          ptext SLIT("2, 31, 31") ]
    ]

    	-- for some reason, "andi" doesn't exist.
	-- we'll use "andi." instead.
pprInstr (AND reg1 reg2 (RIImm imm)) = hcat [
	char '\t',
	ptext SLIT("andi."),
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprImm imm
    ]
pprInstr (AND reg1 reg2 ri) = pprLogic SLIT("and") reg1 reg2 ri

pprInstr (OR reg1 reg2 ri) = pprLogic SLIT("or") reg1 reg2 ri
pprInstr (XOR reg1 reg2 ri) = pprLogic SLIT("xor") reg1 reg2 ri

pprInstr (XORIS reg1 reg2 imm) = hcat [
	char '\t',
	ptext SLIT("xoris"),
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprImm imm
    ]

pprInstr (EXTS sz reg1 reg2) = hcat [
	char '\t',
	ptext SLIT("exts"),
	pprSize sz,
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2
    ]

pprInstr (NEG reg1 reg2) = pprUnary SLIT("neg") reg1 reg2
pprInstr (NOT reg1 reg2) = pprUnary SLIT("not") reg1 reg2

pprInstr (SLW reg1 reg2 ri) = pprLogic SLIT("slw") reg1 reg2 (limitShiftRI ri)
pprInstr (SRW reg1 reg2 ri) = pprLogic SLIT("srw") reg1 reg2 (limitShiftRI ri)
pprInstr (SRAW reg1 reg2 ri) = pprLogic SLIT("sraw") reg1 reg2 (limitShiftRI ri)
pprInstr (RLWINM reg1 reg2 sh mb me) = hcat [
        ptext SLIT("\trlwinm\t"),
        pprReg reg1,
        ptext SLIT(", "),
        pprReg reg2,
        ptext SLIT(", "),
        int sh,
        ptext SLIT(", "),
        int mb,
        ptext SLIT(", "),
        int me
    ]
    
pprInstr (FADD sz reg1 reg2 reg3) = pprBinaryF SLIT("fadd") sz reg1 reg2 reg3
pprInstr (FSUB sz reg1 reg2 reg3) = pprBinaryF SLIT("fsub") sz reg1 reg2 reg3
pprInstr (FMUL sz reg1 reg2 reg3) = pprBinaryF SLIT("fmul") sz reg1 reg2 reg3
pprInstr (FDIV sz reg1 reg2 reg3) = pprBinaryF SLIT("fdiv") sz reg1 reg2 reg3
pprInstr (FNEG reg1 reg2) = pprUnary SLIT("fneg") reg1 reg2

pprInstr (FCMP reg1 reg2) = hcat [
	char '\t',
	ptext SLIT("fcmpu\tcr0, "),
	    -- Note: we're using fcmpu, not fcmpo
	    -- The difference is with fcmpo, compare with NaN is an invalid operation.
	    -- We don't handle invalid fp ops, so we don't care
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2
    ]

pprInstr (FCTIWZ reg1 reg2) = pprUnary SLIT("fctiwz") reg1 reg2
pprInstr (FRSP reg1 reg2) = pprUnary SLIT("frsp") reg1 reg2

pprInstr (CRNOR dst src1 src2) = hcat [
        ptext SLIT("\tcrnor\t"),
        int dst,
        ptext SLIT(", "),
        int src1,
        ptext SLIT(", "),
        int src2
    ]

pprInstr (MFCR reg) = hcat [
	char '\t',
	ptext SLIT("mfcr"),
	char '\t',
	pprReg reg
    ]

pprInstr (MFLR reg) = hcat [
	char '\t',
	ptext SLIT("mflr"),
	char '\t',
	pprReg reg
    ]

pprInstr (FETCHPC reg) = vcat [
        ptext SLIT("\tbcl\t20,31,1f"),
        hcat [ ptext SLIT("1:\tmflr\t"), pprReg reg ]
    ]

pprInstr _ = panic "pprInstr (ppc)"

pprLogic op reg1 reg2 ri = hcat [
	char '\t',
	ptext op,
	case ri of
	    RIReg _ -> empty
	    RIImm _ -> char 'i',
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprRI ri
    ]
    
pprUnary op reg1 reg2 = hcat [
	char '\t',
	ptext op,
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2
    ]
    
pprBinaryF op sz reg1 reg2 reg3 = hcat [
	char '\t',
	ptext op,
	pprFSize sz,
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprReg reg3
    ]
    
pprRI :: RI -> Doc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprFSize F64 = empty
pprFSize F32 = char 's'

    -- limit immediate argument for shift instruction to range 0..32
    -- (yes, the maximum is really 32, not 31)
limitShiftRI :: RI -> RI
limitShiftRI (RIImm (ImmInt i)) | i > 32 || i < 0 = RIImm (ImmInt 32)
limitShiftRI x = x

#endif /* powerpc_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- Converting floating-point literals to integrals for printing

#if __GLASGOW_HASKELL__ >= 504
newFloatArray :: (Int,Int) -> ST s (STUArray s Int Float)
newFloatArray = newArray_

newDoubleArray :: (Int,Int) -> ST s (STUArray s Int Double)
newDoubleArray = newArray_

castFloatToCharArray :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToCharArray = castSTUArray

castDoubleToCharArray :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToCharArray = castSTUArray

writeFloatArray :: STUArray s Int Float -> Int -> Float -> ST s ()
writeFloatArray = writeArray

writeDoubleArray :: STUArray s Int Double -> Int -> Double -> ST s ()
writeDoubleArray = writeArray

readCharArray :: STUArray s Int Word8 -> Int -> ST s Char
readCharArray arr i = do 
  w <- readArray arr i
  return $! (chr (fromIntegral w))

#else

castFloatToCharArray :: MutableByteArray s t -> ST s (MutableByteArray s t)
castFloatToCharArray = return

castDoubleToCharArray :: MutableByteArray s t -> ST s (MutableByteArray s t)


castDoubleToCharArray = return

#endif

-- floatToBytes and doubleToBytes convert to the host's byte
-- order.  Providing that we're not cross-compiling for a 
-- target with the opposite endianness, this should work ok
-- on all targets.

-- ToDo: this stuff is very similar to the shenanigans in PprAbs,
-- could they be merged?

floatToBytes :: Float -> [Int]
floatToBytes f
   = runST (do
        arr <- newFloatArray ((0::Int),3)
        writeFloatArray arr 0 f
	arr <- castFloatToCharArray arr
        i0 <- readCharArray arr 0
        i1 <- readCharArray arr 1
        i2 <- readCharArray arr 2
        i3 <- readCharArray arr 3
        return (map ord [i0,i1,i2,i3])
     )

doubleToBytes :: Double -> [Int]
doubleToBytes d
   = runST (do
        arr <- newDoubleArray ((0::Int),7)
        writeDoubleArray arr 0 d
	arr <- castDoubleToCharArray arr
        i0 <- readCharArray arr 0
        i1 <- readCharArray arr 1
        i2 <- readCharArray arr 2
        i3 <- readCharArray arr 3
        i4 <- readCharArray arr 4
        i5 <- readCharArray arr 5
        i6 <- readCharArray arr 6
        i7 <- readCharArray arr 7
        return (map ord [i0,i1,i2,i3,i4,i5,i6,i7])
     )

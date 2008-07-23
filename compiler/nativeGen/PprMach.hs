{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

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
	pprNatCmmTop, pprBasicBlock, pprSectionHeader, pprData,
	pprInstr, pprSize, pprUserReg
  ) where

#include "HsVersions.h"

import BlockId
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
import Outputable	( Outputable )

import Data.Array.ST
import Data.Word	( Word8 )
import Control.Monad.ST
import Data.Char	( chr, ord )
import Data.Maybe       ( isJust )

#if powerpc_TARGET_ARCH || darwin_TARGET_OS
import Data.Word(Word32)
import Data.Bits
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
pprNatCmmTop (CmmProc [] lbl _ (ListGraph [])) = pprLabel lbl

pprNatCmmTop (CmmProc info lbl params (ListGraph blocks)) = 
  pprSectionHeader Text $$
  (if null info then -- blocks guaranteed not null, so label needed
       pprLabel lbl
   else
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
            pprCLabel_asm (mkDeadStripPreventer $ entryLblToInfoLbl lbl)
                <> char ':' $$
#endif
       vcat (map pprData info) $$
       pprLabel (entryLblToInfoLbl lbl)
  ) $$
  vcat (map pprBasicBlock blocks)
     -- above: Even the first block gets a label, because with branch-chain
     -- elimination, it might be the target of a goto.
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
	 0 -> sLit "$0";    1 -> sLit "$1";
	 2 -> sLit "$2";    3 -> sLit "$3";
	 4 -> sLit "$4";    5 -> sLit "$5";
	 6 -> sLit "$6";    7 -> sLit "$7";
	 8 -> sLit "$8";    9 -> sLit "$9";
	10 -> sLit "$10";  11 -> sLit "$11";
	12 -> sLit "$12";  13 -> sLit "$13";
	14 -> sLit "$14";  15 -> sLit "$15";
	16 -> sLit "$16";  17 -> sLit "$17";
	18 -> sLit "$18";  19 -> sLit "$19";
	20 -> sLit "$20";  21 -> sLit "$21";
	22 -> sLit "$22";  23 -> sLit "$23";
	24 -> sLit "$24";  25 -> sLit "$25";
	26 -> sLit "$26";  27 -> sLit "$27";
	28 -> sLit "$28";  29 -> sLit "$29";
	30 -> sLit "$30";  31 -> sLit "$31";
	32 -> sLit "$f0";  33 -> sLit "$f1";
	34 -> sLit "$f2";  35 -> sLit "$f3";
	36 -> sLit "$f4";  37 -> sLit "$f5";
	38 -> sLit "$f6";  39 -> sLit "$f7";
	40 -> sLit "$f8";  41 -> sLit "$f9";
	42 -> sLit "$f10"; 43 -> sLit "$f11";
	44 -> sLit "$f12"; 45 -> sLit "$f13";
	46 -> sLit "$f14"; 47 -> sLit "$f15";
	48 -> sLit "$f16"; 49 -> sLit "$f17";
	50 -> sLit "$f18"; 51 -> sLit "$f19";
	52 -> sLit "$f20"; 53 -> sLit "$f21";
	54 -> sLit "$f22"; 55 -> sLit "$f23";
	56 -> sLit "$f24"; 57 -> sLit "$f25";
	58 -> sLit "$f26"; 59 -> sLit "$f27";
	60 -> sLit "$f28"; 61 -> sLit "$f29";
	62 -> sLit "$f30"; 63 -> sLit "$f31";
	_  -> sLit "very naughty alpha register"
      })
#endif
#if i386_TARGET_ARCH
    ppr_reg_no :: MachRep -> Int -> Doc
    ppr_reg_no I8   = ppr_reg_byte
    ppr_reg_no I16  = ppr_reg_word
    ppr_reg_no _    = ppr_reg_long

    ppr_reg_byte i = ptext
      (case i of {
	 0 -> sLit "%al";     1 -> sLit "%bl";
	 2 -> sLit "%cl";     3 -> sLit "%dl";
	_  -> sLit "very naughty I386 byte register"
      })

    ppr_reg_word i = ptext
      (case i of {
	 0 -> sLit "%ax";     1 -> sLit "%bx";
	 2 -> sLit "%cx";     3 -> sLit "%dx";
	 4 -> sLit "%si";     5 -> sLit "%di";
	 6 -> sLit "%bp";     7 -> sLit "%sp";
	_  -> sLit "very naughty I386 word register"
      })

    ppr_reg_long i = ptext
      (case i of {
	 0 -> sLit "%eax";    1 -> sLit "%ebx";
	 2 -> sLit "%ecx";    3 -> sLit "%edx";
	 4 -> sLit "%esi";    5 -> sLit "%edi";
	 6 -> sLit "%ebp";    7 -> sLit "%esp";
	 8 -> sLit "%fake0";  9 -> sLit "%fake1";
	10 -> sLit "%fake2"; 11 -> sLit "%fake3";
	12 -> sLit "%fake4"; 13 -> sLit "%fake5";
	_  -> sLit "very naughty I386 register"
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
	 0 -> sLit "%al";     1 -> sLit "%bl";
	 2 -> sLit "%cl";     3 -> sLit "%dl";
	 4 -> sLit "%sil";    5 -> sLit "%dil"; -- new 8-bit regs!
	 6 -> sLit "%bpl";    7 -> sLit "%spl";
	 8 -> sLit "%r8b";    9  -> sLit "%r9b";
	10 -> sLit "%r10b";   11 -> sLit "%r11b";
	12 -> sLit "%r12b";   13 -> sLit "%r13b";
	14 -> sLit "%r14b";   15 -> sLit "%r15b";
	_  -> sLit "very naughty x86_64 byte register"
      })

    ppr_reg_word i = ptext
      (case i of {
	 0 -> sLit "%ax";     1 -> sLit "%bx";
	 2 -> sLit "%cx";     3 -> sLit "%dx";
	 4 -> sLit "%si";     5 -> sLit "%di";
	 6 -> sLit "%bp";     7 -> sLit "%sp";
	 8 -> sLit "%r8w";    9  -> sLit "%r9w";
	10 -> sLit "%r10w";   11 -> sLit "%r11w";
	12 -> sLit "%r12w";   13 -> sLit "%r13w";
	14 -> sLit "%r14w";   15 -> sLit "%r15w";
	_  -> sLit "very naughty x86_64 word register"
      })

    ppr_reg_long i = ptext
      (case i of {
	 0 -> sLit "%eax";    1  -> sLit "%ebx";
	 2 -> sLit "%ecx";    3  -> sLit "%edx";
	 4 -> sLit "%esi";    5  -> sLit "%edi";
	 6 -> sLit "%ebp";    7  -> sLit "%esp";
	 8 -> sLit "%r8d";    9  -> sLit "%r9d";
	10 -> sLit "%r10d";   11 -> sLit "%r11d";
	12 -> sLit "%r12d";   13 -> sLit "%r13d";
	14 -> sLit "%r14d";   15 -> sLit "%r15d";
	_  -> sLit "very naughty x86_64 register"
      })

    ppr_reg_quad i = ptext
      (case i of {
	 0 -> sLit "%rax";	1 -> sLit "%rbx";
	 2 -> sLit "%rcx";	3 -> sLit "%rdx";
	 4 -> sLit "%rsi";	5 -> sLit "%rdi";
	 6 -> sLit "%rbp";	7 -> sLit "%rsp";
	 8 -> sLit "%r8";  	9 -> sLit "%r9";
	10 -> sLit "%r10";    11 -> sLit "%r11";
	12 -> sLit "%r12";    13 -> sLit "%r13";
	14 -> sLit "%r14";    15 -> sLit "%r15";
	16 -> sLit "%xmm0";   17 -> sLit "%xmm1";
	18 -> sLit "%xmm2";   19 -> sLit "%xmm3";
	20 -> sLit "%xmm4";   21 -> sLit "%xmm5";
	22 -> sLit "%xmm6";   23 -> sLit "%xmm7";
	24 -> sLit "%xmm8";   25 -> sLit "%xmm9";
	26 -> sLit "%xmm10";  27 -> sLit "%xmm11";
	28 -> sLit "%xmm12";  29 -> sLit "%xmm13";
	30 -> sLit "%xmm14";  31 -> sLit "%xmm15";
	_  -> sLit "very naughty x86_64 register"
      })
#endif

#if sparc_TARGET_ARCH
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> sLit "%g0";   1 -> sLit "%g1";
	 2 -> sLit "%g2";   3 -> sLit "%g3";
	 4 -> sLit "%g4";   5 -> sLit "%g5";
	 6 -> sLit "%g6";   7 -> sLit "%g7";
	 8 -> sLit "%o0";   9 -> sLit "%o1";
	10 -> sLit "%o2";  11 -> sLit "%o3";
	12 -> sLit "%o4";  13 -> sLit "%o5";
	14 -> sLit "%o6";  15 -> sLit "%o7";
	16 -> sLit "%l0";  17 -> sLit "%l1";
	18 -> sLit "%l2";  19 -> sLit "%l3";
	20 -> sLit "%l4";  21 -> sLit "%l5";
	22 -> sLit "%l6";  23 -> sLit "%l7";
	24 -> sLit "%i0";  25 -> sLit "%i1";
	26 -> sLit "%i2";  27 -> sLit "%i3";
	28 -> sLit "%i4";  29 -> sLit "%i5";
	30 -> sLit "%i6";  31 -> sLit "%i7";
	32 -> sLit "%f0";  33 -> sLit "%f1";
	34 -> sLit "%f2";  35 -> sLit "%f3";
	36 -> sLit "%f4";  37 -> sLit "%f5";
	38 -> sLit "%f6";  39 -> sLit "%f7";
	40 -> sLit "%f8";  41 -> sLit "%f9";
	42 -> sLit "%f10"; 43 -> sLit "%f11";
	44 -> sLit "%f12"; 45 -> sLit "%f13";
	46 -> sLit "%f14"; 47 -> sLit "%f15";
	48 -> sLit "%f16"; 49 -> sLit "%f17";
	50 -> sLit "%f18"; 51 -> sLit "%f19";
	52 -> sLit "%f20"; 53 -> sLit "%f21";
	54 -> sLit "%f22"; 55 -> sLit "%f23";
	56 -> sLit "%f24"; 57 -> sLit "%f25";
	58 -> sLit "%f26"; 59 -> sLit "%f27";
	60 -> sLit "%f28"; 61 -> sLit "%f29";
	62 -> sLit "%f30"; 63 -> sLit "%f31";
	_  -> sLit "very naughty sparc register"
      })
#endif
#if powerpc_TARGET_ARCH
#if darwin_TARGET_OS
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> sLit "r0";   1 -> sLit "r1";
	 2 -> sLit "r2";   3 -> sLit "r3";
	 4 -> sLit "r4";   5 -> sLit "r5";
	 6 -> sLit "r6";   7 -> sLit "r7";
	 8 -> sLit "r8";   9 -> sLit "r9";
	10 -> sLit "r10";  11 -> sLit "r11";
	12 -> sLit "r12";  13 -> sLit "r13";
	14 -> sLit "r14";  15 -> sLit "r15";
	16 -> sLit "r16";  17 -> sLit "r17";
	18 -> sLit "r18";  19 -> sLit "r19";
	20 -> sLit "r20";  21 -> sLit "r21";
	22 -> sLit "r22";  23 -> sLit "r23";
	24 -> sLit "r24";  25 -> sLit "r25";
	26 -> sLit "r26";  27 -> sLit "r27";
	28 -> sLit "r28";  29 -> sLit "r29";
	30 -> sLit "r30";  31 -> sLit "r31";
	32 -> sLit "f0";  33 -> sLit "f1";
	34 -> sLit "f2";  35 -> sLit "f3";
	36 -> sLit "f4";  37 -> sLit "f5";
	38 -> sLit "f6";  39 -> sLit "f7";
	40 -> sLit "f8";  41 -> sLit "f9";
	42 -> sLit "f10"; 43 -> sLit "f11";
	44 -> sLit "f12"; 45 -> sLit "f13";
	46 -> sLit "f14"; 47 -> sLit "f15";
	48 -> sLit "f16"; 49 -> sLit "f17";
	50 -> sLit "f18"; 51 -> sLit "f19";
	52 -> sLit "f20"; 53 -> sLit "f21";
	54 -> sLit "f22"; 55 -> sLit "f23";
	56 -> sLit "f24"; 57 -> sLit "f25";
	58 -> sLit "f26"; 59 -> sLit "f27";
	60 -> sLit "f28"; 61 -> sLit "f29";
	62 -> sLit "f30"; 63 -> sLit "f31";
	_  -> sLit "very naughty powerpc register"
      })
#else
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i | i <= 31 = int i	-- GPRs
                 | i <= 63 = int (i-32) -- FPRs
                 | otherwise = ptext (sLit "very naughty powerpc register")
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
	 B  -> sLit "b"
	 Bu -> sLit "bu"
--	 W  -> sLit "w" UNUSED
--	 Wu -> sLit "wu" UNUSED
	 L  -> sLit "l"
	 Q  -> sLit "q"
--	 FF -> sLit "f" UNUSED
--	 DF -> sLit "d" UNUSED
--	 GF -> sLit "g" UNUSED
--	 SF -> sLit "s" UNUSED
	 TF -> sLit "t"
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	I8   -> sLit "b"
	I16  -> sLit "w"
	I32  -> sLit "l"
	I64  -> sLit "q"
#endif
#if i386_TARGET_ARCH
	F32  -> sLit "s"
	F64  -> sLit "l"
	F80  -> sLit "t"
#endif
#if x86_64_TARGET_ARCH
	F32  -> sLit "ss"	-- "scalar single-precision float" (SSE2)
	F64  -> sLit "sd"	-- "scalar double-precision float" (SSE2)
#endif
#if sparc_TARGET_ARCH
	I8   -> sLit "sb"
        I16   -> sLit "sh"
	I32   -> sLit ""
	F32   -> sLit ""
	F64  -> sLit "d"
    )
pprStSize :: MachRep -> Doc
pprStSize x = ptext (case x of
	I8   -> sLit "b"
	I16  -> sLit "h"
	I32  -> sLit ""
	F32  -> sLit ""
	F64  -> sLit "d"
#endif
#if powerpc_TARGET_ARCH
	I8   -> sLit "b"
        I16  -> sLit "h"
	I32  -> sLit "w"
	F32  -> sLit "fs"
	F64  -> sLit "fd"
#endif
    )

-- -----------------------------------------------------------------------------
-- pprCond: print a 'Cond'

pprCond :: Cond -> Doc

pprCond c = ptext (case c of {
#if alpha_TARGET_ARCH
	EQQ  -> sLit "eq";
	LTT  -> sLit "lt";
	LE  -> sLit "le";
	ULT -> sLit "ult";
	ULE -> sLit "ule";
	NE  -> sLit "ne";
	GTT  -> sLit "gt";
	GE  -> sLit "ge"
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	GEU	-> sLit "ae";	LU    -> sLit "b";
	EQQ	-> sLit "e";	GTT   -> sLit "g";
	GE	-> sLit "ge";	GU    -> sLit "a";
	LTT	-> sLit "l";	LE    -> sLit "le";
	LEU	-> sLit "be";	NE    -> sLit "ne";
	NEG	-> sLit "s";	POS   -> sLit "ns";
        CARRY   -> sLit "c";   OFLO  -> sLit "o";
	PARITY  -> sLit "p";   NOTPARITY -> sLit "np";
	ALWAYS	-> sLit "mp"	-- hack
#endif
#if sparc_TARGET_ARCH
	ALWAYS	-> sLit "";	NEVER -> sLit "n";
	GEU	-> sLit "geu";	LU    -> sLit "lu";
	EQQ	-> sLit "e";	GTT   -> sLit "g";
	GE	-> sLit "ge";	GU    -> sLit "gu";
	LTT	-> sLit "l";	LE    -> sLit "le";
	LEU	-> sLit "leu";	NE    -> sLit "ne";
	NEG	-> sLit "neg";	POS   -> sLit "pos";
	VC	-> sLit "vc";	VS    -> sLit "vs"
#endif
#if powerpc_TARGET_ARCH
	ALWAYS  -> sLit "";
	EQQ	-> sLit "eq";	NE    -> sLit "ne";
	LTT     -> sLit "lt";  GE    -> sLit "ge";
	GTT     -> sLit "gt";  LE    -> sLit "le";
	LU      -> sLit "lt";  GEU   -> sLit "ge";
	GU      -> sLit "gt";  LEU   -> sLit "le";
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

pprImm (ImmFloat _) = ptext (sLit "naughty float immediate")
pprImm (ImmDouble _) = ptext (sLit "naughty double immediate")

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
      (EABaseRip,   EAIndexNone) -> pp_off (ptext (sLit "%rip"))
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
  = pprReg r1 <+> ptext (sLit ", ") <+> pprReg r2

pprAddr (AddrRegImm r1 (ImmInt i)) = hcat [ int i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 (ImmInteger i)) = hcat [ integer i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 imm) = hcat [ pprImm imm, char '(', pprReg r1, char ')' ]
#endif


-- -----------------------------------------------------------------------------
-- pprData: print a 'CmmStatic'

pprSectionHeader Text
    = ptext
	(IF_ARCH_alpha(sLit "\t.text\n\t.align 3" {-word boundary-}
       ,IF_ARCH_sparc(sLit ".text\n\t.align 4" {-word boundary-}
       ,IF_ARCH_i386(IF_OS_darwin(sLit ".text\n\t.align 2",
                                  sLit ".text\n\t.align 4,0x90")
                                  {-needs per-OS variation!-}
       ,IF_ARCH_x86_64(IF_OS_darwin(sLit ".text\n.align 3",
                                    sLit ".text\n\t.align 8")
       ,IF_ARCH_powerpc(sLit ".text\n.align 2"
       ,))))))
pprSectionHeader Data
    = ptext
	 (IF_ARCH_alpha(sLit "\t.data\n\t.align 3"
	,IF_ARCH_sparc(sLit ".data\n\t.align 8" {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(sLit ".data\n\t.align 2",
                                   sLit ".data\n\t.align 4")
	,IF_ARCH_x86_64(IF_OS_darwin(sLit ".data\n.align 3",
	                             sLit ".data\n\t.align 8")
        ,IF_ARCH_powerpc(sLit ".data\n.align 2"
	,))))))
pprSectionHeader ReadOnlyData
    = ptext
	 (IF_ARCH_alpha(sLit "\t.data\n\t.align 3"
	,IF_ARCH_sparc(sLit ".data\n\t.align 8" {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(sLit ".const\n.align 2",
                                   sLit ".section .rodata\n\t.align 4")
	,IF_ARCH_x86_64(IF_OS_darwin(sLit ".const\n.align 3",
	                             sLit ".section .rodata\n\t.align 8")
        ,IF_ARCH_powerpc(IF_OS_darwin(sLit ".const\n.align 2",
                                      sLit ".section .rodata\n\t.align 2")
	,))))))
pprSectionHeader RelocatableReadOnlyData
    = ptext
	 (IF_ARCH_alpha(sLit "\t.data\n\t.align 3"
	,IF_ARCH_sparc(sLit ".data\n\t.align 8" {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(sLit ".const_data\n.align 2",
                                   sLit ".section .data\n\t.align 4")
	,IF_ARCH_x86_64(IF_OS_darwin(sLit ".const_data\n.align 3",
                                     sLit ".section .data\n\t.align 8")
        ,IF_ARCH_powerpc(IF_OS_darwin(sLit ".const_data\n.align 2",
                                      sLit ".data\n\t.align 2")
	,))))))
pprSectionHeader UninitialisedData
    = ptext
	 (IF_ARCH_alpha(sLit "\t.bss\n\t.align 3"
	,IF_ARCH_sparc(sLit ".bss\n\t.align 8" {-<8 will break double constants -}
	,IF_ARCH_i386(IF_OS_darwin(sLit ".data\n\t.align 2",
                                   sLit ".section .bss\n\t.align 4")
	,IF_ARCH_x86_64(IF_OS_darwin(sLit ".data\n\t.align 3",
                                     sLit ".section .bss\n\t.align 8")
        ,IF_ARCH_powerpc(IF_OS_darwin(sLit ".const_data\n.align 2",
                                      sLit ".section .bss\n\t.align 2")
	,))))))
pprSectionHeader ReadOnlyData16
    = ptext
	 (IF_ARCH_alpha(sLit "\t.data\n\t.align 4"
	,IF_ARCH_sparc(sLit ".data\n\t.align 16"
	,IF_ARCH_i386(IF_OS_darwin(sLit ".const\n.align 4",
                                   sLit ".section .rodata\n\t.align 16")
	,IF_ARCH_x86_64(IF_OS_darwin(sLit ".const\n.align 4",
	                             sLit ".section .rodata.cst16\n\t.align 16")
        ,IF_ARCH_powerpc(IF_OS_darwin(sLit ".const\n.align 4",
                                      sLit ".section .rodata\n\t.align 4")
	,))))))

pprSectionHeader (OtherSection sec)
    = panic "PprMach.pprSectionHeader: unknown section"

pprData :: CmmStatic -> Doc
pprData (CmmAlign bytes)         = pprAlign bytes
pprData (CmmDataLabel lbl)       = pprLabel lbl
pprData (CmmString str)          = pprASCII str
pprData (CmmUninitialised bytes) = ptext (sLit ".space ") <> int bytes
pprData (CmmStaticLit lit)       = pprDataItem lit

pprGloblDecl :: CLabel -> Doc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext IF_ARCH_sparc((sLit ".global "), 
				    (sLit ".globl ")) <>
		pprCLabel_asm lbl

pprTypeAndSizeDecl :: CLabel -> Doc
pprTypeAndSizeDecl lbl
#if linux_TARGET_OS
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext (sLit ".type ") <>
		pprCLabel_asm lbl <> ptext (sLit ", @object")
#else
  = empty
#endif

pprLabel :: CLabel -> Doc
pprLabel lbl = pprGloblDecl lbl $$ pprTypeAndSizeDecl lbl $$ (pprCLabel_asm lbl <> char ':')


pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> Doc
       do1 w = ptext (sLit "\t.byte\t") <> int (fromIntegral w)

pprAlign bytes =
	IF_ARCH_alpha(ptext (sLit ".align ") <> int pow2,
	IF_ARCH_i386(ptext (sLit ".align ") <> int IF_OS_darwin(pow2,bytes),
	IF_ARCH_x86_64(ptext (sLit ".align ") <> int IF_OS_darwin(pow2,bytes),
	IF_ARCH_sparc(ptext (sLit ".align ") <> int bytes,
	IF_ARCH_powerpc(ptext (sLit ".align ") <> int pow2,)))))
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
	ppr_item I8   x = [ptext (sLit "\t.byte\t") <> pprImm imm]
	ppr_item I32  x = [ptext (sLit "\t.long\t") <> pprImm imm]
	ppr_item F32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs
    	ppr_item F64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs

#if sparc_TARGET_ARCH
        -- copy n paste of x86 version
	ppr_item I16  x = [ptext (sLit "\t.short\t") <> pprImm imm]
	ppr_item I64  x = [ptext (sLit "\t.quad\t") <> pprImm imm]
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	ppr_item I16  x = [ptext (sLit "\t.word\t") <> pprImm imm]
#endif
#if i386_TARGET_ARCH && darwin_TARGET_OS
        ppr_item I64 (CmmInt x _)  =
                [ptext (sLit "\t.long\t")
                    <> int (fromIntegral (fromIntegral x :: Word32)),
                 ptext (sLit "\t.long\t")
                    <> int (fromIntegral
                        (fromIntegral (x `shiftR` 32) :: Word32))]
#endif
#if i386_TARGET_ARCH || (darwin_TARGET_OS && x86_64_TARGET_ARCH)
	ppr_item I64  x = [ptext (sLit "\t.quad\t") <> pprImm imm]
#endif
#if x86_64_TARGET_ARCH && !darwin_TARGET_OS
	-- x86_64: binutils can't handle the R_X86_64_PC64 relocation
	-- type, which means we can't do pc-relative 64-bit addresses.
	-- Fortunately we're assuming the small memory model, in which
	-- all such offsets will fit into 32 bits, so we have to stick
	-- to 32-bit offset fields and modify the RTS appropriately
        --
        -- See Note [x86-64-relative] in includes/InfoTables.h
	-- 
	ppr_item I64  x 
	   | isRelativeReloc x =
		[ptext (sLit "\t.long\t") <> pprImm imm,
		 ptext (sLit "\t.long\t0")]
	   | otherwise =
		[ptext (sLit "\t.quad\t") <> pprImm imm]
	   where
		isRelativeReloc (CmmLabelDiffOff _ _ _) = True
		isRelativeReloc _ = False
#endif
#if powerpc_TARGET_ARCH
	ppr_item I16 x = [ptext (sLit "\t.short\t") <> pprImm imm]
        ppr_item I64 (CmmInt x _)  =
                [ptext (sLit "\t.long\t")
                    <> int (fromIntegral 
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 ptext (sLit "\t.long\t")
                    <> int (fromIntegral (fromIntegral x :: Word32))]
#endif

-- fall through to rest of (machine-specific) pprInstr...

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr	 instr	= Outputable.docToSDoc $ pprInstr instr

pprInstr :: Instr -> Doc

--pprInstr (COMMENT s) = empty -- nuke 'em
pprInstr (COMMENT s)
   =  IF_ARCH_alpha( ((<>) (ptext (sLit "\t# ")) (ftext s))
     ,IF_ARCH_sparc( ((<>) (ptext (sLit "! "))   (ftext s))
     ,IF_ARCH_i386( ((<>) (ptext (sLit "# "))   (ftext s))
     ,IF_ARCH_x86_64( ((<>) (ptext (sLit "# "))   (ftext s))
     ,IF_ARCH_powerpc( IF_OS_linux(
        ((<>) (ptext (sLit "# ")) (ftext s)),
        ((<>) (ptext (sLit "; ")) (ftext s)))
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

pprInstr (SPILL reg slot)
   = hcat [
   	ptext (sLit "\tSPILL"),
	char '\t',
	pprReg reg,
	comma,
	ptext (sLit "SLOT") <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
   	ptext (sLit "\tRELOAD"),
	char '\t',
	ptext (sLit "SLOT") <> parens (int slot),
	comma,
	pprReg reg]

pprInstr (LD size reg addr)
  = hcat [
	ptext (sLit "\tld"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDA reg addr)
  = hcat [
	ptext (sLit "\tlda\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDAH reg addr)
  = hcat [
	ptext (sLit "\tldah\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDGP reg addr)
  = hcat [
	ptext (sLit "\tldgp\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDI size reg imm)
  = hcat [
	ptext (sLit "\tldi"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprImm imm
    ]

pprInstr (ST size reg addr)
  = hcat [
	ptext (sLit "\tst"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (CLR reg)
  = hcat [
	ptext (sLit "\tclr\t"),
	pprReg reg
    ]

pprInstr (ABS size ri reg)
  = hcat [
	ptext (sLit "\tabs"),
	pprSize size,
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (NEG size ov ri reg)
  = hcat [
	ptext (sLit "\tneg"),
	pprSize size,
	if ov then ptext (sLit "v\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (ADD size ov reg1 ri reg2)
  = hcat [
	ptext (sLit "\tadd"),
	pprSize size,
	if ov then ptext (sLit "v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SADD size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> (sLit "\ts4");-} Q -> (sLit "\ts8")}),
	ptext (sLit "add"),
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
	ptext (sLit "\tsub"),
	pprSize size,
	if ov then ptext (sLit "v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SSUB size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> (sLit "\ts4");-} Q -> (sLit "\ts8")}),
	ptext (sLit "sub"),
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
	ptext (sLit "\tmul"),
	pprSize size,
	if ov then ptext (sLit "v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (DIV size uns reg1 ri reg2)
  = hcat [
	ptext (sLit "\tdiv"),
	pprSize size,
	if uns then ptext (sLit "u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (REM size uns reg1 ri reg2)
  = hcat [
	ptext (sLit "\trem"),
	pprSize size,
	if uns then ptext (sLit "u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (NOT ri reg)
  = hcat [
	ptext (sLit "\tnot"),
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (AND reg1 ri reg2) = pprRegRIReg (sLit "and") reg1 ri reg2
pprInstr (ANDNOT reg1 ri reg2) = pprRegRIReg (sLit "andnot") reg1 ri reg2
pprInstr (OR reg1 ri reg2) = pprRegRIReg (sLit "or") reg1 ri reg2
pprInstr (ORNOT reg1 ri reg2) = pprRegRIReg (sLit "ornot") reg1 ri reg2
pprInstr (XOR reg1 ri reg2) = pprRegRIReg (sLit "xor") reg1 ri reg2
pprInstr (XORNOT reg1 ri reg2) = pprRegRIReg (sLit "xornot") reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg (sLit "sll") reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg (sLit "srl") reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg (sLit "sra") reg1 ri reg2

pprInstr (ZAP reg1 ri reg2) = pprRegRIReg (sLit "zap") reg1 ri reg2
pprInstr (ZAPNOT reg1 ri reg2) = pprRegRIReg (sLit "zapnot") reg1 ri reg2

pprInstr (NOP) = ptext (sLit "\tnop")

pprInstr (CMP cond reg1 ri reg2)
  = hcat [
	ptext (sLit "\tcmp"),
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
	ptext (sLit "\tfclr\t"),
	pprReg reg
    ]

pprInstr (FABS reg1 reg2)
  = hcat [
	ptext (sLit "\tfabs\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FNEG size reg1 reg2)
  = hcat [
	ptext (sLit "\tneg"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FADD size reg1 reg2 reg3) = pprSizeRegRegReg (sLit "add") size reg1 reg2 reg3
pprInstr (FDIV size reg1 reg2 reg3) = pprSizeRegRegReg (sLit "div") size reg1 reg2 reg3
pprInstr (FMUL size reg1 reg2 reg3) = pprSizeRegRegReg (sLit "mul") size reg1 reg2 reg3
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg (sLit "sub") size reg1 reg2 reg3

pprInstr (CVTxy size1 size2 reg1 reg2)
  = hcat [
	ptext (sLit "\tcvt"),
	pprSize size1,
	case size2 of {Q -> ptext (sLit "qc"); _ -> pprSize size2},
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FCMP size cond reg1 reg2 reg3)
  = hcat [
	ptext (sLit "\tcmp"),
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
	ptext (sLit "\tfmov\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (BI ALWAYS reg lab) = pprInstr (BR lab)

pprInstr (BI NEVER reg lab) = empty

pprInstr (BI cond reg lab)
  = hcat [
	ptext (sLit "\tb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BF cond reg lab)
  = hcat [
	ptext (sLit "\tfb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BR lab)
  = (<>) (ptext (sLit "\tbr\t")) (pprImm lab)

pprInstr (JMP reg addr hint)
  = hcat [
	ptext (sLit "\tjmp\t"),
	pprReg reg,
	comma,
	pprAddr addr,
	comma,
	int hint
    ]

pprInstr (BSR imm n)
  = (<>) (ptext (sLit "\tbsr\t")) (pprImm imm)

pprInstr (JSR reg addr n)
  = hcat [
	ptext (sLit "\tjsr\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (FUNBEGIN clab)
  = hcat [
	if (externallyVisibleCLabel clab) then
	    hcat [ptext (sLit "\t.globl\t"), pp_lab, char '\n']
	else
	    empty,
	ptext (sLit "\t.ent "),
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
	pp_ldgp  = hcat [ ptext (sLit ":\n\tldgp $29"), char ',', ptext (sLit "0($27)\n") ]
	pp_frame = hcat [ ptext (sLit "..ng:\n\t.frame $30"), char ',',
                          ptext (sLit "4240"), char ',',
                          ptext (sLit "$26"), char ',',
                          ptext (sLit "0\n\t.prologue 1") ]

pprInstr (FUNEND clab)
  = (<>) (ptext (sLit "\t.align 4\n\t.end ")) (pprCLabel_asm clab)
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

pprInstr (SPILL reg slot)
   = hcat [
   	ptext (sLit "\tSPILL"),
	char ' ',
	pprUserReg reg,
	comma,
	ptext (sLit "SLOT") <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
   	ptext (sLit "\tRELOAD"),
	char ' ',
	ptext (sLit "SLOT") <> parens (int slot),
	comma,
	pprUserReg reg]

pprInstr (MOV size src dst)
  = pprSizeOpOp (sLit "mov") size src dst

pprInstr (MOVZxL I32 src dst) = pprSizeOpOp (sLit "mov") I32 src dst
	-- 32-to-64 bit zero extension on x86_64 is accomplished by a simple
	-- movl.  But we represent it as a MOVZxL instruction, because
	-- the reg alloc would tend to throw away a plain reg-to-reg
	-- move, and we still want it to do that.

pprInstr (MOVZxL sizes src dst) = pprSizeOpOpCoerce (sLit "movz") sizes I32 src dst
	-- zero-extension only needs to extend to 32 bits: on x86_64, 
	-- the remaining zero-extension to 64 bits is automatic, and the 32-bit
	-- instruction is shorter.

pprInstr (MOVSxL sizes src dst) = pprSizeOpOpCoerce (sLit "movs") sizes wordRep src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg1 == reg3
  = pprSizeOpOp (sLit "add") size (OpReg reg2) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg2 == reg3
  = pprSizeOpOp (sLit "add") size (OpReg reg1) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(EABaseReg reg1) EAIndexNone displ)) dst@(OpReg reg3))
  | reg1 == reg3
  = pprInstr (ADD size (OpImm displ) dst)
pprInstr (LEA size src dst) = pprSizeOpOp (sLit "lea") size src dst

pprInstr (ADD size (OpImm (ImmInt (-1))) dst)
  = pprSizeOp (sLit "dec") size dst
pprInstr (ADD size (OpImm (ImmInt 1)) dst)
  = pprSizeOp (sLit "inc") size dst
pprInstr (ADD size src dst)
  = pprSizeOpOp (sLit "add") size src dst
pprInstr (ADC size src dst)
  = pprSizeOpOp (sLit "adc") size src dst
pprInstr (SUB size src dst) = pprSizeOpOp (sLit "sub") size src dst
pprInstr (IMUL size op1 op2) = pprSizeOpOp (sLit "imul") size op1 op2

{- A hack.  The Intel documentation says that "The two and three
   operand forms [of IMUL] may also be used with unsigned operands
   because the lower half of the product is the same regardless if
   (sic) the operands are signed or unsigned.  The CF and OF flags,
   however, cannot be used to determine if the upper half of the
   result is non-zero."  So there.  
-} 
pprInstr (AND size src dst) = pprSizeOpOp (sLit "and") size src dst
pprInstr (OR  size src dst) = pprSizeOpOp (sLit "or")  size src dst

pprInstr (XOR F32 src dst)  = pprOpOp (sLit "xorps") F32 src dst
pprInstr (XOR F64 src dst)  = pprOpOp (sLit "xorpd") F64 src dst
pprInstr (XOR size src dst) = pprSizeOpOp (sLit "xor")  size src dst

pprInstr (NOT size op) = pprSizeOp (sLit "not") size op
pprInstr (NEGI size op) = pprSizeOp (sLit "neg") size op

pprInstr (SHL size src dst) = pprShift (sLit "shl") size src dst
pprInstr (SAR size src dst) = pprShift (sLit "sar") size src dst
pprInstr (SHR size src dst) = pprShift (sLit "shr") size src dst

pprInstr (BT  size imm src) = pprSizeImmOp (sLit "bt") size imm src

pprInstr (CMP size src dst) 
  | isFloatingRep size =  pprSizeOpOp (sLit "ucomi")  size src dst -- SSE2
  | otherwise          =  pprSizeOpOp (sLit "cmp")  size src dst

pprInstr (TEST size src dst) = pprSizeOpOp (sLit "test")  size src dst
pprInstr (PUSH size op) = pprSizeOp (sLit "push") size op
pprInstr (POP size op) = pprSizeOp (sLit "pop") size op

-- both unused (SDM):
-- pprInstr PUSHA = ptext (sLit "\tpushal")
-- pprInstr POPA = ptext (sLit "\tpopal")

pprInstr NOP = ptext (sLit "\tnop")
pprInstr (CLTD I32) = ptext (sLit "\tcltd")
pprInstr (CLTD I64) = ptext (sLit "\tcqto")

pprInstr (SETCC cond op) = pprCondInstr (sLit "set") cond (pprOperand I8 op)

pprInstr (JXX cond (BlockId id)) 
  = pprCondInstr (sLit "j") cond (pprCLabel_asm lab)
  where lab = mkAsmTempLabel id

pprInstr (JXX_GBL cond imm) = pprCondInstr (sLit "j") cond (pprImm imm)

pprInstr (JMP (OpImm imm)) = (<>) (ptext (sLit "\tjmp ")) (pprImm imm)
pprInstr (JMP op)          = (<>) (ptext (sLit "\tjmp *")) (pprOperand wordRep op)
pprInstr (JMP_TBL op ids)  = pprInstr (JMP op)
pprInstr (CALL (Left imm) _)    = (<>) (ptext (sLit "\tcall ")) (pprImm imm)
pprInstr (CALL (Right reg) _)   = (<>) (ptext (sLit "\tcall *")) (pprReg wordRep reg)

pprInstr (IDIV sz op)	= pprSizeOp (sLit "idiv") sz op
pprInstr (DIV sz op)    = pprSizeOp (sLit "div")  sz op
pprInstr (IMUL2 sz op)  = pprSizeOp (sLit "imul") sz op

#if x86_64_TARGET_ARCH
pprInstr (MUL size op1 op2) = pprSizeOpOp (sLit "mul") size op1 op2

pprInstr (FDIV size op1 op2) = pprSizeOpOp (sLit "div") size op1 op2

pprInstr (CVTSS2SD from to)   = pprRegReg (sLit "cvtss2sd") from to
pprInstr (CVTSD2SS from to)   = pprRegReg (sLit "cvtsd2ss") from to
pprInstr (CVTTSS2SIQ from to) = pprOpReg  (sLit "cvttss2siq") from to
pprInstr (CVTTSD2SIQ from to) = pprOpReg  (sLit "cvttsd2siq") from to
pprInstr (CVTSI2SS from to)   = pprOpReg  (sLit "cvtsi2ssq") from to
pprInstr (CVTSI2SD from to)   = pprOpReg  (sLit "cvtsi2sdq") from to
#endif

    -- FETCHGOT for PIC on ELF platforms
pprInstr (FETCHGOT reg)
   = vcat [ ptext (sLit "\tcall 1f"),
            hcat [ ptext (sLit "1:\tpopl\t"), pprReg I32 reg ],
            hcat [ ptext (sLit "\taddl\t$_GLOBAL_OFFSET_TABLE_+(.-1b), "),
                   pprReg I32 reg ]
          ]

    -- FETCHPC for PIC on Darwin/x86
    -- get the instruction pointer into a register
    -- (Terminology note: the IP is called Program Counter on PPC,
    --  and it's a good thing to use the same name on both platforms)
pprInstr (FETCHPC reg)
   = vcat [ ptext (sLit "\tcall 1f"),
            hcat [ ptext (sLit "1:\tpopl\t"), pprReg I32 reg ]
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
   = pprG g (vcat [
         hcat [gtab, text "subl $8, %esp ; fnstcw 4(%esp)"],
         hcat [gtab, gpush src 0],
         hcat [gtab, text "movzwl 4(%esp), ", reg,
                     text " ; orl $0xC00, ", reg],
         hcat [gtab, text "movl ", reg, text ", 0(%esp) ; fldcw 0(%esp)"],
         hcat [gtab, text "fistpl 0(%esp)"],
         hcat [gtab, text "fldcw 4(%esp) ; movl 0(%esp), ", reg],
         hcat [gtab, text "addl $8, %esp"]
     ])
   where
     reg = pprReg I32 dst

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
pprInstr g@(GSIN sz l1 l2 src dst)
   = pprG g (pprTrigOp "fsin" False l1 l2 src dst sz)
pprInstr g@(GCOS sz l1 l2 src dst)
   = pprG g (pprTrigOp "fcos" False l1 l2 src dst sz)
pprInstr g@(GTAN sz l1 l2 src dst)
   = pprG g (pprTrigOp "fptan" True l1 l2 src dst sz)

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
   = vcat [ ptext (sLit "\tffree %st(0) ;ffree %st(1) ;ffree %st(2) ;ffree %st(3)"),
            ptext (sLit "\tffree %st(4) ;ffree %st(5) ;ffree %st(6) ;ffree %st(7)") 
          ]

pprTrigOp :: String -> Bool -> CLabel -> CLabel -> Reg -> Reg -> MachRep -> Doc
pprTrigOp op -- fsin, fcos or fptan
          isTan -- we need a couple of extra steps if we're doing tan
          l1 l2 -- internal labels for us to use
          src dst sz
    = -- We'll be needing %eax later on
      hcat [gtab, text "pushl %eax;"] $$
      -- tan is going to use an extra space on the FP stack
      (if isTan then hcat [gtab, text "ffree %st(6)"] else empty) $$
      -- First put the value in %st(0) and try to apply the op to it
      hcat [gpush src 0, text ("; " ++ op)] $$
      -- Now look to see if C2 was set (overflow, |value| >= 2^63)
      hcat [gtab, text "fnstsw %ax"] $$
      hcat [gtab, text "test   $0x400,%eax"] $$
      -- If we were in bounds then jump to the end
      hcat [gtab, text "je     " <> pprCLabel_asm l1] $$
      -- Otherwise we need to shrink the value. Start by
      -- loading pi, doubleing it (by adding it to itself),
      -- and then swapping pi with the value, so the value we
      -- want to apply op to is in %st(0) again
      hcat [gtab, text "ffree %st(7); fldpi"] $$
      hcat [gtab, text "fadd   %st(0),%st"] $$
      hcat [gtab, text "fxch   %st(1)"] $$
      -- Now we have a loop in which we make the value smaller,
      -- see if it's small enough, and loop if not
      (pprCLabel_asm l2 <> char ':') $$
      hcat [gtab, text "fprem1"] $$
      -- My Debian libc uses fstsw here for the tan code, but I can't
      -- see any reason why it should need to be different for tan.
      hcat [gtab, text "fnstsw %ax"] $$
      hcat [gtab, text "test   $0x400,%eax"] $$
      hcat [gtab, text "jne    " <> pprCLabel_asm l2] $$
      hcat [gtab, text "fstp   %st(1)"] $$
      hcat [gtab, text op] $$
      (pprCLabel_asm l1 <> char ':') $$
      -- Pop the 1.0 tan gave us
      (if isTan then hcat [gtab, text "fstp %st(0)"] else empty) $$
      -- Restore %eax
      hcat [gtab, text "popl %eax;"] $$
      -- And finally make the result the right size
      hcat [gtab, gcoerceto sz, gpop dst 1]

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

pprGInstr (GMOV src dst)   = pprSizeRegReg (sLit "gmov") F64 src dst
pprGInstr (GLD sz src dst) = pprSizeAddrReg (sLit "gld") sz src dst
pprGInstr (GST sz src dst) = pprSizeRegAddr (sLit "gst") sz src dst

pprGInstr (GLDZ dst) = pprSizeReg (sLit "gldz") F64 dst
pprGInstr (GLD1 dst) = pprSizeReg (sLit "gld1") F64 dst

pprGInstr (GFTOI src dst) = pprSizeSizeRegReg (sLit "gftoi") F32 I32  src dst
pprGInstr (GDTOI src dst) = pprSizeSizeRegReg (sLit "gdtoi") F64 I32 src dst

pprGInstr (GITOF src dst) = pprSizeSizeRegReg (sLit "gitof") I32 F32  src dst
pprGInstr (GITOD src dst) = pprSizeSizeRegReg (sLit "gitod") I32 F64 src dst

pprGInstr (GCMP co src dst) = pprCondRegReg (sLit "gcmp_") F64 co src dst
pprGInstr (GABS sz src dst) = pprSizeRegReg (sLit "gabs") sz src dst
pprGInstr (GNEG sz src dst) = pprSizeRegReg (sLit "gneg") sz src dst
pprGInstr (GSQRT sz src dst) = pprSizeRegReg (sLit "gsqrt") sz src dst
pprGInstr (GSIN sz _ _ src dst) = pprSizeRegReg (sLit "gsin") sz src dst
pprGInstr (GCOS sz _ _ src dst) = pprSizeRegReg (sLit "gcos") sz src dst
pprGInstr (GTAN sz _ _ src dst) = pprSizeRegReg (sLit "gtan") sz src dst

pprGInstr (GADD sz src1 src2 dst) = pprSizeRegRegReg (sLit "gadd") sz src1 src2 dst
pprGInstr (GSUB sz src1 src2 dst) = pprSizeRegRegReg (sLit "gsub") sz src1 src2 dst
pprGInstr (GMUL sz src1 src2 dst) = pprSizeRegRegReg (sLit "gmul") sz src1 src2 dst
pprGInstr (GDIV sz src1 src2 dst) = pprSizeRegRegReg (sLit "gdiv") sz src1 src2 dst
#endif

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH

-- Continue with I386-only printing bits and bobs:

pprDollImm :: Imm -> Doc

pprDollImm i =  ptext (sLit "$") <> pprImm i

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

pprInstr (SPILL reg slot)
   = hcat [
   	ptext (sLit "\tSPILL"),
	char '\t',
	pprReg reg,
	comma,
	ptext (sLit "SLOT") <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
   	ptext (sLit "\tRELOAD"),
	char '\t',
	ptext (sLit "SLOT") <> parens (int slot),
	comma,
	pprReg reg]

-- Translate to the following:
--    add g1,g2,g1
--    ld  [g1],%fn
--    ld  [g1+4],%f(n+1)
--    sub g1,g2,g1           -- to restore g1

pprInstr (LD F64 (AddrRegReg g1 g2) reg)
  = vcat [
       hcat [ptext (sLit "\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1],
       hcat [pp_ld_lbracket, pprReg g1, pp_rbracket_comma, pprReg reg],
       hcat [pp_ld_lbracket, pprReg g1, ptext (sLit "+4]"), comma, pprReg (fPair reg)],
       hcat [ptext (sLit "\tsub\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1]
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
       ptext (sLit "\tld"),
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
       hcat [ptext (sLit "\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1],
       hcat [ptext (sLit "\tst\t"), pprReg reg, pp_comma_lbracket, 
             pprReg g1,	rbrack],
       hcat [ptext (sLit "\tst\t"), pprReg (fPair reg), pp_comma_lbracket,
             pprReg g1, ptext (sLit "+4]")],
       hcat [ptext (sLit "\tsub\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1]
    ]

-- Translate to
--    st  %fn,[addr]
--    st  %f(n+1),[addr+4]
pprInstr (ST F64 reg addr) | isJust off_addr 
 = vcat [
      hcat [ptext (sLit "\tst\t"), pprReg reg, pp_comma_lbracket, 
            pprAddr addr, rbrack],
      hcat [ptext (sLit "\tst\t"), pprReg (fPair reg), pp_comma_lbracket,
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
       ptext (sLit "\tst"),
       pprStSize size,
       char '\t',
       pprReg reg,
       pp_comma_lbracket,
       pprAddr addr,
       rbrack
    ]

pprInstr (ADD x cc reg1 ri reg2)
  | not x && not cc && riZero ri
  = hcat [ ptext (sLit "\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then sLit "addx" else sLit "add") cc reg1 ri reg2

pprInstr (SUB x cc reg1 ri reg2)
  | not x && cc && reg2 == g0
  = hcat [ ptext (sLit "\tcmp\t"), pprReg reg1, comma, pprRI ri ]
  | not x && not cc && riZero ri
  = hcat [ ptext (sLit "\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then sLit "subx" else sLit "sub") cc reg1 ri reg2

pprInstr (AND  b reg1 ri reg2) = pprRegRIReg (sLit "and")  b reg1 ri reg2
pprInstr (ANDN b reg1 ri reg2) = pprRegRIReg (sLit "andn") b reg1 ri reg2

pprInstr (OR b reg1 ri reg2)
  | not b && reg1 == g0
  = let doit = hcat [ ptext (sLit "\tmov\t"), pprRI ri, comma, pprReg reg2 ]
    in  case ri of
           RIReg rrr | rrr == reg2 -> empty
           other                   -> doit
  | otherwise
  = pprRegRIReg (sLit "or") b reg1 ri reg2

pprInstr (ORN b reg1 ri reg2) = pprRegRIReg (sLit "orn") b reg1 ri reg2

pprInstr (XOR  b reg1 ri reg2) = pprRegRIReg (sLit "xor")  b reg1 ri reg2
pprInstr (XNOR b reg1 ri reg2) = pprRegRIReg (sLit "xnor") b reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg (sLit "sll") False reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg (sLit "srl") False reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg (sLit "sra") False reg1 ri reg2

pprInstr (RDY rd) = ptext (sLit "\trd\t%y,") <> pprReg rd
pprInstr (SMUL b reg1 ri reg2) = pprRegRIReg (sLit "smul")  b reg1 ri reg2
pprInstr (UMUL b reg1 ri reg2) = pprRegRIReg (sLit "umul")  b reg1 ri reg2

pprInstr (SETHI imm reg)
  = hcat [
	ptext (sLit "\tsethi\t"),
	pprImm imm,
	comma,
	pprReg reg
    ]

pprInstr NOP = ptext (sLit "\tnop")

pprInstr (FABS F32 reg1 reg2) = pprSizeRegReg (sLit "fabs") F32 reg1 reg2
pprInstr (FABS F64 reg1 reg2)
  = (<>) (pprSizeRegReg (sLit "fabs") F32 reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg (sLit "fmov") F32 (fPair reg1) (fPair reg2)))

pprInstr (FADD size reg1 reg2 reg3)
  = pprSizeRegRegReg (sLit "fadd") size reg1 reg2 reg3
pprInstr (FCMP e size reg1 reg2)
  = pprSizeRegReg (if e then sLit "fcmpe" else sLit "fcmp") size reg1 reg2
pprInstr (FDIV size reg1 reg2 reg3)
  = pprSizeRegRegReg (sLit "fdiv") size reg1 reg2 reg3

pprInstr (FMOV F32 reg1 reg2) = pprSizeRegReg (sLit "fmov") F32 reg1 reg2
pprInstr (FMOV F64 reg1 reg2)
  = (<>) (pprSizeRegReg (sLit "fmov") F32 reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg (sLit "fmov") F32 (fPair reg1) (fPair reg2)))

pprInstr (FMUL size reg1 reg2 reg3)
  = pprSizeRegRegReg (sLit "fmul") size reg1 reg2 reg3

pprInstr (FNEG F32 reg1 reg2) = pprSizeRegReg (sLit "fneg") F32 reg1 reg2
pprInstr (FNEG F64 reg1 reg2)
  = (<>) (pprSizeRegReg (sLit "fneg") F32 reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg (sLit "fmov") F32 (fPair reg1) (fPair reg2)))

pprInstr (FSQRT size reg1 reg2)     = pprSizeRegReg (sLit "fsqrt") size reg1 reg2
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg (sLit "fsub") size reg1 reg2 reg3
pprInstr (FxTOy size1 size2 reg1 reg2)
  = hcat [
    	ptext (sLit "\tf"),
	ptext
    	(case size1 of
    	    I32  -> sLit "ito"
    	    F32  -> sLit "sto"
    	    F64  -> sLit "dto"),
	ptext
    	(case size2 of
    	    I32  -> sLit "i\t"
    	    F32  -> sLit "s\t"
    	    F64  -> sLit "d\t"),
	pprReg reg1, comma, pprReg reg2
    ]


pprInstr (BI cond b lab)
  = hcat [
	ptext (sLit "\tb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (BF cond b lab)
  = hcat [
	ptext (sLit "\tfb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (JMP addr) = (<>) (ptext (sLit "\tjmp\t")) (pprAddr addr)

pprInstr (CALL (Left imm) n _)
  = hcat [ ptext (sLit "\tcall\t"), pprImm imm, comma, int n ]
pprInstr (CALL (Right reg) n _)
  = hcat [ ptext (sLit "\tcall\t"), pprReg reg, comma, int n ]

pprRI :: RI -> Doc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprSizeRegReg :: LitString -> MachRep -> Reg -> Reg -> Doc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F32  -> ptext (sLit "s\t")
    	    F64 -> ptext (sLit "d\t")),
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
    	    F32  -> ptext (sLit "s\t")
    	    F64  -> ptext (sLit "d\t")),
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
	if b then ptext (sLit "cc\t") else char '\t',
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
	if b then ptext (sLit "cc\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg1
    ]

pp_ld_lbracket    = ptext (sLit "\tld\t[")
pp_rbracket_comma = text "],"
pp_comma_lbracket = text ",["
pp_comma_a	  = text ",a"

#endif /* sparc_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- pprInstr for PowerPC

#if powerpc_TARGET_ARCH

pprInstr (SPILL reg slot)
   = hcat [
   	ptext (sLit "\tSPILL"),
	char '\t',
	pprReg reg,
	comma,
	ptext (sLit "SLOT") <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
   	ptext (sLit "\tRELOAD"),
	char '\t',
	ptext (sLit "SLOT") <> parens (int slot),
	comma,
	pprReg reg]

pprInstr (LD sz reg addr) = hcat [
	char '\t',
	ptext (sLit "l"),
	ptext (case sz of
	    I8  -> sLit "bz"
	    I16 -> sLit "hz"
	    I32 -> sLit "wz"
	    F32 -> sLit "fs"
	    F64 -> sLit "fd"),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	char '\t',
	pprReg reg,
	ptext (sLit ", "),
	pprAddr addr
    ]
pprInstr (LA sz reg addr) = hcat [
	char '\t',
	ptext (sLit "l"),
	ptext (case sz of
	    I8  -> sLit "ba"
	    I16 -> sLit "ha"
	    I32 -> sLit "wa"
	    F32 -> sLit "fs"
	    F64 -> sLit "fd"),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	char '\t',
	pprReg reg,
	ptext (sLit ", "),
	pprAddr addr
    ]
pprInstr (ST sz reg addr) = hcat [
	char '\t',
	ptext (sLit "st"),
	pprSize sz,
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	char '\t',
	pprReg reg,
	ptext (sLit ", "),
	pprAddr addr
    ]
pprInstr (STU sz reg addr) = hcat [
	char '\t',
	ptext (sLit "st"),
	pprSize sz,
	ptext (sLit "u\t"),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
	pprReg reg,
	ptext (sLit ", "),
	pprAddr addr
    ]
pprInstr (LIS reg imm) = hcat [
	char '\t',
	ptext (sLit "lis"),
	char '\t',
	pprReg reg,
	ptext (sLit ", "),
	pprImm imm
    ]
pprInstr (LI reg imm) = hcat [
	char '\t',
	ptext (sLit "li"),
	char '\t',
	pprReg reg,
	ptext (sLit ", "),
	pprImm imm
    ]
pprInstr (MR reg1 reg2) 
    | reg1 == reg2 = empty
    | otherwise = hcat [
	char '\t',
	case regClass reg1 of
	    RcInteger -> ptext (sLit "mr")
	    _ -> ptext (sLit "fmr"),
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2
    ]
pprInstr (CMP sz reg ri) = hcat [
	char '\t',
	op,
	char '\t',
	pprReg reg,
	ptext (sLit ", "),
	pprRI ri
    ]
    where
	op = hcat [
		ptext (sLit "cmp"),
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
	ptext (sLit ", "),
	pprRI ri
    ]
    where
	op = hcat [
		ptext (sLit "cmpl"),
		pprSize sz,
		case ri of
		    RIReg _ -> empty
		    RIImm _ -> char 'i'
	    ]
pprInstr (BCC cond (BlockId id)) = hcat [
	char '\t',
	ptext (sLit "b"),
	pprCond cond,
	char '\t',
	pprCLabel_asm lbl
    ]
    where lbl = mkAsmTempLabel id

pprInstr (BCCFAR cond (BlockId id)) = vcat [
        hcat [
            ptext (sLit "\tb"),
            pprCond (condNegate cond),
            ptext (sLit "\t$+8")
        ],
        hcat [
            ptext (sLit "\tb\t"),
            pprCLabel_asm lbl
        ]
    ]
    where lbl = mkAsmTempLabel id

pprInstr (JMP lbl) = hcat [ -- an alias for b that takes a CLabel
	char '\t',
	ptext (sLit "b"),
	char '\t',
	pprCLabel_asm lbl
    ]

pprInstr (MTCTR reg) = hcat [
	char '\t',
	ptext (sLit "mtctr"),
	char '\t',
	pprReg reg
    ]
pprInstr (BCTR _) = hcat [
	char '\t',
	ptext (sLit "bctr")
    ]
pprInstr (BL lbl _) = hcat [
	ptext (sLit "\tbl\t"),
        pprCLabel_asm lbl
    ]
pprInstr (BCTRL _) = hcat [
	char '\t',
	ptext (sLit "bctrl")
    ]
pprInstr (ADD reg1 reg2 ri) = pprLogic (sLit "add") reg1 reg2 ri
pprInstr (ADDIS reg1 reg2 imm) = hcat [
	char '\t',
	ptext (sLit "addis"),
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2,
	ptext (sLit ", "),
	pprImm imm
    ]

pprInstr (ADDC reg1 reg2 reg3) = pprLogic (sLit "addc") reg1 reg2 (RIReg reg3)
pprInstr (ADDE reg1 reg2 reg3) = pprLogic (sLit "adde") reg1 reg2 (RIReg reg3)
pprInstr (SUBF reg1 reg2 reg3) = pprLogic (sLit "subf") reg1 reg2 (RIReg reg3)
pprInstr (MULLW reg1 reg2 ri@(RIReg _)) = pprLogic (sLit "mullw") reg1 reg2 ri
pprInstr (MULLW reg1 reg2 ri@(RIImm _)) = pprLogic (sLit "mull") reg1 reg2 ri
pprInstr (DIVW reg1 reg2 reg3) = pprLogic (sLit "divw") reg1 reg2 (RIReg reg3)
pprInstr (DIVWU reg1 reg2 reg3) = pprLogic (sLit "divwu") reg1 reg2 (RIReg reg3)

pprInstr (MULLW_MayOflo reg1 reg2 reg3) = vcat [
         hcat [ ptext (sLit "\tmullwo\t"), pprReg reg1, ptext (sLit ", "),
                                          pprReg reg2, ptext (sLit ", "),
                                          pprReg reg3 ],
         hcat [ ptext (sLit "\tmfxer\t"),  pprReg reg1 ],
         hcat [ ptext (sLit "\trlwinm\t"), pprReg reg1, ptext (sLit ", "),
                                          pprReg reg1, ptext (sLit ", "),
                                          ptext (sLit "2, 31, 31") ]
    ]

    	-- for some reason, "andi" doesn't exist.
	-- we'll use "andi." instead.
pprInstr (AND reg1 reg2 (RIImm imm)) = hcat [
	char '\t',
	ptext (sLit "andi."),
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2,
	ptext (sLit ", "),
	pprImm imm
    ]
pprInstr (AND reg1 reg2 ri) = pprLogic (sLit "and") reg1 reg2 ri

pprInstr (OR reg1 reg2 ri) = pprLogic (sLit "or") reg1 reg2 ri
pprInstr (XOR reg1 reg2 ri) = pprLogic (sLit "xor") reg1 reg2 ri

pprInstr (XORIS reg1 reg2 imm) = hcat [
	char '\t',
	ptext (sLit "xoris"),
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2,
	ptext (sLit ", "),
	pprImm imm
    ]

pprInstr (EXTS sz reg1 reg2) = hcat [
	char '\t',
	ptext (sLit "exts"),
	pprSize sz,
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2
    ]

pprInstr (NEG reg1 reg2) = pprUnary (sLit "neg") reg1 reg2
pprInstr (NOT reg1 reg2) = pprUnary (sLit "not") reg1 reg2

pprInstr (SLW reg1 reg2 ri) = pprLogic (sLit "slw") reg1 reg2 (limitShiftRI ri)
pprInstr (SRW reg1 reg2 ri) = pprLogic (sLit "srw") reg1 reg2 (limitShiftRI ri)
pprInstr (SRAW reg1 reg2 ri) = pprLogic (sLit "sraw") reg1 reg2 (limitShiftRI ri)
pprInstr (RLWINM reg1 reg2 sh mb me) = hcat [
        ptext (sLit "\trlwinm\t"),
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        int sh,
        ptext (sLit ", "),
        int mb,
        ptext (sLit ", "),
        int me
    ]
    
pprInstr (FADD sz reg1 reg2 reg3) = pprBinaryF (sLit "fadd") sz reg1 reg2 reg3
pprInstr (FSUB sz reg1 reg2 reg3) = pprBinaryF (sLit "fsub") sz reg1 reg2 reg3
pprInstr (FMUL sz reg1 reg2 reg3) = pprBinaryF (sLit "fmul") sz reg1 reg2 reg3
pprInstr (FDIV sz reg1 reg2 reg3) = pprBinaryF (sLit "fdiv") sz reg1 reg2 reg3
pprInstr (FNEG reg1 reg2) = pprUnary (sLit "fneg") reg1 reg2

pprInstr (FCMP reg1 reg2) = hcat [
	char '\t',
	ptext (sLit "fcmpu\tcr0, "),
	    -- Note: we're using fcmpu, not fcmpo
	    -- The difference is with fcmpo, compare with NaN is an invalid operation.
	    -- We don't handle invalid fp ops, so we don't care
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2
    ]

pprInstr (FCTIWZ reg1 reg2) = pprUnary (sLit "fctiwz") reg1 reg2
pprInstr (FRSP reg1 reg2) = pprUnary (sLit "frsp") reg1 reg2

pprInstr (CRNOR dst src1 src2) = hcat [
        ptext (sLit "\tcrnor\t"),
        int dst,
        ptext (sLit ", "),
        int src1,
        ptext (sLit ", "),
        int src2
    ]

pprInstr (MFCR reg) = hcat [
	char '\t',
	ptext (sLit "mfcr"),
	char '\t',
	pprReg reg
    ]

pprInstr (MFLR reg) = hcat [
	char '\t',
	ptext (sLit "mflr"),
	char '\t',
	pprReg reg
    ]

pprInstr (FETCHPC reg) = vcat [
        ptext (sLit "\tbcl\t20,31,1f"),
        hcat [ ptext (sLit "1:\tmflr\t"), pprReg reg ]
    ]

pprInstr LWSYNC = ptext (sLit "\tlwsync")

pprInstr _ = panic "pprInstr (ppc)"

pprLogic op reg1 reg2 ri = hcat [
	char '\t',
	ptext op,
	case ri of
	    RIReg _ -> empty
	    RIImm _ -> char 'i',
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2,
	ptext (sLit ", "),
	pprRI ri
    ]
    
pprUnary op reg1 reg2 = hcat [
	char '\t',
	ptext op,
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2
    ]
    
pprBinaryF op sz reg1 reg2 reg3 = hcat [
	char '\t',
	ptext op,
	pprFSize sz,
	char '\t',
	pprReg reg1,
	ptext (sLit ", "),
	pprReg reg2,
	ptext (sLit ", "),
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

castFloatToWord8Array :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToWord8Array = castSTUArray

castDoubleToWord8Array :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToWord8Array = castSTUArray

-- floatToBytes and doubleToBytes convert to the host's byte
-- order.  Providing that we're not cross-compiling for a 
-- target with the opposite endianness, this should work ok
-- on all targets.

-- ToDo: this stuff is very similar to the shenanigans in PprAbs,
-- could they be merged?

floatToBytes :: Float -> [Int]
floatToBytes f
   = runST (do
        arr <- newArray_ ((0::Int),3)
        writeArray arr 0 f
        arr <- castFloatToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        return (map fromIntegral [i0,i1,i2,i3])
     )

doubleToBytes :: Double -> [Int]
doubleToBytes d
   = runST (do
        arr <- newArray_ ((0::Int),7)
        writeArray arr 0 d
        arr <- castDoubleToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        i4 <- readArray arr 4
        i5 <- readArray arr 5
        i6 <- readArray arr 6
        i7 <- readArray arr 7
        return (map fromIntegral [i0,i1,i2,i3,i4,i5,i6,i7])
     )

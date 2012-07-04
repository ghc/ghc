-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module SPARC.Ppr (
	pprNatCmmDecl,
	pprBasicBlock,
	pprSectionHeader,
	pprData,
	pprInstr,
	pprSize,
	pprImm,
	pprDataItem
)

where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import SPARC.Regs
import SPARC.Instr
import SPARC.Cond
import SPARC.Imm
import SPARC.AddrMode
import SPARC.Base
import Instruction
import Reg
import Size
import PprBase

import OldCmm
import OldPprCmm()
import CLabel

import Unique		( Uniquable(..), pprUnique )
import Outputable
import Platform
import FastString
import Data.Word

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: Platform -> NatCmmDecl CmmStatics Instr -> SDoc
pprNatCmmDecl platform (CmmData section dats) =
  pprSectionHeader section $$ pprDatas platform dats

 -- special case for split markers:
pprNatCmmDecl platform (CmmProc Nothing lbl (ListGraph [])) = pprLabel platform lbl

 -- special case for code without info table:
pprNatCmmDecl platform (CmmProc Nothing lbl (ListGraph blocks)) =
  pprSectionHeader Text $$
  pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
  vcat (map (pprBasicBlock platform) blocks)

pprNatCmmDecl platform (CmmProc (Just (Statics info_lbl info)) _entry_lbl (ListGraph blocks)) =
  pprSectionHeader Text $$
  (
       (if platformHasSubsectionsViaSymbols platform
        then pprCLabel platform (mkDeadStripPreventer info_lbl) <> char ':'
        else empty) $$
       vcat (map (pprData platform) info) $$
       pprLabel platform info_lbl
  ) $$
  vcat (map (pprBasicBlock platform) blocks) $$
     -- above: Even the first block gets a label, because with branch-chain
     -- elimination, it might be the target of a goto.
        (if platformHasSubsectionsViaSymbols platform
         then
         -- If we are using the .subsections_via_symbols directive
         -- (available on recent versions of Darwin),
         -- we have to make sure that there is some kind of reference
         -- from the entry code to a label on the _top_ of of the info table,
         -- so that the linker will not think it is unreferenced and dead-strip
         -- it. That's why the label is called a DeadStripPreventer (_dsp).
                  text "\t.long "
              <+> pprCLabel platform info_lbl
              <+> char '-'
              <+> pprCLabel platform (mkDeadStripPreventer info_lbl)
         else empty)


pprBasicBlock :: Platform -> NatBasicBlock Instr -> SDoc
pprBasicBlock platform (BasicBlock blockid instrs) =
  pprLabel platform (mkAsmTempLabel (getUnique blockid)) $$
  vcat (map (pprInstr platform) instrs)


pprDatas :: Platform -> CmmStatics -> SDoc
pprDatas platform (Statics lbl dats) = vcat (pprLabel platform lbl : map (pprData platform) dats)

pprData :: Platform -> CmmStatic -> SDoc
pprData _        (CmmString str)          = pprASCII str
pprData _        (CmmUninitialised bytes) = ptext (sLit ".skip ") <> int bytes
pprData platform (CmmStaticLit lit)       = pprDataItem platform lit

pprGloblDecl :: Platform -> CLabel -> SDoc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext (sLit ".global ") <> pprCLabel platform lbl

pprTypeAndSizeDecl :: Platform -> CLabel -> SDoc
pprTypeAndSizeDecl platform lbl
 | platformOS platform == OSLinux && externallyVisibleCLabel lbl
    = ptext (sLit ".type ") <>
      pprCLabel platform lbl <> ptext (sLit ", @object")
 | otherwise = empty

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl = pprGloblDecl platform lbl
                     $$ pprTypeAndSizeDecl platform lbl
                     $$ (pprCLabel platform lbl <> char ':')


pprASCII :: [Word8] -> SDoc
pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> SDoc
       do1 w = ptext (sLit "\t.byte\t") <> int (fromIntegral w)


-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr instr = sdocWithPlatform $ \platform -> pprInstr platform instr


-- | Pretty print a register.
pprReg :: Reg -> SDoc
pprReg reg
 = case reg of
 	RegVirtual vr
	 -> case vr of
                VirtualRegI   u -> text "%vI_"  <> pprUnique u
                VirtualRegHi  u -> text "%vHi_" <> pprUnique u
                VirtualRegF   u -> text "%vF_"  <> pprUnique u
                VirtualRegD   u -> text "%vD_"  <> pprUnique u
                VirtualRegSSE u -> text "%vSSE_" <> pprUnique u

	RegReal rr
	 -> case rr of
		RealRegSingle r1
		 -> pprReg_ofRegNo r1

		RealRegPair   r1 r2	
		 -> text "(" 	<> pprReg_ofRegNo r1 
		 <> text "|" 	<> pprReg_ofRegNo r2
		 <> text ")"
	


-- | Pretty print a register name, based on this register number.
--	The definition has been unfolded so we get a jump-table in the
--	object code. This function is called quite a lot when emitting the asm file..
--
pprReg_ofRegNo :: Int -> SDoc
pprReg_ofRegNo i
 = ptext
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
	_  -> sLit "very naughty sparc register" })


-- | Pretty print a size for an instruction suffix.
pprSize :: Size -> SDoc
pprSize x 
 = ptext 
    (case x of
	II8	-> sLit "ub"
	II16	-> sLit "uh"
	II32	-> sLit ""
	II64	-> sLit "d"
	FF32	-> sLit ""
	FF64	-> sLit "d"
	_	-> panic "SPARC.Ppr.pprSize: no match")


-- | Pretty print a size for an instruction suffix.
--	eg LD is 32bit on sparc, but LDD is 64 bit.
pprStSize :: Size -> SDoc
pprStSize x 
 = ptext 
    (case x of
	II8   -> sLit "b"
	II16  -> sLit "h"
	II32  -> sLit ""
	II64  -> sLit "x"
	FF32  -> sLit ""
	FF64  -> sLit "d"
	_	-> panic "SPARC.Ppr.pprSize: no match")

		
-- | Pretty print a condition code.
pprCond :: Cond -> SDoc
pprCond c 
 = ptext 
    (case c of 
	ALWAYS	-> sLit ""
	NEVER 	-> sLit "n"
	GEU	-> sLit "geu"
	LU    	-> sLit "lu"
	EQQ	-> sLit "e"
	GTT   	-> sLit "g"
	GE	-> sLit "ge"
	GU	-> sLit "gu"
	LTT	-> sLit "l"
	LE	-> sLit "le"
	LEU	-> sLit "leu"
	NE	-> sLit "ne"
	NEG	-> sLit "neg"
	POS	-> sLit "pos"
	VC	-> sLit "vc"
	VS	-> sLit "vs")


-- | Pretty print an address mode.
pprAddr :: Platform -> AddrMode -> SDoc
pprAddr platform am
 = case am of
 	AddrRegReg r1 (RegReal (RealRegSingle 0))
	 -> pprReg r1

	AddrRegReg r1 r2
	 -> hcat [ pprReg r1, char '+', pprReg r2 ]

	AddrRegImm r1 (ImmInt i)
	 | i == 0 		-> pprReg r1
	 | not (fits13Bits i) 	-> largeOffsetError i
	 | otherwise 		-> hcat [ pprReg r1, pp_sign, int i ]
	 where	
	 	pp_sign = if i > 0 then char '+' else empty

	AddrRegImm r1 (ImmInteger i)
	 | i == 0 		-> pprReg r1
	 | not (fits13Bits i)	-> largeOffsetError i
	 | otherwise		-> hcat [ pprReg r1, pp_sign, integer i ]
	 where
		pp_sign = if i > 0 then char '+' else empty

	AddrRegImm r1 imm
	 -> hcat [ pprReg r1, char '+', pprImm platform imm ]


-- | Pretty print an immediate value.
pprImm :: Platform -> Imm -> SDoc
pprImm platform imm
 = case imm of
 	ImmInt i	-> int i
	ImmInteger i	-> integer i
	ImmCLbl l	-> pprCLabel platform l
	ImmIndex l i	-> pprCLabel platform l <> char '+' <> int i
	ImmLit s	-> s

	ImmConstantSum a b	
	 -> pprImm platform a <> char '+' <> pprImm platform b

	ImmConstantDiff a b	
	 -> pprImm platform a <> char '-' <> lparen <> pprImm platform b <> rparen

	LO i
	 -> hcat [ text "%lo(", pprImm platform i, rparen ]
	
	HI i
	 -> hcat [ text "%hi(", pprImm platform i, rparen ]

	-- these should have been converted to bytes and placed
	--	in the data section.
	ImmFloat _	-> ptext (sLit "naughty float immediate")
	ImmDouble _	-> ptext (sLit "naughty double immediate")


-- | Pretty print a section \/ segment header.
--	On SPARC all the data sections must be at least 8 byte aligned
--	incase we store doubles in them.
--
pprSectionHeader :: Section -> SDoc
pprSectionHeader seg
 = case seg of
 	Text			-> ptext (sLit ".text\n\t.align 4")
	Data			-> ptext (sLit ".data\n\t.align 8")
	ReadOnlyData		-> ptext (sLit ".text\n\t.align 8")
	RelocatableReadOnlyData	-> ptext (sLit ".text\n\t.align 8")
	UninitialisedData	-> ptext (sLit ".bss\n\t.align 8")
	ReadOnlyData16		-> ptext (sLit ".data\n\t.align 16")
	OtherSection _		-> panic "PprMach.pprSectionHeader: unknown section"


-- | Pretty print a data item.
pprDataItem :: Platform -> CmmLit -> SDoc
pprDataItem platform lit
  = vcat (ppr_item (cmmTypeSize $ cmmLitType lit) lit)
    where
	imm = litToImm lit

	ppr_item II8   _ 	= [ptext (sLit "\t.byte\t") <> pprImm platform imm]
	ppr_item II32  _	= [ptext (sLit "\t.long\t") <> pprImm platform imm]

	ppr_item FF32  (CmmFloat r _)
         = let bs = floatToBytes (fromRational r)
           in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm platform (ImmInt b)) bs

    	ppr_item FF64 (CmmFloat r _)
         = let bs = doubleToBytes (fromRational r)
           in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm platform (ImmInt b)) bs

	ppr_item II16  _	= [ptext (sLit "\t.short\t") <> pprImm platform imm]
	ppr_item II64  _	= [ptext (sLit "\t.quad\t") <> pprImm platform imm]
	ppr_item _ _		= panic "SPARC.Ppr.pprDataItem: no match"


-- | Pretty print an instruction.
pprInstr :: Platform -> Instr -> SDoc

-- nuke comments.
pprInstr _        (COMMENT _) 
	= empty 

pprInstr platform (DELTA d)
	= pprInstr platform (COMMENT (mkFastString ("\tdelta = " ++ show d)))

-- Newblocks and LData should have been slurped out before producing the .s file.
pprInstr _        (NEWBLOCK _)
	= panic "X86.Ppr.pprInstr: NEWBLOCK"

pprInstr _        (LDATA _ _)
	= panic "PprMach.pprInstr: LDATA"

-- 64 bit FP loads are expanded into individual instructions in CodeGen.Expand
pprInstr _        (LD FF64 _ reg)
	| RegReal (RealRegSingle{})	<- reg
 	= panic "SPARC.Ppr: not emitting potentially misaligned LD FF64 instr"

pprInstr platform (LD size addr reg)
	= hcat [
	       ptext (sLit "\tld"),
	       pprSize size,
	       char '\t',
	       lbrack,
	       pprAddr platform addr,
	       pp_rbracket_comma,
	       pprReg reg
	    ]

-- 64 bit FP storees are expanded into individual instructions in CodeGen.Expand
pprInstr _        (ST FF64 reg _)
	| RegReal (RealRegSingle{})	<- reg
 	= panic "SPARC.Ppr: not emitting potentially misaligned ST FF64 instr"

-- no distinction is made between signed and unsigned bytes on stores for the
-- Sparc opcodes (at least I cannot see any, and gas is nagging me --SOF),
-- so we call a special-purpose pprSize for ST..
pprInstr platform (ST size reg addr)
	= hcat [
	       ptext (sLit "\tst"),
	       pprStSize size,
	       char '\t',
	       pprReg reg,
	       pp_comma_lbracket,
	       pprAddr platform addr,
	       rbrack
	    ]


pprInstr platform (ADD x cc reg1 ri reg2)
	| not x && not cc && riZero ri
	= hcat [ ptext (sLit "\tmov\t"), pprReg reg1, comma, pprReg reg2 ]

	| otherwise
	= pprRegRIReg platform (if x then sLit "addx" else sLit "add") cc reg1 ri reg2


pprInstr platform (SUB x cc reg1 ri reg2)
	| not x && cc && reg2 == g0
	= hcat [ ptext (sLit "\tcmp\t"), pprReg reg1, comma, pprRI platform ri ]

	| not x && not cc && riZero ri
	= hcat [ ptext (sLit "\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
	
	| otherwise
	= pprRegRIReg platform (if x then sLit "subx" else sLit "sub") cc reg1 ri reg2

pprInstr platform (AND  b reg1 ri reg2)	= pprRegRIReg platform (sLit "and")  b reg1 ri reg2

pprInstr platform (ANDN b reg1 ri reg2)	= pprRegRIReg platform (sLit "andn") b reg1 ri reg2

pprInstr platform (OR b reg1 ri reg2)
	| not b && reg1 == g0
	= let doit = hcat [ ptext (sLit "\tmov\t"), pprRI platform ri, comma, pprReg reg2 ]
	  in  case ri of
	           RIReg rrr | rrr == reg2 -> empty
	           _                       -> doit

	| otherwise
	= pprRegRIReg platform (sLit "or") b reg1 ri reg2

pprInstr platform (ORN b reg1 ri reg2)	= pprRegRIReg platform (sLit "orn") b reg1 ri reg2

pprInstr platform (XOR  b reg1 ri reg2)	= pprRegRIReg platform (sLit "xor")  b reg1 ri reg2
pprInstr platform (XNOR b reg1 ri reg2)	= pprRegRIReg platform (sLit "xnor") b reg1 ri reg2

pprInstr platform (SLL reg1 ri reg2)	= pprRegRIReg platform (sLit "sll") False reg1 ri reg2
pprInstr platform (SRL reg1 ri reg2)	= pprRegRIReg platform (sLit "srl") False reg1 ri reg2
pprInstr platform (SRA reg1 ri reg2)	= pprRegRIReg platform (sLit "sra") False reg1 ri reg2

pprInstr _        (RDY rd) 		= ptext (sLit "\trd\t%y,") <> pprReg rd
pprInstr _        (WRY reg1 reg2) 	
	= ptext (sLit "\twr\t") 
		<> pprReg reg1 
		<> char ','
		<> pprReg reg2
		<> char ','
		<> ptext (sLit "%y") 

pprInstr platform (SMUL b reg1 ri reg2)	= pprRegRIReg platform (sLit "smul")  b reg1 ri reg2
pprInstr platform (UMUL b reg1 ri reg2)	= pprRegRIReg platform (sLit "umul")  b reg1 ri reg2
pprInstr platform (SDIV b reg1 ri reg2)	= pprRegRIReg platform (sLit "sdiv")  b reg1 ri reg2
pprInstr platform (UDIV b reg1 ri reg2)	= pprRegRIReg platform (sLit "udiv")  b reg1 ri reg2

pprInstr platform (SETHI imm reg)
  = hcat [
	ptext (sLit "\tsethi\t"),
	pprImm platform imm,
	comma,
	pprReg reg
    ]

pprInstr _        NOP 
	= ptext (sLit "\tnop")

pprInstr _        (FABS size reg1 reg2)	
 	= pprSizeRegReg (sLit "fabs") size reg1 reg2

pprInstr _        (FADD size reg1 reg2 reg3)	
	= pprSizeRegRegReg (sLit "fadd") size reg1 reg2 reg3

pprInstr _        (FCMP e size reg1 reg2)
	= pprSizeRegReg (if e then sLit "fcmpe" else sLit "fcmp") size reg1 reg2

pprInstr _        (FDIV size reg1 reg2 reg3)
	= pprSizeRegRegReg (sLit "fdiv") size reg1 reg2 reg3

pprInstr _        (FMOV size reg1 reg2)	
	= pprSizeRegReg (sLit "fmov") size reg1 reg2

pprInstr _        (FMUL size reg1 reg2 reg3)
	= pprSizeRegRegReg (sLit "fmul") size reg1 reg2 reg3

pprInstr _        (FNEG size reg1 reg2) 
	= pprSizeRegReg (sLit "fneg") size reg1 reg2

pprInstr _        (FSQRT size reg1 reg2)     
	= pprSizeRegReg (sLit "fsqrt") size reg1 reg2

pprInstr _        (FSUB size reg1 reg2 reg3) 
	= pprSizeRegRegReg (sLit "fsub") size reg1 reg2 reg3

pprInstr _        (FxTOy size1 size2 reg1 reg2)
  = hcat [
    	ptext (sLit "\tf"),
	ptext
    	(case size1 of
    	    II32  -> sLit "ito"
    	    FF32  -> sLit "sto"
    	    FF64  -> sLit "dto"
	    _	  -> panic "SPARC.Ppr.pprInstr.FxToY: no match"),
	ptext
    	(case size2 of
    	    II32  -> sLit "i\t"
	    II64  -> sLit "x\t"
    	    FF32  -> sLit "s\t"
    	    FF64  -> sLit "d\t"
	    _	  -> panic "SPARC.Ppr.pprInstr.FxToY: no match"),
	pprReg reg1, comma, pprReg reg2
    ]


pprInstr platform (BI cond b blockid)
  = hcat [
	ptext (sLit "\tb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprCLabel platform (mkAsmTempLabel (getUnique blockid))
    ]

pprInstr platform (BF cond b blockid)
  = hcat [
	ptext (sLit "\tfb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprCLabel platform (mkAsmTempLabel (getUnique blockid))
    ]

pprInstr platform (JMP addr) = (<>) (ptext (sLit "\tjmp\t")) (pprAddr platform addr)
pprInstr platform (JMP_TBL op _ _)  = pprInstr platform (JMP op)

pprInstr platform (CALL (Left imm) n _)
  = hcat [ ptext (sLit "\tcall\t"), pprImm platform imm, comma, int n ]

pprInstr _        (CALL (Right reg) n _)
  = hcat [ ptext (sLit "\tcall\t"), pprReg reg, comma, int n ]


-- | Pretty print a RI
pprRI :: Platform -> RI -> SDoc
pprRI _        (RIReg r) = pprReg r
pprRI platform (RIImm r) = pprImm platform r


-- | Pretty print a two reg instruction.
pprSizeRegReg :: LitString -> Size -> Reg -> Reg -> SDoc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    FF32 -> ptext (sLit "s\t")
    	    FF64 -> ptext (sLit "d\t")
	    _    -> panic "SPARC.Ppr.pprSizeRegReg: no match"),

	pprReg reg1,
	comma,
	pprReg reg2
    ]


-- | Pretty print a three reg instruction.
pprSizeRegRegReg :: LitString -> Size -> Reg -> Reg -> Reg -> SDoc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    FF32  -> ptext (sLit "s\t")
    	    FF64  -> ptext (sLit "d\t")
	    _    -> panic "SPARC.Ppr.pprSizeRegReg: no match"),
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]


-- | Pretty print an instruction of two regs and a ri.
pprRegRIReg :: Platform -> LitString -> Bool -> Reg -> RI -> Reg -> SDoc
pprRegRIReg platform name b reg1 ri reg2
  = hcat [
	char '\t',
	ptext name,
	if b then ptext (sLit "cc\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI platform ri,
	comma,
	pprReg reg2
    ]

{-
pprRIReg :: LitString -> Bool -> RI -> Reg -> SDoc
pprRIReg name b ri reg1
  = hcat [
	char '\t',
	ptext name,
	if b then ptext (sLit "cc\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg1
    ]
-}

{-
pp_ld_lbracket :: SDoc
pp_ld_lbracket    = ptext (sLit "\tld\t[")
-}

pp_rbracket_comma :: SDoc
pp_rbracket_comma = text "],"


pp_comma_lbracket :: SDoc
pp_comma_lbracket = text ",["


pp_comma_a :: SDoc
pp_comma_a	  = text ",a"


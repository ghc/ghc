%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprMach]{Pretty-printing assembly language}

We start with the @pprXXX@s with some cross-platform commonality
(e.g., @pprReg@); we conclude with the no-commonality monster,
@pprInstr@.

\begin{code}
#include "nativeGen/NCG.h"

module PprMach ( pprInstr, pprSize, pprUserReg ) where

#include "HsVersions.h"

import MachRegs		-- may differ per-platform
import MachMisc

import CLabel		( pprCLabel_asm, externallyVisibleCLabel, labelDynamic )
import CStrings		( charToC )
import Maybes		( maybeToBool )
import Stix		( CodeSegment(..), StixTree(..) )
import Char		( isPrint, isDigit )
import Outputable

import ST
import MutableArray
import Char		( ord )
\end{code}

%************************************************************************
%*									*
\subsection{@pprReg@: print a @Reg@}
%*									*
%************************************************************************

For x86, the way we print a register name depends
on which bit of it we care about.  Yurgh.
\begin{code}
pprUserReg:: Reg -> SDoc
pprUserReg = pprReg IF_ARCH_i386(L,)


pprReg :: IF_ARCH_i386(Size ->,) Reg -> SDoc

pprReg IF_ARCH_i386(s,) r
  = case r of
      RealReg (I# i) -> ppr_reg_no IF_ARCH_i386(s,) i
      VirtualRegI u  -> text "%vI_" <> ppr u
      VirtualRegF u  -> text "%vF_" <> ppr u      
  where
#if alpha_TARGET_ARCH
    ppr_reg_no :: FAST_REG_NO -> SDoc
    ppr_reg_no i = ptext
      (case i of {
	ILIT( 0) -> SLIT("$0");   ILIT( 1) -> SLIT("$1");
	ILIT( 2) -> SLIT("$2");   ILIT( 3) -> SLIT("$3");
	ILIT( 4) -> SLIT("$4");   ILIT( 5) -> SLIT("$5");
	ILIT( 6) -> SLIT("$6");   ILIT( 7) -> SLIT("$7");
	ILIT( 8) -> SLIT("$8");   ILIT( 9) -> SLIT("$9");
	ILIT(10) -> SLIT("$10");  ILIT(11) -> SLIT("$11");
	ILIT(12) -> SLIT("$12");  ILIT(13) -> SLIT("$13");
	ILIT(14) -> SLIT("$14");  ILIT(15) -> SLIT("$15");
	ILIT(16) -> SLIT("$16");  ILIT(17) -> SLIT("$17");
	ILIT(18) -> SLIT("$18");  ILIT(19) -> SLIT("$19");
	ILIT(20) -> SLIT("$20");  ILIT(21) -> SLIT("$21");
	ILIT(22) -> SLIT("$22");  ILIT(23) -> SLIT("$23");
	ILIT(24) -> SLIT("$24");  ILIT(25) -> SLIT("$25");
	ILIT(26) -> SLIT("$26");  ILIT(27) -> SLIT("$27");
	ILIT(28) -> SLIT("$28");  ILIT(29) -> SLIT("$29");
	ILIT(30) -> SLIT("$30");  ILIT(31) -> SLIT("$31");
	ILIT(32) -> SLIT("$f0");  ILIT(33) -> SLIT("$f1");
	ILIT(34) -> SLIT("$f2");  ILIT(35) -> SLIT("$f3");
	ILIT(36) -> SLIT("$f4");  ILIT(37) -> SLIT("$f5");
	ILIT(38) -> SLIT("$f6");  ILIT(39) -> SLIT("$f7");
	ILIT(40) -> SLIT("$f8");  ILIT(41) -> SLIT("$f9");
	ILIT(42) -> SLIT("$f10"); ILIT(43) -> SLIT("$f11");
	ILIT(44) -> SLIT("$f12"); ILIT(45) -> SLIT("$f13");
	ILIT(46) -> SLIT("$f14"); ILIT(47) -> SLIT("$f15");
	ILIT(48) -> SLIT("$f16"); ILIT(49) -> SLIT("$f17");
	ILIT(50) -> SLIT("$f18"); ILIT(51) -> SLIT("$f19");
	ILIT(52) -> SLIT("$f20"); ILIT(53) -> SLIT("$f21");
	ILIT(54) -> SLIT("$f22"); ILIT(55) -> SLIT("$f23");
	ILIT(56) -> SLIT("$f24"); ILIT(57) -> SLIT("$f25");
	ILIT(58) -> SLIT("$f26"); ILIT(59) -> SLIT("$f27");
	ILIT(60) -> SLIT("$f28"); ILIT(61) -> SLIT("$f29");
	ILIT(62) -> SLIT("$f30"); ILIT(63) -> SLIT("$f31");
	_ -> SLIT("very naughty alpha register")
      })
#endif
#if i386_TARGET_ARCH
    ppr_reg_no :: Size -> FAST_REG_NO -> SDoc
    ppr_reg_no B i= ptext
      (case i of {
	ILIT( 0) -> SLIT("%al");  ILIT( 1) -> SLIT("%bl");
	ILIT( 2) -> SLIT("%cl");  ILIT( 3) -> SLIT("%dl");
	_ -> SLIT("very naughty I386 byte register")
      })

    ppr_reg_no _ i = ptext
      (case i of {
	ILIT( 0) -> SLIT("%eax");  ILIT( 1) -> SLIT("%ebx");
	ILIT( 2) -> SLIT("%ecx");  ILIT( 3) -> SLIT("%edx");
	ILIT( 4) -> SLIT("%esi");  ILIT( 5) -> SLIT("%edi");
	ILIT( 6) -> SLIT("%ebp");  ILIT( 7) -> SLIT("%esp");
	ILIT( 8) -> SLIT("%fake0");  ILIT( 9) -> SLIT("%fake1");
	ILIT(10) -> SLIT("%fake2");  ILIT(11) -> SLIT("%fake3");
	ILIT(12) -> SLIT("%fake4");  ILIT(13) -> SLIT("%fake5");
	_ -> SLIT("very naughty I386 register")
      })
#endif
#if sparc_TARGET_ARCH
    ppr_reg_no :: FAST_REG_NO -> SDoc
    ppr_reg_no i = ptext
      (case i of {
	ILIT( 0) -> SLIT("%g0");  ILIT( 1) -> SLIT("%g1");
	ILIT( 2) -> SLIT("%g2");  ILIT( 3) -> SLIT("%g3");
	ILIT( 4) -> SLIT("%g4");  ILIT( 5) -> SLIT("%g5");
	ILIT( 6) -> SLIT("%g6");  ILIT( 7) -> SLIT("%g7");
	ILIT( 8) -> SLIT("%o0");  ILIT( 9) -> SLIT("%o1");
	ILIT(10) -> SLIT("%o2");  ILIT(11) -> SLIT("%o3");
	ILIT(12) -> SLIT("%o4");  ILIT(13) -> SLIT("%o5");
	ILIT(14) -> SLIT("%o6");  ILIT(15) -> SLIT("%o7");
	ILIT(16) -> SLIT("%l0");  ILIT(17) -> SLIT("%l1");
	ILIT(18) -> SLIT("%l2");  ILIT(19) -> SLIT("%l3");
	ILIT(20) -> SLIT("%l4");  ILIT(21) -> SLIT("%l5");
	ILIT(22) -> SLIT("%l6");  ILIT(23) -> SLIT("%l7");
	ILIT(24) -> SLIT("%i0");  ILIT(25) -> SLIT("%i1");
	ILIT(26) -> SLIT("%i2");  ILIT(27) -> SLIT("%i3");
	ILIT(28) -> SLIT("%i4");  ILIT(29) -> SLIT("%i5");
	ILIT(30) -> SLIT("%i6");  ILIT(31) -> SLIT("%i7");
	ILIT(32) -> SLIT("%f0");  ILIT(33) -> SLIT("%f1");
	ILIT(34) -> SLIT("%f2");  ILIT(35) -> SLIT("%f3");
	ILIT(36) -> SLIT("%f4");  ILIT(37) -> SLIT("%f5");
	ILIT(38) -> SLIT("%f6");  ILIT(39) -> SLIT("%f7");
	ILIT(40) -> SLIT("%f8");  ILIT(41) -> SLIT("%f9");
	ILIT(42) -> SLIT("%f10"); ILIT(43) -> SLIT("%f11");
	ILIT(44) -> SLIT("%f12"); ILIT(45) -> SLIT("%f13");
	ILIT(46) -> SLIT("%f14"); ILIT(47) -> SLIT("%f15");
	ILIT(48) -> SLIT("%f16"); ILIT(49) -> SLIT("%f17");
	ILIT(50) -> SLIT("%f18"); ILIT(51) -> SLIT("%f19");
	ILIT(52) -> SLIT("%f20"); ILIT(53) -> SLIT("%f21");
	ILIT(54) -> SLIT("%f22"); ILIT(55) -> SLIT("%f23");
	ILIT(56) -> SLIT("%f24"); ILIT(57) -> SLIT("%f25");
	ILIT(58) -> SLIT("%f26"); ILIT(59) -> SLIT("%f27");
	ILIT(60) -> SLIT("%f28"); ILIT(61) -> SLIT("%f29");
	ILIT(62) -> SLIT("%f30"); ILIT(63) -> SLIT("%f31");
	_ -> SLIT("very naughty sparc register")
      })
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@pprSize@: print a @Size@}
%*									*
%************************************************************************

\begin{code}
pprSize :: Size -> SDoc

pprSize x = ptext (case x of
#if alpha_TARGET_ARCH
	 B  -> SLIT("b")
	 BU -> SLIT("bu")
--	 W  -> SLIT("w") UNUSED
--	 WU -> SLIT("wu") UNUSED
--	 L  -> SLIT("l") UNUSED
	 Q  -> SLIT("q")
--	 FF -> SLIT("f") UNUSED
--	 DF -> SLIT("d") UNUSED
--	 GF -> SLIT("g") UNUSED
--	 SF -> SLIT("s") UNUSED
	 TF -> SLIT("t")
#endif
#if i386_TARGET_ARCH
	B   -> SLIT("b")
--	HB  -> SLIT("b") UNUSED
--	S   -> SLIT("w") UNUSED
	L   -> SLIT("l")
	F   -> SLIT("s")
	DF  -> SLIT("l")
	F80 -> SLIT("t")
#endif
#if sparc_TARGET_ARCH
	B   -> SLIT("sb")
	BU  -> SLIT("ub")
--	HW  -> SLIT("hw") UNUSED
--	HWU -> SLIT("uhw") UNUSED
	W   -> SLIT("")
	F   -> SLIT("")
--	D   -> SLIT("d") UNUSED
	DF  -> SLIT("d")
    )
pprStSize :: Size -> SDoc
pprStSize x = ptext (case x of
	B   -> SLIT("b")
	BU  -> SLIT("b")
--	HW  -> SLIT("hw") UNUSED
--	HWU -> SLIT("uhw") UNUSED
	W   -> SLIT("")
	F   -> SLIT("")
--	D   -> SLIT("d") UNUSED
	DF  -> SLIT("d")
#endif
    )
\end{code}

%************************************************************************
%*									*
\subsection{@pprCond@: print a @Cond@}
%*									*
%************************************************************************

\begin{code}
pprCond :: Cond -> SDoc

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
#if i386_TARGET_ARCH
	GEU	-> SLIT("ae");	LU    -> SLIT("b");
	EQQ	-> SLIT("e");	GTT    -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("a");
	LTT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("be");	NE    -> SLIT("ne");
	NEG	-> SLIT("s");	POS   -> SLIT("ns");
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
    })
\end{code}

%************************************************************************
%*									*
\subsection{@pprImm@: print an @Imm@}
%*									*
%************************************************************************

\begin{code}
pprImm :: Imm -> SDoc

pprImm (ImmInt i)     = int i
pprImm (ImmInteger i) = integer i
pprImm (ImmCLbl l)    = (if labelDynamic l then text "__imp_" else empty)
                        <> pprCLabel_asm l
pprImm (ImmIndex l i) = (if labelDynamic l then text "__imp_" else empty)
                        <> pprCLabel_asm l <> char '+' <> int i
pprImm (ImmLit s)     = s

pprImm (ImmLab dll s) = (if underscorePrefix then char '_' else empty)
                        <> (if dll then text "_imp__" else empty)
                        <> s

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
\end{code}

%************************************************************************
%*									*
\subsection{@pprAddr@: print an @Addr@}
%*									*
%************************************************************************

\begin{code}
pprAddr :: MachRegsAddr -> SDoc

#if alpha_TARGET_ARCH
pprAddr (AddrReg r) = parens (pprReg r)
pprAddr (AddrImm i) = pprImm i
pprAddr (AddrRegImm r1 i)
  = (<>) (pprImm i) (parens (pprReg r1))
#endif

-------------------

#if i386_TARGET_ARCH
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
	pp_reg r = pprReg L r
    in
    case (base,index) of
      (Nothing, Nothing)    -> pp_disp
      (Just b,  Nothing)    -> pp_off (pp_reg b)
      (Nothing, Just (r,i)) -> pp_off (pp_reg r <> comma <> int i)
      (Just b,  Just (r,i)) -> pp_off (pp_reg b <> comma <> pp_reg r 
                                       <> comma <> int i)
  where
    ppr_disp (ImmInt 0) = empty
    ppr_disp imm        = pprImm imm
#endif

-------------------

#if sparc_TARGET_ARCH
pprAddr (AddrRegReg r1 (FixedReg ILIT(0))) = pprReg r1

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
\end{code}

%************************************************************************
%*									*
\subsection{@pprInstr@: print an @Instr@}
%*									*
%************************************************************************

\begin{code}
pprInstr :: Instr -> SDoc

--pprInstr (COMMENT s) = empty -- nuke 'em
pprInstr (COMMENT s)
   =  IF_ARCH_alpha( ((<>) (ptext SLIT("\t# ")) (ptext s))
     ,IF_ARCH_sparc( ((<>) (ptext SLIT("! "))   (ptext s))
     ,IF_ARCH_i386( ((<>) (ptext SLIT("# "))   (ptext s))
     ,)))

pprInstr (DELTA d)
   = pprInstr (COMMENT (_PK_ ("\tdelta = " ++ show d)))

pprInstr (SEGMENT TextSegment)
    =  IF_ARCH_alpha(ptext SLIT("\t.text\n\t.align 3") {-word boundary-}
      ,IF_ARCH_sparc(ptext SLIT("\t.text\n\t.align 4") {-word boundary-}
      ,IF_ARCH_i386((text ".text\n\t.align 4,0x90") {-needs per-OS variation!-}
      ,)))

pprInstr (SEGMENT DataSegment)
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 3")
	,IF_ARCH_sparc(SLIT("\t.data\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(SLIT(".data\n\t.align 4")
	,)))

pprInstr (LABEL clab)
  = let
	pp_lab = pprCLabel_asm clab
    in
    hcat [
	if not (externallyVisibleCLabel clab) then
	    empty
	else
	    hcat [ptext
			 IF_ARCH_alpha(SLIT("\t.globl\t")
		        ,IF_ARCH_i386(SLIT(".globl ")
			,IF_ARCH_sparc(SLIT("\t.global\t")
			,)))
			, pp_lab, char '\n'],
	pp_lab,
	char ':'
    ]

pprInstr (ASCII False{-no backslash conversion-} str)
  = hcat [ ptext SLIT("\t.asciz "), char '\"', text str, char '"' ]

pprInstr (ASCII True str)
  = asciify str
  where
    asciify :: String -> SDoc
    asciify "" = text "\t.ascii \"\\0\""
    asciify str
       = let fst  = take 16 str
             rest = drop 16 str
             this = text ("\t.ascii \"" 
                          ++ concat (map asciify_char fst)
                          ++ "\"")
         in  this $$ asciify rest
    asciify_char :: Char -> String
    asciify_char c = '\\' : 'x' : hshow (ord c)

    hshow :: Int -> String
    hshow n | n >= 0 && n <= 255
            = [ tab !! (n `div` 16), tab !! (n `mod` 16)]
    tab = "0123456789abcdef"



pprInstr (DATA s xs)
  = vcat (concatMap (ppr_item s) xs)
    where
#if alpha_TARGET_ARCH
            ppr_item = error "ppr_item on Alpha"
#if 0
            This needs to be fixed.
	    B  -> SLIT("\t.byte\t")
	    BU -> SLIT("\t.byte\t")
	    Q  -> SLIT("\t.quad\t")
	    TF -> SLIT("\t.t_floating\t")
#endif
#endif
#if sparc_TARGET_ARCH
            ppr_item = error "ppr_item on Sparc"
#if 0
            This needs to be fixed.
	    B  -> SLIT("\t.byte\t")
	    BU -> SLIT("\t.byte\t")
	    W  -> SLIT("\t.word\t")
    	    DF -> SLIT("\t.double\t")
#endif
#endif
#if i386_TARGET_ARCH
	ppr_item B  x = [text "\t.byte\t" <> pprImm x]
	ppr_item L  x = [text "\t.long\t" <> pprImm x]
	ppr_item F  (ImmDouble r)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs
    	ppr_item DF (ImmDouble r)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        floatToBytes :: Float -> [Int]
        floatToBytes f
           = runST (do
                arr <- newFloatArray ((0::Int),3)
                writeFloatArray arr 0 f
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

#endif

-- fall through to rest of (machine-specific) pprInstr...
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for an Alpha}
%*									*
%************************************************************************

\begin{code}
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
pprRI :: RI -> SDoc

pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprRegRIReg :: FAST_STRING -> Reg -> RI -> Reg -> SDoc

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

pprSizeRegRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Reg -> SDoc

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

#endif {-alpha_TARGET_ARCH-}
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for an I386}
%*									*
%************************************************************************

\begin{code}
#if i386_TARGET_ARCH

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
pprInstr (MOVZxL sizes src dst) = pprSizeOpOpCoerce SLIT("movz") sizes L src dst
pprInstr (MOVSxL sizes src dst) = pprSizeOpOpCoerce SLIT("movs") sizes L src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3))
  | reg1 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg2) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3))
  | reg2 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg1) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) Nothing displ)) dst@(OpReg reg3))
  | reg1 == reg3
  = pprInstr (ADD size (OpImm displ) dst)
pprInstr (LEA size src dst) = pprSizeOpOp SLIT("lea") size src dst

pprInstr (ADD size (OpImm (ImmInt (-1))) dst)
  = pprSizeOp SLIT("dec") size dst
pprInstr (ADD size (OpImm (ImmInt 1)) dst)
  = pprSizeOp SLIT("inc") size dst
pprInstr (ADD size src dst)
  = pprSizeOpOp SLIT("add") size src dst
pprInstr (SUB size src dst) = pprSizeOpOp SLIT("sub") size src dst
pprInstr (IMUL size op1 op2) = pprSizeOpOp SLIT("imul") size op1 op2
pprInstr (IDIV size op) = pprSizeOp SLIT("idiv") size op

pprInstr (AND size src dst) = pprSizeOpOp SLIT("and") size src dst
pprInstr (OR  size src dst) = pprSizeOpOp SLIT("or")  size src dst
pprInstr (XOR size src dst) = pprSizeOpOp SLIT("xor")  size src dst
pprInstr (NOT size op) = pprSizeOp SLIT("not") size op
pprInstr (NEGI size op) = pprSizeOp SLIT("neg") size op

pprInstr (SHL size imm dst) = pprSizeImmOp SLIT("shl") size imm dst
pprInstr (SAR size imm dst) = pprSizeImmOp SLIT("sar") size imm dst
pprInstr (SHR size imm dst) = pprSizeImmOp SLIT("shr") size imm dst
pprInstr (BT  size imm src) = pprSizeImmOp SLIT("bt")  size imm src

pprInstr (CMP size src dst) = pprSizeOpOp SLIT("cmp")  size src dst
pprInstr (TEST size src dst) = pprSizeOpOp SLIT("test")  size src dst
pprInstr (PUSH size op) = pprSizeOp SLIT("push") size op
pprInstr (POP size op) = pprSizeOp SLIT("pop") size op
pprInstr PUSHA = ptext SLIT("\tpushal")
pprInstr POPA = ptext SLIT("\tpopal")

pprInstr (NOP) = ptext SLIT("\tnop")
pprInstr (CLTD) = ptext SLIT("\tcltd")

pprInstr (SETCC cond op) = pprCondInstr SLIT("set") cond (pprOperand B op)

pprInstr (JXX cond lab) = pprCondInstr SLIT("j") cond (pprCLabel_asm lab)

pprInstr (JMP (OpImm imm)) = (<>) (ptext SLIT("\tjmp ")) (pprImm imm)
pprInstr (JMP op) = (<>) (ptext SLIT("\tjmp *")) (pprOperand L op)
pprInstr (CALL imm)
   = (<>) (ptext SLIT("\tcall ")) (pprImm imm)


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

pprInstr g@(GFTOD src dst) 
   = pprG g bogus
pprInstr g@(GFTOI src dst) 
   = pprG g bogus

pprInstr g@(GDTOF src dst) 
   = pprG g bogus
pprInstr g@(GDTOI src dst) 
   = pprG g bogus

pprInstr g@(GITOF src dst) 
   = pprInstr (GITOD src dst)
pprInstr g@(GITOD src dst) 
   = pprG g (hcat [gtab, text "pushl ", pprReg L src, 
                   text " ; ffree %st(7); fildl (%esp) ; ",
                   gpop dst 1, text " ; addl $4,%esp"])

pprInstr g@(GCMP sz src1 src2) 
   = pprG g (hcat [gtab, text "pushl %eax ; ",gpush src1 0]
             $$
             hcat [gtab, text "fcomp ", greg src2 1, 
                   text "; fstsw %ax ; sahf ; popl %eax"])

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
gcoerceto DF = empty
gcoerceto  F = text "subl $4,%esp ; fstps (%esp) ; flds (%esp) ; addl $4,%esp ; "

gpush reg offset
   = hcat [text "ffree %st(7) ; fld ", greg reg offset]
gpop reg offset
   = hcat [text "fstp ", greg reg offset]

bogus = text "\tbogus"
greg reg offset = text "%st(" <> int (gregno reg - 8+offset) <> char ')'
gsemi = text " ; "
gtab  = char '\t'
gsp   = char ' '

gregno (RealReg i) = i
gregno other       = --pprPanic "gregno" (ppr other)
                     999   -- bogus; only needed for debug printing

pprG :: Instr -> SDoc -> SDoc
pprG fake actual
   = (char '#' <> pprGInstr fake) $$ actual

pprGInstr (GMOV src dst)   = pprSizeRegReg SLIT("gmov") DF src dst
pprGInstr (GLD sz src dst) = pprSizeAddrReg SLIT("gld") sz src dst
pprGInstr (GST sz src dst) = pprSizeRegAddr SLIT("gst") sz src dst

pprGInstr (GLDZ dst) = pprSizeReg SLIT("gldz") DF dst
pprGInstr (GLD1 dst) = pprSizeReg SLIT("gld1") DF dst

pprGInstr (GFTOD src dst) = pprSizeSizeRegReg SLIT("gftod") F DF src dst
pprGInstr (GFTOI src dst) = pprSizeSizeRegReg SLIT("gftoi") F L  src dst

pprGInstr (GDTOF src dst) = pprSizeSizeRegReg SLIT("gdtof") DF F src dst
pprGInstr (GDTOI src dst) = pprSizeSizeRegReg SLIT("gdtoi") DF L src dst

pprGInstr (GITOF src dst) = pprSizeSizeRegReg SLIT("gitof") L F  src dst
pprGInstr (GITOD src dst) = pprSizeSizeRegReg SLIT("gitod") L DF src dst

pprGInstr (GCMP sz src dst) = pprSizeRegReg SLIT("gcmp") sz src dst
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
\end{code}

Continue with I386-only printing bits and bobs:
\begin{code}
pprDollImm :: Imm -> SDoc

pprDollImm i =  ptext SLIT("$") <> pprImm i

pprOperand :: Size -> Operand -> SDoc
pprOperand s (OpReg r)   = pprReg s r
pprOperand s (OpImm i)   = pprDollImm i
pprOperand s (OpAddr ea) = pprAddr ea

pprSizeImmOp :: FAST_STRING -> Size -> Imm -> Operand -> SDoc
pprSizeImmOp name size imm op1
  = hcat [
        char '\t',
	ptext name,
	pprSize size,
	space,
	char '$',
	pprImm imm,
	comma,
	pprOperand size op1
    ]
	
pprSizeOp :: FAST_STRING -> Size -> Operand -> SDoc
pprSizeOp name size op1
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1
    ]

pprSizeOpOp :: FAST_STRING -> Size -> Operand -> Operand -> SDoc
pprSizeOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprSizeByteOpOp :: FAST_STRING -> Size -> Operand -> Operand -> SDoc
pprSizeByteOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand B op1,
	comma,
	pprOperand size op2
    ]

pprSizeOpReg :: FAST_STRING -> Size -> Operand -> Reg -> SDoc
pprSizeOpReg name size op1 reg
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1,
	comma,
	pprReg size reg
    ]

pprSizeReg :: FAST_STRING -> Size -> Reg -> SDoc
pprSizeReg name size reg1
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size reg1
    ]

pprSizeRegReg :: FAST_STRING -> Size -> Reg -> Reg -> SDoc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size reg1,
        comma,
        pprReg size reg2
    ]

pprSizeSizeRegReg :: FAST_STRING -> Size -> Size -> Reg -> Reg -> SDoc
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

pprSizeRegRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Reg -> SDoc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size reg1,
        comma,
        pprReg size reg2,
        comma,
        pprReg size reg3
    ]

pprSizeAddr :: FAST_STRING -> Size -> MachRegsAddr -> SDoc
pprSizeAddr name size op
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprAddr op
    ]

pprSizeAddrReg :: FAST_STRING -> Size -> MachRegsAddr -> Reg -> SDoc
pprSizeAddrReg name size op dst
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprAddr op,
	comma,
	pprReg size dst
    ]

pprSizeRegAddr :: FAST_STRING -> Size -> Reg -> MachRegsAddr -> SDoc
pprSizeRegAddr name size src op
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size src,
	comma,
	pprAddr op
    ]

pprOpOp :: FAST_STRING -> Size -> Operand -> Operand -> SDoc
pprOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name, space,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprSizeOpOpCoerce :: FAST_STRING -> Size -> Size -> Operand -> Operand -> SDoc
pprSizeOpOpCoerce name size1 size2 op1 op2
  = hcat [ char '\t', ptext name, pprSize size1, pprSize size2, space,
	pprOperand size1 op1,
	comma,
	pprOperand size2 op2
    ]

pprCondInstr :: FAST_STRING -> Cond -> SDoc -> SDoc
pprCondInstr name cond arg
  = hcat [ char '\t', ptext name, pprCond cond, space, arg]

#endif {-i386_TARGET_ARCH-}
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for a SPARC}
%*									*
%************************************************************************

\begin{code}
#if sparc_TARGET_ARCH

-- a clumsy hack for now, to handle possible double alignment problems

-- even clumsier, to allow for RegReg regs that show when doing indexed
-- reads (bytearrays).
--
pprInstr (LD DF (AddrRegReg g1 g2) reg)
  = hcat [
	ptext SLIT("\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1, char '\n',
	pp_ld_lbracket, pprReg g1, pp_rbracket_comma, pprReg reg, char '\n',
	pp_ld_lbracket, pprReg g1, ptext SLIT("+4]"), comma, pprReg (fPair reg)
    ]

pprInstr (LD DF addr reg) | maybeToBool off_addr
  = hcat [
	pp_ld_lbracket,
	pprAddr addr,
	pp_rbracket_comma,
	pprReg reg,

	char '\n',
	pp_ld_lbracket,
	pprAddr addr2,
	pp_rbracket_comma,
	pprReg (fPair reg)
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

pprInstr (ST DF reg (AddrRegReg g1 g2))
 = hcat [
	ptext SLIT("\tadd\t"),
		      pprReg g1,comma,pprReg g2,comma,pprReg g1, char '\n',
	ptext SLIT("\tst\t"),    
	      pprReg reg, pp_comma_lbracket, pprReg g1,
	ptext SLIT("]\n\tst\t"), 
	      pprReg (fPair reg), pp_comma_lbracket, pprReg g1, ptext SLIT("+4]")
    ]

pprInstr (ST DF reg addr) | maybeToBool off_addr 
 = hcat [
	ptext SLIT("\tst\t"),
	pprReg reg, pp_comma_lbracket,	pprAddr addr,

	ptext SLIT("]\n\tst\t"),
	pprReg (fPair reg), pp_comma_lbracket,
	pprAddr addr2, rbrack
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
  = hcat [ ptext SLIT("\tmov\t"), pprRI ri, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg SLIT("or") b reg1 ri reg2

pprInstr (ORN b reg1 ri reg2) = pprRegRIReg SLIT("orn") b reg1 ri reg2

pprInstr (XOR  b reg1 ri reg2) = pprRegRIReg SLIT("xor")  b reg1 ri reg2
pprInstr (XNOR b reg1 ri reg2) = pprRegRIReg SLIT("xnor") b reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg SLIT("sll") False reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg SLIT("srl") False reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg SLIT("sra") False reg1 ri reg2

pprInstr (SETHI imm reg)
  = hcat [
	ptext SLIT("\tsethi\t"),
	pprImm imm,
	comma,
	pprReg reg
    ]

pprInstr NOP = ptext SLIT("\tnop")

pprInstr (FABS F reg1 reg2) = pprSizeRegReg SLIT("fabs") F reg1 reg2
pprInstr (FABS DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fabs") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FADD size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fadd") size reg1 reg2 reg3
pprInstr (FCMP e size reg1 reg2)
  = pprSizeRegReg (if e then SLIT("fcmpe") else SLIT("fcmp")) size reg1 reg2
pprInstr (FDIV size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fdiv") size reg1 reg2 reg3

pprInstr (FMOV F reg1 reg2) = pprSizeRegReg SLIT("fmov") F reg1 reg2
pprInstr (FMOV DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fmov") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FMUL size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fmul") size reg1 reg2 reg3

pprInstr (FNEG F reg1 reg2) = pprSizeRegReg SLIT("fneg") F reg1 reg2
pprInstr (FNEG DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fneg") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FSQRT size reg1 reg2)     = pprSizeRegReg SLIT("fsqrt") size reg1 reg2
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fsub") size reg1 reg2 reg3
pprInstr (FxTOy size1 size2 reg1 reg2)
  = hcat [
    	ptext SLIT("\tf"),
	ptext
    	(case size1 of
    	    W  -> SLIT("ito")
    	    F  -> SLIT("sto")
    	    DF -> SLIT("dto")),
	ptext
    	(case size2 of
    	    W  -> SLIT("i\t")
    	    F  -> SLIT("s\t")
    	    DF -> SLIT("d\t")),
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

pprInstr (CALL imm n _)
  = hcat [ ptext SLIT("\tcall\t"), pprImm imm, comma, int n ]
\end{code}

Continue with SPARC-only printing bits and bobs:
\begin{code}
pprRI :: RI -> SDoc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprSizeRegReg :: FAST_STRING -> Size -> Reg -> Reg -> SDoc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F  -> ptext SLIT("s\t")
    	    DF -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprSizeRegRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Reg -> SDoc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F  -> ptext SLIT("s\t")
    	    DF -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

pprRegRIReg :: FAST_STRING -> Bool -> Reg -> RI -> Reg -> SDoc
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

pprRIReg :: FAST_STRING -> Bool -> RI -> Reg -> SDoc
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

#endif {-sparc_TARGET_ARCH-}
\end{code}

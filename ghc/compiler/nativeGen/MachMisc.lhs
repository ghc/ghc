%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[MachMisc]{Description of various machine-specific things}

\begin{code}
#include "HsVersions.h"
#include "nativeGen/NCG.h"

module MachMisc (

	fixedHdrSizeInWords, varHdrSizeInWords,
	charLikeSize, intLikeSize, mutHS, dataHS, foHS,
	sizeOf, primRepToSize,

	eXTRA_STK_ARGS_HERE,

	volatileSaves, volatileRestores,

	storageMgrInfo, smCAFlist, smOldLim, smOldMutables,
	smStablePtrTable,

	targetMaxDouble, targetMaxInt, targetMinDouble, targetMinInt,

	underscorePrefix,
	fmtAsmLbl,
	cvtLitLit,
	exactLog2,

	Instr(..),  IF_ARCH_i386(Operand(..) COMMA,)
	Cond(..),
	Size(..)
	
#if alpha_TARGET_ARCH
	, RI(..)
#endif
#if i386_TARGET_ARCH
#endif
#if sparc_TARGET_ARCH
	, RI(..), riZero
#endif
    ) where

IMPORT_1_3(Char(isDigit))
IMP_Ubiq(){-uitous-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(AbsCLoop)		( fixedHdrSizeInWords, varHdrSizeInWords ) -- paranoia
IMPORT_DELOOPER(NcgLoop)		( underscorePrefix, fmtAsmLbl ) -- paranoia
#endif

import AbsCSyn		( MagicId(..) ) 
import AbsCUtils	( magicIdPrimRep )
import CLabel           ( CLabel )
import CmdLineOpts	( opt_SccProfilingOn )
import Literal		( mkMachInt, Literal(..) )
import MachRegs		( stgReg, callerSaves, RegLoc(..),
			  Imm(..), Reg(..)
#if __GLASGOW_HASKELL__ >= 202
		        )
import qualified MachRegs (Addr)
#define MachRegsAddr MachRegs.Addr
#else
			, Addr(..)
			)
#define MachRegsAddr Addr
#endif

import OrdList		( OrdList )
import PrimRep		( PrimRep(..) )
import SMRep		( SMRep(..), SMSpecRepKind(..), SMUpdateKind(..) )
import Stix		( StixTree(..), StixReg(..), sStLitLbl,
			  CodeSegment
			)
import Util		( panic )
\end{code}

\begin{code}
underscorePrefix :: Bool   -- leading underscore on labels?

underscorePrefix
  = IF_ARCH_alpha(False
    ,{-else-} IF_ARCH_i386(
	IF_OS_linuxaout(True
	, IF_OS_freebsd(True
	, IF_OS_cygwin32(True
	, IF_OS_bsdi(True
	, {-otherwise-} False)))
        )
     ,{-else-}IF_ARCH_sparc(
	IF_OS_sunos4(True, {-otherwise-} False)
     ,)))

---------------------------
fmtAsmLbl :: String -> String  -- for formatting labels

fmtAsmLbl s
  =  IF_ARCH_alpha(
     {- The alpha assembler likes temporary labels to look like $L123
	instead of L123.  (Don't toss the L, because then Lf28
	turns into $f28.)
     -}
     '$' : s
     ,{-otherwise-}
     s
     )

---------------------------
cvtLitLit :: String -> String

--
-- Rather than relying on guessing, use FILE_SIZE to compute the
-- _iob offsets.
--
cvtLitLit "stdin"  = IF_ARCH_alpha("_iob+0" {-probably OK...-}
		    ,IF_ARCH_i386("_IO_stdin_"
		    ,IF_ARCH_sparc("__iob+0x0"{-probably OK...-}
		    ,)))

cvtLitLit "stdout" = IF_ARCH_alpha("_iob+"++show (``FILE_SIZE''::Int)
		    ,IF_ARCH_i386("_IO_stdout_"
		    ,IF_ARCH_sparc("__iob+"++show (``FILE_SIZE''::Int)
		    ,)))
cvtLitLit "stderr" = IF_ARCH_alpha("_iob+"++show (2*(``FILE_SIZE''::Int))
		    ,IF_ARCH_i386("_IO_stderr_"
		    ,IF_ARCH_sparc("__iob+"++show (2*(``FILE_SIZE''::Int))
		    ,)))
{-
cvtLitLit "stdout" = IF_ARCH_alpha("_iob+56"{-dodgy *at best*...-}
		    ,IF_ARCH_i386("_IO_stdout_"
		    ,IF_ARCH_sparc("__iob+0x10"{-dodgy *at best*...-}
		    ,)))
cvtLitLit "stderr" = IF_ARCH_alpha("_iob+112"{-dodgy *at best*...-}
		    ,IF_ARCH_i386("_IO_stderr_"
		    ,IF_ARCH_sparc("__iob+0x20"{-dodgy *at best*...-}
		    ,)))
-}
cvtLitLit s
  | isHex s   = s
  | otherwise = error ("Native code generator can't handle ``" ++ s ++ "''")
  where
    isHex ('0':'x':xs) = all isHexDigit xs
    isHex _ = False
    -- Now, where have I seen this before?
    isHexDigit c = isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'
\end{code}

% ----------------------------------------------------------------

We (allegedly) put the first six C-call arguments in registers;
where do we start putting the rest of them?
\begin{code}
eXTRA_STK_ARGS_HERE :: Int
eXTRA_STK_ARGS_HERE
  = IF_ARCH_alpha(0, IF_ARCH_i386(23{-6x4bytes-}, IF_ARCH_sparc(23,???)))
\end{code}

% ----------------------------------------------------------------

@fixedHdrSizeInWords@ and @varHdrSizeInWords@: these are not dependent
on target architecture.
\begin{code}
fixedHdrSizeInWords :: Int

fixedHdrSizeInWords
  = 1{-info ptr-} + profFHS + parFHS + tickyFHS
    -- obviously, we aren't taking non-sequential too seriously yet
  where
    profFHS  = if opt_SccProfilingOn then 1 else 0
    parFHS   = {-if PAR or GRAN then 1 else-} 0
    tickyFHS = {-if ticky ... then 1 else-} 0

varHdrSizeInWords :: SMRep -> Int{-in words-}

varHdrSizeInWords sm
  = case sm of
    StaticRep _ _	   -> 0
    SpecialisedRep _ _ _ _ -> 0
    GenericRep _ _ _	   -> 0
    BigTupleRep _	   -> 1
    MuTupleRep _	   -> 2 {- (1 + GC_MUT_RESERVED_WORDS) -}
    DataRep _		   -> 1
    DynamicRep		   -> 2
    BlackHoleRep	   -> 0
    PhantomRep		   -> panic "MachMisc.varHdrSizeInWords:phantom"
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Static closure sizes:
\begin{code}
charLikeSize, intLikeSize :: Int

charLikeSize = blahLikeSize CharLikeRep
intLikeSize  = blahLikeSize IntLikeRep

blahLikeSize blah
  = fromInteger (sizeOf PtrRep)
  * (fixedHdrSizeInWords + varHdrSizeInWords blahLikeRep + 1)
  where
    blahLikeRep = SpecialisedRep blah 0 1 SMNormalForm

--------
mutHS, dataHS, foHS :: StixTree

mutHS  = blah_hs (MuTupleRep 0)
dataHS = blah_hs (DataRep 0)

{- Semi-hack: to avoid introducing ForeignObjRep,
   we hard-code the VHS for ForeignObj here.
-}
foHS   
  = StInt (toInteger words)
  where
    words = fixedHdrSizeInWords + 1{-FOREIGN_VHS-}

blah_hs blah
  = StInt (toInteger words)
  where
    words = fixedHdrSizeInWords + varHdrSizeInWords blah
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Size of a @PrimRep@, in bytes.

\begin{code}
sizeOf :: PrimRep -> Integer{-in bytes-}
    -- the result is an Integer only because it's more convenient

sizeOf pr = case (primRepToSize pr) of
  IF_ARCH_alpha({B -> 1; BU -> 1; {-W -> 2; WU -> 2; L -> 4; SF -> 4;-} _ -> 8},)
  IF_ARCH_sparc({B -> 1; BU -> 1; {-HW -> 2; HWU -> 2;-} W -> 4; {-D -> 8;-} F -> 4; DF -> 8},)
  IF_ARCH_i386( {B -> 1; {-S -> 2;-} L -> 4; F -> 4; DF -> 8 },)
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Now the volatile saves and restores.  We add the basic guys to the
list of ``user'' registers provided.  Note that there are more basic
registers on the restore list, because some are reloaded from
constants.

(@volatileRestores@ used only for wrapper-hungry PrimOps.)

\begin{code}
volatileSaves, volatileRestores :: [MagicId] -> [StixTree]

save_cands    = [BaseReg,SpA,SuA,SpB,SuB,Hp,HpLim,RetReg]
restore_cands = save_cands ++ [StkStubReg,StdUpdRetVecReg]

volatileSaves vols
  = map save ((filter callerSaves) (save_cands ++ vols))
  where
    save x = StAssign (magicIdPrimRep x) loc reg
      where
	reg = StReg (StixMagicId x)
	loc = case stgReg x of
		Save loc -> loc
		Always _ -> panic "volatileSaves"

volatileRestores vols
  = map restore ((filter callerSaves) (restore_cands ++ vols))
  where
    restore x = StAssign (magicIdPrimRep x) reg loc
      where
	reg = StReg (StixMagicId x)
	loc = case stgReg x of
		Save loc -> loc
		Always _ -> panic "volatileRestores"
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Obviously slightly weedy
(Note that the floating point values aren't terribly important.)
ToDo: Fix!(JSM)
\begin{code}
targetMinDouble = MachDouble (-1.7976931348623157e+308)
targetMaxDouble = MachDouble (1.7976931348623157e+308)
targetMinInt = mkMachInt (-2147483647)
targetMaxInt = mkMachInt 2147483647
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Storage manager nonsense.  Note that the indices are dependent on
the definition of the smInfo structure in SMinterface.lh

\begin{code}
storageMgrInfo, smCAFlist, smOldMutables, smOldLim :: StixTree

storageMgrInfo   = sStLitLbl SLIT("StorageMgrInfo")
smCAFlist        = StInd PtrRep (StIndex PtrRep storageMgrInfo (StInt SM_CAFLIST))
smOldMutables    = StInd PtrRep (StIndex PtrRep storageMgrInfo (StInt SM_OLDMUTABLES))
smOldLim         = StInd PtrRep (StIndex PtrRep storageMgrInfo (StInt SM_OLDLIM))
smStablePtrTable = StInd PtrRep (StIndex PtrRep storageMgrInfo (StInt SM_STABLEPOINTERTABLE))
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

This algorithm for determining the $\log_2$ of exact powers of 2 comes
from GCC.  It requires bit manipulation primitives, and we use GHC
extensions.  Tough.

\begin{code}
w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x::Int#)

exactLog2 :: Integer -> Maybe Integer
exactLog2 x
  = if (x <= 0 || x >= 2147483648) then
       Nothing
    else
       case (fromInteger x) of { I# x# ->
       if (w2i ((i2w x#) `and#` (i2w (0# -# x#))) /=# x#) then
	  Nothing
       else
	  Just (toInteger (I# (pow2 x#)))
       }
  where
    shiftr x y = shiftRA# x y

    pow2 x# | x# ==# 1# = 0#
            | otherwise = 1# +# pow2 (w2i (i2w x# `shiftr` i2w_s 1#))
\end{code}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

\begin{code}
data Cond
#if alpha_TARGET_ARCH
  = ALWAYS	-- For BI (same as BR)
  | EQQ		-- For CMP and BI (NB: "EQ" is a 1.3 Prelude name)
  | GE		-- For BI only
  | GTT		-- For BI only (NB: "GT" is a 1.3 Prelude name)
  | LE		-- For CMP and BI
  | LTT		-- For CMP and BI (NB: "LT" is a 1.3 Prelude name)
  | NE		-- For BI only
  | NEVER	-- For BI (null instruction)
  | ULE		-- For CMP only
  | ULT		-- For CMP only
#endif
#if i386_TARGET_ARCH
  = ALWAYS	-- What's really used? ToDo
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
  | POS
#endif
#if sparc_TARGET_ARCH
  = ALWAYS	-- What's really used? ToDo
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
#endif
\end{code}

\begin{code}
data Size
#if alpha_TARGET_ARCH
    = B	    -- byte
    | BU
--  | W	    -- word (2 bytes): UNUSED
--  | WU    -- : UNUSED
--  | L	    -- longword (4 bytes): UNUSED
    | Q	    -- quadword (8 bytes)
--  | FF    -- VAX F-style floating pt: UNUSED
--  | GF    -- VAX G-style floating pt: UNUSED
--  | DF    -- VAX D-style floating pt: UNUSED
--  | SF    -- IEEE single-precision floating pt: UNUSED
    | TF    -- IEEE double-precision floating pt
#endif
#if i386_TARGET_ARCH
    = B	    -- byte (lower)
--  | HB    -- higher byte **UNUSED**
--  | S	    -- : UNUSED
    | L
    | F	    -- IEEE single-precision floating pt
    | DF    -- IEEE single-precision floating pt
#endif
#if sparc_TARGET_ARCH
    = B     -- byte (signed)
    | BU    -- byte (unsigned)
--  | HW    -- halfword, 2 bytes (signed): UNUSED
--  | HWU   -- halfword, 2 bytes (unsigned): UNUSED
    | W	    -- word, 4 bytes
--  | D	    -- doubleword, 8 bytes: UNUSED
    | F	    -- IEEE single-precision floating pt
    | DF    -- IEEE single-precision floating pt
#endif

primRepToSize :: PrimRep -> Size

primRepToSize PtrRep	    = IF_ARCH_alpha( Q,  IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize CodePtrRep    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize DataPtrRep    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize RetRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize CostCentreRep = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize CharRep	    = IF_ARCH_alpha( BU, IF_ARCH_i386( L, IF_ARCH_sparc( BU,)))
primRepToSize IntRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize WordRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize AddrRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize FloatRep	    = IF_ARCH_alpha( TF, IF_ARCH_i386( F, IF_ARCH_sparc( F ,)))
primRepToSize DoubleRep	    = IF_ARCH_alpha( TF, IF_ARCH_i386( DF,IF_ARCH_sparc( DF,)))
primRepToSize ArrayRep	    = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize ByteArrayRep  = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize StablePtrRep  = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
primRepToSize ForeignObjRep  = IF_ARCH_alpha( Q,	 IF_ARCH_i386( L, IF_ARCH_sparc( W ,)))
\end{code}

%************************************************************************
%*									*
\subsection{Machine's assembly language}
%*									*
%************************************************************************

We have a few common ``instructions'' (nearly all the pseudo-ops) but
mostly all of @Instr@ is machine-specific.

\begin{code}
data Instr
  = COMMENT FAST_STRING		-- comment pseudo-op
  | SEGMENT CodeSegment		-- {data,text} segment pseudo-op
  | LABEL   CLabel		-- global label pseudo-op
  | ASCII   Bool		-- True <=> needs backslash conversion
	    String		-- the literal string
  | DATA    Size
	    [Imm]
\end{code}

\begin{code}
#if alpha_TARGET_ARCH

-- data Instr continues...

-- Loads and stores.

	      |	LD	      Size Reg MachRegsAddr -- size, dst, src
	      | LDA	      Reg MachRegsAddr	    -- dst, src
	      | LDAH	      Reg MachRegsAddr	    -- dst, src
	      | LDGP	      Reg MachRegsAddr	    -- dst, src
	      | LDI	      Size Reg Imm  -- size, dst, src
	      | ST	      Size Reg MachRegsAddr -- size, src, dst

-- Int Arithmetic.

	      | CLR	      Reg		    -- dst
	      | ABS	      Size RI Reg	    -- size, src, dst
	      | NEG	      Size Bool RI Reg	    -- size, overflow, src, dst
	      | ADD	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | SADD	      Size Size Reg RI Reg  -- size, scale, src, src, dst
	      | SUB	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | SSUB	      Size Size Reg RI Reg  -- size, scale, src, src, dst
	      | MUL	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | DIV	      Size Bool Reg RI Reg  -- size, unsigned, src, src, dst
	      | REM	      Size Bool Reg RI Reg  -- size, unsigned, src, src, dst

-- Simple bit-twiddling.

	      | NOT	      RI Reg
	      | AND	      Reg RI Reg
	      | ANDNOT	      Reg RI Reg
	      | OR	      Reg RI Reg
	      | ORNOT	      Reg RI Reg
	      | XOR	      Reg RI Reg
	      | XORNOT	      Reg RI Reg
	      | SLL	      Reg RI Reg
	      | SRL	      Reg RI Reg
	      | SRA	      Reg RI Reg

	      | ZAP	      Reg RI Reg
	      | ZAPNOT	      Reg RI Reg

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
	      | JMP	      Reg MachRegsAddr Int
	      | BSR	      Imm Int
	      | JSR	      Reg MachRegsAddr Int

-- Alpha-specific pseudo-ops.

	      | FUNBEGIN CLabel
	      | FUNEND CLabel

data RI
  = RIReg Reg
  | RIImm Imm

#endif {- alpha_TARGET_ARCH -}
\end{code}

\begin{code}
#if i386_TARGET_ARCH

-- data Instr continues...

-- Moves.

	      |	MOV	      Size Operand Operand
	      | MOVZX	      Size Operand Operand -- size is the size of operand 2
	      | MOVSX	      Size Operand Operand -- size is the size of operand 2

-- Load effective address (also a very useful three-operand add instruction :-)

	      | LEA           Size Operand Operand

-- Int Arithmetic.

	      | ADD	      Size Operand Operand
	      | SUB	      Size Operand Operand

-- Multiplication (signed and unsigned), Division (signed and unsigned),
-- result in %eax, %edx.

	      | IMUL	      Size Operand Operand
	      | IDIV	      Size Operand

-- Simple bit-twiddling.

	      | AND	      Size Operand Operand
	      | OR	      Size Operand Operand
	      | XOR	      Size Operand Operand
	      | NOT	      Size Operand
	      | NEGI	      Size Operand -- NEG instruction (name clash with Cond)
	      | SHL	      Size Operand Operand -- 1st operand must be an Imm
	      | SAR	      Size Operand Operand -- 1st operand must be an Imm
	      | SHR	      Size Operand Operand -- 1st operand must be an Imm
	      | NOP

-- Float Arithmetic. -- ToDo for 386

-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single instructions
-- right up until we spit them out.

	      | SAHF	      -- stores ah into flags
    	      | FABS
	      | FADD	      Size Operand -- src
	      | FADDP
	      | FIADD	      Size MachRegsAddr -- src
    	      | FCHS
    	      | FCOM	      Size Operand -- src
    	      | FCOS
	      | FDIV	      Size Operand -- src
	      | FDIVP
	      | FIDIV	      Size MachRegsAddr -- src
	      | FDIVR	      Size Operand -- src
	      | FDIVRP
	      | FIDIVR	      Size MachRegsAddr -- src
    	      | FICOM	      Size MachRegsAddr -- src
    	      | FILD	      Size MachRegsAddr Reg -- src, dst
    	      | FIST	      Size MachRegsAddr -- dst
    	      | FLD	      Size Operand -- src
    	      | FLD1
    	      | FLDZ
    	      | FMUL	      Size Operand -- src
    	      | FMULP
    	      | FIMUL	      Size MachRegsAddr -- src
    	      | FRNDINT
    	      | FSIN
    	      | FSQRT
    	      | FST	      Size Operand -- dst
    	      | FSTP	      Size Operand -- dst
	      | FSUB	      Size Operand -- src
	      | FSUBP
	      | FISUB	      Size MachRegsAddr -- src
	      | FSUBR	      Size Operand -- src
	      | FSUBRP
	      | FISUBR	      Size MachRegsAddr -- src
	      | FTST
    	      | FCOMP	      Size Operand -- src
    	      | FUCOMPP
	      | FXCH
	      | FNSTSW
	      | FNOP

-- Comparison

	      | TEST          Size Operand Operand
	      | CMP           Size Operand Operand
	      | SETCC         Cond Operand

-- Stack Operations.

	      | PUSH          Size Operand
	      | POP           Size Operand

-- Jumping around.

	      | JMP	      Operand -- target
	      | JXX	      Cond CLabel -- target
	      | CALL	      Imm

-- Other things.

	      | CLTD -- sign extend %eax into %edx:%eax

data Operand
  = OpReg  Reg	        -- register
  | OpImm  Imm	        -- immediate value
  | OpAddr MachRegsAddr	-- memory reference

#endif {- i386_TARGET_ARCH -}
\end{code}

\begin{code}
#if sparc_TARGET_ARCH

-- data Instr continues...

-- Loads and stores.

	      | LD	      Size MachRegsAddr Reg -- size, src, dst
	      | ST	      Size Reg MachRegsAddr -- size, src, dst

-- Int Arithmetic.

	      | ADD	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst
	      | SUB	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst

-- Simple bit-twiddling.

	      | AND	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | ANDN	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | OR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | ORN	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | XOR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | XNOR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | SLL	      Reg RI Reg -- src1, src2, dst
	      | SRL	      Reg RI Reg -- src1, src2, dst
	      | SRA	      Reg RI Reg -- src1, src2, dst
	      | SETHI	      Imm Reg -- src, dst
	      | NOP	      -- Really SETHI 0, %g0, but worth an alias

-- Float Arithmetic.

-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single instructions
-- right up until we spit them out.

    	      | FABS	      Size Reg Reg -- src dst
	      | FADD	      Size Reg Reg Reg -- src1, src2, dst
    	      | FCMP	      Bool Size Reg Reg -- exception?, src1, src2, dst
	      | FDIV	      Size Reg Reg Reg -- src1, src2, dst
    	      | FMOV	      Size Reg Reg -- src, dst
	      | FMUL	      Size Reg Reg Reg -- src1, src2, dst
    	      | FNEG	      Size Reg Reg -- src, dst
    	      | FSQRT	      Size Reg Reg -- src, dst
	      | FSUB	      Size Reg Reg Reg -- src1, src2, dst
    	      | FxTOy	      Size Size Reg Reg -- src, dst

-- Jumping around.

	      | BI	      Cond Bool Imm -- cond, annul?, target
    	      | BF  	      Cond Bool Imm -- cond, annul?, target

	      | JMP	      MachRegsAddr -- target
	      | CALL	      Imm Int Bool -- target, args, terminal

data RI = RIReg Reg
	| RIImm Imm

riZero :: RI -> Bool

riZero (RIImm (ImmInt 0))	    = True
riZero (RIImm (ImmInteger 0))	    = True
riZero (RIReg (FixedReg ILIT(0)))   = True
riZero _			    = False

#endif {- sparc_TARGET_ARCH -}
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: AbsCSyn.lhs,v 1.55 2003/07/28 16:05:30 simonmar Exp $
%
\section[AbstractC]{Abstract C: the last stop before machine code}

This ``Abstract C'' data type describes the raw Spineless Tagless
machine model at a C-ish level; it is ``abstract'' in that it only
includes C-like structures that we happen to need.  The conversion of
programs from @StgSyntax@ (basically a functional language) to
@AbstractC@ (basically imperative C) is the heart of code generation.
From @AbstractC@, one may convert to real C (for portability) or to
raw assembler/machine code.

\begin{code}
module AbsCSyn where	-- export everything

#include "HsVersions.h"

import {-# SOURCE #-} ClosureInfo ( ClosureInfo )

import CLabel
import Constants   	( mAX_Vanilla_REG, mAX_Float_REG,
			  mAX_Double_REG, spRelToInt )
import CostCentre       ( CostCentre, CostCentreStack )
import Literal		( mkMachInt, Literal(..) )
import ForeignCall	( CCallSpec )
import PrimRep		( PrimRep(..) )
import MachOp		( MachOp(..) )
import Unique           ( Unique )
import StgSyn		( StgOp )
import TyCon		( TyCon )
import Bitmap		( Bitmap, mAX_SMALL_BITMAP_SIZE )
import SMRep		( StgWord, StgHalfWord )
import FastTypes
import FastString
\end{code}

@AbstractC@ is a list of Abstract~C statements, but the data structure
is tree-ish, for easier and more efficient putting-together.
\begin{code}
absCNop = AbsCNop

data AbstractC
  = AbsCNop
  | AbsCStmts		AbstractC AbstractC

  -- and the individual stmts...
\end{code}

A note on @CAssign@: In general, the type associated with an assignment
is the type of the lhs.  However, when the lhs is a pointer to mixed
types (e.g. SpB relative), the type of the assignment is the type of
the rhs for float types, or the generic StgWord for all other types.
(In particular, a CharRep on the rhs is promoted to IntRep when
stored in a mixed type location.)

\begin{code}
  | CAssign
	!CAddrMode 	-- target
	!CAddrMode	-- source

  | CJump
	CAddrMode	-- Put this in the program counter
			-- eg `CJump (CReg (VanillaReg PtrRep 1))' puts Ret1 in PC
			-- Enter can be done by:
			--	  CJump (CVal NodeRel zeroOff)

  | CFallThrough
	CAddrMode	-- Fall through into this routine
    	    	    	-- (for the benefit of the native code generators)
    	    	    	-- Equivalent to CJump in C land

  | CReturn		-- Perform a return
    	CAddrMode   	-- Address of a RET_<blah> info table
    	ReturnInfo  	-- Whether it's a direct or vectored return

  | CSwitch !CAddrMode
	[(Literal, AbstractC)]	-- alternatives
	AbstractC		-- default; if there is no real Abstract C in here
				-- (e.g., all comments; see function "nonemptyAbsC"),
				-- then that means the default _cannot_ occur.
				-- If there is only one alternative & no default code,
				-- then there is no need to check the tag.
				-- Therefore, e.g.:
				--  CSwitch m [(tag,code)] AbsCNop == code

  | CCodeBlock CLabel AbstractC
			-- A labelled block of code; this "statement" is not
			-- executed; rather, the labelled code will be hoisted
			-- out to the top level (out of line) & it can be
			-- jumped to.

  | CInitHdr		-- to initialise the header of a closure (both fixed/var parts)
	ClosureInfo
	CAddrMode	-- address of the info ptr
	!CAddrMode	-- cost centre to place in closure
			--   CReg CurCostCentre or CC_HDR(R1.p{-Node-})
	Int		-- size of closure, for profiling

  -- NEW CASES FOR EXPANDED PRIMOPS

  | CMachOpStmt			-- Machine-level operation
	CAddrMode		-- result
	MachOp
	[CAddrMode]		-- Arguments
        (Maybe [MagicId])	-- list of regs which need to be preserved
	-- across the primop.  This is allowed to be Nothing only if
	-- machOpIsDefinitelyInline returns True.  And that in turn may
	-- only return True if we are absolutely sure that the mach op
	-- can be done inline on all platforms.  

  | CSequential		-- Do the nested AbstractCs sequentially.
	[AbstractC]	-- In particular, as far as the AbsCUtils.doSimultaneously
			-- is concerned, these stmts are to be treated as atomic
			-- and are not to be reordered.

  -- end of NEW CASES FOR EXPANDED PRIMOPS

  | COpStmt
	[CAddrMode]	-- Results
	StgOp
	[CAddrMode]	-- Arguments
	[MagicId]	-- Potentially volatile/live registers
			-- (to save/restore around the call/op)

	-- INVARIANT: When a PrimOp which can cause GC is used, the
	-- only live data is tidily on the STG stacks or in the STG
	-- registers (the code generator ensures this).
	--
	-- Why this?  Because if the arguments were arbitrary
	-- addressing modes, they might be things like (Hp+6) which
	-- will get utterly spongled by GC.

  | CSimultaneous	-- Perform simultaneously all the statements
	AbstractC	-- in the nested AbstractC.  They are only
			-- allowed to be CAssigns, COpStmts and AbsCNops, so the
			-- "simultaneous" part just concerns making
			-- sure that permutations work.
			-- For example { a := b, b := a }
			-- 	needs to go via (at least one) temporary

  | CCheck 		-- heap or stack checks, or both.  
	CCheckMacro 	-- These might include some code to fill in tags 
	[CAddrMode]	-- on the stack, so we can't use CMacroStmt below.
	AbstractC

  | CRetDirect			-- Direct return
        !Unique			-- for making labels
	AbstractC   		-- return code
	C_SRT			-- SRT info
	Liveness		-- stack liveness at the return point

  -- see the notes about these next few; they follow below...
  | CMacroStmt		CStmtMacro	[CAddrMode]
  | CCallProfCtrMacro	FastString	[CAddrMode]
  | CCallProfCCMacro	FastString	[CAddrMode]

    {- The presence of this constructor is a makeshift solution;
       it being used to work around a gcc-related problem of
       handling typedefs within statement blocks (or, rather,
       the inability to do so.)
       
       The AbstractC flattener takes care of lifting out these
       typedefs if needs be (i.e., when generating .hc code and
       compiling 'foreign import dynamic's)
    -}
  | CCallTypedef Bool {- True => use "typedef"; False => use "extern"-}
  		 CCallSpec Unique [CAddrMode] [CAddrMode]

  -- *** the next three [or so...] are DATA (those above are CODE) ***

  | CStaticClosure
	CLabel			-- The closure's label
	ClosureInfo		-- Todo: maybe info_lbl & closure_lbl instead?
	CAddrMode		-- cost centre identifier to place in closure
	[CAddrMode]		-- free vars; ptrs, then non-ptrs.

  | CSRT CLabel [CLabel]  	-- SRT declarations: basically an array of 
				-- pointers to static closures.
  
  | CBitmap Liveness		-- A "large" bitmap to be emitted

  | CSRTDesc 			-- A "large" SRT descriptor (one that doesn't
				-- fit into the half-word bitmap in the itbl).
	!CLabel			-- Label for this SRT descriptor
	!CLabel			-- Pointer to the SRT
	!Int			-- Offset within the SRT
	!Int			-- Length
	!Bitmap			-- Bitmap

  | CClosureInfoAndCode
	ClosureInfo		-- Explains placement and layout of closure
	AbstractC		-- Entry point code

  | CRetVector			-- A labelled block of static data
	CLabel
	[CAddrMode]
	C_SRT			-- SRT info
	Liveness		-- stack liveness at the return point

  | CClosureTbl 		-- table of constructors for enumerated types
	TyCon			-- which TyCon this table is for

  | CModuleInitBlock		-- module initialisation block
	CLabel			-- "plain" label for init block
	CLabel			-- label for init block (with ver + way info)
	AbstractC		-- initialisation code

  | CCostCentreDecl		-- A cost centre *declaration*
	Bool			-- True  <=> local => full declaration
				-- False <=> extern; just say so
	CostCentre

  | CCostCentreStackDecl	-- A cost centre stack *declaration*
	CostCentreStack		-- this is the declaration for a
				-- pre-defined singleton CCS (see 
				-- CostCentre.lhs)

  | CSplitMarker		-- Split into separate object modules here

-- C_SRT is what StgSyn.SRT gets translated to... 
-- we add a label for the table, and expect only the 'offset/length' form

data C_SRT = NoC_SRT
	   | C_SRT !CLabel !Int{-offset-} !StgHalfWord{-bitmap or escape-}

needsSRT :: C_SRT -> Bool
needsSRT NoC_SRT       = False
needsSRT (C_SRT _ _ _) = True
\end{code}

About @CMacroStmt@, etc.: notionally, they all just call some
arbitrary C~macro or routine, passing the @CAddrModes@ as arguments.
However, we distinguish between various flavours of these things,
mostly just to keep things somewhat less wild and wooly.

\begin{description}
\item[@CMacroStmt@:]
Some {\em essential} bits of the STG execution model are done with C
macros.  An example is @STK_CHK@, which checks for stack-space
overflow.  This enumeration type lists all such macros:
\begin{code}
data CStmtMacro
  = UPD_CAF				-- update CAF closure with indirection
  | UPD_BH_UPDATABLE			-- eager backholing
  | UPD_BH_SINGLE_ENTRY			-- more eager blackholing
  | PUSH_UPD_FRAME			-- push update frame
  | SET_TAG				-- set TagReg if it exists
      -- dataToTag# primop -- *only* used in unregisterised builds.
      -- (see AbsCUtils.dsCOpStmt)
  | DATA_TO_TAGZH

  | REGISTER_FOREIGN_EXPORT		-- register a foreign exported fun
  | REGISTER_IMPORT			-- register an imported module
  | REGISTER_DIMPORT                    -- register an imported module from
                                        -- another DLL

  | GRAN_FETCH	    		-- for GrAnSim only  -- HWL
  | GRAN_RESCHEDULE   		-- for GrAnSim only  -- HWL
  | GRAN_FETCH_AND_RESCHEDULE	-- for GrAnSim only  -- HWL
  | THREAD_CONTEXT_SWITCH   	-- for GrAnSim only  -- HWL
  | GRAN_YIELD   		-- for GrAnSim only  -- HWL 
\end{code}

Heap/Stack checks.  There are far too many of these.

\begin{code}
data CCheckMacro

  = HP_CHK_NP				-- heap/stack checks when
  | STK_CHK_NP				-- node points to the closure
  | HP_STK_CHK_NP

  | HP_CHK_FUN				-- heap/stack checks when
  | STK_CHK_FUN				-- node doesn't point
  | HP_STK_CHK_FUN
					-- case alternative heap checks:

  | HP_CHK_NOREGS			--   no registers live
  | HP_CHK_UNPT_R1			--   R1 is boxed/unlifted
  | HP_CHK_UNBX_R1			--   R1 is unboxed
  | HP_CHK_F1				--   FloatReg1 (only) is live 
  | HP_CHK_D1				--   DblReg1   (only) is live
  | HP_CHK_L1				--   LngReg1   (only) is live

  | HP_CHK_UNBX_TUPLE			-- unboxed tuple heap check
\end{code}

\item[@CCallProfCtrMacro@:]
The @String@ names a macro that, if \tr{#define}d, will bump one/some
of the STG-event profiling counters.

\item[@CCallProfCCMacro@:]
The @String@ names a macro that, if \tr{#define}d, will perform some
cost-centre-profiling-related action.
\end{description}

%************************************************************************
%*									*
\subsection[CAddrMode]{C addressing modes}
%*									*
%************************************************************************

\begin{code}
data CAddrMode
  = CVal  RegRelative PrimRep
			-- On RHS of assign: Contents of Magic[n]
			-- On LHS of assign: location Magic[n]
			-- (ie at addr Magic+n)

  | CAddr RegRelative
			-- On RHS of assign: Address of Magic[n]; ie Magic+n
			-- 	n=0 gets the Magic location itself
			--      (NB: n=0 case superceded by CReg)
			-- On LHS of assign: only sensible if n=0,
			--	which gives the magic location itself
			--      (NB: superceded by CReg)

             -- JRS 2002-02-05: CAddr is really scummy and should be fixed.
             -- The effect is that the semantics of CAddr depend on what the
             -- contained RegRelative is; it is decidely non-orthogonal.

  | CReg MagicId	-- To replace (CAddr MagicId 0)

  | CTemp !Unique !PrimRep	-- Temporary locations
	-- ``Temporaries'' correspond to local variables in C, and registers in
	-- native code.

  | CLbl    CLabel	-- Labels in the runtime system, etc.
	    PrimRep	-- the kind is so we can generate accurate C decls

  | CCharLike CAddrMode	-- The address of a static char-like closure for
			-- the specified character.  It is guaranteed to be in
			-- the range mIN_CHARLIKE..mAX_CHARLIKE

  | CIntLike CAddrMode	-- The address of a static int-like closure for the
			-- specified small integer.  It is guaranteed to be in
			-- the range mIN_INTLIKE..mAX_INTLIKE

  | CLit    Literal

  | CJoinPoint		-- This is used as the amode of a let-no-escape-bound
			-- variable.
	VirtualSpOffset	  -- Sp value after any volatile free vars
			  -- of the rhs have been saved on stack.
			  -- Just before the code for the thing is jumped to,
			  -- Sp will be set to this value,
			  -- and then any stack-passed args pushed,
			  -- then the code for this thing will be entered
  | CMacroExpr
    	!PrimRep    	-- the kind of the result
    	CExprMacro    	-- the macro to generate a value
	[CAddrMode]   	-- and its arguments
\end{code}

Various C macros for values which are dependent on the back-end layout.

\begin{code}

data CExprMacro
  = ENTRY_CODE
  | ARG_TAG				-- stack argument tagging
  | GET_TAG				-- get current constructor tag
  | CCS_HDR
  | BYTE_ARR_CTS		-- used when passing a ByteArray# to a ccall
  | PTRS_ARR_CTS		-- similarly for an Array#
  | ForeignObj_CLOSURE_DATA	-- and again for a ForeignObj#
\end{code}

Convenience functions:

\begin{code}
mkIntCLit :: Int -> CAddrMode
mkIntCLit i = CLit (mkMachInt (toInteger i))

mkWordCLit :: StgWord -> CAddrMode
mkWordCLit wd = CLit (MachWord (fromIntegral wd))

mkCString :: FastString -> CAddrMode
mkCString s = CLit (MachStr s)

mkCCostCentre :: CostCentre -> CAddrMode
mkCCostCentre cc = CLbl (mkCC_Label cc) DataPtrRep

mkCCostCentreStack :: CostCentreStack -> CAddrMode
mkCCostCentreStack ccs = CLbl (mkCCS_Label ccs) DataPtrRep
\end{code}

%************************************************************************
%*									*
\subsection[RegRelative]{@RegRelatives@: ???}
%*									*
%************************************************************************

\begin{code}
data RegRelative
  = HpRel 	FastInt	-- }
  | SpRel 	FastInt	-- }- offsets in StgWords
  | NodeRel	FastInt	-- }
  | CIndex	CAddrMode CAddrMode PrimRep	-- pointer arithmetic :-)
						-- CIndex a b k === (k*)a[b]

data ReturnInfo
  = DirectReturn    	    	    	-- Jump directly, if possible
  | StaticVectoredReturn Int		-- Fixed tag, starting at zero
  | DynamicVectoredReturn CAddrMode	-- Dynamic tag given by amode, starting at zero

hpRel :: VirtualHeapOffset 	-- virtual offset of Hp
      -> VirtualHeapOffset 	-- virtual offset of The Thing
      -> RegRelative		-- integer offset
hpRel hp off = HpRel (iUnbox (hp - off))

spRel :: VirtualSpOffset 	-- virtual offset of Sp
      -> VirtualSpOffset 	-- virtual offset of The Thing
      -> RegRelative		-- integer offset
spRel sp off = SpRel (iUnbox (spRelToInt sp off))

nodeRel :: VirtualHeapOffset
        -> RegRelative
nodeRel off = NodeRel (iUnbox off)

\end{code}

%************************************************************************
%*									*
\subsection[Liveness]{Liveness Masks}
%*									*
%************************************************************************

We represent liveness bitmaps as a BitSet (whose internal
representation really is a bitmap).  These are pinned onto case return
vectors to indicate the state of the stack for the garbage collector.

In the compiled program, liveness bitmaps that fit inside a single
word (StgWord) are stored as a single word, while larger bitmaps are
stored as a pointer to an array of words. 

\begin{code}
data Liveness = Liveness CLabel !Int Bitmap

maybeLargeBitmap :: Liveness -> AbstractC
maybeLargeBitmap liveness@(Liveness _ size _)
  | size <= mAX_SMALL_BITMAP_SIZE = AbsCNop
  | otherwise                     = CBitmap liveness
\end{code}

%************************************************************************
%*									*
\subsection[HeapOffset]{@Heap Offsets@}
%*									*
%************************************************************************

This used to be a grotesquely complicated datatype in an attempt to
hide the details of header sizes from the compiler itself.  Now these
constants are imported from the RTS, and we deal in real Ints.

\begin{code}
type HeapOffset = Int			-- ToDo: remove

type VirtualHeapOffset	= HeapOffset
type VirtualSpOffset	= Int

type HpRelOffset	= HeapOffset
type SpRelOffset	= Int
\end{code}

%************************************************************************
%*									*
\subsection[MagicId]{@MagicIds@: registers and such}
%*									*
%************************************************************************

\begin{code}
data MagicId
  = BaseReg 	-- mentioned only in nativeGen

  -- Argument and return registers
  | VanillaReg		-- pointers, unboxed ints and chars
	PrimRep
	FastInt	-- its number (1 .. mAX_Vanilla_REG)

  | FloatReg		-- single-precision floating-point registers
	FastInt	-- its number (1 .. mAX_Float_REG)

  | DoubleReg		-- double-precision floating-point registers
	FastInt	-- its number (1 .. mAX_Double_REG)

  -- STG registers
  | Sp			-- Stack ptr; points to last occupied stack location.
  | SpLim		-- Stack limit
  | Hp			-- Heap ptr; points to last occupied heap location.
  | HpLim		-- Heap limit register
  | CurCostCentre 	-- current cost centre register.
  | VoidReg 		-- see "VoidPrim" type; just a placeholder; 
			--   no actual register
  | LongReg	        -- long int registers (64-bit, really)
	PrimRep	        -- Int64Rep or Word64Rep
	FastInt	-- its number (1 .. mAX_Long_REG)

  | CurrentTSO		-- pointer to current thread's TSO
  | CurrentNursery	-- pointer to allocation area
  | HpAlloc		-- allocation count for heap check failure


node 	= VanillaReg PtrRep     (_ILIT 1) -- A convenient alias for Node
tagreg  = VanillaReg WordRep    (_ILIT 2) -- A convenient alias for TagReg

nodeReg = CReg node
\end{code}

We need magical @Eq@ because @VanillaReg@s come in multiple flavors.

\begin{code}
instance Eq MagicId where
    reg1 == reg2 = tag reg1 ==# tag reg2
     where
	tag BaseReg	     = (_ILIT(0) :: FastInt)
	tag Sp		     = _ILIT(1)
	tag SpLim	     = _ILIT(3)
	tag Hp		     = _ILIT(4)
	tag HpLim	     = _ILIT(5)
	tag CurCostCentre    = _ILIT(6)
	tag VoidReg	     = _ILIT(7)

	tag (VanillaReg _ i) = _ILIT(8) +# i

	tag (FloatReg i)  = _ILIT(8) +# maxv +# i
	tag (DoubleReg i) = _ILIT(8) +# maxv +# maxf +# i
	tag (LongReg _ i) = _ILIT(8) +# maxv +# maxf +# maxd +# i

        maxv = iUnbox mAX_Vanilla_REG
        maxf = iUnbox mAX_Float_REG
        maxd = iUnbox mAX_Double_REG
\end{code}

Returns True for any register that {\em potentially} dies across
C calls (or anything near equivalent).  We just say @True@ and
let the (machine-specific) registering macros sort things out...

\begin{code}
isVolatileReg :: MagicId -> Bool
isVolatileReg any = True
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: AbsCSyn.lhs,v 1.18 1998/12/02 13:17:16 simonm Exp $
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
module AbsCSyn {- (
	-- export everything
	AbstractC(..),
	CStmtMacro(..),
	CExprMacro(..),
	CAddrMode(..),
	ReturnInfo(..),
	mkAbstractCs, mkAbsCStmts, mkAlgAltsCSwitch,
	mkIntCLit,
	mkAbsCStmtList,
	mkCCostCentre,

	-- RegRelatives
	RegRelative(..),

	-- registers
	MagicId(..), node, infoptr,
	isVolatileReg,
	CostRes(Cost)
    )-} where

#include "HsVersions.h"

import {-# SOURCE #-} ClosureInfo ( ClosureInfo )

#if  ! OMIT_NATIVE_CODEGEN
import {-# SOURCE #-} MachMisc
#endif

import CLabel
import Constants   	( mAX_Vanilla_REG, mAX_Float_REG,
			  mAX_Double_REG, spRelToInt )
import CostCentre       ( CostCentre, CostCentreStack )
import Const		( mkMachInt, Literal )
import PrimRep		( PrimRep(..) )
import PrimOp           ( PrimOp )
import Unique           ( Unique )
import StgSyn		( SRT(..) )
import BitSet				-- for liveness masks

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
	CAddrMode 	-- target
	CAddrMode	-- source

  | CJump
	CAddrMode	-- Put this in the program counter
			-- eg `CJump (CReg (VanillaReg PtrRep 1))' puts Ret1 in PC
			-- Enter can be done by:
			--	  CJump (CVal NodeRel zeroOff)

  | CFallThrough
	CAddrMode	-- Fall through into this routine
    	    	    	-- (for the benefit of the native code generators)
    	    	    	-- Equivalent to CJump in C land

  | CReturn 	    	-- This used to be RetVecRegRel
    	CAddrMode   	-- Any base address mode
    	ReturnInfo  	-- How to get the return address from the base address

  | CSwitch CAddrMode
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
	RegRelative	-- address of the info ptr
	CAddrMode	-- cost centre to place in closure
			--   CReg CurCostCentre or CC_HDR(R1.p{-Node-})

  | COpStmt
	[CAddrMode]	-- Results
	PrimOp
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
        Unique			-- for making labels
	AbstractC   		-- return code
	(CLabel,SRT)		-- SRT info
	Liveness		-- stack liveness at the return point

  -- see the notes about these next few; they follow below...
  | CMacroStmt		CStmtMacro	[CAddrMode]
  | CCallProfCtrMacro	FAST_STRING	[CAddrMode]
  | CCallProfCCMacro	FAST_STRING	[CAddrMode]

    {- The presence of this constructor is a makeshift solution;
       it being used to work around a gcc-related problem of
       handling typedefs within statement blocks (or, rather,
       the inability to do so.)
       
       The AbstractC flattener takes care of lifting out these
       typedefs if needs be (i.e., when generating .hc code and
       compiling 'foreign import dynamic's)
    -}
  | CCallTypedef        PrimOp{-CCallOp-} [CAddrMode] [CAddrMode]

  -- *** the next three [or so...] are DATA (those above are CODE) ***

  | CStaticClosure
	CLabel	-- The (full, not base) label to use for labelling the closure.
	ClosureInfo
	CAddrMode		-- cost centre identifier to place in closure
	[CAddrMode]		-- free vars; ptrs, then non-ptrs.

  | CSRT CLabel [CLabel]  	-- SRT declarations: basically an array of 
				-- pointers to static closures.
  
  | CBitmap CLabel LivenessMask	-- A larger-than-32-bits bitmap.

  | CClosureInfoAndCode
	ClosureInfo		-- Explains placement and layout of closure
	AbstractC		-- Slow entry point code
	(Maybe AbstractC)
				-- Fast entry point code, if any
	(CLabel,SRT)		-- SRT info
	String			-- Closure description; NB we can't get this
				-- from ClosureInfo, because the latter refers 
				-- to the *right* hand side of a defn, whereas
				-- the  "description" refers to *left* hand side

  | CRetVector			-- A labelled block of static data
	CLabel
	[CAddrMode]
	(CLabel,SRT)		-- SRT info
	Liveness		-- stack liveness at the return point

  | CCostCentreDecl		-- A cost centre *declaration*
	Bool			-- True  <=> local => full declaration
				-- False <=> extern; just say so
	CostCentre

  | CCostCentreStackDecl	-- A cost centre stack *declaration*
	CostCentreStack		-- this is the declaration for a
				-- pre-defined singleton CCS (see 
				-- CostCentre.lhs)

  | CSplitMarker		-- Split into separate object modules here
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
  = ARGS_CHK				-- arg satisfaction check
  | ARGS_CHK_LOAD_NODE			-- arg check for top-level functions
  | UPD_CAF				-- update CAF closure with indirection
  | UPD_BH_UPDATABLE			-- eager backholing
  | UPD_BH_SINGLE_ENTRY			-- more eager blackholing
  | PUSH_UPD_FRAME			-- push update frame
  | PUSH_SEQ_FRAME			-- push seq frame
  | SET_TAG				-- set TagReg if it exists
  | GRAN_FETCH	    		-- for GrAnSim only  -- HWL
  | GRAN_RESCHEDULE   		-- for GrAnSim only  -- HWL
  | GRAN_FETCH_AND_RESCHEDULE	-- for GrAnSim only  -- HWL
  | THREAD_CONTEXT_SWITCH   	-- for GrAnSim only  -- HWL
  | GRAN_YIELD   		-- for GrAnSim only  -- HWL 
  deriving Text
\end{code}

Heap/Stack checks.  There are far too many of these.

\begin{code}
data CCheckMacro

  = HP_CHK_NP				-- heap/stack checks when
  | STK_CHK_NP				-- node points to the closure
  | HP_STK_CHK_NP
  | HP_CHK_SEQ_NP			-- for 'seq' style case alternatives

  | HP_CHK				-- heap/stack checks when
  | STK_CHK				-- node doesn't point
  | HP_STK_CHK
					-- case alternative heap checks:

  | HP_CHK_NOREGS			--   no registers live
  | HP_CHK_UNPT_R1			--   R1 is boxed/unlifted
  | HP_CHK_UNBX_R1			--   R1 is unboxed
  | HP_CHK_F1				--   FloatReg1 (only) is live 
  | HP_CHK_D1				--   DblReg1   (only) is live
  | HP_CHK_L1				--   LngReg1   (only) is live
  | HP_CHK_UT_ALT			--   unboxed tuple return.

  | HP_CHK_GEN				-- generic heap check
  deriving Text
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

  | CReg MagicId	-- To replace (CAddr MagicId 0)

  | CTableEntry     	    -- CVal should be generalized to allow this
    	    	CAddrMode   -- Base
    	        CAddrMode   -- Offset
    	    	PrimRep    -- For casting

  | CTemp Unique PrimRep	-- Temporary locations
	-- ``Temporaries'' correspond to local variables in C, and registers in
	-- native code.

  | CLbl    CLabel	-- Labels in the runtime system, etc.
	    PrimRep	-- the kind is so we can generate accurate C decls

  | CCharLike CAddrMode	-- The address of a static char-like closure for
			-- the specified character.  It is guaranteed to be in
			-- the range 0..255.

  | CIntLike CAddrMode	-- The address of a static int-like closure for the
			-- specified small integer.  It is guaranteed to be in
			-- the range mIN_INTLIKE..mAX_INTLIKE

  | CString FAST_STRING	-- The address of the null-terminated string
  | CLit    Literal
  | CLitLit FAST_STRING	-- completely literal literal: just spit this String
			-- into the C output
	    PrimRep

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
  deriving(Text)

\end{code}

Convenience functions:

\begin{code}
mkIntCLit :: Int -> CAddrMode
mkIntCLit i = CLit (mkMachInt (toInteger i))

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
  = HpRel 	FAST_INT	-- }
  | SpRel 	FAST_INT	-- }- offsets in StgWords
  | NodeRel	FAST_INT	-- }

data ReturnInfo
  = DirectReturn    	    	    	-- Jump directly, if possible
  | StaticVectoredReturn Int		-- Fixed tag, starting at zero
  | DynamicVectoredReturn CAddrMode	-- Dynamic tag given by amode, starting at zero

hpRel :: VirtualHeapOffset 	-- virtual offset of Hp
      -> VirtualHeapOffset 	-- virtual offset of The Thing
      -> RegRelative		-- integer offset
hpRel IBOX(hp) IBOX(off) = HpRel (hp _SUB_ off)

spRel :: VirtualSpOffset 	-- virtual offset of Sp
      -> VirtualSpOffset 	-- virtual offset of The Thing
      -> RegRelative		-- integer offset
spRel sp off = SpRel (case spRelToInt sp off of { IBOX(i) -> i })

nodeRel :: VirtualHeapOffset
        -> RegRelative
nodeRel IBOX(off) = NodeRel off

\end{code}

%************************************************************************
%*									*
\subsection[RegRelative]{@RegRelatives@: ???}
%*									*
%************************************************************************

We represent liveness bitmaps as a BitSet (whose internal
representation really is a bitmap).  These are pinned onto case return
vectors to indicate the state of the stack for the garbage collector.

\begin{code}
type LivenessMask = [BitSet]

data Liveness = LvSmall BitSet
              | LvLarge CLabel
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
	FAST_INT	-- its number (1 .. mAX_Vanilla_REG)

  | FloatReg		-- single-precision floating-point registers
	FAST_INT	-- its number (1 .. mAX_Float_REG)

  | DoubleReg		-- double-precision floating-point registers
	FAST_INT	-- its number (1 .. mAX_Double_REG)

  -- STG registers
  | Sp			-- Stack ptr; points to last occupied stack location.
  | Su     		-- Stack update frame pointer
  | SpLim		-- Stack limit
  | Hp			-- Heap ptr; points to last occupied heap location.
  | HpLim		-- Heap limit register
  | CurCostCentre 	-- current cost centre register.
  | VoidReg 		-- see "VoidPrim" type; just a placeholder; 
			--   no actual register
  | LongReg	        -- long int registers (64-bit, really)
	PrimRep	        -- Int64Rep or Word64Rep
	FAST_INT	-- its number (1 .. mAX_Long_REG)


node 	= VanillaReg PtrRep     ILIT(1) -- A convenient alias for Node
tagreg  = VanillaReg WordRep    ILIT(2) -- A convenient alias for TagReg

\end{code}

We need magical @Eq@ because @VanillaReg@s come in multiple flavors.

\begin{code}
instance Eq MagicId where
    reg1 == reg2 = tag reg1 _EQ_ tag reg2
     where
	tag BaseReg	     = (ILIT(0) :: FAST_INT)
	tag Sp		     = ILIT(1)
	tag Su		     = ILIT(2)
	tag SpLim	     = ILIT(3)
	tag Hp		     = ILIT(4)
	tag HpLim	     = ILIT(5)
	tag CurCostCentre    = ILIT(6)
	tag VoidReg	     = ILIT(7)

	tag (VanillaReg _ i) = ILIT(8) _ADD_ i

	tag (FloatReg i)  = ILIT(8) _ADD_ maxv _ADD_ i
	tag (DoubleReg i) = ILIT(8) _ADD_ maxv _ADD_ maxf _ADD_ i
	tag (LongReg _ i) = ILIT(8) _ADD_ maxv _ADD_ maxf _ADD_ maxd _ADD_ i

        maxv = case mAX_Vanilla_REG of { IBOX(x) -> x }
        maxf = case mAX_Float_REG   of { IBOX(x) -> x }
        maxd = case mAX_Double_REG of { IBOX(x) -> x }
\end{code}

Returns True for any register that {\em potentially} dies across
C calls (or anything near equivalent).  We just say @True@ and
let the (machine-specific) registering macros sort things out...

\begin{code}
isVolatileReg :: MagicId -> Bool
isVolatileReg any = True
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
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
#include "HsVersions.h"

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
	isVolatileReg, noLiveRegsMask, mkLiveRegsMask,
	CostRes(Cost)
    )-} where

IMP_Ubiq(){-uitous-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(AbsCLoop)
#else
# if  ! OMIT_NATIVE_CODEGEN
import {-# SOURCE #-} MachMisc
# endif
import {-# SOURCE #-} CLabel
import {-# SOURCE #-} ClosureInfo
import {-# SOURCE #-} CgRetConv
#endif

import Constants   	( mAX_Vanilla_REG, mAX_Float_REG,
			  mAX_Double_REG, lIVENESS_R1, lIVENESS_R2,
			  lIVENESS_R3, lIVENESS_R4, lIVENESS_R5,
			  lIVENESS_R6, lIVENESS_R7, lIVENESS_R8
			)
import HeapOffs		( SYN_IE(VirtualSpAOffset), SYN_IE(VirtualSpBOffset),
			  SYN_IE(VirtualHeapOffset), HeapOffset
			)
import CLabel           ( CLabel )
import CostCentre       ( CostCentre )
import Literal		( mkMachInt, Literal )
import PrimRep		( isFollowableRep, PrimRep(..) )
import PrimOp           ( PrimOp )
import Unique           ( Unique )

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
			-- [amode analog: CLabelledCode]
			-- A labelled block of code; this "statement" is not
			-- executed; rather, the labelled code will be hoisted
			-- out to the top level (out of line) & it can be
			-- jumped to.

  | CInitHdr		-- to initialise the header of a closure (both fixed/var parts)
	ClosureInfo
	RegRelative	-- address of the info ptr
	CAddrMode	-- cost centre to place in closure
			--   CReg CurCostCentre or CC_HDR(R1.p{-Node-})
	Bool		-- inplace update or allocate

  | COpStmt
	[CAddrMode]	-- Results
	PrimOp
	[CAddrMode]	-- Arguments
	Int		-- Live registers (may be obtainable from volatility? ADR)
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

  -- see the notes about these next few; they follow below...
  | CMacroStmt		CStmtMacro	[CAddrMode]
  | CCallProfCtrMacro	FAST_STRING	[CAddrMode]
  | CCallProfCCMacro	FAST_STRING	[CAddrMode]

  -- *** the next three [or so...] are DATA (those above are CODE) ***

  | CStaticClosure
	CLabel	-- The (full, not base) label to use for labelling the closure.
	ClosureInfo
	CAddrMode	-- cost centre identifier to place in closure
	[CAddrMode]	-- free vars; ptrs, then non-ptrs


  | CClosureInfoAndCode
	ClosureInfo	-- Explains placement and layout of closure
	AbstractC	-- Slow entry point code
	(Maybe AbstractC)
			-- Fast entry point code, if any
	CAddrMode	-- Address of update code; Nothing => should never be used
			-- (which is the case for all except constructors)
	String		-- Closure description; NB we can't get this from
			-- ClosureInfo, because the latter refers to the *right* hand
			-- side of a defn, whereas the "description" refers to *left*
			-- hand side
	Int		-- Liveness info; this is here because it is
			-- easy to produce w/in the CgMonad; hard
			-- thereafter.  (WDP 95/11)

  | CRetVector			-- Return vector with "holes"
	  			-- (Nothings) for the default
	CLabel			-- vector-table label
	[Maybe CAddrMode]
	AbstractC		-- (and what to put in a "hole" [when Nothing])

  | CRetUnVector	-- Direct return
	CLabel		-- unvector-table label
	CAddrMode   	-- return code

  | CFlatRetVector	-- A labelled block of static data
	CLabel		-- This is the flattened version of CRetVector
	[CAddrMode]

  | CCostCentreDecl	-- A cost centre *declaration*
	Bool		-- True  <=> local => full declaration
			-- False <=> extern; just say so
	CostCentre

  | CClosureUpdInfo
    	AbstractC 	-- InRegs Info Table (CClosureInfoTable)
			--                    ^^^^^^^^^^^^^^^^^
			--                                out of date -- HWL

  | CSplitMarker	-- Split into separate object modules here
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
  = ARGS_CHK_A_LOAD_NODE
  | ARGS_CHK_A
  | ARGS_CHK_B_LOAD_NODE
  | ARGS_CHK_B
  | HEAP_CHK
  | STK_CHK
  | UPD_CAF
  | UPD_IND
  | UPD_INPLACE_NOPTRS
  | UPD_INPLACE_PTRS
  | UPD_BH_UPDATABLE
  | UPD_BH_SINGLE_ENTRY
  | PUSH_STD_UPD_FRAME
  | POP_STD_UPD_FRAME
  | SET_TAG
  | GRAN_FETCH	    		-- for GrAnSim only  -- HWL
  | GRAN_RESCHEDULE   		-- for GrAnSim only  -- HWL
  | GRAN_FETCH_AND_RESCHEDULE	-- for GrAnSim only  -- HWL
  | THREAD_CONTEXT_SWITCH   	-- for GrAnSim only  -- HWL
  | GRAN_YIELD   		-- for GrAnSim only  -- HWL 
  deriving Text
\end{code}

\item[@CCallProfCtrMacro@:]
The @String@ names a macro that, if \tr{#define}d, will bump one/some
of the STG-event profiling counters.

\item[@CCallProfCCMacro@:]
The @String@ names a macro that, if \tr{#define}d, will perform some
cost-centre-profiling-related action.
\end{description}

HERE ARE SOME OLD NOTES ABOUT HEAP-CHK ENTRY POINTS:

\item[@CCallStgC@:]
Some parts of the system, {\em notably the storage manager}, are
implemented by C~routines that must know something about the internals
of the STG world, e.g., where the heap-pointer is.  (The
``C-as-assembler'' documents describes this stuff in detail.)

This is quite a tricky business, especially with ``optimised~C,'' so
we keep close tabs on these fellows.  This enumeration type lists all
such ``STG~C'' routines:

HERE ARE SOME *OLD* NOTES ABOUT HEAP-CHK ENTRY POINTS:

Heap overflow invokes the garbage collector (of your choice :-), and
we have different entry points, to tell the GC the exact configuration
before it.
\begin{description}
\item[Branch of a boxed case:]
The @Node@ register points off to somewhere legitimate, the @TagReg@
holds the tag, and the @RetReg@ points to the code for the
alterative which should be resumed. (ToDo: update)

\item[Branch of an unboxed case:]
The @Node@ register points nowhere of any particular interest, a
kind-specific register (@IntReg@, @FloatReg@, etc.) holds the unboxed
value, and the @RetReg@ points to the code for the alternative
which should be resumed. (ToDo: update)

\item[Closure entry:]
The @Node@ register points to the closure, and the @RetReg@ points
to the code to be resumed. (ToDo: update)
\end{description}

%************************************************************************
%*									*
\subsection[CAddrMode]{C addressing modes}
%*									*
%************************************************************************

Addressing modes: these have @PrimitiveKinds@ pinned on them.
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
			-- See comment under CLabelledData about (String,Name)
	    PrimRep	-- the kind is so we can generate accurate C decls

  | CUnVecLbl 	    	-- A choice of labels left up to the back end
    	      CLabel	-- direct
    	      CLabel	-- vectored

  | CCharLike CAddrMode	-- The address of a static char-like closure for
			-- the specified character.  It is guaranteed to be in
			-- the range 0..255.

  | CIntLike CAddrMode	-- The address of a static int-like closure for the
			-- specified small integer.  It is guaranteed to be in the
			-- range mIN_INTLIKE..mAX_INTLIKE

  | CString FAST_STRING	-- The address of the null-terminated string
  | CLit    Literal
  | CLitLit FAST_STRING	-- completely literal literal: just spit this String
			-- into the C output
	    PrimRep

  | COffset HeapOffset	-- A literal constant, not an offset *from* anything!
			-- ToDo: this should really be CLitOffset

  | CCode AbstractC	-- Some code.  Used mainly for return addresses.

  | CLabelledCode CLabel AbstractC  -- Almost defunct? (ToDo?) --JSM
			-- Some code that must have a particular label
			-- (which is jumpable to)

  | CJoinPoint		-- This is used as the amode of a let-no-escape-bound variable
	VirtualSpAOffset	-- SpA and SpB values after any volatile free vars
	VirtualSpBOffset	-- of the rhs have been saved on stack.
				-- Just before the code for the thing is jumped to,
				-- SpA/B will be set to these values,
				-- and then any stack-passed args pushed,
				-- then the code for this thing will be entered

  | CMacroExpr
    	PrimRep    	-- the kind of the result
    	CExprMacro    	-- the macro to generate a value
	[CAddrMode]   	-- and its arguments

  | CCostCentre		-- If Bool is True ==> it to be printed as a String,
	CostCentre	-- (*not* as a C identifier or some such).
	Bool		-- (It's not just the double-quotes on either side;
			-- spaces and other funny characters will have been
			-- fiddled in the non-String variant.)

mkCCostCentre cc
  = --ASSERT(not (currentOrSubsumedCosts cc))
    --FALSE: We do put subsumedCC in static closures
    CCostCentre cc False
\end{code}

Various C macros for values which are dependent on the back-end layout.

\begin{code}

data CExprMacro
  = INFO_PTR
  | ENTRY_CODE
  | INFO_TAG
  | EVAL_TAG
  deriving(Text)

\end{code}

A tiny convenience:
\begin{code}
mkIntCLit :: Int -> CAddrMode
mkIntCLit i = CLit (mkMachInt (toInteger i))
\end{code}

%************************************************************************
%*									*
\subsection[RegRelative]{@RegRelatives@: ???}
%*									*
%************************************************************************

\begin{code}
data RegRelative
  = HpRel	 VirtualHeapOffset	-- virtual offset of Hp
		 VirtualHeapOffset	-- virtual offset of The Thing
  | SpARel	 VirtualSpAOffset	-- virtual offset of SpA
		 VirtualSpAOffset	-- virtual offset of The Thing
  | SpBRel	 VirtualSpBOffset	-- virtual offset of SpB
		 VirtualSpBOffset	-- virtual offset of The Thing
  | NodeRel	 VirtualHeapOffset

data ReturnInfo
  = DirectReturn    	    	    	-- Jump directly, if possible
  | StaticVectoredReturn Int		-- Fixed tag, starting at zero
  | DynamicVectoredReturn CAddrMode	-- Dynamic tag given by amode, starting at zero
\end{code}

%************************************************************************
%*									*
\subsection[MagicId]{@MagicIds@: registers and such}
%*									*
%************************************************************************

Much of what happens in Abstract-C is in terms of ``magic'' locations,
such as the stack pointer, heap pointer, etc.  If possible, these will
be held in registers.

Here are some notes about what's active when:
\begin{description}
\item[Always active:]
	Hp, HpLim, SpA, SpB, SuA, SuB

\item[Entry set:]
	ArgPtr1 (= Node)...

\item[Return set:]
Ptr regs: RetPtr1 (= Node), RetPtr2...
Int/char regs:  RetData1 (= TagReg = IntReg), RetData2...
Float regs: RetFloat1, ...
Double regs: RetDouble1, ...
\end{description}

\begin{code}
data MagicId
  = BaseReg 	-- mentioned only in nativeGen

  | StkOReg 	-- mentioned only in nativeGen

  -- Argument and return registers
  | VanillaReg		-- pointers, unboxed ints and chars
	PrimRep	        -- PtrRep, IntRep, CharRep, StablePtrRep or ForeignObjRep
			--	(in case we need to distinguish)
	FAST_INT	-- its number (1 .. mAX_Vanilla_REG)

  | FloatReg	-- single-precision floating-point registers
	FAST_INT	-- its number (1 .. mAX_Float_REG)

  | DoubleReg	-- double-precision floating-point registers
	FAST_INT	-- its number (1 .. mAX_Double_REG)

  | TagReg	-- to return constructor tags; as almost all returns are vectored,
		-- this is rarely used.

  | RetReg	-- topmost return address from the B stack

  | SpA		-- Stack ptr; points to last occupied stack location.
		-- Stack grows downward.
  | SuA     	-- mentioned only in nativeGen

  | SpB		-- Basic values, return addresses and update frames.
		-- Grows upward.
  | SuB	    	-- mentioned only in nativeGen

  | Hp		-- Heap ptr; points to last occupied heap location.
		-- Free space at lower addresses.

  | HpLim	-- Heap limit register: mentioned only in nativeGen

  | LivenessReg	-- (parallel only) used when we need to record explicitly
		-- what registers are live

  | StdUpdRetVecReg 	-- mentioned only in nativeGen
  | StkStubReg	    	-- register holding STK_STUB_closure (for stubbing dead stack slots)

  | CurCostCentre -- current cost centre register.

  | VoidReg -- see "VoidPrim" type; just a placeholder; no actual register

node 	= VanillaReg PtrRep     ILIT(1) -- A convenient alias for Node
infoptr = VanillaReg DataPtrRep ILIT(2) -- An alias for InfoPtr

--------------------
noLiveRegsMask :: Int	-- Mask indicating nothing live
noLiveRegsMask = 0

mkLiveRegsMask
	:: [MagicId]	-- Candidate live regs; depends what they have in them
	-> Int

mkLiveRegsMask regs
  = foldl do_reg noLiveRegsMask regs
  where
    do_reg acc (VanillaReg kind reg_no)
      | isFollowableRep kind
      = acc + (reg_tbl !! IBOX(reg_no _SUB_ ILIT(1)))

    do_reg acc anything_else = acc

    reg_tbl -- ToDo: mk Array!
      = [lIVENESS_R1, lIVENESS_R2, lIVENESS_R3, lIVENESS_R4,
	 lIVENESS_R5, lIVENESS_R6, lIVENESS_R7, lIVENESS_R8]
\end{code}

We need magical @Eq@ because @VanillaReg@s come in multiple flavors.

\begin{code}
instance Eq MagicId where
    reg1 == reg2 = tag reg1 _EQ_ tag reg2
     where
	tag BaseReg	     = (ILIT(0) :: FAST_INT)
	tag StkOReg	     = ILIT(1)
	tag TagReg	     = ILIT(2)
	tag RetReg	     = ILIT(3)
	tag SpA		     = ILIT(4)
	tag SuA		     = ILIT(5)
	tag SpB		     = ILIT(6)
	tag SuB		     = ILIT(7)
	tag Hp		     = ILIT(8)
	tag HpLim	     = ILIT(9)
	tag LivenessReg	     = ILIT(10)
	tag StdUpdRetVecReg  = ILIT(12)
	tag StkStubReg	     = ILIT(13)
	tag CurCostCentre    = ILIT(14)
	tag VoidReg	     = ILIT(15)

	tag (VanillaReg _ i) = ILIT(15) _ADD_ i

	tag (FloatReg i) = ILIT(15) _ADD_ maxv _ADD_ i
	  where
	    maxv = case mAX_Vanilla_REG of { IBOX(x) -> x }

	tag (DoubleReg i) = ILIT(15) _ADD_ maxv _ADD_ maxf _ADD_ i
	  where
	    maxv = case mAX_Vanilla_REG of { IBOX(x) -> x }
	    maxf = case mAX_Float_REG   of { IBOX(x) -> x }
\end{code}

Returns True for any register that {\em potentially} dies across
C calls (or anything near equivalent).  We just say @True@ and
let the (machine-specific) registering macros sort things out...
\begin{code}
isVolatileReg :: MagicId -> Bool

isVolatileReg any = True
--isVolatileReg (FloatReg _)	= True
--isVolatileReg (DoubleReg _)	= True
\end{code}

%************************************************************************
%*									*
\subsection[AbsCSyn-printing]{Pretty-printing Abstract~C}
%*									*
%************************************************************************

It's in \tr{PprAbsC.lhs}.

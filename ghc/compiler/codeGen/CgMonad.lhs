%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CgMonad]{The code generation monad}

See the beginning of the top-level @CodeGen@ module, to see how this
monadic stuff fits into the Big Picture.

\begin{code}
#include "HsVersions.h"

module CgMonad (
	Code(..),	-- type
	FCode(..),	-- type

	initC, thenC, thenFC, listCs, listFCs, mapCs, mapFCs,
	returnFC, fixC, absC, nopC, getAbsC,

	forkClosureBody, forkStatics, forkAlts, forkEval,
	forkEvalHelp, forkAbsC,
	SemiTaggingStuff(..),

	addBindC, addBindsC, modifyBindC, lookupBindC,
--UNUSED:	grabBindsC,

	EndOfBlockInfo(..),
	setEndOfBlockInfo, getEndOfBlockInfo,

	AStackUsage(..), BStackUsage(..), HeapUsage(..),
	StubFlag,
	isStubbed,
--UNUSED:	grabStackSizeC,

	nukeDeadBindings, getUnstubbedAStackSlots,

--	addFreeASlots,	-- no need to export it
	addFreeBSlots,	-- ToDo: Belong elsewhere

	isSwitchSetC, isStringSwitchSetC, getIntSwitchChkrC,

	noBlackHolingFlag,
	profCtrC, --UNUSED: concurrentC,

	costCentresC, costCentresFlag, moduleName,

	Sequel(..), -- ToDo: unabstract?
	sequelToAmode,

	-- out of general friendliness, we also export ...
	CgBindings(..),
	CgInfoDownwards(..), CgState(..),	-- non-abstract
	CgIdInfo, -- abstract
	CompilationInfo(..), IntSwitchChecker(..),
	GlobalSwitch, -- abstract

	stableAmodeIdInfo, heapIdInfo,

	-- and to make the interface self-sufficient...
	AbstractC, CAddrMode, CLabel, LambdaFormInfo, IdEnv(..),
	Unique, HeapOffset, CostCentre, IsCafCC,
	Id, UniqSet(..), UniqFM,
	VirtualSpAOffset(..), VirtualSpBOffset(..),
	VirtualHeapOffset(..), DataCon(..), PlainStgLiveVars(..),
	Maybe
    ) where

import AbsCSyn
import AbsUniType	( kindFromType, UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import CgBindery
import CgUsages         ( getSpBRelOffset )
import CmdLineOpts	( GlobalSwitch(..) )
import Id		( getIdUniType, ConTag(..), DataCon(..) )
import IdEnv		-- ops on CgBindings use these
import Maybes		( catMaybes, maybeToBool, Maybe(..) )
import Pretty		-- debugging only?
import PrimKind		( getKindSize, retKindSize )
import UniqSet		-- ( elementOfUniqSet, UniqSet(..) )
import CostCentre	-- profiling stuff
import StgSyn		( PlainStgAtom(..), PlainStgLiveVars(..) )
import Unique		( UniqueSupply )
import Util

infixr 9 `thenC`	-- Right-associative!
infixr 9 `thenFC`
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-environment]{Stuff for manipulating environments}
%*									*
%************************************************************************

This monadery has some information that it only passes {\em
downwards}, as well as some ``state'' which is modified as we go
along.

\begin{code}
data CgInfoDownwards	-- information only passed *downwards* by the monad
  = MkCgInfoDown
     CompilationInfo	-- COMPLETELY STATIC info about this compilation
			--  (e.g., what flags were passed to the compiler)

     CgBindings		-- [Id -> info] : static environment

     EndOfBlockInfo	-- Info for stuff to do at end of basic block:


data CompilationInfo
  = MkCompInfo
	(GlobalSwitch -> Bool)
			-- use it to look up whatever we like in command-line flags
	IntSwitchChecker-- similar; for flags that have an Int assoc.
			-- with them, notably number of regs available.
	FAST_STRING	-- the module name

type IntSwitchChecker = (Int -> GlobalSwitch) -> Maybe Int

data CgState
  = MkCgState
	AbstractC	-- code accumulated so far
	CgBindings	-- [Id -> info] : *local* bindings environment
			-- Bindings for top-level things are given in the info-down part
	CgStksAndHeapUsage
\end{code}

@EndOfBlockInfo@ tells what to do at the end of this block of code
or, if the expression is a @case@, what to do at the end of each alternative.

\begin{code}
data EndOfBlockInfo
  = EndOfBlockInfo
	VirtualSpAOffset	-- Args SpA: trim the A stack to this point at a return;
				-- push arguments starting just above this point on
				-- a tail call.

				-- This is therefore the A-stk ptr as seen 
				-- by a case alternative.

				-- Args SpA is used when we want to stub any 
				-- currently-unstubbed dead A-stack (ptr) slots;
				-- we want to know what SpA in the continuation is
				-- so that we don't stub any slots which are off the
				-- top of the continuation's stack!

	VirtualSpBOffset	-- Args SpB: Very similar to Args SpA.

				-- Two main differences:
				--  1.  If Sequel isn't OnStack, then Args SpB points 
				-- 	just below the slot in which the return address
				--	should be put.  In effect, the Sequel is
				--	a pending argument.  If it is OnStack, Args SpB
    	    	    	    	--      points to the top word of the return address.
				--
				--  2.  It ain't used for stubbing because there are
				--	no ptrs on B stk.
				
	Sequel


initEobInfo = EndOfBlockInfo 0 0 InRetReg


\end{code}

Any addressing modes inside @Sequel@ must be ``robust,'' in the sense
that it must survive stack pointer adjustments at the end of the
block.

\begin{code}
data Sequel
        = InRetReg              -- The continuation is in RetReg

        | OnStack VirtualSpBOffset
                                -- Continuation is on the stack, at the
                                -- specified location


--UNUSED:	| RestoreCostCentre

	| UpdateCode CAddrMode	-- May be standard update code, or might be
				-- the data-type-specific one.

	| CaseAlts 
		CAddrMode   -- Jump to this; if the continuation is for a vectored
			    -- case this might be the label of a return vector
			    -- Guaranteed to be a non-volatile addressing mode (I think)

		SemiTaggingStuff

type SemiTaggingStuff
  = Maybe			    -- Maybe[1] we don't have any semi-tagging stuff...
     ([(ConTag, JoinDetails)],	    -- Alternatives
      Maybe (Maybe Id, JoinDetails) -- Default (but Maybe[2] we don't have one)
				    -- Maybe[3] the default is a
				    -- bind-default (Just b); that is,
				    -- it expects a ptr to the thing
				    -- in Node, bound to b
     )

type JoinDetails
  = (AbstractC, CLabel)		-- Code to load regs from heap object + profiling macros, 
				-- and join point label
-- The abstract C is executed only from a successful
-- semitagging venture, when a case has looked at a variable, found
-- that it's evaluated, and wants to load up the contents and go to the
-- join point.


-- DIRE WARNING.
-- The OnStack case of sequelToAmode delivers an Amode which is only valid 
-- just before the final control transfer, because it assumes that
-- SpB is pointing to the top word of the return address.
-- This seems unclean but there you go.

sequelToAmode :: Sequel -> FCode CAddrMode

sequelToAmode (OnStack virt_spb_offset)
  = getSpBRelOffset virt_spb_offset `thenFC` \ spb_rel ->
    returnFC (CVal spb_rel RetKind)

sequelToAmode InRetReg		 = returnFC (CReg RetReg)
--UNUSED:sequelToAmode RestoreCostCentre  = returnFC mkRestoreCostCentreLbl
--Andy/Simon's patch:
--WAS: sequelToAmode (UpdateCode amode) = returnFC amode
sequelToAmode (UpdateCode amode) = returnFC (CReg StdUpdRetVecReg)
sequelToAmode (CaseAlts amode _) = returnFC amode

-- ToDo: move/do something
--UNUSED:mkRestoreCostCentreLbl = panic "mkRestoreCostCentreLbl"
\end{code}

See the NOTES about the details of stack/heap usage tracking.

\begin{code}
type CgStksAndHeapUsage		-- stacks and heap usage information
  = (AStackUsage,		-- A-stack usage
     BStackUsage,		-- B-stack usage
     HeapUsage)

type AStackUsage =
	(Int,			-- virtSpA: Virtual offset of topmost allocated slot
	 [(Int,StubFlag)],	-- freeA:   List of free slots, in increasing order
	 Int,			-- realSpA: Virtual offset of real stack pointer
	 Int)			-- hwSpA:   Highest value ever taken by virtSp

data StubFlag = Stubbed | NotStubbed

isStubbed Stubbed    = True  -- so the type can be abstract
isStubbed NotStubbed = False

type BStackUsage =
	(Int,		-- virtSpB: Virtual offset of topmost allocated slot
	 [Int],		-- freeB:   List of free slots, in increasing order
	 Int,		-- realSpB: Virtual offset of real stack pointer
	 Int)		-- hwSpB:   Highest value ever taken by virtSp

type HeapUsage =
	(HeapOffset,	-- virtHp: Virtual offset of highest-numbered allocated word
	 HeapOffset)	-- realHp: Virtual offset of real heap ptr
\end{code}
NB: absolutely every one of the above Ints is really
a VirtualOffset of some description (the code generator
works entirely in terms of VirtualOffsets; see NOTES).

Initialisation.

\begin{code}
initialStateC = MkCgState AbsCNop nullIdEnv initUsage

initUsage :: CgStksAndHeapUsage
initUsage  = ((0,[],0,0), (0,[],0,0), (initVirtHp, initRealHp))
initVirtHp = panic "Uninitialised virtual Hp"
initRealHp = panic "Uninitialised real Hp"
\end{code}

@envInitForAlternatives@ initialises the environment for a case alternative,
assuming that the alternative is entered after an evaluation.
This involves:
\begin{itemize}
\item
zapping any volatile bindings, which aren't valid.
\item
zapping the heap usage.	 It should be restored by a heap check.
\item
setting the virtual AND real stack pointer fields to the given virtual stack offsets.
this doesn't represent any {\em code}; it is a prediction of where the
real stack pointer will be when we come back from the case analysis.
\item
BUT LEAVING the rest of the stack-usage info because it is all valid.
In particular, we leave the tail stack pointers unchanged, becuase the
alternative has to de-allocate the original @case@ expression's stack.
\end{itemize}

@stateIncUsage@$~e_1~e_2$ incorporates in $e_1$ the stack and heap high water
marks found in $e_2$.

\begin{code}
stateIncUsage :: CgState -> CgState -> CgState

stateIncUsage (MkCgState abs_c bs ((vA,fA,rA,hA1),(vB,fB,rB,hB1),(vH1,rH1)))
	      (MkCgState _     _  (( _, _, _,hA2),( _, _, _,hB2),(vH2, _)))
     = MkCgState abs_c
		 bs 
		 ((vA,fA,rA,hA1 `max` hA2),
		  (vB,fB,rB,hB1 `max` hB2),
		  (vH1 `maxOff` vH2, rH1))
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-basics]{Basic code-generation monad magic}
%*									*
%************************************************************************

\begin{code}
type FCode a = CgInfoDownwards -> CgState -> (a, CgState)
type Code    = CgInfoDownwards -> CgState -> CgState

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenC #-}
{-# INLINE thenFC #-}
{-# INLINE returnFC #-}
#endif
\end{code}
The Abstract~C is not in the environment so as to improve strictness.

\begin{code}
initC :: CompilationInfo -> Code -> AbstractC

initC cg_info code
  = case (code (MkCgInfoDown cg_info (error "initC: statics") initEobInfo)
	       initialStateC) of
      MkCgState abc _ _ -> abc

returnFC :: a -> FCode a

returnFC val info_down state = (val, state)
\end{code}

\begin{code}
thenC :: Code
      -> (CgInfoDownwards -> CgState -> a)
      -> CgInfoDownwards -> CgState -> a

-- thenC has both of the following types:
-- thenC :: Code -> Code    -> Code
-- thenC :: Code -> FCode a -> FCode a

(m `thenC` k) info_down state
  = k info_down new_state
  where
    new_state  = m info_down state

listCs :: [Code] -> Code

listCs []     info_down state = state
listCs (c:cs) info_down state = stateN
  where
    state1 = c	       info_down state
    stateN = listCs cs info_down state1

mapCs :: (a -> Code) -> [a] -> Code

mapCs f []     info_down state = state
mapCs f (c:cs) info_down state = stateN
  where
    state1 = (f c)      info_down state
    stateN = mapCs f cs info_down state1
\end{code}

\begin{code}
thenFC	:: FCode a
	-> (a -> CgInfoDownwards -> CgState -> c)
	-> CgInfoDownwards -> CgState -> c

-- thenFC :: FCode a -> (a -> FCode b) -> FCode b
-- thenFC :: FCode a -> (a -> Code)    -> Code

(m `thenFC` k) info_down state
  = k m_result info_down new_state
  where
    (m_result, new_state) = m info_down state

listFCs :: [FCode a] -> FCode [a]

listFCs []	 info_down state = ([],		    state)
listFCs (fc:fcs) info_down state = (thing : things, stateN)
  where
    (thing,  state1) = fc	   info_down state
    (things, stateN) = listFCs fcs info_down state1

mapFCs :: (a -> FCode b) -> [a] -> FCode [b]

mapFCs f [] 	  info_down state = ([],	     state)
mapFCs f (fc:fcs) info_down state = (thing : things, stateN)
  where
    (thing,  state1) = (f fc)	    info_down state
    (things, stateN) = mapFCs f fcs info_down state1
\end{code}

And the knot-tying combinator:
\begin{code}
fixC :: (a -> FCode a) -> FCode a
fixC fcode info_down state = result
  where
    result@(v, _) = fcode v info_down state
    --	    ^-------------^
\end{code}

@forkClosureBody@ takes a code, $c$, and compiles it in a completely
fresh environment, except that:
	- compilation info and statics are passed in unchanged.
The current environment is passed on completely unaltered, except that
abstract C from the fork is incorporated.

@forkAbsC@ takes a code and compiles it in the current environment,
returning the abstract C thus constructed.  The current environment
is passed on completely unchanged.  It is pretty similar to @getAbsC@,
except that the latter does affect the environment. ToDo: combine?

@forkStatics@ $fc$ compiles $fc$ in an environment whose statics come
from the current bindings, but which is otherwise freshly initialised.
The Abstract~C returned is attached to the current state, but the
bindings and usage information is otherwise unchanged.

\begin{code}
forkClosureBody :: Code -> Code

forkClosureBody code 
	(MkCgInfoDown cg_info statics _) 
	(MkCgState absC_in binds un_usage)
  = MkCgState (AbsCStmts absC_in absC_fork) binds un_usage
  where
    fork_state		    = code body_info_down initialStateC
    MkCgState absC_fork _ _ = fork_state
    body_info_down = MkCgInfoDown cg_info statics initEobInfo

forkStatics :: FCode a -> FCode a

forkStatics fcode (MkCgInfoDown cg_info _ _)
		  (MkCgState absC_in statics un_usage)
  = (result, MkCgState (AbsCStmts absC_in absC_fork) statics un_usage)
  where
  (result, state) = fcode rhs_info_down initialStateC
  MkCgState absC_fork _ _ = state	-- Don't merge these this line with the one
					-- above or it becomes too strict!
  rhs_info_down = MkCgInfoDown cg_info statics initEobInfo

forkAbsC :: Code -> FCode AbstractC
forkAbsC code info_down (MkCgState absC1 bs usage)
  = (absC2, new_state)
  where
    MkCgState absC2 _ ((_, _, _,hA2),(_, _, _,hB2), _) = 
	code info_down (MkCgState AbsCNop bs usage)
    ((vA, fA, rA, hA1), (vB, fB, rB, hB1), heap_usage) = usage

    new_usage = ((vA, fA, rA, hA1 `max` hA2), (vB, fB, rB, hB1 `max` hB2), heap_usage)
    new_state = MkCgState absC1 bs new_usage
\end{code}

@forkAlts@ $bs~d$ takes fcodes $bs$ for the branches of a @case@, and
an fcode for the default case $d$, and compiles each in the current
environment.  The current environment is passed on unmodified, except
that
	- the worst stack high-water mark is incorporated
	- the virtual Hp is moved on to the worst virtual Hp for the branches

The "extra branches" arise from handling the default case:

	case f x of
	  C1 a b -> e1
	  z     -> e2

Here we in effect expand to 

	case f x of 
	  C1 a b -> e1
	  C2 c -> let z = C2 c in JUMP(default)
	  C3 d e f -> let z = C2 d e f in JUMP(default)
	  
	  default: e2

The stuff for C2 and C3 are the extra branches.  They are
handled differently by forkAlts, because their
heap usage is joined onto that for the default case.

\begin{code}
forkAlts :: [FCode a] -> [FCode a] -> FCode b -> FCode ([a],b)

forkAlts branch_fcodes extra_branch_fcodes deflt_fcode info_down in_state
 = ((extra_branch_results ++ branch_results , deflt_result), out_state)
  where
    compile fc = fc info_down in_state

    (branch_results,       branch_out_states)       = unzip (map compile branch_fcodes)
    (extra_branch_results, extra_branch_out_states) = unzip (map compile extra_branch_fcodes)

	-- The "in_state" for the default branch is got by worst-casing the
	-- heap usages etc from the "extra_branches"
    default_in_state		    = foldl stateIncUsage in_state extra_branch_out_states
    (deflt_result, deflt_out_state) = deflt_fcode info_down default_in_state

    out_state = foldl stateIncUsage default_in_state (deflt_out_state:branch_out_states)
		-- NB foldl.  in_state is the *left* argument to stateIncUsage
\end{code}

@forkEval@ takes two blocks of code.
\begin{itemize}
\item The first meddles with the environment to set it up as expected by
	the alternatives of a @case@ which does an eval (or gc-possible primop).
\item The second block is the code for the alternatives.
	(plus info for semi-tagging purposes)
\end{itemize}
@forkEval@ picks up the virtual stack pointers and stubbed stack slots
as set up by the first block, and returns a suitable @EndOfBlockInfo@ for
the caller to use, together with whatever value is returned by the second block.

It uses @initEnvForAlternatives@ to initialise the environment, and
@stateIncUsageAlt@ to incorporate usage; the latter ignores the heap
usage.

\begin{code}
forkEval :: EndOfBlockInfo              -- For the body
    	 -> Code			-- Code to set environment
	 -> FCode Sequel		-- Semi-tagging info to store
	 -> FCode EndOfBlockInfo	-- The new end of block info

forkEval body_eob_info env_code body_code 
  = forkEvalHelp body_eob_info env_code body_code `thenFC` \ (vA, vB, sequel) ->
    returnFC (EndOfBlockInfo vA vB sequel)

forkEvalHelp :: EndOfBlockInfo  -- For the body 
    	     -> Code		-- Code to set environment
	     -> FCode a		-- The code to do after the eval
	     -> FCode (Int,	-- SpA
		       Int,	-- SpB
		       a)	-- Result of the FCode

forkEvalHelp body_eob_info env_code body_code 
	 info_down@(MkCgInfoDown cg_info statics _) state
  = ((vA,vB,value_returned), state `stateIncUsageEval` state_at_end_return)
  where
    info_down_for_body = MkCgInfoDown cg_info statics body_eob_info

    (MkCgState _ binds ((vA,fA,_,_), (vB,fB,_,_), _)) = env_code info_down_for_body state
	-- These vA and fA things are now set up as the body code expects them

    state_at_end_return :: CgState

    (value_returned, state_at_end_return) = body_code info_down_for_body state_for_body

    state_for_body :: CgState

    state_for_body = MkCgState AbsCNop
	 		     (nukeVolatileBinds binds)
			     ((vA,stubbed_fA,vA,vA),	-- Set real and hwms 
			      (vB,fB,vB,vB),		-- to virtual ones
			      (initVirtHp, initRealHp))

    stubbed_fA = [ (offset, Stubbed) | (offset,_) <- fA ]
	-- In the branch, all free locations will have been stubbed


stateIncUsageEval :: CgState -> CgState -> CgState
stateIncUsageEval (MkCgState absC1 bs ((vA,fA,rA,hA1),(vB,fB,rB,hB1),heap_usage))
		  (MkCgState absC2 _  (( _, _, _,hA2),( _, _, _,hB2),	     _))
     = MkCgState (absC1 `AbsCStmts` absC2) 
		 -- The AbsC coming back should consist only of nested declarations,
		 -- notably of the return vector!
		 bs 
		 ((vA,fA,rA,hA1 `max` hA2),
		  (vB,fB,rB,hB1 `max` hB2),
		  heap_usage)
	-- We don't max the heap high-watermark because stateIncUsageEval is
	-- used only in forkEval, which in turn is only used for blocks of code
	-- which do their own heap-check.
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-spitting-AbstractC]{Spitting out @AbstractC@}
%*									*
%************************************************************************

@nopC@ is the no-op for the @Code@ monad; it adds no Abstract~C to the
environment; @absC@ glues @ab_C@ onto the Abstract~C collected so far.
\begin{code}
nopC :: Code
nopC info_down state = state

absC :: AbstractC -> Code
absC more_absC info_down state@(MkCgState absC binds usage)
  = MkCgState (mkAbsCStmts absC more_absC) binds usage
\end{code}

These two are just like @absC@, except they examine the compilation
info (whether SCC profiling or profiling-ctrs going) and possibly emit
nothing.

\begin{code}
isSwitchSetC :: GlobalSwitch -> FCode Bool

isSwitchSetC switch (MkCgInfoDown (MkCompInfo sw_chkr _ _) _ _) state
  = (sw_chkr switch, state)

isStringSwitchSetC :: (String -> GlobalSwitch) -> FCode Bool

isStringSwitchSetC switch (MkCgInfoDown (MkCompInfo sw_chkr _ _) _ _) state
  = (sw_chkr (switch (panic "isStringSwitchSetC")), state)

getIntSwitchChkrC :: FCode IntSwitchChecker

getIntSwitchChkrC (MkCgInfoDown (MkCompInfo _ isw_chkr _) _ _) state
  = (isw_chkr, state)

costCentresC :: FAST_STRING -> [CAddrMode] -> Code

costCentresC macro args (MkCgInfoDown (MkCompInfo sw_chkr _ _) _ _)
			state@(MkCgState absC binds usage)
  = if sw_chkr SccProfilingOn
    then MkCgState (mkAbsCStmts absC (CCallProfCCMacro macro args)) binds usage
    else state

profCtrC :: FAST_STRING -> [CAddrMode] -> Code

profCtrC macro args (MkCgInfoDown (MkCompInfo sw_chkr _ _) _ _)
			state@(MkCgState absC binds usage)
  = if not (sw_chkr DoTickyProfiling)
    then state
    else MkCgState (mkAbsCStmts absC (CCallProfCtrMacro macro args)) binds usage

{- Try to avoid adding too many special compilation strategies here.  
   It's better to modify the header files as necessary for particular targets, 
   so that we can get away with as few variants of .hc files as possible.
   'ForConcurrent' is somewhat special anyway, as it changes entry conventions
   pretty significantly.
-}

-- if compiling for concurrency...
  
{- UNUSED, as it happens:
concurrentC :: AbstractC -> Code

concurrentC more_absC (MkCgInfoDown (MkCompInfo sw_chkr _ _) _ _)
			state@(MkCgState absC binds usage)
  = if not (sw_chkr ForConcurrent)
    then state
    else MkCgState (mkAbsCStmts absC more_absC) binds usage
-}
\end{code}

@getAbsC@ compiles the code in the current environment, and returns
the abstract C thus constructed (leaving the abstract C being carried
around in the state untouched).	 @getAbsC@ does not generate any
in-line Abstract~C itself, but the environment it returns is that
obtained from the compilation.

\begin{code}
getAbsC :: Code -> FCode AbstractC

getAbsC code info_down (MkCgState absC binds usage)
  = (absC2, MkCgState absC binds2 usage2)
  where
    (MkCgState absC2 binds2 usage2) = code info_down (MkCgState AbsCNop binds usage)
\end{code}

\begin{code}
noBlackHolingFlag, costCentresFlag :: FCode Bool

noBlackHolingFlag (MkCgInfoDown (MkCompInfo sw_chkr _ _) _ _) state
  = (sw_chkr OmitBlackHoling, state)

costCentresFlag	  (MkCgInfoDown (MkCompInfo sw_chkr _ _) _ _) state
  = (sw_chkr SccProfilingOn, state)
\end{code}

\begin{code}

moduleName :: FCode FAST_STRING
moduleName (MkCgInfoDown (MkCompInfo _ _ mod_name) _ _) state
  = (mod_name, state)

\end{code}

\begin{code}
setEndOfBlockInfo :: EndOfBlockInfo -> Code -> Code
setEndOfBlockInfo eob_info code	(MkCgInfoDown c_info statics _) state
  = code (MkCgInfoDown c_info statics eob_info) state

getEndOfBlockInfo :: FCode EndOfBlockInfo
getEndOfBlockInfo (MkCgInfoDown c_info statics eob_info) state
  = (eob_info, state)
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-bindery]{Monad things for fiddling with @CgBindings@}
%*									*
%************************************************************************

There are three basic routines, for adding (@addBindC@), modifying
(@modifyBindC@) and looking up (@lookupBindC@) bindings.  Each routine
is just a wrapper for its lower-level @Bind@ routine (drop the \tr{C}
on the end of each function name).

A @Id@ is bound to a @(VolatileLoc, StableLoc)@ triple.
The name should not already be bound.
\begin{code}
addBindC :: Id -> CgIdInfo -> Code
addBindC name stuff_to_bind info_down (MkCgState absC binds usage)
  = MkCgState absC (addOneToIdEnv binds name stuff_to_bind) usage
\end{code}

\begin{code}
addBindsC :: [(Id, CgIdInfo)] -> Code
addBindsC new_bindings info_down (MkCgState absC binds usage)
  = MkCgState absC new_binds usage
  where
    new_binds = foldl (\ binds (name,info) -> addOneToIdEnv binds name info)
		      binds
		      new_bindings
\end{code}

\begin{code}
modifyBindC :: Id -> (CgIdInfo -> CgIdInfo) -> Code
modifyBindC name mangle_fn info_down (MkCgState absC binds usage)
  = MkCgState absC (modifyIdEnv binds mangle_fn name) usage
\end{code}

Lookup is expected to find a binding for the @Id@.
\begin{code}
lookupBindC :: Id -> FCode CgIdInfo
lookupBindC name info_down@(MkCgInfoDown _ static_binds _) 
		 state@(MkCgState absC local_binds usage)
  = (val, state)
  where
    val = case (lookupIdEnv local_binds name) of
	    Nothing	-> try_static
	    Just this	-> this

    try_static = case (lookupIdEnv static_binds name) of
		   Just this -> this
		   Nothing
		     -> pprPanic "lookupBindC:no info!\n"
			(ppAboves [
			    ppCat [ppStr "for:", ppr PprShowAll name],
			    ppStr "(probably: data dependencies broken by an optimisation pass)",
			    ppStr "static binds for:",
			    ppAboves [ ppr PprDebug i | (MkCgIdInfo i _ _ _) <- rngIdEnv static_binds ],
			    ppStr "local binds for:",
			    ppAboves [ ppr PprDebug i | (MkCgIdInfo i _ _ _) <- rngIdEnv local_binds ]
			 ])
\end{code}

For dumping debug information, we also have the ability to grab the
local bindings environment.

ToDo: Maybe do the pretty-printing here to restrict what people do
with the environment.

\begin{code}
{- UNUSED:
grabBindsC :: FCode CgBindings
grabBindsC info_down state@(MkCgState absC binds usage)
  = (binds, state)
-}
\end{code}

\begin{code}
{- UNUSED:
grabStackSizeC :: FCode (Int, Int)
grabStackSizeC info_down state -- @(MkCgState absC binds ((vA,_,_,_), (vB,_,_,_), _))
  = panic "grabStackSizeC" -- (vA, vB)
-}
\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-deadslots]{Finding dead stack slots}
%*									*
%************************************************************************

@nukeDeadBindings@ does the following:
\begin{itemize}
\item	Removes all bindings from the environment other than those
	for variables in the argument to @nukeDeadBindings@.
\item	Collects any stack slots so freed, and returns them to the appropriate
	stack free list.
\item	Moves the virtual stack pointers to point to the topmost used
	stack locations.
\end{itemize}

Find dead slots on the stacks *and* remove bindings for dead variables
from the bindings.

You can have multi-word slots on the B stack; if dead, such a slot
will be reported as {\em several} offsets (one per word).

NOT YET: It returns empty lists if the -fno-stack-stubbing flag is
set, so that no stack-stubbing will take place.

Probably *naughty* to look inside monad...

\begin{code}
nukeDeadBindings :: PlainStgLiveVars  -- All the *live* variables
		 -> Code
nukeDeadBindings
	live_vars
	info_down
	state@(MkCgState abs_c binds ((vsp_a, free_a, real_a, hw_a),
				      (vsp_b, free_b, real_b, hw_b),
				      heap_usage))
  = MkCgState abs_c (mkIdEnv bs') new_usage
  where
    new_usage = ((new_vsp_a, new_free_a, real_a, hw_a),
		 (new_vsp_b, new_free_b, real_b, hw_b),
		 heap_usage)

    (dead_a_slots, dead_b_slots, bs')
      = dead_slots live_vars 
		   [] [] [] 
		   [ (i, b) | b@(MkCgIdInfo i _ _ _) <- rngIdEnv binds ]
		   --OLD: (getIdEnvMapping binds)

    extra_free_a = (sortLt (<)  dead_a_slots) `zip` (repeat NotStubbed)
    extra_free_b = sortLt (<) dead_b_slots

    (new_vsp_a, new_free_a) = trim fst vsp_a (addFreeASlots free_a extra_free_a)
    (new_vsp_b, new_free_b) = trim id  vsp_b (addFreeBSlots free_b extra_free_b)

getUnstubbedAStackSlots
	:: VirtualSpAOffset		-- Ignore slots bigger than this
	-> FCode [VirtualSpAOffset]	-- Return the list of slots found

getUnstubbedAStackSlots tail_spa
	info_down state@(MkCgState _ _ ((_, free_a, _, _), _, _))
  = ([ slot | (slot, NotStubbed) <- free_a, slot <= tail_spa ], state)
\end{code}

Several boring auxiliary functions to do the dirty work.

\begin{code}
dead_slots :: PlainStgLiveVars
	   -> [(Id,CgIdInfo)] -> [VirtualSpAOffset] -> [VirtualSpBOffset]
	   -> [(Id,CgIdInfo)]
	   -> ([VirtualSpAOffset], [VirtualSpBOffset], [(Id,CgIdInfo)])

-- dead_slots carries accumulating parameters for
--	filtered bindings, dead a and b slots
dead_slots live_vars fbs das dbs []
  = (nub das, nub dbs, reverse fbs) -- Finished; rm the dups, if any

dead_slots live_vars fbs das dbs ((v,i):bs)
  | v `elementOfUniqSet` live_vars
    = dead_slots live_vars ((v,i):fbs) das dbs bs
	  -- Live, so don't record it in dead slots
	  -- Instead keep it in the filtered bindings

  | otherwise
    = case i of
	MkCgIdInfo _ _ stable_loc _
	 | is_Astk_loc ->
	   dead_slots live_vars fbs (offsetA : das) dbs bs

	 | is_Bstk_loc ->
	   dead_slots live_vars fbs das ([offsetB .. (offsetB + size - 1)] ++ dbs) bs
	 where
	   maybe_Astk_loc = maybeAStkLoc stable_loc
	   is_Astk_loc	  = maybeToBool maybe_Astk_loc
	   (Just offsetA) = maybe_Astk_loc

	   maybe_Bstk_loc = maybeBStkLoc stable_loc
	   is_Bstk_loc	  = maybeToBool maybe_Bstk_loc
	   (Just offsetB) = maybe_Bstk_loc

	_ -> dead_slots live_vars fbs das dbs bs
  where
    size :: Int
    size = (getKindSize . kindFromType . getIdUniType) v

-- addFreeSlots expects *both* args to be in increasing order
addFreeASlots :: [(Int,StubFlag)] -> [(Int,StubFlag)] -> [(Int,StubFlag)]
addFreeASlots = addFreeSlots fst

addFreeBSlots :: [Int] -> [Int] -> [Int]
addFreeBSlots = addFreeSlots id

addFreeSlots :: (slot -> Int{-offset-}) -> [slot] -> [slot] -> [slot]

addFreeSlots get_offset cs [] = cs
addFreeSlots get_offset [] ns = ns
addFreeSlots get_offset (c:cs) (n:ns)
 = if off_c < off_n then
	(c : addFreeSlots get_offset cs (n:ns))
   else if off_c > off_n then
	(n : addFreeSlots get_offset (c:cs) ns)
   else
	panic ("addFreeSlots: equal slots: ")-- ++ show (c:cs) ++ show (n:ns))
 where
  off_c = get_offset c
  off_n = get_offset n

trim :: (slot -> Int{-offset-}) -> Int{-offset-} -> [slot] -> (Int{-offset-}, [slot])

trim get_offset current_sp free_slots
  = try current_sp (reverse free_slots)
  where
    try csp [] = (csp, [])
    try csp (slot:slots)
      = if csp < slot_off then
	    try csp slots		-- Free slot off top of stk; ignore

	else if csp == slot_off then
    	    try (csp-1) slots		-- Free slot at top of stk; trim

	else
	    (csp, reverse (slot:slots))	-- Otherwise gap; give up
      where
	slot_off = get_offset slot
\end{code}

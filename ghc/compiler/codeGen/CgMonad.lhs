%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgMonad.lhs,v 1.23 1999/10/13 16:39:16 simonmar Exp $
%
\section[CgMonad]{The code generation monad}

See the beginning of the top-level @CodeGen@ module, to see how this
monadic stuff fits into the Big Picture.

\begin{code}
module CgMonad (
	Code,	-- type
	FCode,	-- type

	initC, thenC, thenFC, listCs, listFCs, mapCs, mapFCs,
	returnFC, fixC, absC, nopC, getAbsC,

	forkClosureBody, forkStatics, forkAlts, forkEval,
	forkEvalHelp, forkAbsC,
	SemiTaggingStuff,

	EndOfBlockInfo(..),
	setEndOfBlockInfo, getEndOfBlockInfo,

	setSRTLabel, getSRTLabel,
	setTickyCtrLabel, getTickyCtrLabel,

	StackUsage, Slot(..), HeapUsage,

	profCtrC, profCtrAbsC,

	costCentresC, moduleName,

	Sequel(..), -- ToDo: unabstract?
	sequelToAmode,

	-- out of general friendliness, we also export ...
	CgInfoDownwards(..), CgState(..),	-- non-abstract
	CompilationInfo(..)
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgBindery ( CgIdInfo, CgBindings, nukeVolatileBinds )
import {-# SOURCE #-} CgUsages  ( getSpRelOffset )

import AbsCSyn
import AbsCUtils	( mkAbsCStmts )
import CmdLineOpts	( opt_SccProfilingOn, opt_DoTickyProfiling )
import CLabel           ( CLabel, mkUpdInfoLabel, mkTopTickyCtrLabel )
import Module		( Module )
import DataCon		( ConTag )
import Id		( Id )
import VarEnv
import PrimRep		( PrimRep(..) )
import StgSyn		( StgLiveVars )
import Outputable

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

     CLabel		-- label of the current SRT

     CLabel		-- current destination for ticky counts

     EndOfBlockInfo	-- Info for stuff to do at end of basic block:


data CompilationInfo
  = MkCompInfo
	Module		-- the module name

data CgState
  = MkCgState
	AbstractC	-- code accumulated so far
	CgBindings	-- [Id -> info] : *local* bindings environment
			-- Bindings for top-level things are given in the info-down part
	CgStksAndHeapUsage
\end{code}

@EndOfBlockInfo@ tells what to do at the end of this block of code or,
if the expression is a @case@, what to do at the end of each
alternative.

\begin{code}
data EndOfBlockInfo
  = EndOfBlockInfo
	VirtualSpOffset   -- Args Sp: trim the stack to this point at a
			  -- return; push arguments starting just
			  -- above this point on a tail call.
			  
			  -- This is therefore the stk ptr as seen
			  -- by a case alternative.
	Sequel

initEobInfo = EndOfBlockInfo 0 (OnStack 0)
\end{code}

Any addressing modes inside @Sequel@ must be ``robust,'' in the sense
that it must survive stack pointer adjustments at the end of the
block.

\begin{code}
data Sequel
  = OnStack 
	VirtualSpOffset   -- Continuation is on the stack, at the
			  -- specified location

  | UpdateCode

  | CaseAlts
	  CAddrMode   -- Jump to this; if the continuation is for a vectored
		      -- case this might be the label of a return
		      -- vector Guaranteed to be a non-volatile
		      -- addressing mode (I think)
	  SemiTaggingStuff

  | SeqFrame			-- like CaseAlts but push a seq frame too.
	  CAddrMode
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

-- The abstract C is executed only from a successful semitagging
-- venture, when a case has looked at a variable, found that it's
-- evaluated, and wants to load up the contents and go to the join
-- point.

-- DIRE WARNING.
-- The OnStack case of sequelToAmode delivers an Amode which is only
-- valid just before the final control transfer, because it assumes
-- that Sp is pointing to the top word of the return address.  This
-- seems unclean but there you go.

-- sequelToAmode returns an amode which refers to an info table.  The info
-- table will always be of the RET(_VEC)?_(BIG|SMALL) kind.  We're careful
-- not to handle real code pointers, just in case we're compiling for 
-- an unregisterised/untailcallish architecture, where info pointers and
-- code pointers aren't the same.

sequelToAmode :: Sequel -> FCode CAddrMode

sequelToAmode (OnStack virt_sp_offset)
  = getSpRelOffset virt_sp_offset `thenFC` \ sp_rel ->
    returnFC (CVal sp_rel RetRep)

sequelToAmode UpdateCode = returnFC (CLbl mkUpdInfoLabel RetRep)
sequelToAmode (CaseAlts amode _) = returnFC amode
sequelToAmode (SeqFrame _ _) = panic "sequelToAmode: SeqFrame"

type CgStksAndHeapUsage		-- stacks and heap usage information
  = (StackUsage, HeapUsage)

data Slot = Free | NonPointer 
  deriving
#ifdef DEBUG
	(Eq,Show)
#else
	Eq
#endif

type StackUsage =
	(Int,		   -- virtSp: Virtual offset of topmost allocated slot
	 [(Int,Slot)],     -- free:   List of free slots, in increasing order
	 Int,		   -- realSp: Virtual offset of real stack pointer
	 Int)		   -- hwSp:   Highest value ever taken by virtSp

type HeapUsage =
	(HeapOffset,	-- virtHp: Virtual offset of highest-allocated word
	 HeapOffset)	-- realHp: Virtual offset of real heap ptr
\end{code}

NB: absolutely every one of the above Ints is really
a VirtualOffset of some description (the code generator
works entirely in terms of VirtualOffsets).

Initialisation.

\begin{code}
initialStateC = MkCgState AbsCNop emptyVarEnv initUsage

initUsage :: CgStksAndHeapUsage
initUsage  = ((0,[],0,0), (0,0))
\end{code}

"envInitForAlternatives" initialises the environment for a case alternative,
assuming that the alternative is entered after an evaluation.
This involves:

   - zapping any volatile bindings, which aren't valid.
   
   - zapping the heap usage. It should be restored by a heap check.
   
   - setting the virtual AND real stack pointer fields to the given
   virtual stack offsets.  this doesn't represent any {\em code}; it is a
   prediction of where the real stack pointer will be when we come back
   from the case analysis.
   
   - BUT LEAVING the rest of the stack-usage info because it is all
   valid.  In particular, we leave the tail stack pointers unchanged,
   becuase the alternative has to de-allocate the original @case@
   expression's stack.  \end{itemize}

@stateIncUsage@$~e_1~e_2$ incorporates in $e_1$ the stack and heap high water
marks found in $e_2$.

\begin{code}
stateIncUsage :: CgState -> CgState -> CgState

stateIncUsage (MkCgState abs_c bs ((v,f,r,h1),(vH1,rH1)))
	      (MkCgState _     _  ((_,_,_,h2),(vH2, _)))
     = MkCgState abs_c
		 bs
		 ((v,f,r,h1 `max` h2),
		  (vH1 `max` vH2, rH1))
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-basics]{Basic code-generation monad magic}
%*									*
%************************************************************************

\begin{code}
type FCode a = CgInfoDownwards -> CgState -> (a, CgState)
type Code    = CgInfoDownwards -> CgState -> CgState

{-# INLINE thenC #-}
{-# INLINE thenFC #-}
{-# INLINE returnFC #-}
\end{code}
The Abstract~C is not in the environment so as to improve strictness.

\begin{code}
initC :: CompilationInfo -> Code -> AbstractC

initC cg_info code
  = case (code (MkCgInfoDown 
			cg_info 
			(error "initC: statics")
			(error "initC: srt")
			(mkTopTickyCtrLabel)
			initEobInfo)
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

thenC m k info_down state
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

thenFC m k info_down state
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
	(MkCgInfoDown cg_info statics srt ticky _)
	(MkCgState absC_in binds un_usage)
  = MkCgState (AbsCStmts absC_in absC_fork) binds un_usage
  where
    fork_state		    = code body_info_down initialStateC
    MkCgState absC_fork _ _ = fork_state
    body_info_down = MkCgInfoDown cg_info statics srt ticky initEobInfo

forkStatics :: FCode a -> FCode a

forkStatics fcode (MkCgInfoDown cg_info _ srt ticky _)
		  (MkCgState absC_in statics un_usage)
  = (result, MkCgState (AbsCStmts absC_in absC_fork) statics un_usage)
  where
  (result, state) = fcode rhs_info_down initialStateC
  MkCgState absC_fork _ _ = state	-- Don't merge these this line with the one
					-- above or it becomes too strict!
  rhs_info_down = MkCgInfoDown cg_info statics srt ticky initEobInfo

forkAbsC :: Code -> FCode AbstractC
forkAbsC code info_down (MkCgState absC1 bs usage)
  = (absC2, new_state)
  where
    MkCgState absC2 _ ((_, _, _,h2), _) =
	code info_down (MkCgState AbsCNop bs usage)
    ((v, f, r, h1), heap_usage) = usage

    new_usage = ((v, f, r, h1 `max` h2), heap_usage)
    new_state = MkCgState absC1 bs new_usage
\end{code}

@forkAlts@ $bs~d$ takes fcodes $bs$ for the branches of a @case@, and
an fcode for the default case $d$, and compiles each in the current
environment.  The current environment is passed on unmodified, except
that
	- the worst stack high-water mark is incorporated
	- the virtual Hp is moved on to the worst virtual Hp for the branches

\begin{code}
forkAlts :: [FCode a] -> FCode b -> FCode ([a],b)

forkAlts branch_fcodes deflt_fcode info_down in_state
 = ((branch_results , deflt_result), out_state)
  where
    compile fc = fc info_down in_state

    (branch_results, branch_out_states) = unzip (map compile branch_fcodes)

    (deflt_result, deflt_out_state) = deflt_fcode info_down in_state

    out_state = foldl stateIncUsage in_state (deflt_out_state:branch_out_states)
		-- NB foldl.  in_state is the *left* argument to stateIncUsage
\end{code}

@forkEval@ takes two blocks of code.

   -  The first meddles with the environment to set it up as expected by
      the alternatives of a @case@ which does an eval (or gc-possible primop).
   -  The second block is the code for the alternatives.
      (plus info for semi-tagging purposes)

@forkEval@ picks up the virtual stack pointer and returns a suitable
@EndOfBlockInfo@ for the caller to use, together with whatever value
is returned by the second block.

It uses @initEnvForAlternatives@ to initialise the environment, and
@stateIncUsageAlt@ to incorporate usage; the latter ignores the heap
usage.

\begin{code}
forkEval :: EndOfBlockInfo              -- For the body
    	 -> Code			-- Code to set environment
	 -> FCode Sequel		-- Semi-tagging info to store
	 -> FCode EndOfBlockInfo	-- The new end of block info

forkEval body_eob_info env_code body_code
  = forkEvalHelp body_eob_info env_code body_code `thenFC` \ (v, sequel) ->
    returnFC (EndOfBlockInfo v sequel)

forkEvalHelp :: EndOfBlockInfo  -- For the body
    	     -> Code		-- Code to set environment
	     -> FCode a		-- The code to do after the eval
	     -> FCode (Int,	-- Sp
		       a)	-- Result of the FCode

forkEvalHelp body_eob_info env_code body_code
	 info_down@(MkCgInfoDown cg_info statics srt ticky _) state
  = ((v,value_returned), state `stateIncUsageEval` state_at_end_return)
  where
    info_down_for_body = MkCgInfoDown cg_info statics srt ticky body_eob_info

    (MkCgState _ binds ((v,f,_,_), _)) = env_code info_down_for_body state
	-- These v and f things are now set up as the body code expects them

    (value_returned, state_at_end_return) 
	= body_code info_down_for_body state_for_body

    state_for_body = MkCgState AbsCNop
	 		     (nukeVolatileBinds binds)
			     ((v,f,v,v), (0,0))


stateIncUsageEval :: CgState -> CgState -> CgState
stateIncUsageEval (MkCgState absC1 bs ((v,f,r,h1),heap_usage))
		  (MkCgState absC2 _  ((_,_,_,h2),         _))
     = MkCgState (absC1 `AbsCStmts` absC2)
		 -- The AbsC coming back should consist only of nested declarations,
		 -- notably of the return vector!
		 bs
		 ((v,f,r,h1 `max` h2), heap_usage)
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
costCentresC :: FAST_STRING -> [CAddrMode] -> Code

costCentresC macro args _ state@(MkCgState absC binds usage)
  = if opt_SccProfilingOn
    then MkCgState (mkAbsCStmts absC (CCallProfCCMacro macro args)) binds usage
    else state

profCtrC :: FAST_STRING -> [CAddrMode] -> Code

profCtrC macro args _ state@(MkCgState absC binds usage)
  = if not opt_DoTickyProfiling
    then state
    else MkCgState (mkAbsCStmts absC (CCallProfCtrMacro macro args)) binds usage

profCtrAbsC :: FAST_STRING -> [CAddrMode] -> AbstractC

profCtrAbsC macro args
  = if not opt_DoTickyProfiling
    then AbsCNop
    else CCallProfCtrMacro macro args

{- Try to avoid adding too many special compilation strategies here.
   It's better to modify the header files as necessary for particular
   targets, so that we can get away with as few variants of .hc files
   as possible.
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
    (MkCgState absC2 binds2 usage2) 
	= code info_down (MkCgState AbsCNop binds usage)
\end{code}

\begin{code}

moduleName :: FCode Module
moduleName (MkCgInfoDown (MkCompInfo mod_name) _ _ _ _) state
  = (mod_name, state)

\end{code}

\begin{code}
setEndOfBlockInfo :: EndOfBlockInfo -> Code -> Code
setEndOfBlockInfo eob_info code	(MkCgInfoDown c_info statics srt ticky _) state
  = code (MkCgInfoDown c_info statics srt ticky eob_info) state

getEndOfBlockInfo :: FCode EndOfBlockInfo
getEndOfBlockInfo (MkCgInfoDown c_info statics _ _ eob_info) state
  = (eob_info, state)
\end{code}

\begin{code}
getSRTLabel :: FCode CLabel
getSRTLabel (MkCgInfoDown _ _ srt _ _) state
  = (srt, state)

setSRTLabel :: CLabel -> Code -> Code
setSRTLabel srt code (MkCgInfoDown c_info statics _ ticky eob_info) state
  = code (MkCgInfoDown c_info statics srt ticky eob_info) state
\end{code}

\begin{code}
getTickyCtrLabel :: FCode CLabel
getTickyCtrLabel (MkCgInfoDown _ _ _ ticky _) state
  = (ticky, state)

setTickyCtrLabel :: CLabel -> Code -> Code
setTickyCtrLabel ticky code (MkCgInfoDown c_info statics srt _ eob_info) state
  = code (MkCgInfoDown c_info statics srt ticky eob_info) state
\end{code}

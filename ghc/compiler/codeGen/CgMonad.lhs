%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgMonad.lhs,v 1.39 2003/07/02 13:12:38 simonpj Exp $
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

	setSRTLabel, getSRTLabel, getSRTInfo,
	setTickyCtrLabel, getTickyCtrLabel,

	StackUsage, Slot(..), HeapUsage,

	profCtrC, profCtrAbsC, ldvEnter,

	costCentresC, moduleName,

	Sequel(..), -- ToDo: unabstract?
	sequelToAmode,

	-- ideally we wouldn't export these, but some other modules access internal state
	getState, setState, getInfoDown,

	-- more localised access to monad state	
	getUsage, setUsage,
	getBinds, setBinds, getStaticBinds,

	-- out of general friendliness, we also export ...
	CgInfoDownwards(..), CgState(..),	-- non-abstract
	CompilationInfo(..)
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgBindery ( CgBindings, nukeVolatileBinds )
import {-# SOURCE #-} CgUsages  ( getSpRelOffset )

import AbsCSyn
import CLabel
import StgSyn		( SRT(..) )
import AbsCUtils	( mkAbsCStmts )
import CmdLineOpts	( opt_SccProfilingOn, opt_DoTickyProfiling )
import Module		( Module )
import DataCon		( ConTag )
import Id		( Id )
import Name		( Name )
import VarEnv
import PrimRep		( PrimRep(..) )
import SMRep		( StgHalfWord, hALF_WORD )
import FastString
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

	  Bool	      -- True <=> polymorphic, push a SEQ frame too


type SemiTaggingStuff
  = Maybe			    -- Maybe[1] we don't have any semi-tagging stuff...
     ([(ConTag, JoinDetails)],	    -- Alternatives
      Maybe (Id, JoinDetails)	    -- Default (but Maybe[2] we don't have one)
				    -- The default branch expects a 
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

sequelToAmode (CaseAlts amode _ False) = returnFC amode
sequelToAmode (CaseAlts amode _ True)  = returnFC (CLbl mkSeqInfoLabel RetRep)

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
	(Int,		   -- virtSp:  Virtual offset of topmost allocated slot
	 Int,		   -- frameSp: End of the current stack frame
	 [(Int,Slot)],     -- free:    List of free slots, in increasing order
	 Int,		   -- realSp:  Virtual offset of real stack pointer
	 Int)		   -- hwSp:    Highest value ever taken by virtSp

-- ToDo (SDM, 7 Jan 2003): I'm not sure that the distinction between
-- Free and NonPointer in the free list is needed any more.  It used
-- to be needed because we constructed bitmaps from the free list, but
-- now we construct bitmaps by finding all the live pointer bindings
-- instead.  Non-pointer stack slots (i.e. saved cost centres) can
-- just be removed from the free list instead of being recorded as a
-- NonPointer.

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
initUsage  = ((0,0,[],0,0), (0,0))
\end{code}

@stateIncUsage@$~e_1~e_2$ incorporates in $e_1$ the stack and heap high water
marks found in $e_2$.

\begin{code}
stateIncUsage :: CgState -> CgState -> CgState

stateIncUsage (MkCgState abs_c bs ((v,t,f,r,h1),(vH1,rH1)))
	      (MkCgState _     _  ((_,_,_,_,h2),(vH2, _)))
     = MkCgState abs_c
		 bs
		 ((v,t,f,r,h1 `max` h2),
		  (vH1 `max` vH2, rH1))
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-basics]{Basic code-generation monad magic}
%*									*
%************************************************************************

\begin{code}
newtype FCode a = FCode (CgInfoDownwards -> CgState -> (a, CgState))
type Code    = FCode ()

instance Monad FCode where
	(>>=) = thenFC
	return = returnFC

{-# INLINE thenC #-}
{-# INLINE thenFC #-}
{-# INLINE returnFC #-}
\end{code}
The Abstract~C is not in the environment so as to improve strictness.

\begin{code}
initC :: CompilationInfo -> Code -> AbstractC

initC cg_info (FCode code)
  = case (code (MkCgInfoDown 
			cg_info 
			emptyVarEnv -- (error "initC: statics")
			(error "initC: srt")
			(mkTopTickyCtrLabel)
			initEobInfo)
	       initialStateC) of
      ((),MkCgState abc _ _) -> abc

returnFC :: a -> FCode a
returnFC val = FCode (\info_down state -> (val, state))
\end{code}

\begin{code}
thenC :: Code -> FCode a -> FCode a
thenC (FCode m) (FCode k) = 
  	FCode (\info_down state -> let (_,new_state) = m info_down state in 
  		k info_down new_state)

listCs :: [Code] -> Code
listCs [] = return ()
listCs (fc:fcs) = do
	fc
	listCs fcs
   	
mapCs :: (a -> Code) -> [a] -> Code
mapCs = mapM_
\end{code}

\begin{code}
thenFC	:: FCode a -> (a -> FCode c) -> FCode c
thenFC (FCode m) k = FCode (
	\info_down state ->
		let 
			(m_result, new_state) = m info_down state
			(FCode kcode) = k m_result
		in 
			kcode info_down new_state
	)

listFCs :: [FCode a] -> FCode [a]
listFCs = sequence

mapFCs :: (a -> FCode b) -> [a] -> FCode [b]
mapFCs = mapM
\end{code}

And the knot-tying combinator:
\begin{code}
fixC :: (a -> FCode a) -> FCode a
fixC fcode = FCode (
	\info_down state -> 
		let
			FCode fc = fcode v
			result@(v,_) = fc info_down state
			--	    ^--------^
		in
			result
	)
\end{code}

Operators for getting and setting the state and "info_down".
To maximise encapsulation, code should try to only get and set the
state it actually uses.

\begin{code}
getState :: FCode CgState
getState = FCode $ \info_down state -> (state,state)

setState :: CgState -> FCode ()
setState state = FCode $ \info_down _ -> ((),state)

getUsage :: FCode CgStksAndHeapUsage
getUsage = do
	MkCgState absC binds usage <- getState
	return usage

setUsage :: CgStksAndHeapUsage -> FCode ()
setUsage newusage = do
	MkCgState absC binds usage <- getState
	setState $ MkCgState absC binds newusage

getBinds :: FCode CgBindings
getBinds = do
	MkCgState absC binds usage <- getState
	return binds
	
setBinds :: CgBindings -> FCode ()
setBinds newbinds = do
	MkCgState absC binds usage <- getState
	setState $ MkCgState absC newbinds usage

getStaticBinds :: FCode CgBindings
getStaticBinds = do
	(MkCgInfoDown _ static_binds _ _ _) <- getInfoDown
	return static_binds

withState :: FCode a -> CgState -> FCode (a,CgState)
withState (FCode fcode) newstate = FCode $ \info_down state -> 
	let (retval, state2) = fcode info_down newstate in ((retval,state2), state)

getInfoDown :: FCode CgInfoDownwards
getInfoDown = FCode $ \info_down state -> (info_down,state)

withInfoDown :: FCode a -> CgInfoDownwards -> FCode a
withInfoDown (FCode fcode) info_down = FCode $ \_ state -> fcode info_down state 

doFCode :: FCode a -> CgInfoDownwards -> CgState -> (a,CgState)
doFCode (FCode fcode) info_down state = fcode info_down state
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

forkClosureBody (FCode code) = do
	(MkCgInfoDown cg_info statics srt ticky _) <- getInfoDown
	(MkCgState absC_in binds un_usage) <- getState
   	let	body_info_down = MkCgInfoDown cg_info statics srt ticky initEobInfo
	let	((),fork_state)		    = code body_info_down initialStateC
	let	MkCgState absC_fork _ _ = fork_state
	setState $ MkCgState (AbsCStmts absC_in absC_fork) binds un_usage
	
forkStatics :: FCode a -> FCode a

forkStatics (FCode fcode) = FCode (
	\(MkCgInfoDown cg_info _ srt ticky _)
	(MkCgState absC_in statics un_usage)
  -> 
  	let
		(result, state) = fcode rhs_info_down initialStateC
		MkCgState absC_fork _ _ = state	-- Don't merge these this line with the one
				-- above or it becomes too strict!
  		rhs_info_down = MkCgInfoDown cg_info statics srt ticky initEobInfo
	in
		(result, MkCgState (AbsCStmts absC_in absC_fork) statics un_usage)
	)

forkAbsC :: Code -> FCode AbstractC
forkAbsC (FCode code) =
	do
		info_down <- getInfoDown
		(MkCgState absC1 bs usage) <- getState
		let ((),MkCgState absC2 _ ((_, _, _, _,h2), _)) = code info_down (MkCgState AbsCNop bs usage)
		let ((v, t, f, r, h1), heap_usage) = usage
		let new_usage = ((v, t, f, r, h1 `max` h2), heap_usage)
		setState $ MkCgState absC1 bs new_usage
		return absC2
\end{code}

@forkAlts@ $bs~d$ takes fcodes $bs$ for the branches of a @case@, and
an fcode for the default case $d$, and compiles each in the current
environment.  The current environment is passed on unmodified, except
that
	- the worst stack high-water mark is incorporated
	- the virtual Hp is moved on to the worst virtual Hp for the branches

\begin{code}
forkAlts :: [FCode a] -> FCode [a]

forkAlts branch_fcodes
  = do	info_down <- getInfoDown
	in_state  <- getState
	let compile (FCode fc)			= fc info_down in_state
	let (branch_results, branch_out_states) = unzip (map compile branch_fcodes)
	setState $ foldl stateIncUsage in_state branch_out_states
			-- NB foldl.  in_state is the *left* argument to stateIncUsage
	return branch_results
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

forkEvalHelp body_eob_info env_code body_code =
	do
		info_down@(MkCgInfoDown cg_info statics srt ticky _) <- getInfoDown
		state <- getState
		let info_down_for_body = MkCgInfoDown cg_info statics srt ticky body_eob_info
		let (_,MkCgState _ binds ((v,t,f,_,_),_)) = 
			doFCode env_code info_down_for_body state
		let state_for_body = MkCgState AbsCNop
	 		     (nukeVolatileBinds binds)
			     ((v,t,f,v,v), (0,0))
		let (value_returned, state_at_end_return) = 
			doFCode body_code info_down_for_body state_for_body		
		setState $ state `stateIncUsageEval` state_at_end_return
		return (v,value_returned)
		
stateIncUsageEval :: CgState -> CgState -> CgState
stateIncUsageEval (MkCgState absC1 bs ((v,t,f,r,h1),heap_usage))
		  (MkCgState absC2 _  ((_,_,_,_,h2),         _))
     = MkCgState (absC1 `mkAbsCStmts` absC2)
		 -- The AbsC coming back should consist only of nested declarations,
		 -- notably of the return vector!
		 bs
		 ((v,t,f,r,h1 `max` h2), heap_usage)
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
nopC = return ()

absC :: AbstractC -> Code
absC more_absC = do
	state@(MkCgState absC binds usage) <- getState
	setState $ MkCgState (mkAbsCStmts absC more_absC) binds usage
\end{code}

These two are just like @absC@, except they examine the compilation
info (whether SCC profiling or profiling-ctrs going) and possibly emit
nothing.

\begin{code}
costCentresC :: FastString -> [CAddrMode] -> Code
costCentresC macro args
 | opt_SccProfilingOn  = absC (CCallProfCCMacro macro args)
 | otherwise           = nopC

profCtrC :: FastString -> [CAddrMode] -> Code
profCtrC macro args
 | opt_DoTickyProfiling = absC (CCallProfCtrMacro macro args)
 | otherwise            = nopC

profCtrAbsC :: FastString -> [CAddrMode] -> AbstractC
profCtrAbsC macro args
 | opt_DoTickyProfiling = CCallProfCtrMacro macro args
 | otherwise            = AbsCNop

ldvEnter :: Code
ldvEnter = costCentresC FSLIT("LDV_ENTER") [CReg node]

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
getAbsC code = do
	MkCgState absC binds usage <- getState
	((),MkCgState absC2 binds2 usage2) <- withState code (MkCgState AbsCNop binds usage)
	setState $ MkCgState absC binds2 usage2
	return absC2
\end{code}

\begin{code}
moduleName :: FCode Module
moduleName = do
	(MkCgInfoDown (MkCompInfo mod_name) _ _ _ _) <- getInfoDown
	return mod_name
\end{code}

\begin{code}
setEndOfBlockInfo :: EndOfBlockInfo -> Code -> Code
setEndOfBlockInfo eob_info code	= do
	(MkCgInfoDown c_info statics srt ticky _) <- getInfoDown
	withInfoDown code (MkCgInfoDown c_info statics srt ticky eob_info)

getEndOfBlockInfo :: FCode EndOfBlockInfo
getEndOfBlockInfo = do
	(MkCgInfoDown c_info statics _ _ eob_info) <- getInfoDown
	return eob_info
\end{code}

There is just one SRT for each top level binding; all the nested
bindings use sub-sections of this SRT.  The label is passed down to
the nested bindings via the monad.

\begin{code}
getSRTInfo :: Name -> SRT -> FCode C_SRT
getSRTInfo id NoSRT = return NoC_SRT
getSRTInfo id (SRT off len bmp)
  | len > hALF_WORD || bmp == [fromIntegral srt_escape] = do 
	srt_lbl <- getSRTLabel
	let srt_desc_lbl = mkSRTDescLabel id
	absC (CSRTDesc srt_desc_lbl srt_lbl off len bmp)
	return (C_SRT srt_desc_lbl 0 srt_escape)
  | otherwise = do
	srt_lbl <- getSRTLabel
	return (C_SRT srt_lbl off (fromIntegral (head bmp)))

srt_escape = (-1) :: StgHalfWord

getSRTLabel :: FCode CLabel	-- Used only by cgPanic
getSRTLabel = do MkCgInfoDown _ _ srt_lbl _ _ <- getInfoDown
		 return srt_lbl

setSRTLabel :: CLabel -> FCode a -> FCode a
setSRTLabel srt_lbl code
  = do  MkCgInfoDown c_info statics _ ticky eob_info <- getInfoDown
	withInfoDown code (MkCgInfoDown c_info statics srt_lbl ticky eob_info)
\end{code}

\begin{code}
getTickyCtrLabel :: FCode CLabel
getTickyCtrLabel = do
	(MkCgInfoDown _ _ _ ticky _) <- getInfoDown
	return ticky

setTickyCtrLabel :: CLabel -> Code -> Code
setTickyCtrLabel ticky code = do
	(MkCgInfoDown c_info statics srt _ eob_info) <- getInfoDown
	withInfoDown code (MkCgInfoDown c_info statics srt ticky eob_info)
\end{code}

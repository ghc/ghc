%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgStackery.lhs,v 1.23 2002/12/11 15:36:27 simonmar Exp $
%
\section[CgStackery]{Stack management functions}

Stack-twiddling operations, which are pretty low-down and grimy.
(This is the module that knows all about stack layouts, etc.)

\begin{code}
module CgStackery (
	allocStack, allocPrimStack, allocStackTop, deAllocStackTop,
	adjustStackHW, getFinalStackHW, 
	setStackFrame, getStackFrame,
	mkVirtStkOffsets, mkStkAmodes,
	freeStackSlots, dataStackSlots, addFreeSlots,
	updateFrameSize,
	constructSlowCall, slowArgs,
    ) where

#include "HsVersions.h"

import CgMonad
import AbsCSyn
import CLabel		( mkRtsApplyInfoLabel, mkRtsApplyEntryLabel )

import CgUsages		( getRealSp )
import AbsCUtils	( mkAbstractCs, getAmodeRep )
import PrimRep
import CmdLineOpts	( opt_SccProfilingOn, opt_GranMacros )
import Constants
import Util		( sortLt )
import FastString	( LitString )
import Panic
	
import TRACE		( trace )
\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-layout]{Laying out a stack frame}
%*									*
%************************************************************************

'mkVirtStkOffsets' is given a list of arguments.  The first argument
gets the /largest/ virtual stack offset (remember, virtual offsets
increase towards the top of stack).

\begin{code}
mkVirtStkOffsets
	  :: VirtualSpOffset 	-- Offset of the last allocated thing
	  -> (a -> PrimRep)	-- to be able to grab kinds
	  -> [a]			-- things to make offsets for
	  -> (VirtualSpOffset,		-- OUTPUTS: Topmost allocated word
	      [(a, VirtualSpOffset)])	-- things with offsets

mkVirtStkOffsets init_Sp_offset kind_fun things
    = loop init_Sp_offset [] (reverse things)
  where
    loop offset offs [] = (offset,offs)
    loop offset offs (t:things) =
	     let
		 size = getPrimRepSize (kind_fun t)
		 thing_slot = offset + size
	     in
	     loop thing_slot ((t,thing_slot):offs) things
    -- offset of thing is offset+size, because we're growing the stack
    -- *downwards* as the offsets increase.


-- | 'mkStkAmodes' is a higher-level version of
-- 'mkVirtStkOffsets'.  It starts from the tail-call locations.
-- It returns a single list of addressing modes for the stack
-- locations, and therefore is in the monad.  It /doesn't/ adjust the
-- high water mark.

mkStkAmodes 
	:: VirtualSpOffset	    -- Tail call positions
	-> [CAddrMode]		    -- things to make offsets for
	-> FCode (VirtualSpOffset,  -- OUTPUTS: Topmost allocated word
	          AbstractC)	    -- Assignments to appropriate stk slots

mkStkAmodes tail_Sp things
  = getRealSp `thenFC` \ realSp ->
    let
      (last_Sp_offset, offsets) = mkVirtStkOffsets tail_Sp getAmodeRep things

      abs_cs =
	  [ CAssign (CVal (spRel realSp offset) (getAmodeRep thing)) thing
	  | (thing, offset) <- offsets
	  ]
    in
    returnFC (last_Sp_offset, mkAbstractCs abs_cs)
\end{code}

%************************************************************************
%*									*
\subsection{Pushing the arguments for a slow call}
%*									*
%************************************************************************

For a slow call, we must take a bunch of arguments and intersperse
some stg_ap_<pattern>_ret_info return addresses.

\begin{code}
constructSlowCall :: [CAddrMode] -> (CAddrMode, [CAddrMode])
   -- don't forget the zero case
constructSlowCall [] = (CLbl stg_ap_0 CodePtrRep , []) 
constructSlowCall amodes = 
  -- traceSlowCall amodes $ 	
  (CLbl lbl CodePtrRep, these ++ slowArgs rest)
  where (tag, these, rest) = matchSlowPattern amodes
	lbl = mkRtsApplyEntryLabel tag

stg_ap_0 = mkRtsApplyEntryLabel SLIT("0")

-- | 'slowArgs' takes a list of function arguments and prepares them for
-- pushing on the stack for "extra" arguments to a function which requires
-- fewer arguments than we currently have.
slowArgs :: [CAddrMode] -> [CAddrMode]
slowArgs [] = []
slowArgs amodes = CLbl lbl RetRep : args ++ slowArgs rest
  where	(tag, args, rest) = matchSlowPattern amodes
	lbl = mkRtsApplyInfoLabel tag
  
matchSlowPattern :: [CAddrMode] -> (LitString, [CAddrMode], [CAddrMode])
matchSlowPattern amodes = (tag, these, rest)
  where reps = map getAmodeRep amodes
        (tag, n) = findMatch (map primRepToArgRep reps)
	(these, rest) = splitAt n amodes

-- These cases were found to cover about 99% of all slow calls:
findMatch (RepP: RepP: RepP: RepP: RepP: RepP: RepP: _) = (SLIT("ppppppp"), 7)
findMatch (RepP: RepP: RepP: RepP: RepP: RepP: _)       = (SLIT("pppppp"), 6)
findMatch (RepP: RepP: RepP: RepP: RepP: _) 	   	= (SLIT("ppppp"), 5)
findMatch (RepP: RepP: RepP: RepP: _) 		     	= (SLIT("pppp"), 4)
findMatch (RepP: RepP: RepP: _)       		     	= (SLIT("ppp"), 3)
findMatch (RepP: RepP: RepV: _)       		     	= (SLIT("ppv"), 3)
findMatch (RepP: RepP: _)				= (SLIT("pp"), 2)
findMatch (RepP: RepV: _)				= (SLIT("pv"), 2)
findMatch (RepP: _)					= (SLIT("p"), 1)
findMatch (RepV: _)					= (SLIT("v"), 1)
findMatch (RepN: _)					= (SLIT("n"), 1)
findMatch (RepF: _)					= (SLIT("f"), 1)
findMatch (RepD: _)					= (SLIT("d"), 1)
findMatch (RepL: _)					= (SLIT("l"), 1)
findMatch _  = panic "CgStackery.findMatch"

#ifdef DEBUG
primRepChar p | isFollowableRep p     = 'p'
primRepChar VoidRep                   = 'v'
primRepChar FloatRep                  = 'f'
primRepChar DoubleRep                 = 'd'
primRepChar p | getPrimRepSize p == 1 = 'n'
primRepChar p | is64BitRep p          = 'l'

traceSlowCall amodes and_then 
 = trace ("call: " ++ map primRepChar (map getAmodeRep amodes)) and_then
#endif
\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-monadery]{Inside-monad functions for stack manipulation}
%*									*
%************************************************************************

Allocate a virtual offset for something.

\begin{code}
allocStack :: FCode VirtualSpOffset
allocStack = allocPrimStack 1

allocPrimStack :: Int -> FCode VirtualSpOffset
allocPrimStack size = do
	((virt_sp, frame, free_stk, real_sp, hw_sp),h_usage) <- getUsage
	let push_virt_sp = virt_sp + size
	let (chosen_slot, new_stk_usage) = 
		case find_block free_stk of
		   Nothing -> (push_virt_sp, 
				 (push_virt_sp, frame, free_stk, real_sp,
				  hw_sp `max` push_virt_sp))
						-- Adjust high water mark
		   Just slot -> (slot, 
				  (virt_sp, frame, 
				   delete_block free_stk slot, 
				   real_sp, hw_sp))
	setUsage (new_stk_usage, h_usage)
	return chosen_slot
	
	where
		-- find_block looks for a contiguous chunk of free slots
		find_block :: [(VirtualSpOffset,Slot)] -> Maybe VirtualSpOffset
		find_block [] = Nothing
		find_block ((off,free):slots)
			| take size ((off,free):slots) == 
			  zip [off..top_slot] (repeat Free) = Just top_slot
			| otherwise				   = find_block slots
				-- The stack grows downwards, with increasing virtual offsets.
				-- Therefore, the address of a multi-word object is the *highest*
				-- virtual offset it occupies (top_slot below).
			where top_slot = off+size-1

		delete_block free_stk slot = [ (s,f) | (s,f) <- free_stk, 
				           (s<=slot-size) || (s>slot) ]
			      -- Retain slots which are not in the range
			      -- slot-size+1..slot
\end{code}

Allocate a chunk ON TOP OF the stack.  

ToDo: should really register this memory as NonPointer stuff in the
free list.

\begin{code}
allocStackTop :: Int -> FCode VirtualSpOffset
allocStackTop size = do
	((virt_sp, frame, free_stk, real_sp, hw_sp), h_usage) <- getUsage
	let push_virt_sp = virt_sp + size
	let new_stk_usage = (push_virt_sp, frame, free_stk, real_sp, 
				hw_sp `max` push_virt_sp)
	setUsage (new_stk_usage, h_usage)
	return push_virt_sp
\end{code}

Pop some words from the current top of stack.  This is used for
de-allocating the return address in a case alternative.

\begin{code}
deAllocStackTop :: Int -> FCode VirtualSpOffset
deAllocStackTop size = do
	((virt_sp, frame, free_stk, real_sp, hw_sp), h_usage) <- getUsage
	let pop_virt_sp = virt_sp - size
	let new_stk_usage = (pop_virt_sp, frame, free_stk, real_sp, hw_sp)
	setUsage (new_stk_usage, h_usage)
	return pop_virt_sp
\end{code}

\begin{code}
adjustStackHW :: VirtualSpOffset -> Code
adjustStackHW offset = do
	((vSp,fTop,fSp,realSp,hwSp), h_usage) <- getUsage
	setUsage ((vSp, fTop, fSp, realSp, max offset hwSp), h_usage)
\end{code}

A knot-tying beast.

\begin{code}
getFinalStackHW :: (VirtualSpOffset -> Code) -> Code
getFinalStackHW fcode = do
	fixC (\hwSp -> do
		fcode hwSp
		((_,_,_,_, hwSp),_) <- getUsage
		return hwSp)
	return ()
\end{code}

\begin{code}
setStackFrame :: VirtualSpOffset -> Code
setStackFrame offset = do
	((vSp,_,fSp,realSp,hwSp), h_usage) <- getUsage
	setUsage ((vSp, offset, fSp, realSp, hwSp), h_usage)

getStackFrame :: FCode VirtualSpOffset
getStackFrame = do
	((vSp,frame,fSp,realSp,hwSp), h_usage) <- getUsage
	return frame
\end{code}

\begin{code}
updateFrameSize | opt_SccProfilingOn = pROF_UF_SIZE
		| opt_GranMacros     = trace ("updateFrameSize = " ++ (show gRAN_UF_SIZE))gRAN_UF_SIZE
		| otherwise          = uF_SIZE
\end{code}			

%************************************************************************
%*									*
\subsection[CgStackery-free]{Free stack slots}
%*									*
%************************************************************************

Explicitly free some stack space.

\begin{code}
addFreeStackSlots :: [VirtualSpOffset] -> Slot -> Code
addFreeStackSlots extra_free slot = do
	((vsp, frame,free, real, hw),heap_usage) <- getUsage
	let all_free = addFreeSlots free (zip (sortLt (<) extra_free) (repeat slot))
	let (new_vsp, new_free) = trim vsp all_free
	let new_usage = ((new_vsp, frame, new_free, real, hw), heap_usage)
	setUsage new_usage

freeStackSlots :: [VirtualSpOffset] -> Code
freeStackSlots slots = addFreeStackSlots slots Free

dataStackSlots :: [VirtualSpOffset] -> Code
dataStackSlots slots = addFreeStackSlots slots NonPointer

addFreeSlots :: [(Int,Slot)] -> [(Int,Slot)] -> [(Int,Slot)]
addFreeSlots cs [] = cs
addFreeSlots [] ns = ns
addFreeSlots ((c,s):cs) ((n,s'):ns)
 = if c < n then
	(c,s) : addFreeSlots cs ((n,s'):ns)
   else if c > n then
	(n,s') : addFreeSlots ((c,s):cs) ns
   else if s /= s' then -- c == n
	(c,s') : addFreeSlots cs ns
   else
	panic ("addFreeSlots: equal slots: " ++ show (c:map fst cs)
					     ++ show (n:map fst ns))

trim :: Int{-offset-} -> [(Int,Slot)] -> (Int{-offset-}, [(Int,Slot)])
trim current_sp free_slots
  = try current_sp free_slots
  where
	try csp [] = (csp,[])

	try csp (slot@(off,state):slots) = 
		if state == Free && null slots' then
		    if csp' < off then 
			(csp', [])
		    else if csp' == off then
			(csp'-1, [])
		    else 
			(csp',[slot])
		else
		    (csp', slot:slots')
		where
		    (csp',slots') = try csp slots
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgStackery.lhs,v 1.13 2000/01/13 14:33:58 hwloidl Exp $
%
\section[CgStackery]{Stack management functions}

Stack-twiddling operations, which are pretty low-down and grimy.
(This is the module that knows all about stack layouts, etc.)

\begin{code}
module CgStackery (
	allocStack, allocPrimStack, allocStackTop, deAllocStackTop,
	adjustStackHW, getFinalStackHW,
	mkTaggedVirtStkOffsets, mkTaggedStkAmodes, mkTagAssts,
	freeStackSlots, dataStackSlots, addFreeSlots,
	updateFrameSize, seqFrameSize
    ) where

#include "HsVersions.h"

import CgMonad
import AbsCSyn

import CgUsages		( getRealSp )
import AbsCUtils	( mkAbstractCs, mkAbsCStmts, getAmodeRep )
import PrimRep		( getPrimRepSize, PrimRep(..), isFollowableRep )
import CmdLineOpts	( opt_SccProfilingOn, opt_GranMacros )
import Panic		( panic )
import Constants	( uF_SIZE, sCC_UF_SIZE, gRAN_UF_SIZE, 
			  sEQ_FRAME_SIZE, sCC_SEQ_FRAME_SIZE, gRAN_SEQ_FRAME_SIZE )

import IOExts		( trace )
\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-layout]{Laying out a stack frame}
%*									*
%************************************************************************

@mkTaggedVirtStkOffsets@ is given a list of arguments.  The first
argument gets the {\em largest} virtual stack offset (remember,
virtual offsets increase towards the top of stack).  This function
also computes the correct tagging arrangement for standard function
entry points.  Each non-pointer on the stack is preceded by a tag word
indicating the number of non-pointer words above it on the stack.

		offset --> |       |  <---- last allocated stack word
			   ---------  <
			   |       |  .
			   ---------  .
			   |       |  total_nptrs (words)
			   ---------  .
			   |       |  .
			   ---------  <
offset + tot_nptrs + 1 --> |  tag  |  
			   ---------

\begin{code}
mkTaggedVirtStkOffsets
	  :: VirtualSpOffset 	-- Offset of the last allocated thing
	  -> (a -> PrimRep)	-- to be able to grab kinds
	  -> [a]			-- things to make offsets for
	  -> (VirtualSpOffset,		-- OUTPUTS: Topmost allocated word
	      [(a, VirtualSpOffset)],	-- things with offsets
	      [(VirtualSpOffset,Int)])  -- offsets for tags

mkTaggedVirtStkOffsets init_Sp_offset kind_fun things
    = loop init_Sp_offset [] [] (reverse things)
  where
    loop offset tags offs [] = (offset,offs,tags)
    loop offset tags offs (t:things) 
	 | isFollowableRep (kind_fun t) =
	     loop (offset+1) tags ((t,offset+1):offs) things
	 | otherwise =
	     let
		 size = getPrimRepSize (kind_fun t)
		 tag_slot = offset+size+1
	     in
	     loop tag_slot ((tag_slot,size):tags) ((t,offset+size):offs) things
    -- offset of thing is offset+size, because we're growing the stack
    -- *downwards* as the offsets increase.
\end{code}

@mkTaggedStkAmodes@ is a higher-level version of
@mkTaggedVirtStkOffsets@.  It starts from the tail-call locations.  It
returns a single list of addressing modes for the stack locations, and
therefore is in the monad.

It *doesn't* adjust the high water mark.  

\begin{code}
mkTaggedStkAmodes 
	:: VirtualSpOffset	    -- Tail call positions
	-> [CAddrMode]		    -- things to make offsets for
	-> FCode (VirtualSpOffset,  -- OUTPUTS: Topmost allocated word
	          AbstractC,	    -- Assignments to appropriate stk slots
		  AbstractC)	    -- Assignments for tagging

mkTaggedStkAmodes tail_Sp things
  = getRealSp `thenFC` \ realSp ->
    let
      (last_Sp_offset, offsets, tags)
    	= mkTaggedVirtStkOffsets tail_Sp getAmodeRep things

      abs_cs =
	  [ CAssign (CVal (spRel realSp offset) (getAmodeRep thing)) thing
	  | (thing, offset) <- offsets
	  ]
 
      tag_cs =
	  [ CAssign (CVal (spRel realSp offset) WordRep)
		    (CMacroExpr WordRep ARG_TAG [mkIntCLit size])
	  | (offset,size) <- tags
	  ]
    in
    returnFC (last_Sp_offset, mkAbstractCs abs_cs, mkAbstractCs tag_cs)

mkTagAssts :: [(VirtualSpOffset,Int)] -> FCode AbstractC
mkTagAssts tags = 
   getRealSp `thenFC` \realSp ->
   returnFC (mkAbstractCs
	  [ CAssign (CVal (spRel realSp offset) WordRep)
		    (CMacroExpr WordRep ARG_TAG [mkIntCLit size])
	  | (offset,size) <- tags
	  ])

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
allocPrimStack size info_down (MkCgState absC binds
				 ((virt_sp, free_stk, real_sp, hw_sp), h_usage))
  = (chosen_slot, MkCgState absC binds (new_stk_usage, h_usage))
  where
    push_virt_sp = virt_sp + size

    (chosen_slot, new_stk_usage)
	= case find_block free_stk of
		Nothing -> (push_virt_sp, (push_virt_sp, free_stk, real_sp,
				       hw_sp `max` push_virt_sp))
				       -- Adjust high water mark

		Just slot -> (slot, (virt_sp, 
				    delete_block free_stk slot, real_sp, hw_sp))

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
allocStackTop size info_down (MkCgState absC binds
	                     ((virt_sp, free_stk, real_sp, hw_sp), h_usage))
  = (push_virt_sp, MkCgState absC binds (new_stk_usage, h_usage))
  where
    push_virt_sp = virt_sp + size
    new_stk_usage = (push_virt_sp, free_stk, real_sp, hw_sp `max` push_virt_sp)
				                -- Adjust high water mark
\end{code}

Pop some words from the current top of stack.  This is used for
de-allocating the return address in a case alternative.

\begin{code}
deAllocStackTop :: Int -> FCode VirtualSpOffset
deAllocStackTop size info_down (MkCgState absC binds
	                     ((virt_sp, free_stk, real_sp, hw_sp), h_usage))
  = (pop_virt_sp, MkCgState absC binds (new_stk_usage, h_usage))
  where
    pop_virt_sp = virt_sp - size
    new_stk_usage = (pop_virt_sp, free_stk, real_sp, hw_sp)
\end{code}

\begin{code}
adjustStackHW :: VirtualSpOffset -> Code
adjustStackHW offset info_down (MkCgState absC binds usage) 
  = MkCgState absC binds new_usage
  where
    ((vSp,fSp,realSp,hwSp), h_usage) = usage
    new_usage = ((vSp, fSp, realSp, max offset hwSp), h_usage)
    -- No need to fiddle with virtual Sp etc because this call is
    -- only done just before the end of a block
\end{code}

A knot-tying beast.

\begin{code}
getFinalStackHW :: (VirtualSpOffset -> Code) -> Code
getFinalStackHW fcode info_down (MkCgState absC binds usages) = state1
  where
    state1 = fcode hwSp info_down (MkCgState absC binds usages)
    (MkCgState _ _ ((_,_,_, hwSp), _)) = state1
\end{code}

\begin{code}
updateFrameSize | opt_SccProfilingOn = trace ("updateFrameSize = " ++ (show sCC_UF_SIZE)) sCC_UF_SIZE
		| opt_GranMacros     = trace ("updateFrameSize = " ++ (show gRAN_UF_SIZE))gRAN_UF_SIZE
		| otherwise          = trace ("updateFrameSize = " ++ (show uF_SIZE)) uF_SIZE

seqFrameSize    | opt_SccProfilingOn  = sCC_SEQ_FRAME_SIZE
	        | opt_GranMacros      = gRAN_SEQ_FRAME_SIZE
		| otherwise           = sEQ_FRAME_SIZE
\end{code}			

%************************************************************************
%*									*
\subsection[CgStackery-free]{Free stack slots}
%*									*
%************************************************************************

Explicitly free some stack space.

\begin{code}
addFreeStackSlots :: [VirtualSpOffset] -> Slot -> Code
addFreeStackSlots extra_free slot info_down
	state@(MkCgState abs_c binds ((vsp, free, real, hw), heap_usage))
  = MkCgState abs_c binds new_usage
  where
    new_usage = ((new_vsp, new_free, real, hw), heap_usage)
    (new_vsp, new_free) = trim vsp all_free
    all_free = addFreeSlots free (zip extra_free (repeat slot))

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

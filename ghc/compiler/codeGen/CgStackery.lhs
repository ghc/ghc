%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CgStackery]{Stack management functions}

Stack-twiddling operations, which are pretty low-down and grimy.
(This is the module that knows all about stack layouts, etc.)

\begin{code}
#include "HsVersions.h"

module CgStackery (
	allocAStack, allocBStack, allocAStackTop, allocBStackTop,
	allocUpdateFrame,
	adjustRealSps, getFinalStackHW,
	mkVirtStkOffsets, mkStkAmodes
    ) where

import Ubiq{-uitous-}

import CgMonad
import AbsCSyn

import AbsCUtils	( mkAbstractCs, mkAbsCStmts, getAmodeRep )
import HeapOffs		( VirtualSpAOffset(..), VirtualSpBOffset(..) )
import PrimRep		( getPrimRepSize, separateByPtrFollowness,
			  PrimRep(..)
			)
import Util		( mapAccumR, panic )
\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-layout]{Laying out a stack frame}
%*									*
%************************************************************************

@mkVirtStkOffsets@ is given a list of arguments.  The first argument
gets the {\em largest} virtual stack offset (remember, virtual offsets
increase towards the top of stack).

\begin{code}
mkVirtStkOffsets :: VirtualSpAOffset 	-- Offset of the last allocated thing
	  -> VirtualSpBOffset		-- ditto
	  -> (a -> PrimRep)	-- to be able to grab kinds
	  -> [a]			-- things to make offsets for
	  -> (VirtualSpAOffset,		-- OUTPUTS: Topmost allocated word
	      VirtualSpBOffset,		-- ditto
	      [(a, VirtualSpAOffset)],	--  boxed things with offsets
	      [(a, VirtualSpBOffset)])	--  unboxed things with offsets

mkVirtStkOffsets init_SpA_offset init_SpB_offset kind_fun things
  = let (boxeds, unboxeds)
	    = separateByPtrFollowness kind_fun things
	(last_SpA_offset, boxd_w_offsets)
	    = mapAccumR computeOffset init_SpA_offset boxeds
	(last_SpB_offset, ubxd_w_offsets)
	    = mapAccumR computeOffset init_SpB_offset unboxeds
    in
	(last_SpA_offset, last_SpB_offset, boxd_w_offsets, ubxd_w_offsets)
  where
    computeOffset offset thing
      = (offset + (max 1 . getPrimRepSize . kind_fun) thing, (thing, offset+(1::Int)))
	-- The "max 1" bit is ULTRA important
	-- Why?  mkVirtStkOffsets is the unique function that lays out function
	-- arguments on the stack. The "max 1" ensures that every argument takes
	-- at least one stack slot, even if it's of kind VoidKind that actually
	-- takes no space at all.
	-- This is important to make sure that argument satisfaction checks work
	-- properly.  Consider
	--	f a b s# = (a,b)
	-- where s# is a VoidKind.  f's argument satisfaction check will check
	-- that s# is on the B stack above SuB; but if s# takes zero space, the
	-- check will be ARGS_B_CHK(0), which always succeeds.  As a result, even
	-- if a,b aren't available either, the PAP update won't trigger and
	-- we are throughly hosed. (SLPJ 96/05)
\end{code}

@mkStackAmodes@ is a higher-level version of @mkStackOffsets@.
It starts from the tail-call locations.
It returns a single list of addressing modes for the stack locations,
and therefore is in the monad.

It also adjusts the high water mark if necessary.

\begin{code}
mkStkAmodes :: VirtualSpAOffset	    	    -- Tail call positions
    	    -> VirtualSpBOffset
	    -> [CAddrMode]		    -- things to make offsets for
	    -> FCode (VirtualSpAOffset,	    -- OUTPUTS: Topmost allocated word
		      VirtualSpBOffset,	    -- ditto
		      AbstractC)	    -- Assignments to appropriate stk slots

mkStkAmodes tail_spa tail_spb things
	    info_down (MkCgState absC binds usage)
  = (result, MkCgState absC binds new_usage)
  where
    result = (last_SpA_offset, last_SpB_offset, mkAbstractCs abs_cs)

    (last_SpA_offset, last_SpB_offset, ptrs_w_offsets, non_ptrs_w_offsets)
    	= mkVirtStkOffsets tail_spa tail_spb getAmodeRep things

    abs_cs
	= [ CAssign (CVal (SpARel realSpA offset) PtrRep) thing
	  | (thing, offset) <- ptrs_w_offsets
	  ]
	  ++
	  [ CAssign (CVal (SpBRel realSpB offset) (getAmodeRep thing)) thing
	  | (thing, offset) <- non_ptrs_w_offsets
	  ]

    ((vspA,fspA,realSpA,hwSpA), (vspB,fspB,realSpB,hwSpB), h_usage) = usage

    new_usage = ((vspA,fspA,realSpA,max last_SpA_offset hwSpA),
		 (vspB,fspB,realSpB,max last_SpB_offset hwSpB),
		 h_usage)
    -- No need to fiddle with virtual SpA etc because this call is
    -- only done just before the end of a block


\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-monadery]{Inside-monad functions for stack manipulation}
%*									*
%************************************************************************

Allocate a virtual offset for something.
\begin{code}
allocAStack :: FCode VirtualSpAOffset

allocAStack info_down (MkCgState absC binds
		    ((virt_a, free_a, real_a, hw_a), b_usage, h_usage))
  = (chosen_slot, MkCgState absC binds (new_a_usage, b_usage, h_usage))
  where
    push_virt_a = virt_a + 1

    (chosen_slot, new_a_usage)
	= if null free_a then
		-- No free slots, so push a new one
		-- We need to adjust the high-water mark
		(push_virt_a, (push_virt_a, [], real_a, hw_a `max` push_virt_a))
	  else
		-- Free slots available, so use one
		(free_slot, (virt_a, new_free_a, real_a, hw_a))

    (free_slot, _) = head ([f | f@(slot, st) <- free_a, not (isStubbed st)] ++ free_a)
		    -- Try to find an un-stubbed location;
		    -- if none, return the first in the free list
		    -- We'll only try this if free_a is known to be non-empty

    -- Free list with the free_slot deleted
    new_free_a = [ f | f@(s,_) <- free_a, s /= free_slot ]

allocBStack :: Int -> FCode VirtualSpBOffset
allocBStack size info_down (MkCgState absC binds
				 (a_usage, (virt_b, free_b, real_b, hw_b), h_usage))
  = (chosen_slot, MkCgState absC binds (a_usage, new_b_usage, h_usage))
  where
    push_virt_b = virt_b + size

    (chosen_slot, new_b_usage)
	= case find_block free_b of
		Nothing -> (virt_b+1, (push_virt_b, free_b, real_b,
				       hw_b `max` push_virt_b))
				       -- Adjust high water mark

		Just slot -> (slot, (virt_b, delete_block free_b slot, real_b, hw_b))

    -- find_block looks for a contiguous chunk of free slots
    find_block :: [VirtualSpBOffset] -> Maybe VirtualSpBOffset
    find_block [] = Nothing
    find_block (slot:slots)
      | take size (slot:slots) == [slot..slot+size-1]
      = Just slot
      | otherwise
      = find_block slots

    delete_block free_b slot = [s | s <- free_b, (s<slot) || (s>=slot+size)]
			      -- Retain slots which are not in the range
			      -- slot..slot+size-1

-- Allocate a chunk ON TOP OF the stack
allocAStackTop :: Int -> FCode VirtualSpAOffset
allocAStackTop size info_down (MkCgState absC binds
	                     ((virt_a, free_a, real_a, hw_a), b_usage, h_usage))
  = (chosen_slot, MkCgState absC binds (new_a_usage, b_usage, h_usage))
  where
    push_virt_a = virt_a + size
    chosen_slot = virt_a + 1
    new_a_usage = (push_virt_a, free_a, real_a, hw_a `max` push_virt_a)
				                -- Adjust high water mark

-- Allocate a chunk ON TOP OF the stack
allocBStackTop :: Int -> FCode VirtualSpBOffset
allocBStackTop size info_down (MkCgState absC binds
	                     (a_usage, (virt_b, free_b, real_b, hw_b), h_usage))
  = (chosen_slot, MkCgState absC binds (a_usage, new_b_usage, h_usage))
  where
    push_virt_b = virt_b + size
    chosen_slot = virt_b+1
    new_b_usage = (push_virt_b, free_b, real_b, hw_b `max` push_virt_b)
				                -- Adjust high water mark
\end{code}

@allocUpdateFrame@ allocates enough space for an update frame
on the B stack, records the fact in the end-of-block info (in the ``args''
fields), and passes on the old ``args'' fields to the enclosed code.

This is all a bit disgusting.

\begin{code}
allocUpdateFrame :: Int			-- Size of frame
		 -> CAddrMode		-- Return address which is to be the
					-- top word of frame
		 -> ((VirtualSpAOffset, VirtualSpBOffset, VirtualSpBOffset) -> Code)
						-- Scope of update
		 -> Code

allocUpdateFrame size update_amode code
	(MkCgInfoDown c_info statics (EndOfBlockInfo args_spa args_spb sequel))
	(MkCgState absc binds (a_usage, (vB,rr,qq,hwB),h_usage))
  = case sequel of

	InRetReg -> code (args_spa, args_spb, vB)
			 (MkCgInfoDown c_info statics new_eob_info)
			 (MkCgState absc binds new_usage)

	other    -> panic "allocUpdateFrame"

  where
    new_vB = vB + size
    new_eob_info = EndOfBlockInfo args_spa new_vB (UpdateCode update_amode)
    new_usage = (a_usage, (new_vB,rr,qq,hwB `max` new_vB), h_usage)
\end{code}


A knot-tying beast.

\begin{code}
getFinalStackHW :: (VirtualSpAOffset -> VirtualSpBOffset -> Code) -> Code
getFinalStackHW fcode info_down (MkCgState absC binds usages) = state1
  where
    state1 = fcode hwSpA hwSpB info_down (MkCgState absC binds usages)
    (MkCgState _ _ ((_,_,_, hwSpA), (_,_,_, hwSpB), _)) = state1
\end{code}


%************************************************************************
%*									*
\subsection[CgStackery-adjust]{Adjusting the stack pointers}
%*									*
%************************************************************************

@adjustRealSpX@ generates code to alter the actual stack pointer, and
adjusts the environment accordingly.  We are careful to push the
conditional inside the abstract C code to avoid black holes.
ToDo: combine together?

These functions {\em do not} deal with high-water-mark adjustment.
That's done by functions which allocate stack space.

\begin{code}
adjustRealSpA :: VirtualSpAOffset 	-- New offset for Arg stack ptr
	      -> Code
adjustRealSpA newRealSpA info_down (MkCgState absC binds
					((vspA,fA,realSpA,hwspA),
				 	b_usage, h_usage))
  = MkCgState (mkAbsCStmts absC move_instrA) binds new_usage
    where
    move_instrA = if (newRealSpA == realSpA) then AbsCNop
		 else (CAssign
			    (CReg SpA)
			    (CAddr (SpARel realSpA newRealSpA)))
    new_usage = ((vspA, fA, newRealSpA, hwspA),
		 b_usage, h_usage)

adjustRealSpB :: VirtualSpBOffset 	-- New offset for Basic/Control stack ptr
	      -> Code
adjustRealSpB newRealSpB info_down (MkCgState absC binds
					(a_usage,
				 	(vspB,fB,realSpB,hwspB),
				 	h_usage))
  = MkCgState (mkAbsCStmts absC move_instrB) binds new_usage
    where
    move_instrB = if (newRealSpB == realSpB) then AbsCNop
		 else (CAssign {-PtrRep-}
			    (CReg SpB)
			    (CAddr (SpBRel realSpB newRealSpB)))
    new_usage = (a_usage,
		 (vspB, fB, newRealSpB, hwspB),
		 h_usage)

adjustRealSps :: VirtualSpAOffset 	-- New offset for Arg stack ptr
	      -> VirtualSpBOffset 	-- Ditto B stack
	      -> Code
adjustRealSps newRealSpA newRealSpB
  = adjustRealSpA newRealSpA `thenC` adjustRealSpB newRealSpB
\end{code}

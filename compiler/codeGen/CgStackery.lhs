%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgStackery.lhs,v 1.27 2004/09/30 10:35:49 simonpj Exp $
%
\section[CgStackery]{Stack management functions}

Stack-twiddling operations, which are pretty low-down and grimy.
(This is the module that knows all about stack layouts, etc.)

\begin{code}
module CgStackery (
	spRel, getVirtSp, getRealSp, setRealSp,
	setRealAndVirtualSp, getSpRelOffset,

	allocPrimStack, allocStackTop, deAllocStackTop,
	adjustStackHW, getFinalStackHW, 
	setStackFrame, getStackFrame,
	mkVirtStkOffsets, mkStkAmodes,
	freeStackSlots, 
	pushUpdateFrame, emitPushUpdateFrame,
    ) where

#include "HsVersions.h"

import CgMonad
import CgUtils		( cmmOffsetB, cmmRegOffW )
import CgProf		( initUpdFrameProf )
import SMRep
import Cmm
import CmmUtils		( CmmStmts, mkLblExpr )
import CLabel		( mkUpdInfoLabel )
import Constants
import Util		( sortLe )
import FastString	( LitString )
import OrdList		( toOL )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[CgUsages-stackery]{Monad things for fiddling with stack usage}
%*									*
%************************************************************************

spRel is a little function that abstracts the stack direction.  Note that most
of the code generator is dependent on the stack direction anyway, so
changing this on its own spells certain doom.  ToDo: remove?

	THIS IS DIRECTION SENSITIVE!

Stack grows down, positive virtual offsets correspond to negative
additions to the stack pointer.

\begin{code}
spRel :: VirtualSpOffset 	-- virtual offset of Sp
      -> VirtualSpOffset 	-- virtual offset of The Thing
      -> WordOff		-- integer offset
spRel sp off = sp - off
\end{code}

@setRealAndVirtualSp@ sets into the environment the offsets of the
current position of the real and virtual stack pointers in the current
stack frame.  The high-water mark is set too.  It generates no code.
It is used to initialise things at the beginning of a closure body.

\begin{code}
setRealAndVirtualSp :: VirtualSpOffset 	-- New real Sp
		     -> Code

setRealAndVirtualSp new_sp 
  = do	{ stk_usg <- getStkUsage
	; setStkUsage (stk_usg {virtSp = new_sp, 
				realSp = new_sp, 
				hwSp   = new_sp}) }

getVirtSp :: FCode VirtualSpOffset
getVirtSp
  = do	{ stk_usg <- getStkUsage
	; return (virtSp stk_usg) }

getRealSp :: FCode VirtualSpOffset
getRealSp
  = do	{ stk_usg <- getStkUsage
	; return (realSp stk_usg) }

setRealSp :: VirtualSpOffset -> Code
setRealSp new_real_sp
  = do	{ stk_usg <- getStkUsage
	; setStkUsage (stk_usg {realSp = new_real_sp}) }

getSpRelOffset :: VirtualSpOffset -> FCode CmmExpr
getSpRelOffset virtual_offset
  = do	{ real_sp <- getRealSp
	; return (cmmRegOffW spReg (spRel real_sp virtual_offset)) }
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
	  -> [(CgRep,a)]		-- things to make offsets for
	  -> (VirtualSpOffset,		-- OUTPUTS: Topmost allocated word
	      [(a, VirtualSpOffset)])	-- things with offsets (voids filtered out)

mkVirtStkOffsets init_Sp_offset things
    = loop init_Sp_offset [] (reverse things)
  where
    loop offset offs [] = (offset,offs)
    loop offset offs ((VoidArg,t):things) = loop offset offs things
	-- ignore Void arguments
    loop offset offs ((rep,t):things)
	= loop thing_slot ((t,thing_slot):offs) things
	where
	  thing_slot = offset + cgRepSizeW rep
	    -- offset of thing is offset+size, because we're 
	    -- growing the stack *downwards* as the offsets increase.

-- | 'mkStkAmodes' is a higher-level version of
-- 'mkVirtStkOffsets'.  It starts from the tail-call locations.
-- It returns a single list of addressing modes for the stack
-- locations, and therefore is in the monad.  It /doesn't/ adjust the
-- high water mark.

mkStkAmodes 
	:: VirtualSpOffset	    -- Tail call positions
	-> [(CgRep,CmmExpr)]	    -- things to make offsets for
	-> FCode (VirtualSpOffset,  -- OUTPUTS: Topmost allocated word
	          CmmStmts)	    -- Assignments to appropriate stk slots

mkStkAmodes tail_Sp things
  = do	{ rSp <- getRealSp
	; let (last_Sp_offset, offsets) = mkVirtStkOffsets tail_Sp things
	      abs_cs = [ CmmStore (cmmRegOffW spReg (spRel rSp offset)) amode
		       | (amode, offset) <- offsets
		       ]
	; returnFC (last_Sp_offset, toOL abs_cs) }
\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-monadery]{Inside-monad functions for stack manipulation}
%*									*
%************************************************************************

Allocate a virtual offset for something.

\begin{code}
allocPrimStack :: CgRep -> FCode VirtualSpOffset
allocPrimStack rep
  = do	{ stk_usg <- getStkUsage
	; let free_stk = freeStk stk_usg
	; case find_block free_stk of
	     Nothing -> do 
		{ let push_virt_sp = virtSp stk_usg + size
		; setStkUsage (stk_usg { virtSp = push_virt_sp,
					 hwSp   = hwSp stk_usg `max` push_virt_sp })
						-- Adjust high water mark
		; return push_virt_sp }
	     Just slot -> do
		{ setStkUsage (stk_usg { freeStk = delete_block free_stk slot }) 
		; return slot }
	}
  where
    size :: WordOff
    size = cgRepSizeW rep

	-- Find_block looks for a contiguous chunk of free slots
	-- returning the offset of its topmost word
    find_block :: [VirtualSpOffset] -> Maybe VirtualSpOffset
    find_block [] = Nothing
    find_block (slot:slots)
	| take size (slot:slots) == [slot..top_slot]
	= Just top_slot
	| otherwise
	= find_block slots
	where	-- The stack grows downwards, with increasing virtual offsets.
		-- Therefore, the address of a multi-word object is the *highest*
		-- virtual offset it occupies (top_slot below).
	    top_slot = slot+size-1

    delete_block free_stk slot = [ s | s <- free_stk, 
				       (s<=slot-size) || (s>slot) ]
		      -- Retain slots which are not in the range
		      -- slot-size+1..slot
\end{code}

Allocate a chunk ON TOP OF the stack.  

\begin{code}
allocStackTop :: WordOff -> FCode VirtualSpOffset
allocStackTop size
  = do	{ stk_usg <- getStkUsage
	; let push_virt_sp = virtSp stk_usg + size
	; setStkUsage (stk_usg { virtSp = push_virt_sp,
				 hwSp   = hwSp stk_usg `max` push_virt_sp })
	; return push_virt_sp }
\end{code}

Pop some words from the current top of stack.  This is used for
de-allocating the return address in a case alternative.

\begin{code}
deAllocStackTop :: WordOff -> FCode VirtualSpOffset
deAllocStackTop size
  = do	{ stk_usg <- getStkUsage
	; let pop_virt_sp = virtSp stk_usg - size
	; setStkUsage (stk_usg { virtSp = pop_virt_sp })
	; return pop_virt_sp }
\end{code}

\begin{code}
adjustStackHW :: VirtualSpOffset -> Code
adjustStackHW offset
  = do	{ stk_usg <- getStkUsage
	; setStkUsage (stk_usg { hwSp = hwSp stk_usg `max` offset }) }
\end{code}

A knot-tying beast.

\begin{code}
getFinalStackHW :: (VirtualSpOffset -> Code) -> Code
getFinalStackHW fcode
  = do	{ fixC (\hw_sp -> do
		{ fcode hw_sp
		; stk_usg <- getStkUsage
		; return (hwSp stk_usg) })
	; return () }
\end{code}

\begin{code}
setStackFrame :: VirtualSpOffset -> Code
setStackFrame offset
  = do	{ stk_usg <- getStkUsage
	; setStkUsage (stk_usg { frameSp = offset }) }

getStackFrame :: FCode VirtualSpOffset
getStackFrame
  = do	{ stk_usg <- getStkUsage
	; return (frameSp stk_usg) }
\end{code}


%********************************************************
%*							*
%*		Setting up update frames		*
%*							*
%********************************************************

@pushUpdateFrame@ $updatee$ pushes a general update frame which
points to $updatee$ as the thing to be updated.  It is only used
when a thunk has just been entered, so the (real) stack pointers
are guaranteed to be nicely aligned with the top of stack.
@pushUpdateFrame@ adjusts the virtual and tail stack pointers
to reflect the frame pushed.

\begin{code}
pushUpdateFrame :: CmmExpr -> Code -> Code

pushUpdateFrame updatee code
  = do	{
#ifdef DEBUG
	  EndOfBlockInfo _ sequel <- getEndOfBlockInfo ;
	  ASSERT(case sequel of { OnStack -> True; _ -> False})
#endif

	  allocStackTop (fixedHdrSize + 
			   sIZEOF_StgUpdateFrame_NoHdr `quot` wORD_SIZE)
	; vsp <- getVirtSp
	; setStackFrame vsp
	; frame_addr <- getSpRelOffset vsp
		-- The location of the lowest-address
		-- word of the update frame itself

	; setEndOfBlockInfo (EndOfBlockInfo vsp UpdateCode) $
	    do	{ emitPushUpdateFrame frame_addr updatee
		; code }
	}

emitPushUpdateFrame :: CmmExpr -> CmmExpr -> Code
emitPushUpdateFrame frame_addr updatee = do
	stmtsC [  -- Set the info word
		  CmmStore frame_addr (mkLblExpr mkUpdInfoLabel)
		, -- And the updatee
		  CmmStore (cmmOffsetB frame_addr off_updatee) updatee ]
	initUpdFrameProf frame_addr

off_updatee :: ByteOff
off_updatee = fixedHdrSize*wORD_SIZE + oFFSET_StgUpdateFrame_updatee
\end{code}			


%************************************************************************
%*									*
\subsection[CgStackery-free]{Free stack slots}
%*									*
%************************************************************************

Explicitly free some stack space.

\begin{code}
freeStackSlots :: [VirtualSpOffset] -> Code
freeStackSlots extra_free
  = do	{ stk_usg <- getStkUsage
	; let all_free = addFreeSlots (freeStk stk_usg) (sortLe (<=) extra_free)
	; let (new_vsp, new_free) = trim (virtSp stk_usg) all_free
	; setStkUsage (stk_usg { virtSp = new_vsp, freeStk = new_free }) }

addFreeSlots :: [VirtualSpOffset] -> [VirtualSpOffset] -> [VirtualSpOffset]
-- Merge the two, assuming both are in increasing order
addFreeSlots cs [] = cs
addFreeSlots [] ns = ns
addFreeSlots (c:cs) (n:ns)
  | c < n     = c : addFreeSlots cs (n:ns)
  | otherwise = n : addFreeSlots (c:cs) ns

trim :: VirtualSpOffset -> [VirtualSpOffset] -> (VirtualSpOffset, [VirtualSpOffset])
-- Try to trim back the virtual stack pointer, where there is a
-- continuous bunch of free slots at the end of the free list
trim vsp [] = (vsp, [])
trim vsp (slot:slots)
  = case trim vsp slots of
      (vsp', []) 
	| vsp' < slot  -> pprTrace "trim: strange" (ppr vsp <+> ppr (slot:slots))
			  (vsp',   [])
	| vsp' == slot -> (vsp'-1, [])
	| otherwise    -> (vsp',   [slot])
      (vsp', slots')   -> (vsp',   slot:slots')
\end{code}

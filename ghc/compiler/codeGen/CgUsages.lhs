%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgUsages]{Accessing and modifying stacks and heap usage info}

This module provides the functions to access (\tr{get*} functions) and
modify (\tr{set*} functions) the stacks and heap usage information.

\begin{code}
module CgUsages (
	initHeapUsage, setVirtHp, getVirtAndRealHp, setRealHp,
	setRealAndVirtualSp,

	getVirtSp, getRealSp,

	getHpRelOffset,	getSpRelOffset,

	adjustSpAndHp
    ) where

#include "HsVersions.h"

import AbsCSyn
import PrimRep		( PrimRep(..) )
import AbsCUtils	( mkAbstractCs )
import CgMonad
\end{code}

%************************************************************************
%*									*
\subsection[CgUsages-heapery]{Monad things for fiddling with heap usage}
%*									*
%************************************************************************

@initHeapUsage@ applies a function to the amount of heap that it uses.
It initialises the heap usage to zeros, and passes on an unchanged
heap usage.

It is usually a prelude to performing a GC check, so everything must
be in a tidy and consistent state.

rje: Note the slightly suble fixed point behaviour needed here
\begin{code}
initHeapUsage :: (VirtualHeapOffset -> Code) -> Code

initHeapUsage fcode = do 
	(stk_usage, heap_usage) <- getUsage
	setUsage (stk_usage, (0,0))
	fixC (\heap_usage2 -> do
		fcode (heapHWM heap_usage2)
		(_, heap_usage2) <- getUsage
		return heap_usage2)
	(stk_usage2, heap_usage2) <- getUsage
	setUsage (stk_usage2, heap_usage {-unchanged -})
\end{code}

\begin{code}
setVirtHp :: VirtualHeapOffset -> Code
setVirtHp new_virtHp = do
	(stk, (virtHp, realHp)) <- getUsage
	setUsage (stk, (new_virtHp, realHp))
\end{code}

\begin{code}
getVirtAndRealHp :: FCode (VirtualHeapOffset, VirtualHeapOffset)
getVirtAndRealHp = do 
	(_, (virtHp, realHp)) <- getUsage
	return (virtHp, realHp)
\end{code}

\begin{code}
setRealHp ::  VirtualHeapOffset -> Code
setRealHp realHp = do
	(stk_usage, (vHp, _)) <- getUsage
	setUsage (stk_usage, (vHp, realHp))
\end{code}

\begin{code}
getHpRelOffset :: VirtualHeapOffset -> FCode RegRelative
getHpRelOffset virtual_offset = do
	(_,(_,realHp)) <- getUsage
	return $ hpRel realHp virtual_offset
\end{code}

The heap high water mark is the larger of virtHp and hwHp.  The latter is
only records the high water marks of forked-off branches, so to find the
heap high water mark you have to take the max of virtHp and hwHp.  Remember,
virtHp never retreats!

\begin{code}
heapHWM (virtHp, realHp) = virtHp
\end{code}

%************************************************************************
%*									*
\subsection[CgUsages-stackery]{Monad things for fiddling with stack usage}
%*									*
%************************************************************************

@setRealAndVirtualSp@ sets into the environment the offsets of the
current position of the real and virtual stack pointers in the current
stack frame.  The high-water mark is set too.  It generates no code.
It is used to initialise things at the beginning of a closure body.

\begin{code}
setRealAndVirtualSp :: VirtualSpOffset 	-- New real Sp
		     -> Code

setRealAndVirtualSp sp = do
	((vsp,frame,f,realSp,hwsp), h_usage) <- getUsage
	let new_usage = ((sp, frame, f, sp, sp), h_usage)
	setUsage new_usage
\end{code}

\begin{code}
getVirtSp :: FCode VirtualSpOffset
getVirtSp = do 
	((virtSp,_,_,_,_), _) <- getUsage
	return virtSp

getRealSp :: FCode VirtualSpOffset
getRealSp = do
	((_,_,_,realSp,_),_) <- getUsage
	return realSp
\end{code}

\begin{code}
getSpRelOffset :: VirtualSpOffset -> FCode RegRelative
getSpRelOffset virtual_offset = do
	((_,_,_,realSp,_),_) <- getUsage
  	return $ spRel realSp virtual_offset
\end{code}

%************************************************************************
%*									*
\subsection[CgStackery-adjust]{Adjusting the stack pointers}
%*									*
%************************************************************************

This function adjusts the stack and heap pointers just before a tail
call or return.  The stack pointer is adjusted to its final position
(i.e. to point to the last argument for a tail call, or the activation
record for a return).  The heap pointer may be moved backwards, in
cases where we overallocated at the beginning of the basic block (see
CgCase.lhs for discussion).

These functions {\em do not} deal with high-water-mark adjustment.
That's done by functions which allocate stack space.

\begin{code}
adjustSpAndHp :: VirtualSpOffset 	-- New offset for Arg stack ptr
	      -> Code
adjustSpAndHp newRealSp = do
	(MkCgInfoDown _ _ _ ticky_ctr _) <- getInfoDown
	(MkCgState absC binds
		   ((vSp,frame,fSp,realSp,hwSp),	
		   (vHp, rHp))) <- getState
	let move_sp = if (newRealSp == realSp) then AbsCNop
	      else (CAssign (CReg Sp)
			    (CAddr (spRel realSp newRealSp)))
	let move_hp = 
		if (rHp == vHp) then AbsCNop
	 	else mkAbstractCs [
		CAssign (CReg Hp) (CAddr (hpRel rHp vHp)),
			profCtrAbsC FSLIT("TICK_ALLOC_HEAP") 
			[ mkIntCLit (vHp - rHp), CLbl ticky_ctr DataPtrRep ]
		]
	let new_usage = ((vSp, frame, fSp, newRealSp, hwSp), (vHp,vHp))
	setState $ MkCgState (mkAbstractCs [absC,move_sp,move_hp]) binds new_usage
\end{code}

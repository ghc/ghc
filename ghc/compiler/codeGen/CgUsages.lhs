%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CgUsages]{Accessing and modifying stacks and heap usage info}

This module provides the functions to access (\tr{get*} functions) and
modify (\tr{set*} functions) the stacks and heap usage information.

\begin{code}
module CgUsages (
	initHeapUsage, setVirtHp, getVirtAndRealHp, setRealHp,
	setRealAndVirtualSps, 

	getVirtSps,

	getHpRelOffset,	getSpARelOffset, getSpBRelOffset,
--UNUSED: getVirtSpRelOffsets,

	freeBStkSlot,

	-- and to make the interface self-sufficient...
	AbstractC, HeapOffset, RegRelative, CgState
    ) where

import AbsCSyn
import CgMonad
import Util
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

\begin{code}
initHeapUsage :: (VirtualHeapOffset -> Code) -> Code

initHeapUsage fcode info_down (MkCgState absC binds (a_usage, b_usage, heap_usage))
  = state3
  where
    state1 = MkCgState absC binds (a_usage, b_usage, (zeroOff, zeroOff))
    state2 = fcode (heapHWM heap_usage2) info_down state1
    (MkCgState absC2 binds2 (a_usage2, b_usage2, heap_usage2)) = state2
    state3 = MkCgState  absC2
			binds2
			(a_usage2, b_usage2, heap_usage {- unchanged -})
\end{code}

\begin{code}
setVirtHp :: VirtualHeapOffset -> Code
setVirtHp new_virtHp info_down
	  state@(MkCgState absC binds (a_stk, b_stk, (virtHp, realHp)))
  = MkCgState absC binds (a_stk, b_stk, (new_virtHp, realHp))
\end{code}

\begin{code}
getVirtAndRealHp :: FCode (VirtualHeapOffset, VirtualHeapOffset)
getVirtAndRealHp info_down state@(MkCgState _ _ (au, bu, (virtHp, realHp)))
  = ((virtHp, realHp), state)
\end{code}

\begin{code}
setRealHp ::  VirtualHeapOffset -> Code
setRealHp realHp info_down (MkCgState absC binds (au, bu, (vHp, _)))
  = MkCgState absC binds (au, bu, (vHp, realHp))
\end{code}

\begin{code}
getHpRelOffset :: VirtualHeapOffset -> FCode RegRelative
getHpRelOffset virtual_offset info_down state@(MkCgState _ _ (_,_,(_,realHp)))
  = (HpRel realHp virtual_offset, state)
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

@setRealAndVirtualSps@ sets into the environment the offsets of the
current position of the real and virtual stack pointers in the current
stack frame.  The high-water mark is set too.  It generates no code.
It is used to initialise things at the beginning of a closure body.

\begin{code}
setRealAndVirtualSps :: VirtualSpAOffset 	-- New real SpA
	   	     -> VirtualSpBOffset 	-- Ditto B stack
		     -> Code

setRealAndVirtualSps spA spB info_down (MkCgState absC binds
					((vspA,fA,realSpA,hwspA),
				 	 (vspB,fB,realSpB,hwspB),
				 	 h_usage))
  = MkCgState absC binds new_usage
  where
    new_usage = ((spA, fA, spA, spA),
		 (spB, fB, spB, spB),
		 h_usage)
\end{code}

\begin{code}
getVirtSps :: FCode (VirtualSpAOffset,VirtualSpBOffset)
getVirtSps info_down state@(MkCgState absC binds ((virtSpA,_,_,_), (virtSpB,_,_,_), _))
  = ((virtSpA,virtSpB), state)
\end{code}

\begin{code}
getSpARelOffset :: VirtualSpAOffset -> FCode RegRelative
getSpARelOffset virtual_offset info_down state@(MkCgState _ _ ((_,_,realSpA,_),_,_))
  = (SpARel realSpA virtual_offset, state)

getSpBRelOffset :: VirtualSpBOffset -> FCode RegRelative
getSpBRelOffset virtual_offset info_down state@(MkCgState _ _ (_,(_,_,realSpB,_),_))
  = (SpBRel realSpB virtual_offset, state)
\end{code}


\begin{code}
{- UNUSED:
getVirtSpRelOffsets :: FCode (RegRelative, RegRelative)
getVirtSpRelOffsets info_down
	state@(MkCgState absC binds ((virtSpA,_,realSpA,_), (virtSpB,_,realSpB,_), _))
  = ((SpARel realSpA virtSpA, SpBRel realSpB virtSpB), state)
-}
\end{code}

\begin{code}
freeBStkSlot :: VirtualSpBOffset -> Code
freeBStkSlot b_slot info_down
	state@(MkCgState absC binds (spa_usage, (virtSpB,free_b,realSpB,hwSpB), heap_usage))
 = MkCgState absC binds (spa_usage, (virtSpB,new_free_b,realSpB,hwSpB), heap_usage)
 where
 new_free_b = addFreeBSlots free_b [b_slot]

\end{code}

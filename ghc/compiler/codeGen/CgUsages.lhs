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

	getHpRelOffset,	getSpRelOffset
    ) where

#include "HsVersions.h"

import AbsCSyn		( RegRelative(..), VirtualHeapOffset, VirtualSpOffset,
			  hpRel, spRel )
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

\begin{code}
initHeapUsage :: (VirtualHeapOffset -> Code) -> Code

initHeapUsage fcode info_down (MkCgState absC binds (stk_usage, heap_usage))
  = state3
  where
    state1 = MkCgState absC binds (stk_usage, (0, 0))
    state2 = fcode (heapHWM heap_usage2) info_down state1
    (MkCgState absC2 binds2 (stk_usage2, heap_usage2)) = state2
    state3 = MkCgState  absC2
			binds2
			(stk_usage2, heap_usage {- unchanged -})
\end{code}

\begin{code}
setVirtHp :: VirtualHeapOffset -> Code
setVirtHp new_virtHp info_down
	  state@(MkCgState absC binds (stk, (virtHp, realHp)))
  = MkCgState absC binds (stk, (new_virtHp, realHp))
\end{code}

\begin{code}
getVirtAndRealHp :: FCode (VirtualHeapOffset, VirtualHeapOffset)
getVirtAndRealHp info_down state@(MkCgState _ _ (_, (virtHp, realHp)))
  = ((virtHp, realHp), state)
\end{code}

\begin{code}
setRealHp ::  VirtualHeapOffset -> Code
setRealHp realHp info_down (MkCgState absC binds (stk_usage, (vHp, _)))
  = MkCgState absC binds (stk_usage, (vHp, realHp))
\end{code}

\begin{code}
getHpRelOffset :: VirtualHeapOffset -> FCode RegRelative
getHpRelOffset virtual_offset info_down state@(MkCgState _ _ (_,(_,realHp)))
  = (hpRel realHp virtual_offset, state)
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

setRealAndVirtualSp sp info_down (MkCgState absC binds
					((vsp,f,realSp,hwsp), h_usage))
  = MkCgState absC binds new_usage
  where
    new_usage = ((sp, f, sp, sp), h_usage)
\end{code}

\begin{code}
getVirtSp :: FCode VirtualSpOffset
getVirtSp info_down state@(MkCgState absC binds ((virtSp,_,_,_), _))
  = (virtSp, state)

getRealSp :: FCode VirtualSpOffset
getRealSp info_down state@(MkCgState absC binds ((_,_,realSp,_),_)) 
  = (realSp,state)
\end{code}

\begin{code}
getSpRelOffset :: VirtualSpOffset -> FCode RegRelative
getSpRelOffset virtual_offset info_down state@(MkCgState _ _ ((_,_,realSp,_),_))
  = (spRel realSp virtual_offset, state)
\end{code}

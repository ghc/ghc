%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
\section[PreludeGlaMisc]{Miscellaneous Glasgow Stuff}

\begin{code}
module PreludeGlaMisc( PreludeGlaMisc.. {-, PreludePS..-} ) where

import Cls
import Core
import IInt
import List		( (++) )
import PreludeGlaST
import PS		( _PackedString, _unpackPS )
import TyArray		( Array(..) )
import Text
\end{code}

Note: the above used to say:

\begin{pseudocode}
module PreludeGlaMisc( 
	_MallocPtr,

#ifndef __PARALLEL_HASKELL__
	_StablePtr,
	makeStablePtr, deRefStablePtr, freeStablePtr,

	performGC
#endif /* !__PARALLEL_HASKELL__ */

	) where
\end{pseudocode}

But then the names @_MallocPtr@ and @_StablePtr@ get shoved out into
the interface file and anyone importing it becomes unhappy about
seeing a preludish name.

They report: 

@
Bad name on a datatype constructor (a Prelude name?): _MallocPtr
@

(This is horrid!)

(Oh, btw, don't try not exporting them either - that just makes the
info-tables, etc local to this module so that no-one can get at them.)





The next two definitions must match those in
@compiler/prelude/TysWiredIn.lhs@ exactly.

\begin{code}
#ifndef __PARALLEL_HASKELL__

-- ** MOVED TO prelude/TysBasic.hs **
-- data _MallocPtr = _MallocPtr MallocPtr#
-- data _StablePtr a = _StablePtr (StablePtr# a)

\end{code}

Nota Bene: it is important {\em not\/} to inline calls to
@makeStablePtr#@ since the corresponding macro is very long and we'll
get terrible code-bloat.

\begin{code}
makeStablePtr :: a -> PrimIO (_StablePtr a)
deRefStablePtr :: _StablePtr a -> PrimIO a
freeStablePtr :: _StablePtr a -> PrimIO ()

eqMallocPtr :: _MallocPtr -> _MallocPtr -> Bool

performGC :: PrimIO ()

{-# INLINE deRefStablePtr #-}
{-# INLINE freeStablePtr #-}
{-# INLINE performGC #-}

makeStablePtr f (S# rw1#) = 
	case makeStablePtr# f rw1# of
	  StateAndStablePtr# rw2# sp# -> (_StablePtr sp#, S# rw2#)

deRefStablePtr (_StablePtr sp#) (S# rw1#) =
	case deRefStablePtr# sp# rw1# of
	  StateAndPtr# rw2# a -> (a, S# rw2#)

freeStablePtr sp = _ccall_ freeStablePointer sp

eqMallocPtr mp1 mp2 = unsafePerformPrimIO (
	_ccall_ eqMallocPtr mp1 mp2
	)
	/= (0::Int)

instance Eq _MallocPtr where 
	p == q = eqMallocPtr p q
	p /= q = if eqMallocPtr p q then False else True

performGC = _ccall_GC_ StgPerformGarbageCollection

#endif /* !__PARALLEL_HASKELL__ */
\end{code}

Like they say: this is as good a place as any to put it:

\begin{code}
addr2Int :: _Addr -> Int
addr2Int (A# a#) = I# (addr2Int# a#)

int2Addr :: Int -> _Addr
int2Addr (I# i#) = A# (int2Addr# i#)
\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[TysPrim]{Wired-in knowledge about primitive types}

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
#include "HsVersions.h"

module TysPrim where

import PrelFuns		-- help functions, types and things
import PrimKind

import AbsUniType	( applyTyCon )
import Unique
import Util
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
%*									*
%************************************************************************

\begin{code}
charPrimTy	= applyTyCon charPrimTyCon []
charPrimTyCon	= pcPrimTyCon charPrimTyConKey SLIT("Char#") 0 (\ [] -> CharKind)

intPrimTy	= applyTyCon intPrimTyCon []
intPrimTyCon	= pcPrimTyCon intPrimTyConKey SLIT("Int#") 0 (\ [] -> IntKind)

wordPrimTy	= applyTyCon wordPrimTyCon []
wordPrimTyCon	= pcPrimTyCon wordPrimTyConKey SLIT("Word#") 0 (\ [] -> WordKind)

addrPrimTy	= applyTyCon addrPrimTyCon []
addrPrimTyCon	= pcPrimTyCon addrPrimTyConKey SLIT("Addr#") 0 (\ [] -> AddrKind)

floatPrimTy	= applyTyCon floatPrimTyCon []
floatPrimTyCon	= pcPrimTyCon floatPrimTyConKey SLIT("Float#") 0 (\ [] -> FloatKind)

doublePrimTy	= applyTyCon doublePrimTyCon []
doublePrimTyCon	= pcPrimTyCon doublePrimTyConKey SLIT("Double#") 0 (\ [] -> DoubleKind)
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-void]{The @Void#@ type}
%*									*
%************************************************************************

Very similar to the @State#@ type.
\begin{code}
voidPrimTy = applyTyCon voidPrimTyCon []
  where
   voidPrimTyCon = pcPrimTyCon voidPrimTyConKey SLIT("Void#") 0
			(\ [] -> VoidKind)
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
%*									*
%************************************************************************

\begin{code}
mkStatePrimTy ty = applyTyCon statePrimTyCon [ty]
statePrimTyCon	 = pcPrimTyCon statePrimTyConKey SLIT("State#") 1
			(\ [s_kind] -> VoidKind)
\end{code}

@_RealWorld@ is deeply magical.  It {\em is primitive}, but it
{\em is not unboxed}.
\begin{code}
realWorldTy	  = applyTyCon realWorldTyCon []
realWorldTyCon
  = pcDataTyCon realWorldTyConKey pRELUDE_BUILTIN SLIT("_RealWorld") []
	[{-no data cons!-}] -- we tell you *nothing* about this guy

realWorldStatePrimTy = mkStatePrimTy realWorldTy
\end{code}

Note: the ``state-pairing'' types are not truly primitive, so they are
defined in \tr{TysWiredIn.lhs}, not here.

%************************************************************************
%*									*
\subsection[TysPrim-arrays]{The primitive array types}
%*									*
%************************************************************************

\begin{code}
arrayPrimTyCon	= pcPrimTyCon arrayPrimTyConKey SLIT("Array#") 1
			(\ [elt_kind] -> ArrayKind)

byteArrayPrimTyCon = pcPrimTyCon byteArrayPrimTyConKey SLIT("ByteArray#") 0
			(\ [] -> ByteArrayKind)

mutableArrayPrimTyCon = pcPrimTyCon mutableArrayPrimTyConKey SLIT("MutableArray#") 2
			(\ [s_kind, elt_kind] -> ArrayKind)

mutableByteArrayPrimTyCon = pcPrimTyCon mutableByteArrayPrimTyConKey SLIT("MutableByteArray#") 1
			(\ [s_kind] -> ByteArrayKind)

mkArrayPrimTy elt    	    = applyTyCon arrayPrimTyCon [elt]
byteArrayPrimTy	    	    = applyTyCon byteArrayPrimTyCon []
mkMutableArrayPrimTy s elt  = applyTyCon mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy s  = applyTyCon mutableByteArrayPrimTyCon [s]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-synch-var]{The synchronizing variable type}
%*									*
%************************************************************************

\begin{code}
synchVarPrimTyCon = pcPrimTyCon synchVarPrimTyConKey SLIT("SynchVar#") 2
			(\ [s_kind, elt_kind] -> PtrKind)

mkSynchVarPrimTy s elt 	    = applyTyCon synchVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
%*									*
%************************************************************************

\begin{code}
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConKey SLIT("StablePtr#") 1
			(\ [elt_kind] -> StablePtrKind)

mkStablePtrPrimTy ty = applyTyCon stablePtrPrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-malloc-ptrs]{The ``malloc''-pointer type}
%*									*
%************************************************************************

``Malloc'' pointers provide a mechanism which will let Haskell's
garbage collector communicate with a {\em simple\/} garbage collector
in the IO world (probably \tr{malloc}, hence the name).We want Haskell
to be able to hold onto references to objects in the IO world and for
Haskell's garbage collector to tell the IO world when these references
become garbage.  We are not aiming to provide a mechanism that could
talk to a sophisticated garbage collector such as that provided by a
LISP system (with a correspondingly complex interface); in particular,
we shall ignore the danger of circular structures spread across the
two systems.

There are no primitive operations on @CHeapPtr#@s (although equality
could possibly be added?)

\begin{code}
mallocPtrPrimTyCon = pcPrimTyCon mallocPtrPrimTyConKey SLIT("MallocPtr#") 0
			(\ [] -> MallocPtrKind)
\end{code}

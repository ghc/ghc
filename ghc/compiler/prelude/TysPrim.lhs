%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[TysPrim]{Wired-in knowledge about primitive types}

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
#include "HsVersions.h"

module TysPrim where

import Ubiq

import Kind		( mkUnboxedTypeKind, mkBoxedTypeKind )
import NameTypes	( mkPreludeCoreName, FullName )
import PrelMods		( pRELUDE_BUILTIN )
import PrimRep		( PrimRep(..) )	-- getPrimRepInfo uses PrimRep repn
import TyCon		( mkPrimTyCon, mkDataTyCon,
			  ConsVisible(..), NewOrData(..) )
import TyVar		( GenTyVar(..), alphaTyVars )
import Type		( applyTyCon, mkTyVarTys )
import Usage		( usageOmega )
import Unique

\end{code}

\begin{code}
alphaTys = mkTyVarTys alphaTyVars
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
%*									*
%************************************************************************

\begin{code}
-- only used herein
pcPrimTyCon :: Unique{-TyConKey-} -> FAST_STRING -> Int -> ([PrimRep] -> PrimRep) -> TyCon
pcPrimTyCon key name arity{-UNUSED-} kind_fn{-UNUSED-}
  = mkPrimTyCon key full_name mkUnboxedTypeKind
  where
    full_name = mkPreludeCoreName pRELUDE_BUILTIN name


charPrimTy	= applyTyCon charPrimTyCon []
charPrimTyCon	= pcPrimTyCon charPrimTyConKey SLIT("Char#") 0 (\ [] -> CharRep)

intPrimTy	= applyTyCon intPrimTyCon []
intPrimTyCon	= pcPrimTyCon intPrimTyConKey SLIT("Int#") 0 (\ [] -> IntRep)

wordPrimTy	= applyTyCon wordPrimTyCon []
wordPrimTyCon	= pcPrimTyCon wordPrimTyConKey SLIT("Word#") 0 (\ [] -> WordRep)

addrPrimTy	= applyTyCon addrPrimTyCon []
addrPrimTyCon	= pcPrimTyCon addrPrimTyConKey SLIT("Addr#") 0 (\ [] -> AddrRep)

floatPrimTy	= applyTyCon floatPrimTyCon []
floatPrimTyCon	= pcPrimTyCon floatPrimTyConKey SLIT("Float#") 0 (\ [] -> FloatRep)

doublePrimTy	= applyTyCon doublePrimTyCon []
doublePrimTyCon	= pcPrimTyCon doublePrimTyConKey SLIT("Double#") 0 (\ [] -> DoubleRep)
\end{code}

@PrimitiveKinds@ are used in @PrimitiveOps@, for which we often need
to reconstruct various type information.  (It's slightly more
convenient/efficient to make type info from kinds, than kinds [etc.]
from type info.)

\begin{code}
getPrimRepInfo ::
    PrimRep -> (String,		-- tag string
		Type, TyCon)	-- prim type and tycon

getPrimRepInfo CharRep   = ("Char",   charPrimTy,   charPrimTyCon)
getPrimRepInfo IntRep    = ("Int",    intPrimTy,    intPrimTyCon)
getPrimRepInfo WordRep   = ("Word",   wordPrimTy,   wordPrimTyCon)
getPrimRepInfo AddrRep   = ("Addr",   addrPrimTy,   addrPrimTyCon)
getPrimRepInfo FloatRep  = ("Float",  floatPrimTy,  floatPrimTyCon)
getPrimRepInfo DoubleRep = ("Double", doublePrimTy, doublePrimTyCon)
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
			(\ [] -> VoidRep)
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
%*									*
%************************************************************************

\begin{code}
mkStatePrimTy ty = applyTyCon statePrimTyCon [ty]
statePrimTyCon	 = pcPrimTyCon statePrimTyConKey SLIT("State#") 1
			(\ [s_kind] -> VoidRep)
\end{code}

@_RealWorld@ is deeply magical.  It {\em is primitive}, but it
{\em is not unboxed}.
\begin{code}
realWorldTy = applyTyCon realWorldTyCon []
realWorldTyCon
  = mkDataTyCon realWorldTyConKey mkBoxedTypeKind full_name
	[{-no tyvars-}]
	[{-no context-}]
	[{-no data cons!-}] -- we tell you *nothing* about this guy
	[{-no derivings-}]
	ConsInvisible
	DataType
  where
    full_name = mkPreludeCoreName pRELUDE_BUILTIN SLIT("_RealWorld")

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
			(\ [elt_kind] -> ArrayRep)

byteArrayPrimTyCon = pcPrimTyCon byteArrayPrimTyConKey SLIT("ByteArray#") 0
			(\ [] -> ByteArrayRep)

mutableArrayPrimTyCon = pcPrimTyCon mutableArrayPrimTyConKey SLIT("MutableArray#") 2
			(\ [s_kind, elt_kind] -> ArrayRep)

mutableByteArrayPrimTyCon = pcPrimTyCon mutableByteArrayPrimTyConKey SLIT("MutableByteArray#") 1
			(\ [s_kind] -> ByteArrayRep)

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
			(\ [s_kind, elt_kind] -> PtrRep)

mkSynchVarPrimTy s elt 	    = applyTyCon synchVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
%*									*
%************************************************************************

\begin{code}
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConKey SLIT("StablePtr#") 1
			(\ [elt_kind] -> StablePtrRep)

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
			(\ [] -> MallocPtrRep)
\end{code}

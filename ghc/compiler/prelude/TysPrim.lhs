%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[TysPrim]{Wired-in knowledge about primitive types}

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
module TysPrim(
	alphaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
	alphaTy, betaTy, gammaTy, deltaTy,
	openAlphaTyVar, openAlphaTyVars,

	charPrimTyCon, 		charPrimTy,
	intPrimTyCon,		intPrimTy,
	wordPrimTyCon,		wordPrimTy,
	addrPrimTyCon,		addrPrimTy,
	floatPrimTyCon,		floatPrimTy,
	doublePrimTyCon,	doublePrimTy,

	statePrimTyCon,		mkStatePrimTy,
	realWorldTyCon,		realWorldTy, realWorldStatePrimTy,

	arrayPrimTyCon,			mkArrayPrimTy, 
	byteArrayPrimTyCon,		byteArrayPrimTy,
	mutableArrayPrimTyCon,		mkMutableArrayPrimTy,
	mutableByteArrayPrimTyCon,	mkMutableByteArrayPrimTy,
	mutVarPrimTyCon,		mkMutVarPrimTy,

	mVarPrimTyCon,			mkMVarPrimTy,	
	stablePtrPrimTyCon,		mkStablePtrPrimTy,
	weakPrimTyCon,  		mkWeakPrimTy,
	foreignObjPrimTyCon,		foreignObjPrimTy,
	threadIdPrimTyCon,		threadIdPrimTy,
	
	int64PrimTyCon,		int64PrimTy,
	word64PrimTyCon,	word64PrimTy,

	primRepTyCon,

	pcPrimTyCon
  ) where

#include "HsVersions.h"

import Var		( TyVar, mkSysTyVar )
import Name		( mkWiredInTyConName )
import PrimRep		( PrimRep(..), isFollowableRep )
import TyCon		( mkPrimTyCon, TyCon )
import Type		( Type, 
			  mkTyConApp, mkTyConTy, mkTyVarTys,
			  unboxedTypeKind, boxedTypeKind, openTypeKind, mkArrowKinds
			)
import PrelMods		( pREL_GHC )
import Outputable
import Unique
\end{code}

\begin{code}
alphaTyVars :: [TyVar]
alphaTyVars = [ mkSysTyVar u boxedTypeKind
	      | u <- map mkAlphaTyVarUnique [2..] ]

alphaTyVar, betaTyVar, gammaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys = mkTyVarTys alphaTyVars
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

	-- openAlphaTyVar is prepared to be instantiated
	-- to a boxed or unboxed type variable.  It's used for the 
	-- result type for "error", so that we can have (error Int# "Help")
openAlphaTyVar :: TyVar
openAlphaTyVar = mkSysTyVar (mkAlphaTyVarUnique 1) openTypeKind

openAlphaTyVars :: [TyVar]
openAlphaTyVars = [ mkSysTyVar u openTypeKind
		  | u <- map mkAlphaTyVarUnique [2..] ]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
%*									*
%************************************************************************

\begin{code}
-- only used herein
pcPrimTyCon :: Unique{-TyConKey-} -> FAST_STRING -> Int -> PrimRep -> TyCon
pcPrimTyCon key str arity rep
  = the_tycon
  where
    name      = mkWiredInTyConName key pREL_GHC str the_tycon
    the_tycon = mkPrimTyCon name kind arity rep
    kind      = mkArrowKinds (take arity (repeat boxedTypeKind)) result_kind
    result_kind | isFollowableRep rep = boxedTypeKind	-- Represented by a GC-ish ptr
	        | otherwise	      = unboxedTypeKind	-- Represented by a non-ptr

charPrimTy	= mkTyConTy charPrimTyCon
charPrimTyCon	= pcPrimTyCon charPrimTyConKey SLIT("Char#") 0 CharRep

intPrimTy	= mkTyConTy intPrimTyCon
intPrimTyCon	= pcPrimTyCon intPrimTyConKey SLIT("Int#") 0 IntRep

int64PrimTy	= mkTyConTy int64PrimTyCon
int64PrimTyCon	= pcPrimTyCon int64PrimTyConKey SLIT("Int64#") 0 Int64Rep

wordPrimTy	= mkTyConTy wordPrimTyCon
wordPrimTyCon	= pcPrimTyCon wordPrimTyConKey SLIT("Word#") 0 WordRep

word64PrimTy	= mkTyConTy word64PrimTyCon
word64PrimTyCon	= pcPrimTyCon word64PrimTyConKey SLIT("Word64#") 0 Word64Rep

addrPrimTy	= mkTyConTy addrPrimTyCon
addrPrimTyCon	= pcPrimTyCon addrPrimTyConKey SLIT("Addr#") 0 AddrRep

floatPrimTy	= mkTyConTy floatPrimTyCon
floatPrimTyCon	= pcPrimTyCon floatPrimTyConKey SLIT("Float#") 0 FloatRep

doublePrimTy	= mkTyConTy doublePrimTyCon
doublePrimTyCon	= pcPrimTyCon doublePrimTyConKey SLIT("Double#") 0 DoubleRep
\end{code}


%************************************************************************
%*									*
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
%*									*
%************************************************************************

State# is the primitive, unboxed type of states.  It has one type parameter,
thus
	State# RealWorld
or
	State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

\begin{code}
mkStatePrimTy ty = mkTyConApp statePrimTyCon [ty]
statePrimTyCon	 = pcPrimTyCon statePrimTyConKey SLIT("State#") 1 VoidRep
\end{code}

@_RealWorld@ is deeply magical.  It {\em is primitive}, but it
{\em is not unboxed} (hence PtrRep).
We never manipulate values of type RealWorld; it's only used in the type
system, to parameterise State#.

\begin{code}
realWorldTy	     = mkTyConTy realWorldTyCon
realWorldTyCon	     = pcPrimTyCon realWorldTyConKey SLIT("RealWorld") 0 PtrRep
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
arrayPrimTyCon	= pcPrimTyCon arrayPrimTyConKey SLIT("Array#") 1 ArrayRep

byteArrayPrimTyCon = pcPrimTyCon byteArrayPrimTyConKey SLIT("ByteArray#") 0 ByteArrayRep

mutableArrayPrimTyCon = pcPrimTyCon mutableArrayPrimTyConKey SLIT("MutableArray#") 2 ArrayRep

mutableByteArrayPrimTyCon = pcPrimTyCon mutableByteArrayPrimTyConKey SLIT("MutableByteArray#") 1 ByteArrayRep

mkArrayPrimTy elt    	    = mkTyConApp arrayPrimTyCon [elt]
byteArrayPrimTy	    	    = mkTyConTy byteArrayPrimTyCon
mkMutableArrayPrimTy s elt  = mkTyConApp mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy s  = mkTyConApp mutableByteArrayPrimTyCon [s]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-mut-var]{The mutable variable type}
%*									*
%************************************************************************

\begin{code}
mutVarPrimTyCon = pcPrimTyCon mutVarPrimTyConKey SLIT("MutVar#") 2 PtrRep

mkMutVarPrimTy s elt 	    = mkTyConApp mutVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-synch-var]{The synchronizing variable type}
%*									*
%************************************************************************

\begin{code}
mVarPrimTyCon = pcPrimTyCon mVarPrimTyConKey SLIT("MVar#") 2 PtrRep

mkMVarPrimTy s elt 	    = mkTyConApp mVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
%*									*
%************************************************************************

\begin{code}
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConKey SLIT("StablePtr#") 1 StablePtrRep

mkStablePtrPrimTy ty = mkTyConApp stablePtrPrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-foreign-objs]{The ``foreign object'' type}
%*									*
%************************************************************************

A Foreign Object is just a boxed, unlifted, Addr#.  They're needed
because finalisers (weak pointers) can't watch Addr#s, they can only
watch heap-resident objects.  

We can't use a lifted Addr# (such as Addr) because race conditions
could bite us.  For example, if the program deconstructed the Addr
before passing its contents to a ccall, and a weak pointer was
watching the Addr, the weak pointer might deduce that the Addr was
dead before it really was.

\begin{code}
foreignObjPrimTy    = mkTyConTy foreignObjPrimTyCon
foreignObjPrimTyCon = pcPrimTyCon foreignObjPrimTyConKey SLIT("ForeignObj#") 0 ForeignObjRep
\end{code}
  
%************************************************************************
%*									*
\subsection[TysPrim-Weak]{The ``weak pointer'' type}
%*									*
%************************************************************************

\begin{code}
weakPrimTyCon = pcPrimTyCon weakPrimTyConKey SLIT("Weak#") 1 WeakPtrRep

mkWeakPrimTy v = mkTyConApp weakPrimTyCon [v]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-thread-ids]{The ``thread id'' type}
%*									*
%************************************************************************

A thread id is represented by a pointer to the TSO itself, to ensure
that they are always unique and we can always find the TSO for a given
thread id.  However, this has the unfortunate consequence that a
ThreadId# for a given thread is treated as a root by the garbage
collector and can keep TSOs around for too long.

Hence the programmer API for thread manipulation uses a weak pointer
to the thread id internally.

\begin{code}
threadIdPrimTy    = mkTyConTy threadIdPrimTyCon
threadIdPrimTyCon = pcPrimTyCon threadIdPrimTyConKey SLIT("ThreadId#") 0 ThreadIdRep
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-PrimRep]{Making types from PrimReps}
%*									*
%************************************************************************

Each of the primitive types from this module is equivalent to a
PrimRep (see PrimRep.lhs).  The following function returns the
primitive TyCon for a given PrimRep.

\begin{code}
primRepTyCon CharRep   = charPrimTyCon
primRepTyCon IntRep    = intPrimTyCon
primRepTyCon WordRep   = wordPrimTyCon
primRepTyCon Int64Rep  = int64PrimTyCon
primRepTyCon Word64Rep = word64PrimTyCon
primRepTyCon AddrRep   = addrPrimTyCon
primRepTyCon FloatRep  = floatPrimTyCon
primRepTyCon DoubleRep = doublePrimTyCon
primRepTyCon StablePtrRep  = stablePtrPrimTyCon
primRepTyCon ForeignObjRep = foreignObjPrimTyCon
primRepTyCon WeakPtrRep = weakPrimTyCon
primRepTyCon other     = pprPanic "primRepTyCon" (ppr other)
\end{code}

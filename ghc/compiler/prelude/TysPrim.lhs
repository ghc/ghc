%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[TysPrim]{Wired-in knowledge about primitive types}

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
module TysPrim(
	alphaTyVars, betaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
	alphaTy, betaTy, gammaTy, deltaTy,
	openAlphaTy, openAlphaTyVar, openAlphaTyVars,

	primTyCons,

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
	stableNamePrimTyCon,		mkStableNamePrimTy,
	bcoPrimTyCon,			bcoPrimTy,
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
import Name		( Name )
import PrimRep		( PrimRep(..), isFollowableRep )
import TyCon		( TyCon, ArgVrcs, mkPrimTyCon )
import Type		( mkTyConApp, mkTyConTy, mkTyVarTys, mkTyVarTy,
			  unliftedTypeKind, liftedTypeKind, openTypeKind, mkArrowKinds
			)
import Unique		( mkAlphaTyVarUnique )
import PrelNames
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{Primitive type constructors}
%*									*
%************************************************************************

\begin{code}
primTyCons :: [TyCon]
primTyCons 
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int64PrimTyCon
    , foreignObjPrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , statePrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word64PrimTyCon
    ]
\end{code}


%************************************************************************
%*									*
\subsection{Support code}
%*									*
%************************************************************************

\begin{code}
alphaTyVars :: [TyVar]
alphaTyVars = [ mkSysTyVar u liftedTypeKind
	      | u <- map mkAlphaTyVarUnique [2..] ]

betaTyVars = tail alphaTyVars

alphaTyVar, betaTyVar, gammaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys = mkTyVarTys alphaTyVars
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

	-- openAlphaTyVar is prepared to be instantiated
	-- to a lifted or unlifted type variable.  It's used for the 
	-- result type for "error", so that we can have (error Int# "Help")
openAlphaTyVar :: TyVar
openAlphaTyVar = mkSysTyVar (mkAlphaTyVarUnique 1) openTypeKind

openAlphaTyVars :: [TyVar]
openAlphaTyVars = [ mkSysTyVar u openTypeKind
		  | u <- map mkAlphaTyVarUnique [2..] ]

openAlphaTy = mkTyVarTy openAlphaTyVar

vrcPos,vrcZero :: (Bool,Bool)
vrcPos  = (True,False)
vrcZero = (False,False)

vrcsP,vrcsZ,vrcsZP :: ArgVrcs
vrcsP  = [vrcPos]
vrcsZ  = [vrcZero]
vrcsZP = [vrcZero,vrcPos]
\end{code}


%************************************************************************
%*									*
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
%*									*
%************************************************************************

\begin{code}
-- only used herein
pcPrimTyCon :: Name -> Int -> ArgVrcs -> PrimRep -> TyCon
pcPrimTyCon name arity arg_vrcs rep
  = the_tycon
  where
    the_tycon   = mkPrimTyCon name kind arity arg_vrcs rep
    kind        = mkArrowKinds (take arity (repeat liftedTypeKind)) result_kind
    result_kind = unliftedTypeKind -- all primitive types are unlifted

charPrimTy	= mkTyConTy charPrimTyCon
charPrimTyCon	= pcPrimTyCon charPrimTyConName 0 [] CharRep

intPrimTy	= mkTyConTy intPrimTyCon
intPrimTyCon	= pcPrimTyCon intPrimTyConName 0 [] IntRep

int64PrimTy	= mkTyConTy int64PrimTyCon
int64PrimTyCon	= pcPrimTyCon int64PrimTyConName 0 [] Int64Rep

wordPrimTy	= mkTyConTy wordPrimTyCon
wordPrimTyCon	= pcPrimTyCon wordPrimTyConName 0 [] WordRep

word64PrimTy	= mkTyConTy word64PrimTyCon
word64PrimTyCon	= pcPrimTyCon word64PrimTyConName 0 [] Word64Rep

addrPrimTy	= mkTyConTy addrPrimTyCon
addrPrimTyCon	= pcPrimTyCon addrPrimTyConName 0 [] AddrRep

floatPrimTy	= mkTyConTy floatPrimTyCon
floatPrimTyCon	= pcPrimTyCon floatPrimTyConName 0 [] FloatRep

doublePrimTy	= mkTyConTy doublePrimTyCon
doublePrimTyCon	= pcPrimTyCon doublePrimTyConName 0 [] DoubleRep
\end{code}


%************************************************************************
%*									*
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
%*									*
%************************************************************************

State# is the primitive, unlifted type of states.  It has one type parameter,
thus
	State# RealWorld
or
	State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

\begin{code}
mkStatePrimTy ty = mkTyConApp statePrimTyCon [ty]
statePrimTyCon	 = pcPrimTyCon statePrimTyConName 1 vrcsZ VoidRep
\end{code}

RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence PrimPtrRep).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.

\begin{code}
realWorldTyCon = mkPrimTyCon realWorldTyConName liftedTypeKind 0 [] PrimPtrRep
realWorldTy	     = mkTyConTy realWorldTyCon
realWorldStatePrimTy = mkStatePrimTy realWorldTy	-- State# RealWorld
\end{code}

Note: the ``state-pairing'' types are not truly primitive, so they are
defined in \tr{TysWiredIn.lhs}, not here.


%************************************************************************
%*									*
\subsection[TysPrim-arrays]{The primitive array types}
%*									*
%************************************************************************

\begin{code}
arrayPrimTyCon		  = pcPrimTyCon arrayPrimTyConName	      1 vrcsP  ArrayRep
byteArrayPrimTyCon	  = pcPrimTyCon byteArrayPrimTyConName	      0 []     ByteArrayRep
mutableArrayPrimTyCon	  = pcPrimTyCon mutableArrayPrimTyConName     2 vrcsZP ArrayRep
mutableByteArrayPrimTyCon = pcPrimTyCon mutableByteArrayPrimTyConName 1 vrcsZ  ByteArrayRep

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
mutVarPrimTyCon = pcPrimTyCon mutVarPrimTyConName 2 vrcsZP PrimPtrRep

mkMutVarPrimTy s elt 	    = mkTyConApp mutVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-synch-var]{The synchronizing variable type}
%*									*
%************************************************************************

\begin{code}
mVarPrimTyCon = pcPrimTyCon mVarPrimTyConName 2 vrcsZP PrimPtrRep

mkMVarPrimTy s elt 	    = mkTyConApp mVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
%*									*
%************************************************************************

\begin{code}
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConName 1 vrcsP StablePtrRep

mkStablePtrPrimTy ty = mkTyConApp stablePtrPrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-names]{The stable-name type}
%*									*
%************************************************************************

\begin{code}
stableNamePrimTyCon = pcPrimTyCon stableNamePrimTyConName 1 vrcsP StableNameRep

mkStableNamePrimTy ty = mkTyConApp stableNamePrimTyCon [ty]
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
foreignObjPrimTyCon = pcPrimTyCon foreignObjPrimTyConName 0 [] ForeignObjRep
\end{code}
  
%************************************************************************
%*									*
\subsection[TysPrim-BCOs]{The ``bytecode object'' type}
%*									*
%************************************************************************

\begin{code}
bcoPrimTy    = mkTyConTy bcoPrimTyCon
bcoPrimTyCon = pcPrimTyCon bcoPrimTyConName 0 [] BCORep
\end{code}
  
%************************************************************************
%*									*
\subsection[TysPrim-Weak]{The ``weak pointer'' type}
%*									*
%************************************************************************

\begin{code}
weakPrimTyCon = pcPrimTyCon weakPrimTyConName 1 vrcsP WeakPtrRep

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
threadIdPrimTyCon = pcPrimTyCon threadIdPrimTyConName 0 [] ThreadIdRep
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
primRepTyCon CharRep       = charPrimTyCon
primRepTyCon Int8Rep       = charPrimTyCon
primRepTyCon IntRep        = intPrimTyCon
primRepTyCon WordRep       = wordPrimTyCon
primRepTyCon Int64Rep      = int64PrimTyCon
primRepTyCon Word64Rep     = word64PrimTyCon
primRepTyCon AddrRep       = addrPrimTyCon
primRepTyCon FloatRep      = floatPrimTyCon
primRepTyCon DoubleRep     = doublePrimTyCon
primRepTyCon StablePtrRep  = stablePtrPrimTyCon
primRepTyCon ForeignObjRep = foreignObjPrimTyCon
primRepTyCon WeakPtrRep    = weakPrimTyCon
primRepTyCon other         = pprPanic "primRepTyCon" (ppr other)
\end{code}

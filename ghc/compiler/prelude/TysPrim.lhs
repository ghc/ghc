%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[TysPrim]{Wired-in knowledge about primitive types}

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
#include "HsVersions.h"

module TysPrim where

IMP_Ubiq(){-uitous-}

import Kind		( mkUnboxedTypeKind, mkBoxedTypeKind, mkTypeKind, mkArrowKind )
import Name		( mkWiredInTyConName )
import PrimRep		( PrimRep(..) )	-- getPrimRepInfo uses PrimRep repn
import TyCon		( mkPrimTyCon, mkDataTyCon, TyCon )
import BasicTypes	( NewOrData(..) )
import Type		( applyTyCon, mkTyVarTys, mkTyConTy, SYN_IE(Type) )
import TyVar		( GenTyVar(..), alphaTyVars )
import Usage		( usageOmega )
import PrelMods		( gHC__ )
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
pcPrimTyCon :: Unique{-TyConKey-} -> FAST_STRING -> Int -> PrimRep -> TyCon

pcPrimTyCon key str arity primrep
  = the_tycon
  where
    name      = mkWiredInTyConName key gHC__ str the_tycon
    the_tycon = mkPrimTyCon name arity primrep


charPrimTy	= applyTyCon charPrimTyCon []
charPrimTyCon	= pcPrimTyCon charPrimTyConKey SLIT("Char#") 0 CharRep

intPrimTy	= applyTyCon intPrimTyCon []
intPrimTyCon	= pcPrimTyCon intPrimTyConKey SLIT("Int#") 0 IntRep

wordPrimTy	= applyTyCon wordPrimTyCon []
wordPrimTyCon	= pcPrimTyCon wordPrimTyConKey SLIT("Word#") 0 WordRep

addrPrimTy	= applyTyCon addrPrimTyCon []
addrPrimTyCon	= pcPrimTyCon addrPrimTyConKey SLIT("Addr#") 0 AddrRep

floatPrimTy	= applyTyCon floatPrimTyCon []
floatPrimTyCon	= pcPrimTyCon floatPrimTyConKey SLIT("Float#") 0 FloatRep

doublePrimTy	= applyTyCon doublePrimTyCon []
doublePrimTyCon	= pcPrimTyCon doublePrimTyConKey SLIT("Double#") 0 DoubleRep
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
mkStatePrimTy ty = applyTyCon statePrimTyCon [ty]
statePrimTyCon	 = pcPrimTyCon statePrimTyConKey SLIT("State#") 1 VoidRep
\end{code}

@_RealWorld@ is deeply magical.  It {\em is primitive}, but it
{\em is not unboxed}.
We never manipulate values of type RealWorld; it's only used in the type
system, to parameterise State#.

\begin{code}
realWorldTy	     = applyTyCon realWorldTyCon []
realWorldTyCon	     = mk_no_constr_tycon realWorldTyConKey SLIT("RealWorld") 
realWorldStatePrimTy = mkStatePrimTy realWorldTy
\end{code}

Note: the ``state-pairing'' types are not truly primitive, so they are
defined in \tr{TysWiredIn.lhs}, not here.

\begin{code}
-- The Void type is represented as a data type with no constructors
-- It's a built in type (i.e. there's no way to define it in Haskell;
--	the nearest would be
--
--		data Void =		-- No constructors!
--
-- ) It's boxed; there is only one value of this
-- type, namely "void", whose semantics is just bottom.
voidTy    = mkTyConTy voidTyCon
voidTyCon = mk_no_constr_tycon voidTyConKey SLIT("Void")
\end{code}

\begin{code}
mk_no_constr_tycon key str
  = the_tycon
  where
    name      = mkWiredInTyConName key gHC__ str the_tycon
    the_tycon = mkDataTyCon name mkBoxedTypeKind 
			[{-no tyvars-}]
			[{-no context-}]
			[{-no data cons!-}] -- we tell you *nothing* about this guy
			[{-no derivings-}]
			DataType
\end{code}

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
synchVarPrimTyCon = pcPrimTyCon synchVarPrimTyConKey SLIT("SynchVar#") 2 PtrRep

mkSynchVarPrimTy s elt 	    = applyTyCon synchVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
%*									*
%************************************************************************

\begin{code}
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConKey SLIT("StablePtr#") 1 StablePtrRep

mkStablePtrPrimTy ty = applyTyCon stablePtrPrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-foreign-objs]{The ``foreign object'' type}
%*									*
%************************************************************************

Foreign objects (formerly ``Malloc'' pointers) provide a mechanism which
will let Haskell's garbage collector communicate with a {\em simple\/}
garbage collector in the IO world. We want Haskell to be able to hold
onto references to objects in the IO world and for Haskell's garbage
collector to tell the IO world when these references become garbage.
We are not aiming to provide a mechanism that could
talk to a sophisticated garbage collector such as that provided by a
LISP system (with a correspondingly complex interface); in particular,
we shall ignore the danger of circular structures spread across the
two systems.

There are no primitive operations on @ForeignObj#@s (although equality
could possibly be added?)

\begin{code}
foreignObjPrimTy    = applyTyCon foreignObjPrimTyCon []
foreignObjPrimTyCon = pcPrimTyCon foreignObjPrimTyConKey SLIT("ForeignObj#") 0 ForeignObjRep
\end{code}

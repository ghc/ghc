%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[TysPrim]{Wired-in knowledge about primitive types}

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
module TysPrim where

#include "HsVersions.h"

import Kind		( mkBoxedTypeKind )
import Name		( mkWiredInTyConName )
import PrimRep		( PrimRep(..) )	-- getPrimRepInfo uses PrimRep repn
import TyCon		( mkPrimTyCon, mkDataTyCon, TyCon )
import BasicTypes	( NewOrData(..), RecFlag(..) )
import Type		( mkTyConApp, mkTyConTy, mkTyVarTys, Type )
import TyVar		( GenTyVar(..), alphaTyVars )
import PrelMods		( pREL_GHC )
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
    name      = mkWiredInTyConName key pREL_GHC str the_tycon
    the_tycon = mkPrimTyCon name arity primrep


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

@PrimitiveKinds@ are used in @PrimitiveOps@, for which we often need
to reconstruct various type information.  (It's slightly more
convenient/efficient to make type info from kinds, than kinds [etc.]
from type info.)

\begin{code}
getPrimRepInfo ::
    PrimRep -> (String,		-- tag string
		Type, TyCon)	-- prim type and tycon

getPrimRepInfo CharRep       = ("Char",   charPrimTy,   charPrimTyCon)
getPrimRepInfo IntRep        = ("Int",    intPrimTy,    intPrimTyCon)
getPrimRepInfo WordRep       = ("Word",   wordPrimTy,   wordPrimTyCon)
getPrimRepInfo AddrRep       = ("Addr",   addrPrimTy,   addrPrimTyCon)
getPrimRepInfo FloatRep      = ("Float",  floatPrimTy,  floatPrimTyCon)
getPrimRepInfo DoubleRep     = ("Double", doublePrimTy, doublePrimTyCon)
getPrimRepInfo Int64Rep      = ("Int64",  int64PrimTy,  int64PrimTyCon)
getPrimRepInfo Word64Rep     = ("Word64", word64PrimTy, word64PrimTyCon)
getPrimRepInfo StablePtrRep  = ("StablePtr", mkStablePtrPrimTy alphaTy, stablePtrPrimTyCon)
getPrimRepInfo ForeignObjRep = ("ForeignObj", foreignObjPrimTy, foreignObjPrimTyCon)

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
{\em is not unboxed}.
We never manipulate values of type RealWorld; it's only used in the type
system, to parameterise State#.

\begin{code}
realWorldTy	     = mkTyConTy realWorldTyCon
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
    name      = mkWiredInTyConName key pREL_GHC str the_tycon
    the_tycon = mkDataTyCon name mkBoxedTypeKind 
			[]		-- No tyvars
			[]		-- No context
			[]		-- No constructors; we tell you *nothing* about this guy
			[]		-- No derivings
			Nothing		-- Not a dictionary
			DataType
			NonRecursive
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

mkArrayPrimTy elt    	    = mkTyConApp arrayPrimTyCon [elt]
byteArrayPrimTy	    	    = mkTyConTy byteArrayPrimTyCon
mkMutableArrayPrimTy s elt  = mkTyConApp mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy s  = mkTyConApp mutableByteArrayPrimTyCon [s]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-synch-var]{The synchronizing variable type}
%*									*
%************************************************************************

\begin{code}
synchVarPrimTyCon = pcPrimTyCon synchVarPrimTyConKey SLIT("SynchVar#") 2 PtrRep

mkSynchVarPrimTy s elt 	    = mkTyConApp synchVarPrimTyCon [s, elt]
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
foreignObjPrimTy    = mkTyConTy foreignObjPrimTyCon
foreignObjPrimTyCon = pcPrimTyCon foreignObjPrimTyConKey SLIT("ForeignObj#") 0 ForeignObjRep
\end{code}

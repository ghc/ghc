%
% (c) The AQUA Project, Glasgow University, 1994-1998
%

     
\section[TysPrim]{Wired-in knowledge about primitive types}

\begin{code}
-- | This module defines TyCons that can't be expressed in Haskell. 
--   They are all, therefore, wired-in TyCons.  C.f module TysWiredIn
module TysPrim(
	alphaTyVars, betaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
	alphaTy, betaTy, gammaTy, deltaTy,
	openAlphaTy, openBetaTy, openAlphaTyVar, openBetaTyVar, openAlphaTyVars,
        argAlphaTy, argAlphaTyVar, argBetaTy, argBetaTyVar,

        -- Kind constructors...
        tySuperKindTyCon, tySuperKind,
        liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
        argTypeKindTyCon, ubxTupleKindTyCon,

        tySuperKindTyConName, liftedTypeKindTyConName,
        openTypeKindTyConName, unliftedTypeKindTyConName,
        ubxTupleKindTyConName, argTypeKindTyConName,

        -- Kinds
	liftedTypeKind, unliftedTypeKind, openTypeKind,
        argTypeKind, ubxTupleKind,
        mkArrowKind, mkArrowKinds, isCoercionKind,

        funTyCon, funTyConName,
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
        tVarPrimTyCon,                  mkTVarPrimTy,
	stablePtrPrimTyCon,		mkStablePtrPrimTy,
	stableNamePrimTyCon,		mkStableNamePrimTy,
	bcoPrimTyCon,			bcoPrimTy,
	weakPrimTyCon,  		mkWeakPrimTy,
	threadIdPrimTyCon,		threadIdPrimTy,
	
	int32PrimTyCon,		int32PrimTy,
	word32PrimTyCon,	word32PrimTy,

	int64PrimTyCon,		int64PrimTy,
        word64PrimTyCon,        word64PrimTy,

        eqPredPrimTyCon,            -- ty1 ~ ty2

	-- * Any
	anyTyCon, anyTyConOfKind, anyTypeOfKind
  ) where

#include "HsVersions.h"

import Var		( TyVar, mkTyVar )
import Name		( Name, BuiltInSyntax(..), mkInternalName, mkWiredInName )
import OccName          ( mkTcOcc,mkTyVarOccFS, mkTcOccFS )
import TyCon
import TypeRep
import SrcLoc
import Unique		( mkAlphaTyVarUnique )
import PrelNames
import FastString
import Outputable

import Data.Char
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
    , int32PrimTyCon
    , int64PrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mVarPrimTyCon
    , tVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , statePrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon
    , anyTyCon
    , eqPredPrimTyCon
    ]

mkPrimTc :: FastString -> Unique -> TyCon -> Name
mkPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs) 
		  unique
		  (ATyCon tycon)	-- Relevant TyCon
		  UserSyntax		-- None are built-in syntax

charPrimTyConName, intPrimTyConName, int32PrimTyConName, int64PrimTyConName, wordPrimTyConName, word32PrimTyConName, word64PrimTyConName, addrPrimTyConName, floatPrimTyConName, doublePrimTyConName, statePrimTyConName, realWorldTyConName, arrayPrimTyConName, byteArrayPrimTyConName, mutableArrayPrimTyConName, mutableByteArrayPrimTyConName, mutVarPrimTyConName, mVarPrimTyConName, tVarPrimTyConName, stablePtrPrimTyConName, stableNamePrimTyConName, bcoPrimTyConName, weakPrimTyConName, threadIdPrimTyConName, eqPredPrimTyConName :: Name
charPrimTyConName    	      = mkPrimTc (fsLit "Char#") charPrimTyConKey charPrimTyCon
intPrimTyConName     	      = mkPrimTc (fsLit "Int#") intPrimTyConKey  intPrimTyCon
int32PrimTyConName	      = mkPrimTc (fsLit "Int32#") int32PrimTyConKey int32PrimTyCon
int64PrimTyConName   	      = mkPrimTc (fsLit "Int64#") int64PrimTyConKey int64PrimTyCon
wordPrimTyConName    	      = mkPrimTc (fsLit "Word#") wordPrimTyConKey wordPrimTyCon
word32PrimTyConName  	      = mkPrimTc (fsLit "Word32#") word32PrimTyConKey word32PrimTyCon
word64PrimTyConName  	      = mkPrimTc (fsLit "Word64#") word64PrimTyConKey word64PrimTyCon
addrPrimTyConName    	      = mkPrimTc (fsLit "Addr#") addrPrimTyConKey addrPrimTyCon
floatPrimTyConName   	      = mkPrimTc (fsLit "Float#") floatPrimTyConKey floatPrimTyCon
doublePrimTyConName  	      = mkPrimTc (fsLit "Double#") doublePrimTyConKey doublePrimTyCon
statePrimTyConName            = mkPrimTc (fsLit "State#") statePrimTyConKey statePrimTyCon
eqPredPrimTyConName           = mkPrimTc (fsLit "~") eqPredPrimTyConKey eqPredPrimTyCon
realWorldTyConName            = mkPrimTc (fsLit "RealWorld") realWorldTyConKey realWorldTyCon
arrayPrimTyConName   	      = mkPrimTc (fsLit "Array#") arrayPrimTyConKey arrayPrimTyCon
byteArrayPrimTyConName	      = mkPrimTc (fsLit "ByteArray#") byteArrayPrimTyConKey byteArrayPrimTyCon
mutableArrayPrimTyConName     = mkPrimTc (fsLit "MutableArray#") mutableArrayPrimTyConKey mutableArrayPrimTyCon
mutableByteArrayPrimTyConName = mkPrimTc (fsLit "MutableByteArray#") mutableByteArrayPrimTyConKey mutableByteArrayPrimTyCon
mutVarPrimTyConName	      = mkPrimTc (fsLit "MutVar#") mutVarPrimTyConKey mutVarPrimTyCon
mVarPrimTyConName	      = mkPrimTc (fsLit "MVar#") mVarPrimTyConKey mVarPrimTyCon
tVarPrimTyConName	      = mkPrimTc (fsLit "TVar#") tVarPrimTyConKey tVarPrimTyCon
stablePtrPrimTyConName        = mkPrimTc (fsLit "StablePtr#") stablePtrPrimTyConKey stablePtrPrimTyCon
stableNamePrimTyConName       = mkPrimTc (fsLit "StableName#") stableNamePrimTyConKey stableNamePrimTyCon
bcoPrimTyConName 	      = mkPrimTc (fsLit "BCO#") bcoPrimTyConKey bcoPrimTyCon
weakPrimTyConName  	      = mkPrimTc (fsLit "Weak#") weakPrimTyConKey weakPrimTyCon
threadIdPrimTyConName  	      = mkPrimTc (fsLit "ThreadId#") threadIdPrimTyConKey threadIdPrimTyCon
\end{code}

%************************************************************************
%*									*
\subsection{Support code}
%*									*
%************************************************************************

alphaTyVars is a list of type variables for use in templates: 
	["a", "b", ..., "z", "t1", "t2", ... ]

\begin{code}
tyVarList :: Kind -> [TyVar]
tyVarList kind = [ mkTyVar (mkInternalName (mkAlphaTyVarUnique u) 
				(mkTyVarOccFS (mkFastString name))
			 	noSrcSpan) kind
	         | u <- [2..],
		   let name | c <= 'z'  = [c]
		            | otherwise = 't':show u
			    where c = chr (u-2 + ord 'a')
	         ]

alphaTyVars :: [TyVar]
alphaTyVars = tyVarList liftedTypeKind

betaTyVars :: [TyVar]
betaTyVars = tail alphaTyVars

alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys :: [Type]
alphaTys = mkTyVarTys alphaTyVars
alphaTy, betaTy, gammaTy, deltaTy :: Type
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

	-- openAlphaTyVar is prepared to be instantiated
	-- to a lifted or unlifted type variable.  It's used for the 
	-- result type for "error", so that we can have (error Int# "Help")
openAlphaTyVars :: [TyVar]
openAlphaTyVar, openBetaTyVar :: TyVar
openAlphaTyVars@(openAlphaTyVar:openBetaTyVar:_) = tyVarList openTypeKind

openAlphaTy, openBetaTy :: Type
openAlphaTy = mkTyVarTy openAlphaTyVar
openBetaTy  = mkTyVarTy openBetaTyVar

argAlphaTyVar, argBetaTyVar :: TyVar
(argAlphaTyVar : argBetaTyVar : _) = tyVarList argTypeKind
argAlphaTy, argBetaTy :: Type
argAlphaTy = mkTyVarTy argAlphaTyVar
argBetaTy  = mkTyVarTy argBetaTyVar
\end{code}


%************************************************************************
%*									*
                FunTyCon
%*									*
%************************************************************************

\begin{code}
funTyConName :: Name
funTyConName = mkPrimTyConName (fsLit "(->)") funTyConKey funTyCon

funTyCon :: TyCon
funTyCon = mkFunTyCon funTyConName (mkArrowKinds [argTypeKind, openTypeKind] liftedTypeKind)
        -- You might think that (->) should have type (?? -> ? -> *), and you'd be right
	-- But if we do that we get kind errors when saying
	--	instance Control.Arrow (->)
	-- becuase the expected kind is (*->*->*).  The trouble is that the
	-- expected/actual stuff in the unifier does not go contra-variant, whereas
	-- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
	-- a prefix way, thus:  (->) Int# Int#.  And this is unusual.
        -- because they are never in scope in the source
\end{code}


%************************************************************************
%*									*
                Kinds
%*									*
%************************************************************************

\begin{code}
-- | See "Type#kind_subtyping" for details of the distinction between the 'Kind' 'TyCon's
tySuperKindTyCon, liftedTypeKindTyCon,
      openTypeKindTyCon, unliftedTypeKindTyCon,
      ubxTupleKindTyCon, argTypeKindTyCon
   :: TyCon
tySuperKindTyConName, liftedTypeKindTyConName,
      openTypeKindTyConName, unliftedTypeKindTyConName,
      ubxTupleKindTyConName, argTypeKindTyConName
   :: Name

tySuperKindTyCon      = mkSuperKindTyCon tySuperKindTyConName
liftedTypeKindTyCon   = mkKindTyCon liftedTypeKindTyConName   tySuperKind
openTypeKindTyCon     = mkKindTyCon openTypeKindTyConName     tySuperKind
unliftedTypeKindTyCon = mkKindTyCon unliftedTypeKindTyConName tySuperKind
ubxTupleKindTyCon     = mkKindTyCon ubxTupleKindTyConName     tySuperKind
argTypeKindTyCon      = mkKindTyCon argTypeKindTyConName      tySuperKind

--------------------------
-- ... and now their names

tySuperKindTyConName      = mkPrimTyConName (fsLit "BOX") tySuperKindTyConKey tySuperKindTyCon
liftedTypeKindTyConName   = mkPrimTyConName (fsLit "*") liftedTypeKindTyConKey liftedTypeKindTyCon
openTypeKindTyConName     = mkPrimTyConName (fsLit "?") openTypeKindTyConKey openTypeKindTyCon
unliftedTypeKindTyConName = mkPrimTyConName (fsLit "#") unliftedTypeKindTyConKey unliftedTypeKindTyCon
ubxTupleKindTyConName     = mkPrimTyConName (fsLit "(#)") ubxTupleKindTyConKey ubxTupleKindTyCon
argTypeKindTyConName      = mkPrimTyConName (fsLit "??") argTypeKindTyConKey argTypeKindTyCon

mkPrimTyConName :: FastString -> Unique -> TyCon -> Name
mkPrimTyConName occ key tycon = mkWiredInName gHC_PRIM (mkTcOccFS occ) 
					      key 
					      (ATyCon tycon)
					      BuiltInSyntax
	-- All of the super kinds and kinds are defined in Prim and use BuiltInSyntax,
	-- because they are never in scope in the source
\end{code}


\begin{code}
kindTyConType :: TyCon -> Type
kindTyConType kind = TyConApp kind []

-- | See "Type#kind_subtyping" for details of the distinction between these 'Kind's
liftedTypeKind, unliftedTypeKind, openTypeKind, argTypeKind, ubxTupleKind :: Kind

liftedTypeKind   = kindTyConType liftedTypeKindTyCon
unliftedTypeKind = kindTyConType unliftedTypeKindTyCon
openTypeKind     = kindTyConType openTypeKindTyCon
argTypeKind      = kindTyConType argTypeKindTyCon
ubxTupleKind	 = kindTyConType ubxTupleKindTyCon

-- | Given two kinds @k1@ and @k2@, creates the 'Kind' @k1 -> k2@
mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = FunTy k1 k2

-- | Iterated application of 'mkArrowKind'
mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds

tySuperKind :: SuperKind
tySuperKind = kindTyConType tySuperKindTyCon 
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
%*									*
%************************************************************************

\begin{code}
-- only used herein
pcPrimTyCon :: Name -> Int -> PrimRep -> TyCon
pcPrimTyCon name arity rep
  = mkPrimTyCon name kind arity rep
  where
    kind        = mkArrowKinds (replicate arity liftedTypeKind) result_kind
    result_kind = unliftedTypeKind

pcPrimTyCon0 :: Name -> PrimRep -> TyCon
pcPrimTyCon0 name rep
  = mkPrimTyCon name result_kind 0 rep
  where
    result_kind = unliftedTypeKind

charPrimTy :: Type
charPrimTy	= mkTyConTy charPrimTyCon
charPrimTyCon :: TyCon
charPrimTyCon	= pcPrimTyCon0 charPrimTyConName WordRep

intPrimTy :: Type
intPrimTy	= mkTyConTy intPrimTyCon
intPrimTyCon :: TyCon
intPrimTyCon	= pcPrimTyCon0 intPrimTyConName IntRep

int32PrimTy :: Type
int32PrimTy	= mkTyConTy int32PrimTyCon
int32PrimTyCon :: TyCon
int32PrimTyCon	= pcPrimTyCon0 int32PrimTyConName IntRep

int64PrimTy :: Type
int64PrimTy	= mkTyConTy int64PrimTyCon
int64PrimTyCon :: TyCon
int64PrimTyCon	= pcPrimTyCon0 int64PrimTyConName Int64Rep

wordPrimTy :: Type
wordPrimTy	= mkTyConTy wordPrimTyCon
wordPrimTyCon :: TyCon
wordPrimTyCon	= pcPrimTyCon0 wordPrimTyConName WordRep

word32PrimTy :: Type
word32PrimTy	= mkTyConTy word32PrimTyCon
word32PrimTyCon :: TyCon
word32PrimTyCon	= pcPrimTyCon0 word32PrimTyConName WordRep

word64PrimTy :: Type
word64PrimTy	= mkTyConTy word64PrimTyCon
word64PrimTyCon :: TyCon
word64PrimTyCon	= pcPrimTyCon0 word64PrimTyConName Word64Rep

addrPrimTy :: Type
addrPrimTy	= mkTyConTy addrPrimTyCon
addrPrimTyCon :: TyCon
addrPrimTyCon	= pcPrimTyCon0 addrPrimTyConName AddrRep

floatPrimTy	:: Type
floatPrimTy	= mkTyConTy floatPrimTyCon
floatPrimTyCon :: TyCon
floatPrimTyCon	= pcPrimTyCon0 floatPrimTyConName FloatRep

doublePrimTy :: Type
doublePrimTy	= mkTyConTy doublePrimTyCon
doublePrimTyCon	:: TyCon
doublePrimTyCon	= pcPrimTyCon0 doublePrimTyConName DoubleRep
\end{code}


%************************************************************************
%*									*
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
%*									*
%************************************************************************

Note [The (~) TyCon)
~~~~~~~~~~~~~~~~~~~~
There is a perfectly ordinary type constructor (~) that represents the type
of coercions (which, remember, are values).  For example
   Refl Int :: Int ~ Int

Atcually it is not quite "perfectly ordinary" because it is kind-polymorphic:
   Refl Maybe :: Maybe ~ Maybe

So the true kind of (~) :: forall k. k -> k -> #.  But we don't have
polymorphic kinds (yet). However, (~) really only appears saturated in
which case there is no problem in finding the kind of (ty1 ~ ty2). So
we check that in CoreLint (and, in an assertion, in Kind.typeKind).

Note [The State# TyCon]
~~~~~~~~~~~~~~~~~~~~~~~
State# is the primitive, unlifted type of states.  It has one type parameter,
thus
	State# RealWorld
or
	State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

\begin{code}
mkStatePrimTy :: Type -> Type
mkStatePrimTy ty = mkTyConApp statePrimTyCon [ty]

statePrimTyCon :: TyCon   -- See Note [The State# TyCon]
statePrimTyCon	 = pcPrimTyCon statePrimTyConName 1 VoidRep

eqPredPrimTyCon :: TyCon  -- The representation type for equality predicates
		   	  -- See Note [The (~) TyCon]
eqPredPrimTyCon  = pcPrimTyCon eqPredPrimTyConName 2 VoidRep
\end{code}

RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence ptrArg).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.

\begin{code}
realWorldTyCon :: TyCon
realWorldTyCon = mkLiftedPrimTyCon realWorldTyConName liftedTypeKind 0 PtrRep
realWorldTy :: Type
realWorldTy	     = mkTyConTy realWorldTyCon
realWorldStatePrimTy :: Type
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
arrayPrimTyCon, mutableArrayPrimTyCon, mutableByteArrayPrimTyCon,
    byteArrayPrimTyCon :: TyCon
arrayPrimTyCon		  = pcPrimTyCon  arrayPrimTyConName	       1 PtrRep
mutableArrayPrimTyCon	  = pcPrimTyCon  mutableArrayPrimTyConName     2 PtrRep
mutableByteArrayPrimTyCon = pcPrimTyCon  mutableByteArrayPrimTyConName 1 PtrRep
byteArrayPrimTyCon	  = pcPrimTyCon0 byteArrayPrimTyConName	         PtrRep

mkArrayPrimTy :: Type -> Type
mkArrayPrimTy elt    	    = mkTyConApp arrayPrimTyCon [elt]
byteArrayPrimTy :: Type
byteArrayPrimTy	    	    = mkTyConTy byteArrayPrimTyCon
mkMutableArrayPrimTy :: Type -> Type -> Type
mkMutableArrayPrimTy s elt  = mkTyConApp mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy :: Type -> Type
mkMutableByteArrayPrimTy s  = mkTyConApp mutableByteArrayPrimTyCon [s]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-mut-var]{The mutable variable type}
%*									*
%************************************************************************

\begin{code}
mutVarPrimTyCon :: TyCon
mutVarPrimTyCon = pcPrimTyCon mutVarPrimTyConName 2 PtrRep

mkMutVarPrimTy :: Type -> Type -> Type
mkMutVarPrimTy s elt 	    = mkTyConApp mutVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-synch-var]{The synchronizing variable type}
%*									*
%************************************************************************

\begin{code}
mVarPrimTyCon :: TyCon
mVarPrimTyCon = pcPrimTyCon mVarPrimTyConName 2 PtrRep

mkMVarPrimTy :: Type -> Type -> Type
mkMVarPrimTy s elt 	    = mkTyConApp mVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stm-var]{The transactional variable type}
%*									*
%************************************************************************

\begin{code}
tVarPrimTyCon :: TyCon
tVarPrimTyCon = pcPrimTyCon tVarPrimTyConName 2 PtrRep

mkTVarPrimTy :: Type -> Type -> Type
mkTVarPrimTy s elt = mkTyConApp tVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
%*									*
%************************************************************************

\begin{code}
stablePtrPrimTyCon :: TyCon
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConName 1 AddrRep

mkStablePtrPrimTy :: Type -> Type
mkStablePtrPrimTy ty = mkTyConApp stablePtrPrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-names]{The stable-name type}
%*									*
%************************************************************************

\begin{code}
stableNamePrimTyCon :: TyCon
stableNamePrimTyCon = pcPrimTyCon stableNamePrimTyConName 1 PtrRep

mkStableNamePrimTy :: Type -> Type
mkStableNamePrimTy ty = mkTyConApp stableNamePrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-BCOs]{The ``bytecode object'' type}
%*									*
%************************************************************************

\begin{code}
bcoPrimTy    :: Type
bcoPrimTy    = mkTyConTy bcoPrimTyCon
bcoPrimTyCon :: TyCon
bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName PtrRep
\end{code}
  
%************************************************************************
%*									*
\subsection[TysPrim-Weak]{The ``weak pointer'' type}
%*									*
%************************************************************************

\begin{code}
weakPrimTyCon :: TyCon
weakPrimTyCon = pcPrimTyCon weakPrimTyConName 1 PtrRep

mkWeakPrimTy :: Type -> Type
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
threadIdPrimTy :: Type
threadIdPrimTy    = mkTyConTy threadIdPrimTyCon
threadIdPrimTyCon :: TyCon
threadIdPrimTyCon = pcPrimTyCon0 threadIdPrimTyConName PtrRep
\end{code}



%************************************************************************
%*									*
		Any
%*									*
%************************************************************************

Note [Any types]
~~~~~~~~~~~~~~~~
The type constructor Any::* has these properties

  * It is defined in module GHC.Prim, and exported so that it is 
    available to users.  For this reason it's treated like any other 
    primitive type:
      - has a fixed unique, anyTyConKey, 
      - lives in the global name cache
      - built with TyCon.PrimTyCon

  * It is lifted, and hence represented by a pointer

  * It is inhabited by at least one value, namely bottom

  * You can unsafely coerce any lifted type to Ayny, and back.

  * It does not claim to be a *data* type, and that's important for
    the code generator, because the code gen may *enter* a data value
    but never enters a function value. 

  * It is used to instantiate otherwise un-constrained type variables of kind *
    For example   	length Any []
    See Note [Strangely-kinded void TyCons]

In addition, we have a potentially-infinite family of types, one for
each kind /other than/ *, needed to instantiate otherwise
un-constrained type variables of kinds other than *.  This is a bit
like tuples; there is a potentially-infinite family.  They have slightly
different characteristics to Any::*:
  
  * They are built with TyCon.AnyTyCon
  * They have non-user-writable names like "Any(*->*)" 
  * They are not exported by GHC.Prim
  * They are uninhabited (of course; not kind *)
  * They have a unique derived from their OccName (see Note [Uniques of Any])
  * Their Names do not live in the global name cache

Note [Uniques of Any]
~~~~~~~~~~~~~~~~~~~~~
Although Any(*->*), say, doesn't have a binding site, it still needs
to have a Unique.  Unlike tuples (which are also an infinite family)
there is no convenient way to index them, so we use the Unique from
their OccName instead.  That should be unique, 
  - both wrt each other, because their strings differ

  - and wrt any other Name, because Names get uniques with 
    various 'char' tags, but the OccName of Any will 
    get a Unique built with mkTcOccUnique, which has a particular 'char' 
    tag; see Unique.mkTcOccUnique!

Note [Strangely-kinded void TyCons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #959 for more examples

When the type checker finds a type variable with no binding, which
means it can be instantiated with an arbitrary type, it usually
instantiates it to Void.  Eg.

	length []
===>
	length Any (Nil Any)

But in really obscure programs, the type variable might have a kind
other than *, so we need to invent a suitably-kinded type.

This commit uses
	Any for kind *
	Any(*->*) for kind *->*
	etc

\begin{code}
anyTyConName :: Name
anyTyConName = mkPrimTc (fsLit "Any") anyTyConKey anyTyCon

anyTyCon :: TyCon
anyTyCon = mkLiftedPrimTyCon anyTyConName liftedTypeKind 0 PtrRep

anyTypeOfKind :: Kind -> Type
anyTypeOfKind kind = mkTyConApp (anyTyConOfKind kind) []

anyTyConOfKind :: Kind -> TyCon
-- Map all superkinds of liftedTypeKind to liftedTypeKind
anyTyConOfKind kind 
  | isLiftedTypeKind kind = anyTyCon
  | otherwise             = tycon
  where
	  -- Derive the name from the kind, thus:
	  --     Any(*->*), Any(*->*->*)
	  -- These are names that can't be written by the user,
	  -- and are not allocated in the global name cache
    str = "Any" ++ showSDoc (pprParendKind kind)

    occ   = mkTcOcc str
    uniq  = getUnique occ  -- See Note [Uniques of Any]
    name  = mkWiredInName gHC_PRIM occ uniq (ATyCon tycon) UserSyntax
    tycon = mkAnyTyCon name kind 
\end{code}

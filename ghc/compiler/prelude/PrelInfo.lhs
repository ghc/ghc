%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
module PrelInfo (
	module PrelNames,
	module MkId,

	wiredInThings, 	-- Names of wired in things
	wiredInThingEnv,
	ghcPrimExports,
	cCallableClassDecl, cReturnableClassDecl, assertDecl,
	
	-- Random other things
	maybeCharLikeCon, maybeIntLikeCon,

	-- Class categories
	isCcallishClass, isCreturnableClass, isNoDictClass, 
	isNumericClass, isStandardClass

    ) where

#include "HsVersions.h"

import PrelNames	-- Prelude module names

import PrimOp		( allThePrimOps, primOpOcc )
import DataCon		( DataCon )
import Id		( idName )
import MkId		( mkPrimOpId, wiredInIds )
import MkId		-- All of it, for re-export
import Name		( nameOccName )
import RdrName		( mkRdrUnqual, getRdrName )
import HsSyn		( HsTyVarBndr(..), TyClDecl(..), HsType(..) )
import OccName		( mkVarOcc )
import TysPrim		( primTyCons )
import TysWiredIn	( wiredInTyCons )
import RdrHsSyn		( mkClassDecl )
import HscTypes 	( TyThing(..), implicitTyThingIds, TypeEnv, mkTypeEnv,
			  GenAvailInfo(..), RdrAvailInfo )
import Class	 	( Class, classKey )
import Type		( funTyCon, openTypeKind, liftedTypeKind )
import TyCon		( tyConName )
import SrcLoc		( noSrcLoc )
import Util		( isIn )
\end{code}

%************************************************************************
%*									*
\subsection[builtinNameInfo]{Lookup built-in names}
%*									*
%************************************************************************

We have two ``builtin name funs,'' one to look up @TyCons@ and
@Classes@, the other to look up values.

\begin{code}
wiredInThings :: [TyThing]
wiredInThings
  = concat
    [		-- Wired in TyCons and their implicit Ids
	  tycon_things
	, map AnId (implicitTyThingIds tycon_things)

		-- Wired in Ids
	, map AnId wiredInIds

		-- PrimOps
	, map (AnId . mkPrimOpId) allThePrimOps
    ]
  where
    tycon_things = map ATyCon ([funTyCon] ++ primTyCons ++ wiredInTyCons)

wiredInThingEnv :: TypeEnv
wiredInThingEnv = mkTypeEnv wiredInThings
\end{code}

We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

%************************************************************************
%*									*
\subsection{Export lists for pseudo-modules (GHC.Prim)}
%*									*
%************************************************************************

GHC.Prim "exports" all the primops and primitive types, some 
wired-in Ids, and the CCallable & CReturnable classes.

\begin{code}
ghcPrimExports :: [RdrAvailInfo]
 = AvailTC cCallableOcc [ cCallableOcc ] :
   AvailTC cReturnableOcc [ cReturnableOcc ] :
   map (Avail . nameOccName . idName) ghcPrimIds ++
   map (Avail . primOpOcc) allThePrimOps ++
   [ AvailTC occ [occ] |
     n <- funTyCon : primTyCons, let occ = nameOccName (tyConName n) 
   ]
 where
   cCallableOcc = nameOccName cCallableClassName
   cReturnableOcc = nameOccName cReturnableClassName

assertDecl
  = IfaceSig { 
 	tcdName = getRdrName assertName,
	tcdType = HsForAllTy (Just [liftedAlpha]) [] (HsTyVar alpha),
	tcdIdInfo = [],
	tcdLoc = noSrcLoc
    }

cCallableClassDecl
  = mkClassDecl
    ([], getRdrName cCallableClassName, [openAlpha])
    [] -- no fds
    [] -- no sigs
    Nothing -- no mbinds
    noSrcLoc

cReturnableClassDecl
  = mkClassDecl
    ([], getRdrName cReturnableClassName, [openAlpha])
    [] -- no fds
    [] -- no sigs
    Nothing -- no mbinds
    noSrcLoc

alpha = mkRdrUnqual (mkVarOcc FSLIT("a"))
openAlpha = IfaceTyVar alpha openTypeKind
liftedAlpha = IfaceTyVar alpha liftedTypeKind
\end{code}


%************************************************************************
%*									*
\subsection{Built-in keys}
%*									*
%************************************************************************

ToDo: make it do the ``like'' part properly (as in 0.26 and before).

\begin{code}
maybeCharLikeCon, maybeIntLikeCon :: DataCon -> Bool
maybeCharLikeCon con = con `hasKey` charDataConKey
maybeIntLikeCon  con = con `hasKey` intDataConKey
\end{code}


%************************************************************************
%*									*
\subsection{Class predicates}
%*									*
%************************************************************************

\begin{code}
isCcallishClass, isCreturnableClass, isNoDictClass, 
  isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys
isCcallishClass	   clas = classKey clas `is_elem` cCallishClassKeys
isCreturnableClass clas = classKey clas == cReturnableClassKey
isNoDictClass      clas = classKey clas `is_elem` noDictClassKeys
is_elem = isIn "is_X_Class"
\end{code}

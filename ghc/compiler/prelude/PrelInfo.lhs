%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
module PrelInfo (
	module PrelNames,
	module MkId,

	wiredInThingEnv,
	ghcPrimExports,
	knownKeyNames,
	
	-- Random other things
	maybeCharLikeCon, maybeIntLikeCon,

	-- Class categories
	isNoDictClass, isNumericClass, isStandardClass

    ) where

#include "HsVersions.h"

import PrelNames	( basicKnownKeyNames, 
			  hasKey, charDataConKey, intDataConKey,
			  numericClassKeys, standardClassKeys,
			  noDictClassKeys )
#ifdef GHCI
import DsMeta		( templateHaskellNames )
import NameSet		( nameSetToList )
#endif

import PrimOp		( allThePrimOps, primOpOcc )
import DataCon		( DataCon )
import Id		( idName )
import MkId		( mkPrimOpId, wiredInIds )
import MkId		-- All of it, for re-export
import Name		( Name, nameOccName, NamedThing(..) )
import RdrName		( mkRdrUnqual )
import HsSyn		( HsTyVarBndr(..) )
import OccName		( mkVarOcc )
import TysPrim		( primTyCons )
import TysWiredIn	( wiredInTyCons )
import HscTypes 	( TyThing(..), implicitTyThings, TypeEnv, mkTypeEnv,
			  GenAvailInfo(..), RdrAvailInfo )
import Class	 	( Class, classKey, className )
import Type		( funTyCon, openTypeKind, liftedTypeKind )
import TyCon		( tyConName )
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
	, implicitTyThings tycon_things

		-- Wired in Ids
	, map AnId wiredInIds

		-- PrimOps
	, map (AnId . mkPrimOpId) allThePrimOps
    ]
  where
    tycon_things = map ATyCon ([funTyCon] ++ primTyCons ++ wiredInTyCons)

wiredInThingEnv :: TypeEnv
wiredInThingEnv = mkTypeEnv wiredInThings

knownKeyNames :: [Name]
knownKeyNames 
  = map getName wiredInThings 
    ++ basicKnownKeyNames
#ifdef GHCI
    ++ nameSetToList templateHaskellNames
#endif
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
wired-in Ids.

\begin{code}
ghcPrimExports :: [RdrAvailInfo]
 = map (Avail . nameOccName . idName) ghcPrimIds ++
   map (Avail . primOpOcc) allThePrimOps ++
   [ AvailTC occ [occ] |
     n <- funTyCon : primTyCons, let occ = nameOccName (tyConName n) 
   ]

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
isNoDictClass, isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys
isNoDictClass      clas = classKey clas `is_elem` noDictClassKeys
is_elem = isIn "is_X_Class"
\end{code}

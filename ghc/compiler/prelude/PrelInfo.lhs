%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
module PrelInfo (
	module MkId,

	ghcPrimExports,
	wiredInThings, basicKnownKeyNames,
	primOpId,
	
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

import PrimOp		( PrimOp, allThePrimOps, primOpOcc, primOpTag, maxPrimOpTag )
import DataCon		( DataCon )
import Id		( Id, idName )
import MkId		( mkPrimOpId, wiredInIds )
import MkId		-- All of it, for re-export
import Name		( nameOccName )
import TysPrim		( primTyCons )
import TysWiredIn	( wiredInTyCons )
import HscTypes 	( TyThing(..), implicitTyThings, GenAvailInfo(..), RdrAvailInfo )
import Class	 	( Class, classKey )
import Type		( funTyCon )
import TyCon		( tyConName )
import Util		( isIn )

import Array		( Array, array, (!) )
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
	, concatMap implicitTyThings tycon_things

		-- Wired in Ids
	, map AnId wiredInIds

		-- PrimOps
	, map (AnId . mkPrimOpId) allThePrimOps
    ]
  where
    tycon_things = map ATyCon ([funTyCon] ++ primTyCons ++ wiredInTyCons)
\end{code}

We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

%************************************************************************
%*									*
		PrimOpIds
%*									*
%************************************************************************

\begin{code}
primOpIds :: Array Int Id	-- Indexed by PrimOp tag
primOpIds = array (1,maxPrimOpTag) [ (primOpTag op, mkPrimOpId op) 
				   | op <- allThePrimOps]

primOpId :: PrimOp -> Id
primOpId op = primOpIds ! primOpTag op
\end{code}


%************************************************************************
%*									*
\subsection{Export lists for pseudo-modules (GHC.Prim)}
%*									*
%************************************************************************

GHC.Prim "exports" all the primops and primitive types, some 
wired-in Ids.

\begin{code}
ghcPrimExports :: [RdrAvailInfo]
ghcPrimExports
 = map (Avail . nameOccName . idName) ghcPrimIds ++
   map (Avail . primOpOcc) allThePrimOps ++
   [ AvailTC occ [occ] |
     n <- funTyCon : primTyCons, let occ = nameOccName (tyConName n) 
   ]
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

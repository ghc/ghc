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
	
	-- Primop RdrNames
	eqH_Char_RDR,   ltH_Char_RDR,   eqH_Word_RDR,  ltH_Word_RDR, 
	eqH_Addr_RDR,   ltH_Addr_RDR,   eqH_Float_RDR, ltH_Float_RDR, 
	eqH_Double_RDR, ltH_Double_RDR, eqH_Int_RDR,   ltH_Int_RDR,
	geH_RDR, leH_RDR, minusH_RDR, tagToEnumH_RDR, 

	-- Random other things
	maybeCharLikeCon, maybeIntLikeCon,

	-- Class categories
	isCcallishClass, isCreturnableClass, isNoDictClass, 
	isNumericClass, isStandardClass

    ) where

#include "HsVersions.h"

-- friends:
import PrelNames	-- Prelude module names

import PrimOp		( PrimOp(..), allThePrimOps, primOpRdrName )
import DataCon		( DataCon, dataConId, dataConWrapId )
import MkId		( mkPrimOpId, wiredInIds )
import MkId		-- All of it, for re-export
import TysPrim		( primTyCons )
import TysWiredIn	( wiredInTyCons )
import HscTypes 	( TyThing(..), TypeEnv, mkTypeEnv )

-- others:
import TyCon		( tyConDataConsIfAvailable, TyCon )
import Class	 	( Class, classKey )
import Type		( funTyCon )
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
    [		-- Wired in TyCons
	  concat (map wiredInTyConThings ([funTyCon] ++ primTyCons ++ wiredInTyCons))

		-- Wired in Ids
	, map AnId wiredInIds

		-- PrimOps
	, map (AnId . mkPrimOpId) allThePrimOps
    ]

wiredInTyConThings :: TyCon -> [TyThing]
wiredInTyConThings tc
   = ATyCon tc : [ AnId n | dc <- tyConDataConsIfAvailable tc, 
                            n  <- [dataConId dc, dataConWrapId dc] ]
			-- Synonyms return empty list of constructors

wiredInThingEnv :: TypeEnv
wiredInThingEnv = mkTypeEnv wiredInThings
\end{code}

We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.


%************************************************************************
%*									*
\subsection{RdrNames for the primops}
%*									*
%************************************************************************

These can't be in PrelNames, because we get the RdrName from the PrimOp,
which is above PrelNames in the module hierarchy.

\begin{code}
eqH_Char_RDR	= primOpRdrName CharEqOp
ltH_Char_RDR	= primOpRdrName CharLtOp
eqH_Word_RDR	= primOpRdrName WordEqOp
ltH_Word_RDR	= primOpRdrName WordLtOp
eqH_Addr_RDR	= primOpRdrName AddrEqOp
ltH_Addr_RDR	= primOpRdrName AddrLtOp
eqH_Float_RDR	= primOpRdrName FloatEqOp
ltH_Float_RDR	= primOpRdrName FloatLtOp
eqH_Double_RDR	= primOpRdrName DoubleEqOp
ltH_Double_RDR	= primOpRdrName DoubleLtOp
eqH_Int_RDR	= primOpRdrName IntEqOp
ltH_Int_RDR	= primOpRdrName IntLtOp
geH_RDR		= primOpRdrName IntGeOp
leH_RDR		= primOpRdrName IntLeOp
minusH_RDR	= primOpRdrName IntSubOp

tagToEnumH_RDR	= primOpRdrName TagToEnumOp
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

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
module PrelInfo (
	module PrelNames,
	module MkId,

	builtinNames, 	-- Names of things whose *unique* must be known, but 
			-- that is all. If something is in here, you know that
			-- if it's used at all then it's Name will be just as
			-- it is here, unique and all.  Includes all the 



	
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
import MkId		-- Ditto
import PrelNames	-- Prelude module names

import PrimOp		( PrimOp(..), allThePrimOps, primOpRdrName )
import DataCon		( DataCon, dataConId, dataConWrapId )
import TysPrim		-- TYPES
import TysWiredIn

-- others:
import RdrName		( RdrName )
import Name		( Name, mkKnownKeyGlobal, getName )
import TyCon		( tyConDataConsIfAvailable, TyCon )
import Class	 	( Class, classKey )
import Type		( funTyCon )
import Bag
import BasicTypes	( Boxity(..) )
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
builtinNames :: Bag Name
builtinNames
  = unionManyBags
	[	-- Wired in TyCons
	  unionManyBags (map getTyConNames wired_in_tycons)

		-- Wired in Ids
	, listToBag (map getName wiredInIds)

		-- PrimOps
	, listToBag (map (getName . mkPrimOpId) allThePrimOps)

		-- Other names with magic keys
	, listToBag (map mkKnownKeyGlobal knownKeyRdrNames)
	]
\end{code}


\begin{code}
getTyConNames :: TyCon -> Bag Name
getTyConNames tycon
    = getName tycon `consBag` 
      unionManyBags (map get_data_con_names (tyConDataConsIfAvailable tycon))
	-- Synonyms return empty list of constructors
    where
      get_data_con_names dc = listToBag [getName (dataConId dc),	-- Worker
					 getName (dataConWrapId dc)]	-- Wrapper
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
\subsection{Wired in TyCons}
%*									*
%************************************************************************

\begin{code}
wired_in_tycons = [funTyCon] ++
		  prim_tycons ++
		  tuple_tycons ++
		  unboxed_tuple_tycons ++
		  data_tycons

prim_tycons
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

tuple_tycons = unitTyCon : [tupleTyCon Boxed i | i <- [2..37] ]
unboxed_tuple_tycons = [tupleTyCon Unboxed i | i <- [1..37] ]

data_tycons
  = [ addrTyCon
    , boolTyCon
    , charTyCon
    , doubleTyCon
    , floatTyCon
    , intTyCon
    , integerTyCon
    , listTyCon
    , wordTyCon
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
isCcallishClass, isCreturnableClass, isNoDictClass, 
  isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys
isCcallishClass	   clas = classKey clas `is_elem` cCallishClassKeys
isCreturnableClass clas = classKey clas == cReturnableClassKey
isNoDictClass      clas = classKey clas `is_elem` noDictClassKeys
is_elem = isIn "is_X_Class"
\end{code}

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

	derivingOccurrences, 	-- For a given class C, this tells what other 
	derivableClassKeys,	-- things are needed as a result of a 
				-- deriving(C) clause


	
	-- Primop RdrNames
	eqH_Char_RDR,   ltH_Char_RDR,   eqH_Word_RDR,  ltH_Word_RDR, 
	eqH_Addr_RDR,   ltH_Addr_RDR,   eqH_Float_RDR, ltH_Float_RDR, 
	eqH_Double_RDR, ltH_Double_RDR, eqH_Int_RDR,   ltH_Int_RDR,
	geH_RDR, leH_RDR, minusH_RDR, tagToEnumH_RDR, 

	-- Random other things
	maybeCharLikeCon, maybeIntLikeCon,
	needsDataDeclCtxtClassKeys, cCallishClassKeys, cCallishTyKeys, 
	isNoDictClass, isNumericClass, isStandardClass, isCcallishClass, 
	isCreturnableClass, numericTyKeys, fractionalClassKeys,

    ) where

#include "HsVersions.h"

-- friends:
import MkId		-- Ditto
import PrelNames	-- Prelude module names

import PrimOp		( PrimOp(..), allThePrimOps, primOpRdrName )
import DataCon		( DataCon, dataConId, dataConWrapId )
import PrimRep		( PrimRep(..) )
import TysPrim		-- TYPES
import TysWiredIn

-- others:
import RdrName		( RdrName )
import Var		( varUnique, Id )
import Name		( Name, OccName, Provenance(..), 
			  NameSpace, tcName, clsName, varName, dataName,
			  mkKnownKeyGlobal,
			  getName, mkGlobalName, nameRdrName
			)
import Class		( Class, classKey )
import TyCon		( tyConDataConsIfAvailable, TyCon )
import Type		( funTyCon )
import Bag
import BasicTypes	( Boxity(..) )
import Unique		-- *Key stuff
import UniqFM		( UniqFM, listToUFM )
import Util		( isIn )
import Panic		( panic )
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
	, listToBag knownKeyNames
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

Ids, Synonyms, Classes and ClassOps with builtin keys. 

\begin{code}
knownKeyNames :: [Name]
knownKeyNames
  = map mkKnownKeyGlobal
    [
	-- Type constructors (synonyms especially)
      (ioTyCon_RDR,		ioTyConKey)
    , (main_RDR,		mainKey)
    , (orderingTyCon_RDR,  	orderingTyConKey)
    , (rationalTyCon_RDR,  	rationalTyConKey)
    , (ratioDataCon_RDR,   	ratioDataConKey)
    , (ratioTyCon_RDR,     	ratioTyConKey)
    , (byteArrayTyCon_RDR, 	byteArrayTyConKey)
    , (mutableByteArrayTyCon_RDR, mutableByteArrayTyConKey)
    , (foreignObjTyCon_RDR, 	foreignObjTyConKey)
    , (stablePtrTyCon_RDR, 	stablePtrTyConKey)
    , (stablePtrDataCon_RDR,    stablePtrDataConKey)

	--  Classes.  *Must* include:
	--  	classes that are grabbed by key (e.g., eqClassKey)
	--  	classes in "Class.standardClassKeys" (quite a few)
    , (eqClass_RDR,		eqClassKey)		-- mentioned, derivable
    , (ordClass_RDR,		ordClassKey)		-- derivable
    , (boundedClass_RDR, 	boundedClassKey)	-- derivable
    , (numClass_RDR, 		numClassKey)		-- mentioned, numeric
    , (enumClass_RDR,		enumClassKey)		-- derivable
    , (monadClass_RDR,		monadClassKey)
    , (monadPlusClass_RDR,	monadPlusClassKey)
    , (functorClass_RDR,	functorClassKey)
    , (showClass_RDR, 		showClassKey)		-- derivable
    , (realClass_RDR, 		realClassKey)		-- numeric
    , (integralClass_RDR,	integralClassKey)	-- numeric
    , (fractionalClass_RDR,	fractionalClassKey)	-- numeric
    , (floatingClass_RDR,	floatingClassKey)	-- numeric
    , (realFracClass_RDR,	realFracClassKey)	-- numeric
    , (realFloatClass_RDR,	realFloatClassKey)	-- numeric
    , (readClass_RDR,		readClassKey)		-- derivable
    , (ixClass_RDR,		ixClassKey)		-- derivable (but it isn't Prelude.Ix; hmmm)
    , (ccallableClass_RDR, 	cCallableClassKey)	-- mentioned, ccallish
    , (creturnableClass_RDR, 	cReturnableClassKey)	-- mentioned, ccallish

	-- ClassOps 
    , (fromInt_RDR,		fromIntClassOpKey)
    , (fromInteger_RDR,		fromIntegerClassOpKey)
    , (ge_RDR,			geClassOpKey) 
    , (minus_RDR,		minusClassOpKey)
    , (enumFrom_RDR,		enumFromClassOpKey)
    , (enumFromThen_RDR,	enumFromThenClassOpKey)
    , (enumFromTo_RDR,		enumFromToClassOpKey)
    , (enumFromThenTo_RDR,	enumFromThenToClassOpKey)
    , (fromEnum_RDR,		fromEnumClassOpKey)
    , (toEnum_RDR,		toEnumClassOpKey)
    , (eq_RDR,			eqClassOpKey)
    , (thenM_RDR,		thenMClassOpKey)
    , (returnM_RDR,		returnMClassOpKey)
    , (failM_RDR,		failMClassOpKey)
    , (fromRational_RDR,	fromRationalClassOpKey)
    
    , (deRefStablePtr_RDR,	deRefStablePtrIdKey)
    , (makeStablePtr_RDR,	makeStablePtrIdKey)
    , (bindIO_RDR,		bindIOIdKey)
    , (returnIO_RDR,		returnIOIdKey)
    , (addr2Integer_RDR,	addr2IntegerIdKey)

	-- Strings and lists
    , (map_RDR,			mapIdKey)
    , (append_RDR,		appendIdKey)
    , (unpackCString_RDR, 	unpackCStringIdKey)
    , (unpackCString2_RDR,  	unpackCString2IdKey)
    , (unpackCStringAppend_RDR,	unpackCStringAppendIdKey)
    , (unpackCStringFoldr_RDR,	unpackCStringFoldrIdKey)

	-- List operations
    , (concat_RDR,		concatIdKey)
    , (filter_RDR,		filterIdKey)
    , (zip_RDR,			zipIdKey)
    , (foldr_RDR,		foldrIdKey)
    , (build_RDR,		buildIdKey)
    , (augment_RDR,		augmentIdKey)

	-- FFI primitive types that are not wired-in.
    , (int8TyCon_RDR,           int8TyConKey)
    , (int16TyCon_RDR,          int16TyConKey)
    , (int32TyCon_RDR,          int32TyConKey)
    , (int64TyCon_RDR,          int64TyConKey)
    , (word8TyCon_RDR,          word8TyConKey)
    , (word16TyCon_RDR,         word16TyConKey)
    , (word32TyCon_RDR,         word32TyConKey)
    , (word64TyCon_RDR,         word64TyConKey)

	-- Others
    , (otherwiseId_RDR,		otherwiseIdKey)
    , (assert_RDR,		assertIdKey)
    , (runSTRep_RDR,		runSTRepIdKey)
    ]
\end{code}

ToDo: make it do the ``like'' part properly (as in 0.26 and before).

\begin{code}
maybeCharLikeCon, maybeIntLikeCon :: DataCon -> Bool
maybeCharLikeCon con = con `hasKey` charDataConKey
maybeIntLikeCon  con = con `hasKey` intDataConKey
\end{code}

%************************************************************************
%*									*
\subsection[Class-std-groups]{Standard groups of Prelude classes}
%*									*
%************************************************************************

@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).

@derivingOccurrences@ maps a class name to a list of the (qualified) occurrences
that will be mentioned by  the derived code for the class when it is later generated.
We don't need to put in things that are WiredIn (because they are already mapped to their
correct name by the @NameSupply@.  The class itself, and all its class ops, is
already flagged as an occurrence so we don't need to mention that either.

@derivingOccurrences@ has an item for every derivable class, even if that item is empty,
because we treat lookup failure as indicating that the class is illegal in a deriving clause.

\begin{code}
derivingOccurrences :: UniqFM [RdrName]
derivingOccurrences = listToUFM deriving_occ_info

derivableClassKeys  = map fst deriving_occ_info

deriving_occ_info
  = [ (eqClassKey, 	[intTyCon_RDR, and_RDR, not_RDR])
    , (ordClassKey, 	[intTyCon_RDR, compose_RDR, eqTag_RDR])
				-- EQ (from Ordering) is needed to force in the constructors
				-- as well as the type constructor.
    , (enumClassKey, 	[intTyCon_RDR, eq_RDR, ge_RDR, and_RDR, map_RDR, plus_RDR, showsPrec_RDR, append_RDR]) 
				-- The last two Enum deps are only used to produce better
				-- error msgs for derived toEnum methods.
    , (boundedClassKey,	[intTyCon_RDR])
    , (showClassKey,	[intTyCon_RDR, numClass_RDR, ordClass_RDR, compose_RDR, showString_RDR, 
			 showParen_RDR, showSpace_RDR, showList___RDR])
    , (readClassKey,	[intTyCon_RDR, numClass_RDR, ordClass_RDR, append_RDR, 
			 lex_RDR, readParen_RDR, readList___RDR, thenM_RDR])
			     -- returnM (and the rest of the Monad class decl) 
			     -- will be forced in as result of depending
			     -- on thenM.   -- SOF 1/99
    , (ixClassKey,	[intTyCon_RDR, numClass_RDR, and_RDR, map_RDR, enumFromTo_RDR, 
			 returnM_RDR, failM_RDR])
			     -- the last two are needed to force returnM, thenM and failM
			     -- in before typechecking the list(monad) comprehension
			     -- generated for derived Ix instances (range method)
			     -- of single constructor types.  -- SOF 8/97
    ]
	-- intTyCon: Practically any deriving needs Int, either for index calculations, 
	--		or for taggery.
	-- ordClass: really it's the methods that are actually used.
	-- numClass: for Int literals
\end{code}


NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.

\begin{code}
isCcallishClass, isCreturnableClass, isNoDictClass, 
  isNumericClass, isStandardClass :: Class -> Bool

isFractionalClass  clas = classKey clas `is_elem` fractionalClassKeys
isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys
isCcallishClass	   clas = classKey clas `is_elem` cCallishClassKeys
isCreturnableClass clas = classKey clas == cReturnableClassKey
isNoDictClass      clas = classKey clas `is_elem` noDictClassKeys
is_elem = isIn "is_X_Class"

numericClassKeys =
	[ numClassKey
    	, realClassKey
    	, integralClassKey
	]
	++ fractionalClassKeys

fractionalClassKeys = 
    	[ fractionalClassKey
    	, floatingClassKey
    	, realFracClassKey
    	, realFloatClassKey
    	]

	-- the strictness analyser needs to know about numeric types
	-- (see SaAbsInt.lhs)
numericTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, intTyConKey
	, integerTyConKey
	, doubleTyConKey
	, floatTyConKey
	]

needsDataDeclCtxtClassKeys = -- see comments in TcDeriv
  	[ readClassKey
    	]

cCallishClassKeys = 
	[ cCallableClassKey
	, cReturnableClassKey
	]

	-- Renamer always imports these data decls replete with constructors
	-- so that desugarer can always see their constructors.  Ugh!
cCallishTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, byteArrayTyConKey
	, mutableByteArrayTyConKey
	, foreignObjTyConKey
	, stablePtrTyConKey
	, int8TyConKey
	, int16TyConKey
	, int32TyConKey
	, int64TyConKey
	, word8TyConKey
	, word16TyConKey
	, word32TyConKey
	, word64TyConKey
	]

standardClassKeys
  = derivableClassKeys ++ numericClassKeys ++ cCallishClassKeys
    --
    -- We have to have "CCallable" and "CReturnable" in the standard
    -- classes, so that if you go...
    --
    --	    _ccall_ foo ... 93{-numeric literal-} ...
    --
    -- ... it can do The Right Thing on the 93.

noDictClassKeys 	-- These classes are used only for type annotations;
			-- they are not implemented by dictionaries, ever.
  = cCallishClassKeys
\end{code}


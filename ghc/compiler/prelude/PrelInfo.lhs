%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
module PrelInfo (
	-- finite maps for built-in things (for the renamer and typechecker):
	builtinNames, derivingOccurrences,
	BuiltinNames,

	maybeCharLikeTyCon, maybeIntLikeTyCon,

	eq_RDR, ne_RDR, le_RDR, lt_RDR, ge_RDR, gt_RDR, max_RDR, min_RDR, 
	compare_RDR, minBound_RDR, maxBound_RDR, enumFrom_RDR, enumFromTo_RDR,
	enumFromThen_RDR, enumFromThenTo_RDR, fromEnum_RDR, toEnum_RDR, 
	ratioDataCon_RDR, range_RDR, index_RDR, inRange_RDR, readsPrec_RDR,
	readList_RDR, showsPrec_RDR, showList_RDR, plus_RDR, times_RDR,
	ltTag_RDR, eqTag_RDR, gtTag_RDR, eqH_Char_RDR, ltH_Char_RDR, 
	eqH_Word_RDR, ltH_Word_RDR, eqH_Addr_RDR, ltH_Addr_RDR, eqH_Float_RDR,
	ltH_Float_RDR, eqH_Double_RDR, ltH_Double_RDR, eqH_Int_RDR, 
	ltH_Int_RDR, geH_RDR, leH_RDR, minusH_RDR, false_RDR, true_RDR,
	and_RDR, not_RDR, append_RDR, map_RDR, compose_RDR, mkInt_RDR,
	error_RDR, assert_RDR,
	showString_RDR, showParen_RDR, readParen_RDR, lex_RDR,
	showSpace_RDR, showList___RDR, readList___RDR, negate_RDR,

	numClass_RDR, fractionalClass_RDR, eqClass_RDR, 
	ccallableClass_RDR, creturnableClass_RDR,
	monadZeroClass_RDR, enumClass_RDR, evalClass_RDR, ordClass_RDR,
	ioDataCon_RDR, ioOkDataCon_RDR,

	main_NAME, allClass_NAME, ioTyCon_NAME,

	needsDataDeclCtxtClassKeys, cCallishClassKeys, cCallishTyKeys, isNoDictClass,
	isNumericClass, isStandardClass, isCcallishClass
    ) where

#include "HsVersions.h"

import IdUtils ( primOpName )

-- friends:
import PrelMods		-- Prelude module names
import PrelVals		-- VALUES
import PrimOp		( PrimOp(..), allThePrimOps )
import PrimRep		( PrimRep(..) )
import TysPrim		-- TYPES
import TysWiredIn

-- others:
import RdrHsSyn		( RdrName(..), varQual, tcQual, qual )
import BasicTypes	( IfaceFlavour )
import Id		( GenId, Id )
import Name		( Name, OccName(..), Provenance(..),
			  getName, mkGlobalName, modAndOcc
			)
import Class		( Class, classKey )
import TyCon		( tyConDataCons, mkFunTyCon, TyCon )
import Type
import Bag
import Unique		-- *Key stuff
import UniqFM		( UniqFM, listToUFM ) 
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
type BuiltinNames = Bag Name

builtinNames :: BuiltinNames
builtinNames
  = 	-- Wired in TyCons
    unionManyBags (map getTyConNames wired_in_tycons)	`unionBags`

	-- Wired in Ids
    listToBag (map getName wired_in_ids)		`unionBags`

	-- PrimOps
    listToBag (map (getName.primOpName) allThePrimOps)	`unionBags`

	-- Other names with magic keys
    listToBag knownKeyNames
\end{code}


\begin{code}
getTyConNames :: TyCon -> Bag Name
getTyConNames tycon
    =  getName tycon `consBag` listToBag (map getName (tyConDataCons tycon))
	-- Synonyms return empty list of constructors
\end{code}


We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

%************************************************************************
%*									*
\subsection{Wired in TyCons}
%*									*
%************************************************************************


\begin{code}
wired_in_tycons = [mkFunTyCon] ++
		  prim_tycons ++
		  tuple_tycons ++
		  data_tycons

prim_tycons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , foreignObjPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , synchVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , statePrimTyCon
    , wordPrimTyCon
    ]

tuple_tycons = unitTyCon : [tupleTyCon i | i <- [2..37] ]


data_tycons
  = [ listTyCon
    , addrTyCon
    , boolTyCon
    , charTyCon
    , doubleTyCon
    , floatTyCon
    , foreignObjTyCon
    , intTyCon
    , integerTyCon
    , liftTyCon
    , return2GMPsTyCon
    , returnIntAndGMPTyCon
    , stTyCon
    , stRetTyCon
    , stablePtrTyCon
    , stateAndAddrPrimTyCon
    , stateAndArrayPrimTyCon
    , stateAndByteArrayPrimTyCon
    , stateAndCharPrimTyCon
    , stateAndDoublePrimTyCon
    , stateAndFloatPrimTyCon
    , stateAndForeignObjPrimTyCon
    , stateAndIntPrimTyCon
    , stateAndMutableArrayPrimTyCon
    , stateAndMutableByteArrayPrimTyCon
    , stateAndPtrPrimTyCon
    , stateAndStablePtrPrimTyCon
    , stateAndSynchVarPrimTyCon
    , stateAndWordPrimTyCon
    , voidTyCon
    , wordTyCon
    ]

min_nonprim_tycon_list 	-- used w/ HideMostBuiltinNames
  = [ boolTyCon
    , charTyCon
    , intTyCon
    , floatTyCon
    , doubleTyCon
    , integerTyCon
    , liftTyCon
    , return2GMPsTyCon	-- ADR asked for these last two (WDP 94/11)
    , returnIntAndGMPTyCon
    ]
\end{code}

%************************************************************************
%*									*
\subsection{Wired in Ids}
%*									*
%************************************************************************

The WiredIn Ids ...
ToDo: Some of these should be moved to id_keys_infos!

\begin{code}
wired_in_ids
  = [ aBSENT_ERROR_ID
    , augmentId
    , buildId
    , eRROR_ID
    , foldlId
    , foldrId
    , iRREFUT_PAT_ERROR_ID
    , integerMinusOneId
    , integerPlusOneId
    , integerPlusTwoId
    , integerZeroId
    , nON_EXHAUSTIVE_GUARDS_ERROR_ID
    , nO_DEFAULT_METHOD_ERROR_ID
    , nO_EXPLICIT_METHOD_ERROR_ID
    , pAR_ERROR_ID
    , pAT_ERROR_ID
    , packStringForCId
    , rEC_CON_ERROR_ID
    , rEC_UPD_ERROR_ID
    , realWorldPrimId
    , tRACE_ID
    , unpackCString2Id
    , unpackCStringAppendId
    , unpackCStringFoldrId
    , unpackCStringId
    , voidId

--  , copyableId
--  , forkId
--  , noFollowId
--    , parAtAbsId
--    , parAtForNowId
--    , parAtId
--    , parAtRelId
--    , parGlobalId
--    , parId
--    , parLocalId
--    , seqId
    ]
\end{code}


%************************************************************************
%*									*
\subsection{Built-in keys}
%*									*
%************************************************************************

Ids, Synonyms, Classes and ClassOps with builtin keys. 

\begin{code}
mkKnownKeyGlobal :: (RdrName, Unique) -> Name
mkKnownKeyGlobal (Qual mod occ hif, uniq)
  = mkGlobalName uniq mod occ NoProvenance

allClass_NAME    = mkKnownKeyGlobal (allClass_RDR,   allClassKey)
ioTyCon_NAME	 = mkKnownKeyGlobal (ioTyCon_RDR,    ioTyConKey)
main_NAME	 = mkKnownKeyGlobal (main_RDR,	     mainKey)

knownKeyNames :: [Name]
knownKeyNames
  = [main_NAME, allClass_NAME, ioTyCon_NAME]
    ++
    map mkKnownKeyGlobal
    [
	-- Type constructors (synonyms especially)
      (ioOkDataCon_RDR,    ioOkDataConKey)
    , (orderingTyCon_RDR,  orderingTyConKey)
    , (rationalTyCon_RDR,  rationalTyConKey)
    , (ratioDataCon_RDR,   ratioDataConKey)
    , (ratioTyCon_RDR,     ratioTyConKey)
    , (byteArrayTyCon_RDR, byteArrayTyConKey)
    , (mutableByteArrayTyCon_RDR, mutableByteArrayTyConKey)


	--  Classes.  *Must* include:
	--  	classes that are grabbed by key (e.g., eqClassKey)
	--  	classes in "Class.standardClassKeys" (quite a few)
    , (eqClass_RDR,		eqClassKey)		-- mentioned, derivable
    , (ordClass_RDR,		ordClassKey)		-- derivable
    , (evalClass_RDR,		evalClassKey)		-- mentioned
    , (boundedClass_RDR, 	boundedClassKey)	-- derivable
    , (numClass_RDR, 		numClassKey)		-- mentioned, numeric
    , (enumClass_RDR,		enumClassKey)		-- derivable
    , (monadClass_RDR,		monadClassKey)
    , (monadZeroClass_RDR,	monadZeroClassKey)
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
    , (zeroM_RDR,		zeroClassOpKey)
    , (fromRational_RDR,	fromRationalClassOpKey)

	-- Others
    , (otherwiseId_RDR,		otherwiseIdKey)
    ]
\end{code}

ToDo: make it do the ``like'' part properly (as in 0.26 and before).

\begin{code}
maybeCharLikeTyCon tc = if (uniqueOf tc == charDataConKey) then Just charDataCon else Nothing
maybeIntLikeTyCon  tc = if (uniqueOf tc == intDataConKey)  then Just intDataCon  else Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Commonly-used RdrNames}
%*									*
%************************************************************************

These RdrNames are not really "built in", but some parts of the compiler
(notably the deriving mechanism) need to mention their names, and it's convenient
to write them all down in one place.

\begin{code}
prelude_primop op = qual (modAndOcc (primOpName op))

intTyCon_RDR		= qual (modAndOcc intTyCon)
ioTyCon_RDR		= tcQual (iO_BASE,   SLIT("IO"))
ioDataCon_RDR  	   	= varQual (iO_BASE,   SLIT("IO"))
ioOkDataCon_RDR		= varQual (iO_BASE,   SLIT("IOok"))
orderingTyCon_RDR	= tcQual (pREL_BASE, SLIT("Ordering"))
rationalTyCon_RDR	= tcQual (pREL_NUM,  SLIT("Rational"))
ratioTyCon_RDR		= tcQual (pREL_NUM,  SLIT("Ratio"))
ratioDataCon_RDR	= varQual (pREL_NUM, SLIT(":%"))

byteArrayTyCon_RDR		= tcQual (aRR_BASE,  SLIT("ByteArray"))
mutableByteArrayTyCon_RDR	= tcQual (aRR_BASE,  SLIT("MutableByteArray"))

allClass_RDR		= tcQual (gHC__,     SLIT("All"))
eqClass_RDR		= tcQual (pREL_BASE, SLIT("Eq"))
ordClass_RDR		= tcQual (pREL_BASE, SLIT("Ord"))
evalClass_RDR 		= tcQual (pREL_BASE, SLIT("Eval"))
boundedClass_RDR	= tcQual (pREL_BASE, SLIT("Bounded"))
numClass_RDR		= tcQual (pREL_BASE, SLIT("Num"))
enumClass_RDR 		= tcQual (pREL_BASE, SLIT("Enum"))
monadClass_RDR		= tcQual (pREL_BASE, SLIT("Monad"))
monadZeroClass_RDR	= tcQual (pREL_BASE, SLIT("MonadZero"))
monadPlusClass_RDR	= tcQual (pREL_BASE, SLIT("MonadPlus"))
functorClass_RDR	= tcQual (pREL_BASE, SLIT("Functor"))
showClass_RDR		= tcQual (pREL_BASE, SLIT("Show"))
realClass_RDR		= tcQual (pREL_NUM,  SLIT("Real"))
integralClass_RDR	= tcQual (pREL_NUM,  SLIT("Integral"))
fractionalClass_RDR	= tcQual (pREL_NUM,  SLIT("Fractional"))
floatingClass_RDR	= tcQual (pREL_NUM,  SLIT("Floating"))
realFracClass_RDR	= tcQual (pREL_NUM,  SLIT("RealFrac"))
realFloatClass_RDR	= tcQual (pREL_NUM,  SLIT("RealFloat"))
readClass_RDR		= tcQual (pREL_READ, SLIT("Read"))
ixClass_RDR		= tcQual (iX,	     SLIT("Ix"))
ccallableClass_RDR	= tcQual (gHC__,   SLIT("CCallable"))
creturnableClass_RDR	= tcQual (gHC__,   SLIT("CReturnable"))

fromInt_RDR	   = varQual (pREL_BASE, SLIT("fromInt"))
fromInteger_RDR	   = varQual (pREL_BASE, SLIT("fromInteger"))
minus_RDR	   = varQual (pREL_BASE, SLIT("-"))
toEnum_RDR	   = varQual (pREL_BASE, SLIT("toEnum"))
fromEnum_RDR	   = varQual (pREL_BASE, SLIT("fromEnum"))
enumFrom_RDR	   = varQual (pREL_BASE, SLIT("enumFrom"))
enumFromTo_RDR	   = varQual (pREL_BASE, SLIT("enumFromTo"))
enumFromThen_RDR   = varQual (pREL_BASE, SLIT("enumFromThen"))
enumFromThenTo_RDR = varQual (pREL_BASE, SLIT("enumFromThenTo"))

thenM_RDR	   = varQual (pREL_BASE, SLIT(">>="))
returnM_RDR	   = varQual (pREL_BASE, SLIT("return"))
zeroM_RDR	   = varQual (pREL_BASE, SLIT("zero"))
fromRational_RDR   = varQual (pREL_NUM,  SLIT("fromRational"))

negate_RDR	   = varQual (pREL_BASE, SLIT("negate"))
eq_RDR		   = varQual (pREL_BASE, SLIT("=="))
ne_RDR		   = varQual (pREL_BASE, SLIT("/="))
le_RDR		   = varQual (pREL_BASE, SLIT("<="))
lt_RDR		   = varQual (pREL_BASE, SLIT("<"))
ge_RDR		   = varQual (pREL_BASE, SLIT(">="))
gt_RDR		   = varQual (pREL_BASE, SLIT(">"))
ltTag_RDR	   = varQual (pREL_BASE,  SLIT("LT"))
eqTag_RDR	   = varQual (pREL_BASE,  SLIT("EQ"))
gtTag_RDR	   = varQual (pREL_BASE,  SLIT("GT"))
max_RDR		   = varQual (pREL_BASE, SLIT("max"))
min_RDR		   = varQual (pREL_BASE, SLIT("min"))
compare_RDR	   = varQual (pREL_BASE, SLIT("compare"))
minBound_RDR	   = varQual (pREL_BASE, SLIT("minBound"))
maxBound_RDR	   = varQual (pREL_BASE, SLIT("maxBound"))
false_RDR	   = varQual (pREL_BASE,  SLIT("False"))
true_RDR	   = varQual (pREL_BASE,  SLIT("True"))
and_RDR		   = varQual (pREL_BASE,  SLIT("&&"))
not_RDR		   = varQual (pREL_BASE,  SLIT("not"))
compose_RDR	   = varQual (pREL_BASE, SLIT("."))
append_RDR	   = varQual (pREL_BASE, SLIT("++"))
map_RDR		   = varQual (pREL_BASE, SLIT("map"))

showList___RDR     = varQual (pREL_BASE,  SLIT("showList__"))
showsPrec_RDR	   = varQual (pREL_BASE, SLIT("showsPrec"))
showList_RDR	   = varQual (pREL_BASE, SLIT("showList"))
showSpace_RDR	   = varQual (pREL_BASE,  SLIT("showSpace"))
showString_RDR	   = varQual (pREL_BASE, SLIT("showString"))
showParen_RDR	   = varQual (pREL_BASE, SLIT("showParen"))

range_RDR	   = varQual (iX,   SLIT("range"))
index_RDR	   = varQual (iX,   SLIT("index"))
inRange_RDR	   = varQual (iX,   SLIT("inRange"))

readsPrec_RDR	   = varQual (pREL_READ, SLIT("readsPrec"))
readList_RDR	   = varQual (pREL_READ, SLIT("readList"))
readParen_RDR	   = varQual (pREL_READ, SLIT("readParen"))
lex_RDR		   = varQual (pREL_READ,  SLIT("lex"))
readList___RDR     = varQual (pREL_READ,  SLIT("readList__"))

plus_RDR	   = varQual (pREL_BASE, SLIT("+"))
times_RDR	   = varQual (pREL_BASE, SLIT("*"))
mkInt_RDR	   = varQual (pREL_BASE, SLIT("I#"))

error_RDR	   = varQual (gHC_ERR, SLIT("error"))
assert_RDR         = varQual (gHC_ERR, SLIT("assert__"))

eqH_Char_RDR	= prelude_primop CharEqOp
ltH_Char_RDR	= prelude_primop CharLtOp
eqH_Word_RDR	= prelude_primop WordEqOp
ltH_Word_RDR	= prelude_primop WordLtOp
eqH_Addr_RDR	= prelude_primop AddrEqOp
ltH_Addr_RDR	= prelude_primop AddrLtOp
eqH_Float_RDR	= prelude_primop FloatEqOp
ltH_Float_RDR	= prelude_primop FloatLtOp
eqH_Double_RDR	= prelude_primop DoubleEqOp
ltH_Double_RDR	= prelude_primop DoubleLtOp
eqH_Int_RDR	= prelude_primop IntEqOp
ltH_Int_RDR	= prelude_primop IntLtOp
geH_RDR		= prelude_primop IntGeOp
leH_RDR		= prelude_primop IntLeOp
minusH_RDR	= prelude_primop IntSubOp

main_RDR	= varQual (mAIN,     SLIT("main"))

otherwiseId_RDR = varQual (pREL_BASE, SLIT("otherwise"))
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
    , (enumClassKey, 	[intTyCon_RDR, map_RDR])
    , (evalClassKey,	[intTyCon_RDR])
    , (boundedClassKey,	[intTyCon_RDR])
    , (showClassKey,	[intTyCon_RDR, numClass_RDR, ordClass_RDR, compose_RDR, showString_RDR, 
			 showParen_RDR, showSpace_RDR, showList___RDR])
    , (readClassKey,	[intTyCon_RDR, numClass_RDR, ordClass_RDR, append_RDR, 
			 lex_RDR, readParen_RDR, readList___RDR])
    , (ixClassKey,	[intTyCon_RDR, numClass_RDR, and_RDR, map_RDR, enumFromTo_RDR, 
			 returnM_RDR, zeroM_RDR])
			     -- the last two are needed to force returnM, thenM and zeroM
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
isCcallishClass, isNoDictClass, isNumericClass, isStandardClass :: Class -> Bool

isNumericClass   clas = classKey clas `is_elem` numericClassKeys
isStandardClass  clas = classKey clas `is_elem` standardClassKeys
isCcallishClass	 clas = classKey clas `is_elem` cCallishClassKeys
isNoDictClass    clas = classKey clas `is_elem` noDictClassKeys
is_elem = isIn "is_X_Class"

numericClassKeys
  = [ numClassKey
    , realClassKey
    , integralClassKey
    , fractionalClassKey
    , floatingClassKey
    , realFracClassKey
    , realFloatClassKey
    ]

needsDataDeclCtxtClassKeys -- see comments in TcDeriv
  = [ readClassKey
    ]

cCallishClassKeys = [ cCallableClassKey, cReturnableClassKey ]

	-- Renamer always imports these data decls replete with constructors
	-- so that desugarer can always see the constructor.  Ugh!
cCallishTyKeys = [ addrTyConKey, wordTyConKey, byteArrayTyConKey, 
		   mutableByteArrayTyConKey, foreignObjTyConKey ]

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
	-- I used to think that class Eval belonged in here, but
	-- we really want functions with type (Eval a => ...) and that
	-- means that we really want to pass a placeholder for an Eval
	-- dictionary.  The unit tuple is what we'll get if we leave things
	-- alone, and that'll do for now.  Could arrange to drop that parameter
	-- in the end.
\end{code}

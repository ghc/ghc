%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelNames]{Definitions of prelude modules and names}


Nota Bene: all Names defined in here should come from the base package

* ModuleNames for prelude modules, 
	e.g.	pREL_BASE_Name :: ModuleName

* Modules for prelude modules
	e.g.	pREL_Base :: Module

* Uniques for Ids, DataCons, TyCons and Classes that the compiler 
  "knows about" in some way
	e.g.	intTyConKey :: Unique
		minusClassOpKey :: Unique

* Names for Ids, DataCons, TyCons and Classes that the compiler 
  "knows about" in some way
	e.g.	intTyConName :: Name
		minusName    :: Name
  One of these Names contains
	(a) the module and occurrence name of the thing
	(b) its Unique
  The may way the compiler "knows about" one of these things is
  where the type checker or desugarer needs to look it up. For
  example, when desugaring list comprehensions the desugarer
  needs to conjure up 'foldr'.  It does this by looking up
  foldrName in the environment.

* RdrNames for Ids, DataCons etc that the compiler may emit into
  generated code (e.g. for deriving).  It's not necessary to know
  the uniques for these guys, only their names


\begin{code}
module PrelNames (
	Unique, Uniquable(..), hasKey, 	-- Re-exported for convenience

	-----------------------------------------------------------
	module PrelNames,	-- A huge bunch of (a) Names,  e.g. intTyConName
				--		   (b) Uniques e.g. intTyConKey
				--		   (c) Groups of classes and types
				--		   (d) miscellaneous things
				-- So many that we export them all
    ) where

#include "HsVersions.h"

import Module	  ( Module, mkModule )
import OccName	  ( dataName, tcName, clsName, varName, mkOccNameFS,
		    mkVarOccFS )
import RdrName	  ( RdrName, nameRdrName, mkOrig, rdrNameOcc, mkUnqual )
import Unique	  ( Unique, Uniquable(..), hasKey,
		    mkPreludeMiscIdUnique, mkPreludeDataConUnique,
		    mkPreludeTyConUnique, mkPreludeClassUnique,
		    mkTupleTyConUnique
		  ) 
import BasicTypes ( Boxity(..), Arity )
import Name	  ( Name, mkInternalName, mkExternalName, nameModule )
import SrcLoc     ( noSrcLoc )
import FastString
\end{code}


%************************************************************************
%*									*
\subsection{Local Names}
%*									*
%************************************************************************

This *local* name is used by the interactive stuff

\begin{code}
itName uniq = mkInternalName uniq (mkOccNameFS varName FSLIT("it")) noSrcLoc
\end{code}

\begin{code}
-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: RdrName -> Name
mkUnboundName rdr_name = mkInternalName unboundKey (rdrNameOcc rdr_name) noSrcLoc

isUnboundName :: Name -> Bool
isUnboundName name = name `hasKey` unboundKey
\end{code}


%************************************************************************
%*									*
\subsection{Known key Names}
%*									*
%************************************************************************

This section tells what the compiler knows about the assocation of
names with uniques.  These ones are the *non* wired-in ones.  The
wired in ones are defined in TysWiredIn etc.

\begin{code}
basicKnownKeyNames :: [Name]
basicKnownKeyNames
 = genericTyConNames
 ++ typeableClassNames
 ++ [	-- Type constructors (synonyms especially)
	ioTyConName, ioDataConName,
	runMainIOName,
	orderingTyConName,
	rationalTyConName,
	ratioDataConName,
	ratioTyConName,
	integerTyConName, smallIntegerDataConName, largeIntegerDataConName,

	--  Classes.  *Must* include:
	--  	classes that are grabbed by key (e.g., eqClassKey)
	--  	classes in "Class.standardClassKeys" (quite a few)
	eqClassName,			-- mentioned, derivable
	ordClassName,			-- derivable
	boundedClassName,		-- derivable
	numClassName,			-- mentioned, numeric
	enumClassName,			-- derivable
	monadClassName,
    	functorClassName,
	realClassName,			-- numeric
	integralClassName,		-- numeric
	fractionalClassName,		-- numeric
	floatingClassName,		-- numeric
	realFracClassName,		-- numeric
	realFloatClassName,		-- numeric
	dataClassName, 

	-- Numeric stuff
	negateName, minusName, 
	fromRationalName, fromIntegerName, 
	geName, eqName, 
	
	-- Enum stuff
	enumFromName, enumFromThenName,	
	enumFromThenToName, enumFromToName,
	enumFromToPName, enumFromThenToPName,

	-- Monad stuff
	thenIOName, bindIOName, returnIOName, failIOName,
	failMName, bindMName, thenMName, returnMName,

	-- MonadRec stuff
	mfixName,

	-- Arrow stuff
	arrAName, composeAName, firstAName,
	appAName, choiceAName, loopAName,

	-- Ix stuff
	ixClassName, 

	-- Show stuff
	showClassName, 

	-- Read stuff
	readClassName, 
	
	-- Stable pointers
	newStablePtrName,

	-- Strings and lists
	unpackCStringName, unpackCStringAppendName,
	unpackCStringFoldrName, unpackCStringUtf8Name,

	-- List operations
	concatName, filterName,
	zipName, foldrName, buildName, augmentName, appendName,

        -- Parallel array operations
	nullPName, lengthPName, replicatePName,	mapPName,
	filterPName, zipPName, crossPName, indexPName,
	toPName, bpermutePName, bpermuteDftPName, indexOfPName,

	-- FFI primitive types that are not wired-in.
	stablePtrTyConName, ptrTyConName, funPtrTyConName,
	int8TyConName, int16TyConName, int32TyConName, int64TyConName,
	wordTyConName, word8TyConName, word16TyConName, word32TyConName, word64TyConName,

	-- Others
	otherwiseIdName, 
	plusIntegerName, timesIntegerName,
	eqStringName, assertName, breakpointName, breakpointCondName,
        assertErrorName, runSTRepName,
	printName, fstName, sndName,

	-- MonadFix
	monadFixClassName, mfixName,

	-- Splittable class
	splittableClassName, splitName,

	-- Other classes
	randomClassName, randomGenClassName, monadPlusClassName,

	-- Booleans
	andName, orName
	
	-- The Either type
	, eitherTyConName, leftDataConName, rightDataConName

	-- dotnet interop
	, objectTyConName, marshalObjectName, unmarshalObjectName
	, marshalStringName, unmarshalStringName, checkDotnetResName
    ]

genericTyConNames :: [Name]
genericTyConNames = [crossTyConName, plusTyConName, genUnitTyConName]
\end{code}


%************************************************************************
%*									*
\subsection{Module names}
%*									*
%************************************************************************


--MetaHaskell Extension Add a new module here
\begin{code}
pRELUDE		= mkModule "Prelude"
gHC_PRIM	= mkModule "GHC.Prim"	   -- Primitive types and values
pREL_BASE	= mkModule "GHC.Base"
pREL_ENUM	= mkModule "GHC.Enum"
pREL_SHOW	= mkModule "GHC.Show"
pREL_READ	= mkModule "GHC.Read"
pREL_NUM	= mkModule "GHC.Num"
pREL_LIST	= mkModule "GHC.List"
pREL_PARR	= mkModule "GHC.PArr"
pREL_TUP	= mkModule "Data.Tuple"
pREL_EITHER	= mkModule "Data.Either"
pREL_PACK	= mkModule "GHC.Pack"
pREL_CONC	= mkModule "GHC.Conc"
pREL_IO_BASE	= mkModule "GHC.IOBase"
pREL_ST		= mkModule "GHC.ST"
pREL_ARR	= mkModule "GHC.Arr"
pREL_STABLE	= mkModule "GHC.Stable"
pREL_ADDR	= mkModule "GHC.Addr"
pREL_PTR	= mkModule "GHC.Ptr"
pREL_ERR	= mkModule "GHC.Err"
pREL_REAL	= mkModule "GHC.Real"
pREL_FLOAT	= mkModule "GHC.Float"
pREL_TOP_HANDLER= mkModule "GHC.TopHandler"
sYSTEM_IO	= mkModule "System.IO"
dYNAMIC		= mkModule "Data.Dynamic"
tYPEABLE	= mkModule "Data.Typeable"
gENERICS	= mkModule "Data.Generics.Basics"
dOTNET		= mkModule "GHC.Dotnet"

rEAD_PREC	= mkModule "Text.ParserCombinators.ReadPrec"
lEX		= mkModule "Text.Read.Lex"

mAIN		= mkModule "Main"
pREL_INT	= mkModule "GHC.Int"
pREL_WORD	= mkModule "GHC.Word"
mONAD		= mkModule "Control.Monad"
mONAD_FIX	= mkModule "Control.Monad.Fix"
aRROW		= mkModule "Control.Arrow"
rANDOM		= mkModule "System.Random"

gLA_EXTS	= mkModule "GHC.Exts"
rOOT_MAIN	= mkModule ":Main"		-- Root module for initialisation 
	-- The ':xxx' makes a module name that the user can never
	-- use himself.  The z-encoding for ':' is "ZC", so the z-encoded
	-- module name still starts with a capital letter, which keeps
	-- the z-encoded version consistent.

iNTERACTIVE    = mkModule ":Interactive"
thFAKE         = mkModule ":THFake"
\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupleModule :: Boxity -> Arity -> Module
mkTupleModule Boxed   0 = pREL_BASE
mkTupleModule Boxed   _ = pREL_TUP
mkTupleModule Unboxed _ = gHC_PRIM
\end{code}


%************************************************************************
%*									*
			RdrNames
%*									*
%************************************************************************

\begin{code}
main_RDR_Unqual 	= mkUnqual varName FSLIT("main")
	-- We definitely don't want an Orig RdrName, because
	-- main might, in principle, be imported into module Main

eq_RDR 			= nameRdrName eqName
ge_RDR 			= nameRdrName geName
ne_RDR 			= varQual_RDR  pREL_BASE FSLIT("/=")
le_RDR 			= varQual_RDR  pREL_BASE FSLIT("<=") 
gt_RDR 			= varQual_RDR  pREL_BASE FSLIT(">")  
compare_RDR		= varQual_RDR  pREL_BASE FSLIT("compare") 
ltTag_RDR		= dataQual_RDR pREL_BASE FSLIT("LT") 
eqTag_RDR		= dataQual_RDR pREL_BASE FSLIT("EQ")
gtTag_RDR		= dataQual_RDR pREL_BASE FSLIT("GT")

eqClass_RDR		= nameRdrName eqClassName
numClass_RDR 		= nameRdrName numClassName
ordClass_RDR 		= nameRdrName ordClassName
enumClass_RDR		= nameRdrName enumClassName
monadClass_RDR		= nameRdrName monadClassName

map_RDR 		= varQual_RDR pREL_BASE FSLIT("map")
append_RDR 		= varQual_RDR pREL_BASE FSLIT("++")

foldr_RDR 		= nameRdrName foldrName
build_RDR 		= nameRdrName buildName
returnM_RDR 		= nameRdrName returnMName
bindM_RDR 		= nameRdrName bindMName
failM_RDR 		= nameRdrName failMName

and_RDR			= nameRdrName andName

left_RDR		= nameRdrName leftDataConName
right_RDR		= nameRdrName rightDataConName

fromEnum_RDR		= varQual_RDR pREL_ENUM FSLIT("fromEnum")
toEnum_RDR		= varQual_RDR pREL_ENUM FSLIT("toEnum")

enumFrom_RDR		= nameRdrName enumFromName
enumFromTo_RDR 		= nameRdrName enumFromToName
enumFromThen_RDR	= nameRdrName enumFromThenName
enumFromThenTo_RDR	= nameRdrName enumFromThenToName

ratioDataCon_RDR	= nameRdrName ratioDataConName
plusInteger_RDR		= nameRdrName plusIntegerName
timesInteger_RDR	= nameRdrName timesIntegerName

ioDataCon_RDR		= nameRdrName ioDataConName

eqString_RDR		= nameRdrName eqStringName
unpackCString_RDR      	= nameRdrName unpackCStringName
unpackCStringFoldr_RDR 	= nameRdrName unpackCStringFoldrName
unpackCStringUtf8_RDR  	= nameRdrName unpackCStringUtf8Name

newStablePtr_RDR 	= nameRdrName newStablePtrName
wordDataCon_RDR		= dataQual_RDR pREL_WORD FSLIT("W#")

bindIO_RDR	  	= nameRdrName bindIOName
returnIO_RDR	  	= nameRdrName returnIOName

fromInteger_RDR		= nameRdrName fromIntegerName
fromRational_RDR	= nameRdrName fromRationalName
minus_RDR		= nameRdrName minusName
times_RDR		= varQual_RDR  pREL_NUM FSLIT("*")
plus_RDR                = varQual_RDR pREL_NUM FSLIT("+")

compose_RDR		= varQual_RDR pREL_BASE FSLIT(".")

not_RDR 		= varQual_RDR pREL_BASE FSLIT("not")
getTag_RDR	 	= varQual_RDR pREL_BASE FSLIT("getTag")
succ_RDR 		= varQual_RDR pREL_ENUM FSLIT("succ")
pred_RDR                = varQual_RDR pREL_ENUM FSLIT("pred")
minBound_RDR            = varQual_RDR pREL_ENUM FSLIT("minBound")
maxBound_RDR            = varQual_RDR pREL_ENUM FSLIT("maxBound")
range_RDR               = varQual_RDR pREL_ARR FSLIT("range")
inRange_RDR             = varQual_RDR pREL_ARR FSLIT("inRange")
index_RDR		= varQual_RDR pREL_ARR FSLIT("index")
unsafeIndex_RDR		= varQual_RDR pREL_ARR FSLIT("unsafeIndex")
unsafeRangeSize_RDR	= varQual_RDR pREL_ARR FSLIT("unsafeRangeSize")

readList_RDR            = varQual_RDR pREL_READ FSLIT("readList")
readListDefault_RDR     = varQual_RDR pREL_READ FSLIT("readListDefault")
readListPrec_RDR        = varQual_RDR pREL_READ FSLIT("readListPrec")
readListPrecDefault_RDR = varQual_RDR pREL_READ FSLIT("readListPrecDefault")
readPrec_RDR            = varQual_RDR pREL_READ FSLIT("readPrec")
parens_RDR              = varQual_RDR pREL_READ FSLIT("parens")
choose_RDR              = varQual_RDR pREL_READ FSLIT("choose")
lexP_RDR                = varQual_RDR pREL_READ FSLIT("lexP")

punc_RDR                = dataQual_RDR lEX FSLIT("Punc")
ident_RDR               = dataQual_RDR lEX FSLIT("Ident")
symbol_RDR              = dataQual_RDR lEX FSLIT("Symbol")

step_RDR                = varQual_RDR  rEAD_PREC FSLIT("step")
alt_RDR                 = varQual_RDR  rEAD_PREC FSLIT("+++") 
reset_RDR               = varQual_RDR  rEAD_PREC FSLIT("reset")
prec_RDR                = varQual_RDR  rEAD_PREC FSLIT("prec")

showList_RDR            = varQual_RDR pREL_SHOW FSLIT("showList")
showList___RDR          = varQual_RDR pREL_SHOW FSLIT("showList__")
showsPrec_RDR           = varQual_RDR pREL_SHOW FSLIT("showsPrec") 
showString_RDR          = varQual_RDR pREL_SHOW FSLIT("showString")
showSpace_RDR           = varQual_RDR pREL_SHOW FSLIT("showSpace") 
showParen_RDR           = varQual_RDR pREL_SHOW FSLIT("showParen") 

typeOf_RDR     = varQual_RDR tYPEABLE FSLIT("typeOf")
mkTypeRep_RDR  = varQual_RDR tYPEABLE FSLIT("mkTyConApp")
mkTyConRep_RDR = varQual_RDR tYPEABLE FSLIT("mkTyCon")

undefined_RDR = varQual_RDR pREL_ERR FSLIT("undefined")

crossDataCon_RDR   = dataQual_RDR pREL_BASE FSLIT(":*:")
inlDataCon_RDR     = dataQual_RDR pREL_BASE FSLIT("Inl")
inrDataCon_RDR     = dataQual_RDR pREL_BASE FSLIT("Inr")
genUnitDataCon_RDR = dataQual_RDR pREL_BASE FSLIT("Unit")

----------------------
varQual_RDR  mod str = mkOrig mod (mkOccNameFS varName str)
tcQual_RDR   mod str = mkOrig mod (mkOccNameFS tcName str)
clsQual_RDR  mod str = mkOrig mod (mkOccNameFS clsName str)
dataQual_RDR mod str = mkOrig mod (mkOccNameFS dataName str)
\end{code}

%************************************************************************
%*									*
\subsection{Known-key names}
%*									*
%************************************************************************

Many of these Names are not really "built in", but some parts of the
compiler (notably the deriving mechanism) need to mention their names,
and it's convenient to write them all down in one place.

--MetaHaskell Extension  add the constrs and the lower case case
-- guys as well (perhaps) e.g. see  trueDataConName	below


\begin{code}
runMainIOName = varQual pREL_TOP_HANDLER FSLIT("runMainIO") runMainKey

orderingTyConName = tcQual   pREL_BASE FSLIT("Ordering") orderingTyConKey

eitherTyConName	  = tcQual  pREL_EITHER     FSLIT("Either") eitherTyConKey
leftDataConName   = conName eitherTyConName FSLIT("Left")   leftDataConKey
rightDataConName  = conName eitherTyConName FSLIT("Right")  rightDataConKey

-- Generics
crossTyConName     = tcQual   pREL_BASE FSLIT(":*:") crossTyConKey
plusTyConName      = tcQual   pREL_BASE FSLIT(":+:") plusTyConKey
genUnitTyConName   = tcQual   pREL_BASE FSLIT("Unit") genUnitTyConKey

-- Base strings Strings
unpackCStringName       = varQual pREL_BASE FSLIT("unpackCString#") unpackCStringIdKey
unpackCStringAppendName = varQual pREL_BASE FSLIT("unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual pREL_BASE FSLIT("unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual pREL_BASE FSLIT("unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName	 	= varQual pREL_BASE FSLIT("eqString")  eqStringIdKey

-- The 'inline' function
inlineIdName	 	= varQual pREL_BASE FSLIT("inline") inlineIdKey

-- Base classes (Eq, Ord, Functor)
eqClassName	  = clsQual pREL_BASE FSLIT("Eq")      eqClassKey
eqName		  = methName eqClassName FSLIT("==")   eqClassOpKey
ordClassName	  = clsQual pREL_BASE FSLIT("Ord")     ordClassKey
geName		  = methName ordClassName FSLIT(">=")  geClassOpKey
functorClassName  = clsQual pREL_BASE FSLIT("Functor") functorClassKey

-- Class Monad
monadClassName	   = clsQual pREL_BASE FSLIT("Monad")        monadClassKey
thenMName	   = methName monadClassName FSLIT(">>")     thenMClassOpKey
bindMName	   = methName monadClassName FSLIT(">>=")    bindMClassOpKey
returnMName	   = methName monadClassName FSLIT("return") returnMClassOpKey
failMName	   = methName monadClassName FSLIT("fail")   failMClassOpKey

-- Random PrelBase functions
otherwiseIdName   = varQual pREL_BASE FSLIT("otherwise")  otherwiseIdKey
foldrName	  = varQual pREL_BASE FSLIT("foldr")      foldrIdKey
buildName	  = varQual pREL_BASE FSLIT("build")      buildIdKey
augmentName	  = varQual pREL_BASE FSLIT("augment")    augmentIdKey
appendName	  = varQual pREL_BASE FSLIT("++")         appendIdKey
andName		  = varQual pREL_BASE FSLIT("&&")	  andIdKey
orName		  = varQual pREL_BASE FSLIT("||")	  orIdKey
assertName        = varQual pREL_BASE FSLIT("assert")     assertIdKey
breakpointName    = varQual pREL_BASE FSLIT("breakpoint") breakpointIdKey
breakpointCondName= varQual pREL_BASE FSLIT("breakpointCond") breakpointCondIdKey
breakpointJumpName
    = mkInternalName
        breakpointJumpIdKey
        (mkOccNameFS varName FSLIT("breakpointJump"))
        noSrcLoc
breakpointCondJumpName
    = mkInternalName
        breakpointCondJumpIdKey
        (mkOccNameFS varName FSLIT("breakpointCondJump"))
        noSrcLoc

-- PrelTup
fstName		  = varQual pREL_TUP FSLIT("fst") fstIdKey
sndName		  = varQual pREL_TUP FSLIT("snd") sndIdKey

-- Module PrelNum
numClassName	  = clsQual pREL_NUM FSLIT("Num") numClassKey
fromIntegerName   = methName numClassName FSLIT("fromInteger") fromIntegerClassOpKey
minusName	  = methName numClassName FSLIT("-") minusClassOpKey
negateName	  = methName numClassName FSLIT("negate") negateClassOpKey
plusIntegerName   = varQual pREL_NUM FSLIT("plusInteger") plusIntegerIdKey
timesIntegerName  = varQual pREL_NUM FSLIT("timesInteger") timesIntegerIdKey
integerTyConName  = tcQual  pREL_NUM FSLIT("Integer") integerTyConKey
smallIntegerDataConName = conName integerTyConName FSLIT("S#") smallIntegerDataConKey
largeIntegerDataConName = conName integerTyConName FSLIT("J#") largeIntegerDataConKey

-- PrelReal types and classes
rationalTyConName   = tcQual  pREL_REAL  FSLIT("Rational") rationalTyConKey
ratioTyConName	    = tcQual  pREL_REAL  FSLIT("Ratio") ratioTyConKey
ratioDataConName    = conName ratioTyConName FSLIT(":%") ratioDataConKey
realClassName	    = clsQual pREL_REAL  FSLIT("Real") realClassKey
integralClassName   = clsQual pREL_REAL  FSLIT("Integral") integralClassKey
realFracClassName   = clsQual pREL_REAL  FSLIT("RealFrac") realFracClassKey
fractionalClassName = clsQual pREL_REAL  FSLIT("Fractional") fractionalClassKey
fromRationalName    = methName fractionalClassName  FSLIT("fromRational") fromRationalClassOpKey

-- PrelFloat classes
floatingClassName  = clsQual  pREL_FLOAT FSLIT("Floating") floatingClassKey
realFloatClassName = clsQual  pREL_FLOAT FSLIT("RealFloat") realFloatClassKey

-- Class Ix
ixClassName = clsQual pREL_ARR FSLIT("Ix") ixClassKey

-- Class Typeable
typeableClassName  = clsQual tYPEABLE FSLIT("Typeable") typeableClassKey
typeable1ClassName = clsQual tYPEABLE FSLIT("Typeable1") typeable1ClassKey
typeable2ClassName = clsQual tYPEABLE FSLIT("Typeable2") typeable2ClassKey
typeable3ClassName = clsQual tYPEABLE FSLIT("Typeable3") typeable3ClassKey
typeable4ClassName = clsQual tYPEABLE FSLIT("Typeable4") typeable4ClassKey
typeable5ClassName = clsQual tYPEABLE FSLIT("Typeable5") typeable5ClassKey
typeable6ClassName = clsQual tYPEABLE FSLIT("Typeable6") typeable6ClassKey
typeable7ClassName = clsQual tYPEABLE FSLIT("Typeable7") typeable7ClassKey

typeableClassNames = 	[ typeableClassName, typeable1ClassName, typeable2ClassName
		 	, typeable3ClassName, typeable4ClassName, typeable5ClassName
			, typeable6ClassName, typeable7ClassName ]

-- Class Data
dataClassName = clsQual gENERICS FSLIT("Data") dataClassKey

-- Error module
assertErrorName	  = varQual pREL_ERR FSLIT("assertError") assertErrorIdKey

-- Enum module (Enum, Bounded)
enumClassName 	   = clsQual pREL_ENUM FSLIT("Enum") enumClassKey
enumFromName	   = methName enumClassName FSLIT("enumFrom") enumFromClassOpKey
enumFromToName	   = methName enumClassName FSLIT("enumFromTo") enumFromToClassOpKey
enumFromThenName   = methName enumClassName FSLIT("enumFromThen") enumFromThenClassOpKey
enumFromThenToName = methName enumClassName FSLIT("enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual pREL_ENUM FSLIT("Bounded") boundedClassKey

-- List functions
concatName	  = varQual pREL_LIST FSLIT("concat") concatIdKey
filterName	  = varQual pREL_LIST FSLIT("filter") filterIdKey
zipName	   	  = varQual pREL_LIST FSLIT("zip") zipIdKey

-- Class Show
showClassName	  = clsQual pREL_SHOW FSLIT("Show")       showClassKey

-- Class Read
readClassName	   = clsQual pREL_READ FSLIT("Read") readClassKey

-- parallel array types and functions
enumFromToPName	   = varQual pREL_PARR FSLIT("enumFromToP") enumFromToPIdKey
enumFromThenToPName= varQual pREL_PARR FSLIT("enumFromThenToP") enumFromThenToPIdKey
nullPName	  = varQual pREL_PARR FSLIT("nullP")      	 nullPIdKey
lengthPName	  = varQual pREL_PARR FSLIT("lengthP")    	 lengthPIdKey
replicatePName	  = varQual pREL_PARR FSLIT("replicateP") 	 replicatePIdKey
mapPName	  = varQual pREL_PARR FSLIT("mapP")       	 mapPIdKey
filterPName	  = varQual pREL_PARR FSLIT("filterP")    	 filterPIdKey
zipPName	  = varQual pREL_PARR FSLIT("zipP")       	 zipPIdKey
crossPName	  = varQual pREL_PARR FSLIT("crossP")     	 crossPIdKey
indexPName	  = varQual pREL_PARR FSLIT("!:")	       	 indexPIdKey
toPName	          = varQual pREL_PARR FSLIT("toP")	       	 toPIdKey
bpermutePName     = varQual pREL_PARR FSLIT("bpermuteP")    bpermutePIdKey
bpermuteDftPName  = varQual pREL_PARR FSLIT("bpermuteDftP") bpermuteDftPIdKey
indexOfPName      = varQual pREL_PARR FSLIT("indexOfP")     indexOfPIdKey

-- IOBase things
ioTyConName	  = tcQual  pREL_IO_BASE FSLIT("IO") ioTyConKey
ioDataConName     = conName ioTyConName  FSLIT("IO") ioDataConKey
thenIOName	  = varQual pREL_IO_BASE FSLIT("thenIO") thenIOIdKey
bindIOName	  = varQual pREL_IO_BASE FSLIT("bindIO") bindIOIdKey
returnIOName	  = varQual pREL_IO_BASE FSLIT("returnIO") returnIOIdKey
failIOName	  = varQual pREL_IO_BASE FSLIT("failIO") failIOIdKey

-- IO things
printName	  = varQual sYSTEM_IO FSLIT("print") printIdKey

-- Int, Word, and Addr things
int8TyConName     = tcQual pREL_INT  FSLIT("Int8") int8TyConKey
int16TyConName    = tcQual pREL_INT  FSLIT("Int16") int16TyConKey
int32TyConName    = tcQual pREL_INT  FSLIT("Int32") int32TyConKey
int64TyConName    = tcQual pREL_INT  FSLIT("Int64") int64TyConKey

-- Word module
word8TyConName    = tcQual  pREL_WORD FSLIT("Word8")  word8TyConKey
word16TyConName   = tcQual  pREL_WORD FSLIT("Word16") word16TyConKey
word32TyConName   = tcQual  pREL_WORD FSLIT("Word32") word32TyConKey
word64TyConName   = tcQual  pREL_WORD FSLIT("Word64") word64TyConKey
wordTyConName     = tcQual  pREL_WORD FSLIT("Word")   wordTyConKey
wordDataConName   = conName wordTyConName FSLIT("W#") wordDataConKey

-- PrelPtr module
ptrTyConName	  = tcQual   pREL_PTR FSLIT("Ptr") ptrTyConKey
funPtrTyConName	  = tcQual   pREL_PTR FSLIT("FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName    = tcQual   pREL_STABLE FSLIT("StablePtr") stablePtrTyConKey
newStablePtrName      = varQual  pREL_STABLE FSLIT("newStablePtr") newStablePtrIdKey

-- PrelST module
runSTRepName	   = varQual pREL_ST  FSLIT("runSTRep") runSTRepIdKey

-- The "split" Id for splittable implicit parameters
splittableClassName = clsQual gLA_EXTS FSLIT("Splittable") splittableClassKey
splitName           = methName splittableClassName FSLIT("split") splitIdKey

-- Recursive-do notation
monadFixClassName  = clsQual mONAD_FIX FSLIT("MonadFix") monadFixClassKey
mfixName	   = methName monadFixClassName FSLIT("mfix") mfixIdKey

-- Arrow notation
arrAName	   = varQual aRROW FSLIT("arr")	  arrAIdKey
composeAName	   = varQual aRROW FSLIT(">>>")	  composeAIdKey
firstAName	   = varQual aRROW FSLIT("first") firstAIdKey
appAName	   = varQual aRROW FSLIT("app")	  appAIdKey
choiceAName	   = varQual aRROW FSLIT("|||")	  choiceAIdKey
loopAName	   = varQual aRROW FSLIT("loop")  loopAIdKey

-- Other classes, needed for type defaulting
monadPlusClassName  = clsQual mONAD FSLIT("MonadPlus")  monadPlusClassKey
randomClassName     = clsQual rANDOM FSLIT("Random")    randomClassKey
randomGenClassName  = clsQual rANDOM FSLIT("RandomGen") randomGenClassKey

-- dotnet interop
objectTyConName	    = tcQual   dOTNET FSLIT("Object") objectTyConKey
	-- objectTyConName was "wTcQual", but that's gone now, and
	-- I can't see why it was wired in anyway...
unmarshalObjectName = varQual  dOTNET FSLIT("unmarshalObject") unmarshalObjectIdKey
marshalObjectName   = varQual  dOTNET FSLIT("marshalObject") marshalObjectIdKey
marshalStringName   = varQual  dOTNET FSLIT("marshalString") marshalStringIdKey
unmarshalStringName = varQual  dOTNET FSLIT("unmarshalString") unmarshalStringIdKey
checkDotnetResName  = varQual  dOTNET FSLIT("checkResult")     checkDotnetResNameIdKey
\end{code}

%************************************************************************
%*									*
\subsection{Local helpers}
%*									*
%************************************************************************

All these are original names; hence mkOrig

\begin{code}
varQual  = mk_known_key_name varName
tcQual   = mk_known_key_name tcName
clsQual  = mk_known_key_name clsName

mk_known_key_name space mod str uniq 
  = mkExternalName uniq mod (mkOccNameFS space str) 
		   Nothing noSrcLoc

conName :: Name -> FastString -> Unique -> Name
conName tycon occ uniq
  = mkExternalName uniq (nameModule tycon) (mkOccNameFS dataName occ) 
		   (Just tycon) noSrcLoc

methName :: Name -> FastString -> Unique -> Name
methName cls occ uniq
  = mkExternalName uniq (nameModule cls) (mkVarOccFS occ) 
		   (Just cls) noSrcLoc
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}
%*									*
%************************************************************************
--MetaHaskell extension hand allocate keys here

\begin{code}
boundedClassKey		= mkPreludeClassUnique 1 
enumClassKey		= mkPreludeClassUnique 2 
eqClassKey		= mkPreludeClassUnique 3 
floatingClassKey	= mkPreludeClassUnique 5 
fractionalClassKey	= mkPreludeClassUnique 6 
integralClassKey	= mkPreludeClassUnique 7 
monadClassKey		= mkPreludeClassUnique 8 
dataClassKey		= mkPreludeClassUnique 9
functorClassKey		= mkPreludeClassUnique 10
numClassKey		= mkPreludeClassUnique 11
ordClassKey		= mkPreludeClassUnique 12
readClassKey		= mkPreludeClassUnique 13
realClassKey		= mkPreludeClassUnique 14
realFloatClassKey	= mkPreludeClassUnique 15
realFracClassKey	= mkPreludeClassUnique 16
showClassKey		= mkPreludeClassUnique 17
ixClassKey		= mkPreludeClassUnique 18

typeableClassKey	= mkPreludeClassUnique 20
typeable1ClassKey	= mkPreludeClassUnique 21
typeable2ClassKey	= mkPreludeClassUnique 22
typeable3ClassKey	= mkPreludeClassUnique 23
typeable4ClassKey	= mkPreludeClassUnique 24
typeable5ClassKey	= mkPreludeClassUnique 25
typeable6ClassKey	= mkPreludeClassUnique 26
typeable7ClassKey	= mkPreludeClassUnique 27

monadFixClassKey	= mkPreludeClassUnique 28
splittableClassKey	= mkPreludeClassUnique 29

monadPlusClassKey	= mkPreludeClassUnique 30
randomClassKey		= mkPreludeClassUnique 31
randomGenClassKey	= mkPreludeClassUnique 32
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
%*									*
%************************************************************************

\begin{code}
addrPrimTyConKey			= mkPreludeTyConUnique	1
arrayPrimTyConKey			= mkPreludeTyConUnique	3
boolTyConKey				= mkPreludeTyConUnique	4
byteArrayPrimTyConKey			= mkPreludeTyConUnique	5
charPrimTyConKey			= mkPreludeTyConUnique	7
charTyConKey				= mkPreludeTyConUnique  8
doublePrimTyConKey			= mkPreludeTyConUnique  9
doubleTyConKey				= mkPreludeTyConUnique 10 
floatPrimTyConKey			= mkPreludeTyConUnique 11
floatTyConKey				= mkPreludeTyConUnique 12
funTyConKey				= mkPreludeTyConUnique 13
intPrimTyConKey				= mkPreludeTyConUnique 14
intTyConKey				= mkPreludeTyConUnique 15
int8TyConKey				= mkPreludeTyConUnique 16
int16TyConKey				= mkPreludeTyConUnique 17
int32PrimTyConKey			= mkPreludeTyConUnique 18
int32TyConKey				= mkPreludeTyConUnique 19
int64PrimTyConKey			= mkPreludeTyConUnique 20
int64TyConKey				= mkPreludeTyConUnique 21
integerTyConKey				= mkPreludeTyConUnique 22
listTyConKey				= mkPreludeTyConUnique 23
foreignObjPrimTyConKey			= mkPreludeTyConUnique 24
weakPrimTyConKey			= mkPreludeTyConUnique 27
mutableArrayPrimTyConKey		= mkPreludeTyConUnique 28
mutableByteArrayPrimTyConKey		= mkPreludeTyConUnique 29
orderingTyConKey			= mkPreludeTyConUnique 30
mVarPrimTyConKey		    	= mkPreludeTyConUnique 31
ratioTyConKey				= mkPreludeTyConUnique 32
rationalTyConKey			= mkPreludeTyConUnique 33
realWorldTyConKey			= mkPreludeTyConUnique 34
stablePtrPrimTyConKey			= mkPreludeTyConUnique 35
stablePtrTyConKey			= mkPreludeTyConUnique 36
statePrimTyConKey			= mkPreludeTyConUnique 50
stableNamePrimTyConKey			= mkPreludeTyConUnique 51
stableNameTyConKey		        = mkPreludeTyConUnique 52
mutVarPrimTyConKey			= mkPreludeTyConUnique 55
ioTyConKey				= mkPreludeTyConUnique 56
wordPrimTyConKey			= mkPreludeTyConUnique 58
wordTyConKey				= mkPreludeTyConUnique 59
word8TyConKey				= mkPreludeTyConUnique 60
word16TyConKey				= mkPreludeTyConUnique 61 
word32PrimTyConKey			= mkPreludeTyConUnique 62 
word32TyConKey				= mkPreludeTyConUnique 63
word64PrimTyConKey			= mkPreludeTyConUnique 64
word64TyConKey				= mkPreludeTyConUnique 65
liftedConKey				= mkPreludeTyConUnique 66
unliftedConKey				= mkPreludeTyConUnique 67
anyBoxConKey				= mkPreludeTyConUnique 68
kindConKey				= mkPreludeTyConUnique 69
boxityConKey				= mkPreludeTyConUnique 70
typeConKey				= mkPreludeTyConUnique 71
threadIdPrimTyConKey			= mkPreludeTyConUnique 72
bcoPrimTyConKey				= mkPreludeTyConUnique 73
ptrTyConKey				= mkPreludeTyConUnique 74
funPtrTyConKey				= mkPreludeTyConUnique 75
tVarPrimTyConKey		    	= mkPreludeTyConUnique 76

-- Generic Type Constructors
crossTyConKey		      		= mkPreludeTyConUnique 79
plusTyConKey		      		= mkPreludeTyConUnique 80
genUnitTyConKey				= mkPreludeTyConUnique 81

-- Parallel array type constructor
parrTyConKey				= mkPreludeTyConUnique 82

-- dotnet interop
objectTyConKey				= mkPreludeTyConUnique 83

eitherTyConKey				= mkPreludeTyConUnique 84

---------------- Template Haskell -------------------
--	USES TyConUniques 100-129
-----------------------------------------------------

unitTyConKey = mkTupleTyConUnique Boxed 0
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
%*									*
%************************************************************************

\begin{code}
charDataConKey				= mkPreludeDataConUnique  1
consDataConKey				= mkPreludeDataConUnique  2
doubleDataConKey			= mkPreludeDataConUnique  3
falseDataConKey				= mkPreludeDataConUnique  4
floatDataConKey				= mkPreludeDataConUnique  5
intDataConKey				= mkPreludeDataConUnique  6
smallIntegerDataConKey			= mkPreludeDataConUnique  7
largeIntegerDataConKey			= mkPreludeDataConUnique  8
nilDataConKey				= mkPreludeDataConUnique 11
ratioDataConKey				= mkPreludeDataConUnique 12
stableNameDataConKey			= mkPreludeDataConUnique 14
trueDataConKey				= mkPreludeDataConUnique 15
wordDataConKey				= mkPreludeDataConUnique 16
ioDataConKey				= mkPreludeDataConUnique 17

-- Generic data constructors
crossDataConKey		      		= mkPreludeDataConUnique 20
inlDataConKey		      		= mkPreludeDataConUnique 21
inrDataConKey		      		= mkPreludeDataConUnique 22
genUnitDataConKey			= mkPreludeDataConUnique 23

-- Data constructor for parallel arrays
parrDataConKey				= mkPreludeDataConUnique 24

leftDataConKey				= mkPreludeDataConUnique 25
rightDataConKey				= mkPreludeDataConUnique 26
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
%*									*
%************************************************************************

\begin{code}
absentErrorIdKey	      = mkPreludeMiscIdUnique  1
augmentIdKey		      = mkPreludeMiscIdUnique  3
appendIdKey		      = mkPreludeMiscIdUnique  4
buildIdKey		      = mkPreludeMiscIdUnique  5
errorIdKey		      = mkPreludeMiscIdUnique  6
foldlIdKey		      = mkPreludeMiscIdUnique  7
foldrIdKey		      = mkPreludeMiscIdUnique  8
recSelErrorIdKey	      = mkPreludeMiscIdUnique  9
integerMinusOneIdKey	      = mkPreludeMiscIdUnique 10
integerPlusOneIdKey	      = mkPreludeMiscIdUnique 11
integerPlusTwoIdKey	      = mkPreludeMiscIdUnique 12
integerZeroIdKey	      = mkPreludeMiscIdUnique 13
int2IntegerIdKey	      = mkPreludeMiscIdUnique 14
seqIdKey		      = mkPreludeMiscIdUnique 15
irrefutPatErrorIdKey	      = mkPreludeMiscIdUnique 16
eqStringIdKey		      = mkPreludeMiscIdUnique 17
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 18
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 19
runtimeErrorIdKey	      = mkPreludeMiscIdUnique 20 
parErrorIdKey		      = mkPreludeMiscIdUnique 21
parIdKey		      = mkPreludeMiscIdUnique 22
patErrorIdKey		      = mkPreludeMiscIdUnique 23
realWorldPrimIdKey	      = mkPreludeMiscIdUnique 24
recConErrorIdKey	      = mkPreludeMiscIdUnique 25
recUpdErrorIdKey	      = mkPreludeMiscIdUnique 26
traceIdKey		      = mkPreludeMiscIdUnique 27
unpackCStringUtf8IdKey	      = mkPreludeMiscIdUnique 28
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 29
unpackCStringFoldrIdKey	      = mkPreludeMiscIdUnique 30
unpackCStringIdKey	      = mkPreludeMiscIdUnique 31

unsafeCoerceIdKey	      = mkPreludeMiscIdUnique 32
concatIdKey		      = mkPreludeMiscIdUnique 33
filterIdKey		      = mkPreludeMiscIdUnique 34
zipIdKey		      = mkPreludeMiscIdUnique 35
bindIOIdKey		      = mkPreludeMiscIdUnique 36
returnIOIdKey		      = mkPreludeMiscIdUnique 37
deRefStablePtrIdKey	      = mkPreludeMiscIdUnique 38
newStablePtrIdKey	      = mkPreludeMiscIdUnique 39
plusIntegerIdKey	      = mkPreludeMiscIdUnique 41
timesIntegerIdKey	      = mkPreludeMiscIdUnique 42
printIdKey		      = mkPreludeMiscIdUnique 43
failIOIdKey		      = mkPreludeMiscIdUnique 44
nullAddrIdKey		      = mkPreludeMiscIdUnique 46
voidArgIdKey		      = mkPreludeMiscIdUnique 47
splitIdKey		      = mkPreludeMiscIdUnique 48
fstIdKey		      = mkPreludeMiscIdUnique 49
sndIdKey		      = mkPreludeMiscIdUnique 50
otherwiseIdKey		      = mkPreludeMiscIdUnique 51
assertIdKey		      = mkPreludeMiscIdUnique 53
runSTRepIdKey		      = mkPreludeMiscIdUnique 54

rootMainKey		      = mkPreludeMiscIdUnique 55
runMainKey		      = mkPreludeMiscIdUnique 56

andIdKey		      = mkPreludeMiscIdUnique 57
orIdKey			      = mkPreludeMiscIdUnique 58
thenIOIdKey		      = mkPreludeMiscIdUnique 59
lazyIdKey		      = mkPreludeMiscIdUnique 60
assertErrorIdKey	      = mkPreludeMiscIdUnique 61

breakpointIdKey               = mkPreludeMiscIdUnique 62
breakpointCondIdKey           = mkPreludeMiscIdUnique 63
breakpointJumpIdKey           = mkPreludeMiscIdUnique 64
breakpointCondJumpIdKey       = mkPreludeMiscIdUnique 65

inlineIdKey		      = mkPreludeMiscIdUnique 66

-- Parallel array functions
nullPIdKey	              = mkPreludeMiscIdUnique 80
lengthPIdKey		      = mkPreludeMiscIdUnique 81
replicatePIdKey		      = mkPreludeMiscIdUnique 82
mapPIdKey		      = mkPreludeMiscIdUnique 83
filterPIdKey		      = mkPreludeMiscIdUnique 84
zipPIdKey		      = mkPreludeMiscIdUnique 85
crossPIdKey		      = mkPreludeMiscIdUnique 86
indexPIdKey		      = mkPreludeMiscIdUnique 87
toPIdKey		      = mkPreludeMiscIdUnique 88
enumFromToPIdKey              = mkPreludeMiscIdUnique 89
enumFromThenToPIdKey          = mkPreludeMiscIdUnique 90
bpermutePIdKey		      = mkPreludeMiscIdUnique 91
bpermuteDftPIdKey	      = mkPreludeMiscIdUnique 92
indexOfPIdKey		      = mkPreludeMiscIdUnique 93

-- dotnet interop
unmarshalObjectIdKey          = mkPreludeMiscIdUnique 94
marshalObjectIdKey            = mkPreludeMiscIdUnique 95
marshalStringIdKey            = mkPreludeMiscIdUnique 96
unmarshalStringIdKey          = mkPreludeMiscIdUnique 97
checkDotnetResNameIdKey       = mkPreludeMiscIdUnique 98

\end{code}

Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.

\begin{code}
	-- Just a place holder for  unbound variables  produced by the renamer:
unboundKey		      = mkPreludeMiscIdUnique 101 

fromIntegerClassOpKey	      = mkPreludeMiscIdUnique 102
minusClassOpKey		      = mkPreludeMiscIdUnique 103
fromRationalClassOpKey	      = mkPreludeMiscIdUnique 104
enumFromClassOpKey	      = mkPreludeMiscIdUnique 105
enumFromThenClassOpKey	      = mkPreludeMiscIdUnique 106
enumFromToClassOpKey	      = mkPreludeMiscIdUnique 107
enumFromThenToClassOpKey      = mkPreludeMiscIdUnique 108
eqClassOpKey		      = mkPreludeMiscIdUnique 109
geClassOpKey		      = mkPreludeMiscIdUnique 110
negateClassOpKey	      = mkPreludeMiscIdUnique 111
failMClassOpKey		      = mkPreludeMiscIdUnique 112
bindMClassOpKey		      = mkPreludeMiscIdUnique 113 -- (>>=)
thenMClassOpKey		      = mkPreludeMiscIdUnique 114 -- (>>)
returnMClassOpKey	      = mkPreludeMiscIdUnique 117

-- Recursive do notation
mfixIdKey	= mkPreludeMiscIdUnique 118

-- Arrow notation
arrAIdKey	= mkPreludeMiscIdUnique 119
composeAIdKey	= mkPreludeMiscIdUnique 120 -- >>>
firstAIdKey	= mkPreludeMiscIdUnique 121
appAIdKey	= mkPreludeMiscIdUnique 122
choiceAIdKey	= mkPreludeMiscIdUnique 123 --  |||
loopAIdKey	= mkPreludeMiscIdUnique 124

---------------- Template Haskell -------------------
--	USES IdUniques 200-399
-----------------------------------------------------
\end{code}


%************************************************************************
%*									*
\subsection{Standard groups of types}
%*									*
%************************************************************************

\begin{code}
numericTyKeys = 
	[ wordTyConKey
	, intTyConKey
	, integerTyConKey
	, doubleTyConKey
	, floatTyConKey
	]
\end{code}


%************************************************************************
%*									*
\subsection[Class-std-groups]{Standard groups of Prelude classes}
%*									*
%************************************************************************

NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.

\begin{code}
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
needsDataDeclCtxtClassKeys = -- see comments in TcDeriv
  	[ readClassKey
    	]

-- The "standard classes" are used in defaulting (Haskell 98 report 4.3.4),
-- and are: "classes defined in the Prelude or a standard library"
standardClassKeys = derivableClassKeys ++ numericClassKeys
		  ++ [randomClassKey, randomGenClassKey,
		      functorClassKey, 
		      monadClassKey, monadPlusClassKey]
\end{code}

@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).

\begin{code}
derivableClassKeys
  = [ eqClassKey, ordClassKey, enumClassKey, ixClassKey,
      boundedClassKey, showClassKey, readClassKey ]
\end{code}


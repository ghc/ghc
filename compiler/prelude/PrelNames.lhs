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

import PackageConfig
import Module	  ( Module, ModuleName, mkModule, mkModuleNameFS )
import OccName	  ( dataName, tcName, clsName, varName, mkOccNameFS,
		    mkVarOccFS )
import RdrName	  ( RdrName, nameRdrName, mkOrig, rdrNameOcc, mkUnqual )
import Unique	  ( Unique, Uniquable(..), hasKey,
		    mkPreludeMiscIdUnique, mkPreludeDataConUnique,
		    mkPreludeTyConUnique, mkPreludeClassUnique,
		    mkTupleTyConUnique
		  ) 
import BasicTypes ( Boxity(..), Arity )
import Name	  ( Name, mkInternalName, mkExternalName )
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
        breakpointAutoName,  opaqueTyConName, unknownTyConName, 
        unknown1TyConName, unknown2TyConName, unknown3TyConName,
        assertErrorName, runSTRepName,
	printName, fstName, sndName,

	-- MonadFix
	monadFixClassName, mfixName,

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
pRELUDE		= mkBaseModule_ pRELUDE_NAME
gHC_PRIM	= mkBaseModule FSLIT("GHC.Prim")   -- Primitive types and values
gHC_BASE	= mkBaseModule FSLIT("GHC.Base")
gHC_ENUM	= mkBaseModule FSLIT("GHC.Enum")
gHC_SHOW	= mkBaseModule FSLIT("GHC.Show")
gHC_READ	= mkBaseModule FSLIT("GHC.Read")
gHC_NUM		= mkBaseModule FSLIT("GHC.Num")
gHC_LIST	= mkBaseModule FSLIT("GHC.List")
gHC_PARR	= mkBaseModule FSLIT("GHC.PArr")
dATA_TUP	= mkBaseModule FSLIT("Data.Tuple")
dATA_EITHER	= mkBaseModule FSLIT("Data.Either")
gHC_PACK	= mkBaseModule FSLIT("GHC.Pack")
gHC_CONC	= mkBaseModule FSLIT("GHC.Conc")
gHC_IO_BASE	= mkBaseModule FSLIT("GHC.IOBase")
gHC_ST		= mkBaseModule FSLIT("GHC.ST")
gHC_ARR		= mkBaseModule FSLIT("GHC.Arr")
gHC_STABLE	= mkBaseModule FSLIT("GHC.Stable")
gHC_ADDR	= mkBaseModule FSLIT("GHC.Addr")
gHC_PTR		= mkBaseModule FSLIT("GHC.Ptr")
gHC_ERR		= mkBaseModule FSLIT("GHC.Err")
gHC_REAL	= mkBaseModule FSLIT("GHC.Real")
gHC_FLOAT	= mkBaseModule FSLIT("GHC.Float")
gHC_TOP_HANDLER	= mkBaseModule FSLIT("GHC.TopHandler")
sYSTEM_IO	= mkBaseModule FSLIT("System.IO")
dYNAMIC		= mkBaseModule FSLIT("Data.Dynamic")
tYPEABLE	= mkBaseModule FSLIT("Data.Typeable")
gENERICS	= mkBaseModule FSLIT("Data.Generics.Basics")
dOTNET		= mkBaseModule FSLIT("GHC.Dotnet")
rEAD_PREC	= mkBaseModule FSLIT("Text.ParserCombinators.ReadPrec")
lEX		= mkBaseModule FSLIT("Text.Read.Lex")
gHC_INT		= mkBaseModule FSLIT("GHC.Int")
gHC_WORD	= mkBaseModule FSLIT("GHC.Word")
mONAD		= mkBaseModule FSLIT("Control.Monad")
mONAD_FIX	= mkBaseModule FSLIT("Control.Monad.Fix")
aRROW		= mkBaseModule FSLIT("Control.Arrow")
rANDOM		= mkBaseModule FSLIT("System.Random")
gLA_EXTS	= mkBaseModule FSLIT("GHC.Exts")

mAIN	        = mkMainModule_ mAIN_NAME
rOOT_MAIN	= mkMainModule FSLIT(":Main") -- Root module for initialisation 

	-- The ':xxx' makes a module name that the user can never
	-- use himself.  The z-encoding for ':' is "ZC", so the z-encoded
	-- module name still starts with a capital letter, which keeps
	-- the z-encoded version consistent.
iNTERACTIVE    = mkMainModule FSLIT(":Interactive")
thFAKE         = mkMainModule FSLIT(":THFake")

pRELUDE_NAME   = mkModuleNameFS FSLIT("Prelude")
mAIN_NAME      = mkModuleNameFS FSLIT("Main")

mkBaseModule :: FastString -> Module
mkBaseModule m = mkModule basePackageId (mkModuleNameFS m)

mkBaseModule_ :: ModuleName -> Module
mkBaseModule_ m = mkModule basePackageId m

mkMainModule :: FastString -> Module
mkMainModule m = mkModule mainPackageId (mkModuleNameFS m)

mkMainModule_ :: ModuleName -> Module
mkMainModule_ m = mkModule mainPackageId m
\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupleModule :: Boxity -> Arity -> Module
mkTupleModule Boxed   0 = gHC_BASE
mkTupleModule Boxed   _ = dATA_TUP
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
ne_RDR 			= varQual_RDR  gHC_BASE FSLIT("/=")
le_RDR 			= varQual_RDR  gHC_BASE FSLIT("<=") 
gt_RDR 			= varQual_RDR  gHC_BASE FSLIT(">")  
compare_RDR		= varQual_RDR  gHC_BASE FSLIT("compare") 
ltTag_RDR		= dataQual_RDR gHC_BASE FSLIT("LT") 
eqTag_RDR		= dataQual_RDR gHC_BASE FSLIT("EQ")
gtTag_RDR		= dataQual_RDR gHC_BASE FSLIT("GT")

eqClass_RDR		= nameRdrName eqClassName
numClass_RDR 		= nameRdrName numClassName
ordClass_RDR 		= nameRdrName ordClassName
enumClass_RDR		= nameRdrName enumClassName
monadClass_RDR		= nameRdrName monadClassName

map_RDR 		= varQual_RDR gHC_BASE FSLIT("map")
append_RDR 		= varQual_RDR gHC_BASE FSLIT("++")

foldr_RDR 		= nameRdrName foldrName
build_RDR 		= nameRdrName buildName
returnM_RDR 		= nameRdrName returnMName
bindM_RDR 		= nameRdrName bindMName
failM_RDR 		= nameRdrName failMName

and_RDR			= nameRdrName andName

left_RDR		= nameRdrName leftDataConName
right_RDR		= nameRdrName rightDataConName

fromEnum_RDR		= varQual_RDR gHC_ENUM FSLIT("fromEnum")
toEnum_RDR		= varQual_RDR gHC_ENUM FSLIT("toEnum")

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
wordDataCon_RDR		= dataQual_RDR gHC_WORD FSLIT("W#")

bindIO_RDR	  	= nameRdrName bindIOName
returnIO_RDR	  	= nameRdrName returnIOName

fromInteger_RDR		= nameRdrName fromIntegerName
fromRational_RDR	= nameRdrName fromRationalName
minus_RDR		= nameRdrName minusName
times_RDR		= varQual_RDR  gHC_NUM FSLIT("*")
plus_RDR                = varQual_RDR gHC_NUM FSLIT("+")

compose_RDR		= varQual_RDR gHC_BASE FSLIT(".")

not_RDR 		= varQual_RDR gHC_BASE FSLIT("not")
getTag_RDR	 	= varQual_RDR gHC_BASE FSLIT("getTag")
succ_RDR 		= varQual_RDR gHC_ENUM FSLIT("succ")
pred_RDR                = varQual_RDR gHC_ENUM FSLIT("pred")
minBound_RDR            = varQual_RDR gHC_ENUM FSLIT("minBound")
maxBound_RDR            = varQual_RDR gHC_ENUM FSLIT("maxBound")
range_RDR               = varQual_RDR gHC_ARR FSLIT("range")
inRange_RDR             = varQual_RDR gHC_ARR FSLIT("inRange")
index_RDR		= varQual_RDR gHC_ARR FSLIT("index")
unsafeIndex_RDR		= varQual_RDR gHC_ARR FSLIT("unsafeIndex")
unsafeRangeSize_RDR	= varQual_RDR gHC_ARR FSLIT("unsafeRangeSize")

readList_RDR            = varQual_RDR gHC_READ FSLIT("readList")
readListDefault_RDR     = varQual_RDR gHC_READ FSLIT("readListDefault")
readListPrec_RDR        = varQual_RDR gHC_READ FSLIT("readListPrec")
readListPrecDefault_RDR = varQual_RDR gHC_READ FSLIT("readListPrecDefault")
readPrec_RDR            = varQual_RDR gHC_READ FSLIT("readPrec")
parens_RDR              = varQual_RDR gHC_READ FSLIT("parens")
choose_RDR              = varQual_RDR gHC_READ FSLIT("choose")
lexP_RDR                = varQual_RDR gHC_READ FSLIT("lexP")

punc_RDR                = dataQual_RDR lEX FSLIT("Punc")
ident_RDR               = dataQual_RDR lEX FSLIT("Ident")
symbol_RDR              = dataQual_RDR lEX FSLIT("Symbol")

step_RDR                = varQual_RDR  rEAD_PREC FSLIT("step")
alt_RDR                 = varQual_RDR  rEAD_PREC FSLIT("+++") 
reset_RDR               = varQual_RDR  rEAD_PREC FSLIT("reset")
prec_RDR                = varQual_RDR  rEAD_PREC FSLIT("prec")

showList_RDR            = varQual_RDR gHC_SHOW FSLIT("showList")
showList___RDR          = varQual_RDR gHC_SHOW FSLIT("showList__")
showsPrec_RDR           = varQual_RDR gHC_SHOW FSLIT("showsPrec") 
showString_RDR          = varQual_RDR gHC_SHOW FSLIT("showString")
showSpace_RDR           = varQual_RDR gHC_SHOW FSLIT("showSpace") 
showParen_RDR           = varQual_RDR gHC_SHOW FSLIT("showParen") 

typeOf_RDR     = varQual_RDR tYPEABLE FSLIT("typeOf")
mkTypeRep_RDR  = varQual_RDR tYPEABLE FSLIT("mkTyConApp")
mkTyConRep_RDR = varQual_RDR tYPEABLE FSLIT("mkTyCon")

undefined_RDR = varQual_RDR gHC_ERR FSLIT("undefined")

crossDataCon_RDR   = dataQual_RDR gHC_BASE FSLIT(":*:")
inlDataCon_RDR     = dataQual_RDR gHC_BASE FSLIT("Inl")
inrDataCon_RDR     = dataQual_RDR gHC_BASE FSLIT("Inr")
genUnitDataCon_RDR = dataQual_RDR gHC_BASE FSLIT("Unit")

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
runMainIOName = varQual gHC_TOP_HANDLER FSLIT("runMainIO") runMainKey

orderingTyConName = tcQual   gHC_BASE FSLIT("Ordering") orderingTyConKey

eitherTyConName	  = tcQual  dATA_EITHER FSLIT("Either") eitherTyConKey
leftDataConName   = conName dATA_EITHER FSLIT("Left")   leftDataConKey
rightDataConName  = conName dATA_EITHER FSLIT("Right")  rightDataConKey

-- Generics
crossTyConName     = tcQual   gHC_BASE FSLIT(":*:") crossTyConKey
plusTyConName      = tcQual   gHC_BASE FSLIT(":+:") plusTyConKey
genUnitTyConName   = tcQual   gHC_BASE FSLIT("Unit") genUnitTyConKey

-- Base strings Strings
unpackCStringName       = varQual gHC_BASE FSLIT("unpackCString#") unpackCStringIdKey
unpackCStringAppendName = varQual gHC_BASE FSLIT("unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual gHC_BASE FSLIT("unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual gHC_BASE FSLIT("unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName	 	= varQual gHC_BASE FSLIT("eqString")  eqStringIdKey

-- The 'inline' function
inlineIdName	 	= varQual gHC_BASE FSLIT("inline") inlineIdKey

-- Base classes (Eq, Ord, Functor)
eqClassName	  = clsQual  gHC_BASE FSLIT("Eq")      eqClassKey
eqName		  = methName gHC_BASE FSLIT("==")      eqClassOpKey
ordClassName	  = clsQual  gHC_BASE FSLIT("Ord")     ordClassKey
geName		  = methName gHC_BASE FSLIT(">=")      geClassOpKey
functorClassName  = clsQual  gHC_BASE FSLIT("Functor") functorClassKey

-- Class Monad
monadClassName	   = clsQual  gHC_BASE FSLIT("Monad")  monadClassKey
thenMName	   = methName gHC_BASE FSLIT(">>")     thenMClassOpKey
bindMName	   = methName gHC_BASE FSLIT(">>=")    bindMClassOpKey
returnMName	   = methName gHC_BASE FSLIT("return") returnMClassOpKey
failMName	   = methName gHC_BASE FSLIT("fail")   failMClassOpKey

-- Random PrelBase functions
otherwiseIdName   = varQual gHC_BASE FSLIT("otherwise")  otherwiseIdKey
foldrName	  = varQual gHC_BASE FSLIT("foldr")      foldrIdKey
buildName	  = varQual gHC_BASE FSLIT("build")      buildIdKey
augmentName	  = varQual gHC_BASE FSLIT("augment")    augmentIdKey
appendName	  = varQual gHC_BASE FSLIT("++")         appendIdKey
andName		  = varQual gHC_BASE FSLIT("&&")	  andIdKey
orName		  = varQual gHC_BASE FSLIT("||")	  orIdKey
assertName        = varQual gHC_BASE FSLIT("assert")     assertIdKey
breakpointName    = varQual gHC_BASE FSLIT("breakpoint") breakpointIdKey
breakpointCondName= varQual gHC_BASE FSLIT("breakpointCond") breakpointCondIdKey
breakpointAutoName= varQual gHC_BASE FSLIT("breakpointAuto") breakpointAutoIdKey
unknownTyConName  = tcQual  gHC_BASE FSLIT("Unknown") unknownTyConKey
unknown1TyConName = tcQual  gHC_BASE FSLIT("Unknown1") unknown1TyConKey
unknown2TyConName = tcQual  gHC_BASE FSLIT("Unknown2") unknown2TyConKey
unknown3TyConName = tcQual  gHC_BASE FSLIT("Unknown3") unknown3TyConKey
opaqueTyConName   = tcQual  gHC_BASE FSLIT("Opaque")   opaqueTyConKey

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
breakpointAutoJumpName
    = mkInternalName
        breakpointAutoJumpIdKey
        (mkOccNameFS varName FSLIT("breakpointAutoJump"))
        noSrcLoc

-- PrelTup
fstName		  = varQual dATA_TUP FSLIT("fst") fstIdKey
sndName		  = varQual dATA_TUP FSLIT("snd") sndIdKey

-- Module PrelNum
numClassName	  = clsQual  gHC_NUM FSLIT("Num") numClassKey
fromIntegerName   = methName gHC_NUM FSLIT("fromInteger") fromIntegerClassOpKey
minusName	  = methName gHC_NUM FSLIT("-") minusClassOpKey
negateName	  = methName gHC_NUM FSLIT("negate") negateClassOpKey
plusIntegerName   = varQual  gHC_NUM FSLIT("plusInteger") plusIntegerIdKey
timesIntegerName  = varQual  gHC_NUM FSLIT("timesInteger") timesIntegerIdKey
integerTyConName  = tcQual   gHC_NUM FSLIT("Integer") integerTyConKey
smallIntegerDataConName = conName gHC_NUM FSLIT("S#") smallIntegerDataConKey
largeIntegerDataConName = conName gHC_NUM FSLIT("J#") largeIntegerDataConKey

-- PrelReal types and classes
rationalTyConName   = tcQual  gHC_REAL FSLIT("Rational") rationalTyConKey
ratioTyConName	    = tcQual  gHC_REAL FSLIT("Ratio") ratioTyConKey
ratioDataConName    = conName gHC_REAL FSLIT(":%") ratioDataConKey
realClassName	    = clsQual gHC_REAL FSLIT("Real") realClassKey
integralClassName   = clsQual gHC_REAL FSLIT("Integral") integralClassKey
realFracClassName   = clsQual gHC_REAL FSLIT("RealFrac") realFracClassKey
fractionalClassName = clsQual gHC_REAL FSLIT("Fractional") fractionalClassKey
fromRationalName    = methName gHC_REAL  FSLIT("fromRational") fromRationalClassOpKey

-- PrelFloat classes
floatingClassName  = clsQual  gHC_FLOAT FSLIT("Floating") floatingClassKey
realFloatClassName = clsQual  gHC_FLOAT FSLIT("RealFloat") realFloatClassKey

-- Class Ix
ixClassName = clsQual gHC_ARR FSLIT("Ix") ixClassKey

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
assertErrorName	  = varQual gHC_ERR FSLIT("assertError") assertErrorIdKey

-- Enum module (Enum, Bounded)
enumClassName 	   = clsQual gHC_ENUM FSLIT("Enum") enumClassKey
enumFromName	   = methName gHC_ENUM FSLIT("enumFrom") enumFromClassOpKey
enumFromToName	   = methName gHC_ENUM FSLIT("enumFromTo") enumFromToClassOpKey
enumFromThenName   = methName gHC_ENUM FSLIT("enumFromThen") enumFromThenClassOpKey
enumFromThenToName = methName gHC_ENUM FSLIT("enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual gHC_ENUM FSLIT("Bounded") boundedClassKey

-- List functions
concatName	  = varQual gHC_LIST FSLIT("concat") concatIdKey
filterName	  = varQual gHC_LIST FSLIT("filter") filterIdKey
zipName	   	  = varQual gHC_LIST FSLIT("zip") zipIdKey

-- Class Show
showClassName	  = clsQual gHC_SHOW FSLIT("Show")       showClassKey

-- Class Read
readClassName	   = clsQual gHC_READ FSLIT("Read") readClassKey

-- parallel array types and functions
enumFromToPName	   = varQual gHC_PARR FSLIT("enumFromToP") enumFromToPIdKey
enumFromThenToPName= varQual gHC_PARR FSLIT("enumFromThenToP") enumFromThenToPIdKey
nullPName	  = varQual gHC_PARR FSLIT("nullP")      	 nullPIdKey
lengthPName	  = varQual gHC_PARR FSLIT("lengthP")    	 lengthPIdKey
replicatePName	  = varQual gHC_PARR FSLIT("replicateP") 	 replicatePIdKey
mapPName	  = varQual gHC_PARR FSLIT("mapP")       	 mapPIdKey
filterPName	  = varQual gHC_PARR FSLIT("filterP")    	 filterPIdKey
zipPName	  = varQual gHC_PARR FSLIT("zipP")       	 zipPIdKey
crossPName	  = varQual gHC_PARR FSLIT("crossP")     	 crossPIdKey
indexPName	  = varQual gHC_PARR FSLIT("!:")	       	 indexPIdKey
toPName	          = varQual gHC_PARR FSLIT("toP")	       	 toPIdKey
bpermutePName     = varQual gHC_PARR FSLIT("bpermuteP")    bpermutePIdKey
bpermuteDftPName  = varQual gHC_PARR FSLIT("bpermuteDftP") bpermuteDftPIdKey
indexOfPName      = varQual gHC_PARR FSLIT("indexOfP")     indexOfPIdKey

-- IOBase things
ioTyConName	  = tcQual  gHC_IO_BASE FSLIT("IO") ioTyConKey
ioDataConName     = conName gHC_IO_BASE FSLIT("IO") ioDataConKey
thenIOName	  = varQual gHC_IO_BASE FSLIT("thenIO") thenIOIdKey
bindIOName	  = varQual gHC_IO_BASE FSLIT("bindIO") bindIOIdKey
returnIOName	  = varQual gHC_IO_BASE FSLIT("returnIO") returnIOIdKey
failIOName	  = varQual gHC_IO_BASE FSLIT("failIO") failIOIdKey

-- IO things
printName	  = varQual sYSTEM_IO FSLIT("print") printIdKey

-- Int, Word, and Addr things
int8TyConName     = tcQual gHC_INT  FSLIT("Int8") int8TyConKey
int16TyConName    = tcQual gHC_INT  FSLIT("Int16") int16TyConKey
int32TyConName    = tcQual gHC_INT  FSLIT("Int32") int32TyConKey
int64TyConName    = tcQual gHC_INT  FSLIT("Int64") int64TyConKey

-- Word module
word8TyConName    = tcQual  gHC_WORD FSLIT("Word8")  word8TyConKey
word16TyConName   = tcQual  gHC_WORD FSLIT("Word16") word16TyConKey
word32TyConName   = tcQual  gHC_WORD FSLIT("Word32") word32TyConKey
word64TyConName   = tcQual  gHC_WORD FSLIT("Word64") word64TyConKey
wordTyConName     = tcQual  gHC_WORD FSLIT("Word")   wordTyConKey
wordDataConName   = conName gHC_WORD FSLIT("W#") wordDataConKey

-- PrelPtr module
ptrTyConName	  = tcQual   gHC_PTR FSLIT("Ptr") ptrTyConKey
funPtrTyConName	  = tcQual   gHC_PTR FSLIT("FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName    = tcQual   gHC_STABLE FSLIT("StablePtr") stablePtrTyConKey
newStablePtrName      = varQual  gHC_STABLE FSLIT("newStablePtr") newStablePtrIdKey

-- PrelST module
runSTRepName	   = varQual gHC_ST  FSLIT("runSTRep") runSTRepIdKey

-- Recursive-do notation
monadFixClassName  = clsQual mONAD_FIX FSLIT("MonadFix") monadFixClassKey
mfixName	   = methName mONAD_FIX FSLIT("mfix") mfixIdKey

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
  = mkExternalName uniq mod (mkOccNameFS space str) noSrcLoc

conName :: Module -> FastString -> Unique -> Name
conName mod occ uniq
  = mkExternalName uniq mod (mkOccNameFS dataName occ) noSrcLoc

methName :: Module -> FastString -> Unique -> Name
methName mod occ uniq
  = mkExternalName uniq mod (mkVarOccFS occ) noSrcLoc
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

anyPrimTyConKey				= mkPreludeTyConUnique 37
anyPrimTyCon1Key			= mkPreludeTyConUnique 38

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

-- Super Kinds constructors
tySuperKindTyConKey                    = mkPreludeTyConUnique 85
coSuperKindTyConKey                    = mkPreludeTyConUnique 86

-- Kind constructors
liftedTypeKindTyConKey                  = mkPreludeTyConUnique 87
openTypeKindTyConKey                    = mkPreludeTyConUnique 88
unliftedTypeKindTyConKey                = mkPreludeTyConUnique 89
ubxTupleKindTyConKey                    = mkPreludeTyConUnique 90
argTypeKindTyConKey                     = mkPreludeTyConUnique 91

-- Coercion constructors
symCoercionTyConKey                     = mkPreludeTyConUnique 93
transCoercionTyConKey                   = mkPreludeTyConUnique 94
leftCoercionTyConKey                    = mkPreludeTyConUnique 95
rightCoercionTyConKey                   = mkPreludeTyConUnique 96
instCoercionTyConKey                    = mkPreludeTyConUnique 97
unsafeCoercionTyConKey                  = mkPreludeTyConUnique 98


unknownTyConKey				= mkPreludeTyConUnique 99
unknown1TyConKey			= mkPreludeTyConUnique 130
unknown2TyConKey			= mkPreludeTyConUnique 131
unknown3TyConKey			= mkPreludeTyConUnique 132
opaqueTyConKey                          = mkPreludeTyConUnique 133

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
breakpointAutoIdKey           = mkPreludeMiscIdUnique 64
breakpointJumpIdKey           = mkPreludeMiscIdUnique 65
breakpointCondJumpIdKey       = mkPreludeMiscIdUnique 66
breakpointAutoJumpIdKey       = mkPreludeMiscIdUnique 67

inlineIdKey		      = mkPreludeMiscIdUnique 68

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


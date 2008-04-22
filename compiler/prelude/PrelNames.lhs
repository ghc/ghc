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

import Module
import OccName
import RdrName	  ( RdrName, nameRdrName, mkOrig, rdrNameOcc, mkUnqual )
import Unique	  ( Unique, Uniquable(..), hasKey,
		    mkPreludeMiscIdUnique, mkPreludeDataConUnique,
		    mkPreludeTyConUnique, mkPreludeClassUnique,
		    mkTupleTyConUnique
		  ) 
import BasicTypes ( Boxity(..), Arity )
import Name	  ( Name, mkInternalName, mkExternalName )
import SrcLoc
import FastString
\end{code}


%************************************************************************
%*									*
\subsection{Local Names}
%*									*
%************************************************************************

This *local* name is used by the interactive stuff

\begin{code}
itName :: Unique -> Name
itName uniq = mkInternalName uniq (mkOccNameFS varName FSLIT("it")) noSrcSpan
\end{code}

\begin{code}
-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: RdrName -> Name
mkUnboundName rdr_name = mkInternalName unboundKey (rdrNameOcc rdr_name) noSrcSpan

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
	stringTyConName,
	ratioDataConName,
	ratioTyConName,
	integerTyConName, smallIntegerName,

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
	isStringClassName,

	-- Numeric stuff
	negateName, minusName, 
	fromRationalName, fromIntegerName, 
	geName, eqName, 

        -- String stuff
        fromStringName,
	
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

    -- GHC Extensions
    groupWithName,

	-- Strings and lists
	unpackCStringName, unpackCStringAppendName,
	unpackCStringFoldrName, unpackCStringUtf8Name,

	-- List operations
	concatName, filterName, mapName,
	zipName, foldrName, buildName, augmentName, appendName,

        -- Parallel array operations
	nullPName, lengthPName, replicatePName,	singletonPName, mapPName,
	filterPName, zipPName, crossMapPName, indexPName,
	toPName, bpermutePName, bpermuteDftPName, indexOfPName,

	-- FFI primitive types that are not wired-in.
	stablePtrTyConName, ptrTyConName, funPtrTyConName,
	int8TyConName, int16TyConName, int32TyConName, int64TyConName,
	wordTyConName, word8TyConName, word16TyConName, word32TyConName, word64TyConName,

	-- Others
	otherwiseIdName, inlineIdName,
	plusIntegerName, timesIntegerName,
	eqStringName, assertName, breakpointName, breakpointCondName,
        breakpointAutoName,  opaqueTyConName,
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
pRELUDE :: Module
pRELUDE		= mkBaseModule_ pRELUDE_NAME

gHC_PRIM, gHC_BOOL, gHC_GENERICS, gHC_BASE, gHC_ENUM, gHC_SHOW, gHC_READ,
    gHC_NUM, gHC_INTEGER, gHC_LIST, gHC_PARR, dATA_TUP, dATA_EITHER,
    dATA_STRING, gHC_PACK, gHC_CONC, gHC_IO_BASE, gHC_ST, gHC_ARR,
    gHC_STABLE, gHC_ADDR, gHC_PTR, gHC_ERR, gHC_REAL, gHC_FLOAT,
    gHC_TOP_HANDLER, sYSTEM_IO, dYNAMIC, tYPEABLE, gENERICS, dOTNET,
    rEAD_PREC, lEX, gHC_INT, gHC_WORD, mONAD, mONAD_FIX, aRROW,
    gHC_DESUGAR, rANDOM, gHC_EXTS :: Module
gHC_PRIM	= mkPrimModule FSLIT("GHC.Prim")   -- Primitive types and values
gHC_BOOL	= mkPrimModule FSLIT("GHC.Bool")
gHC_ORDERING	= mkPrimModule FSLIT("GHC.Ordering")
gHC_GENERICS	= mkPrimModule FSLIT("GHC.Generics")
gHC_BASE	= mkBaseModule FSLIT("GHC.Base")
gHC_ENUM	= mkBaseModule FSLIT("GHC.Enum")
gHC_SHOW	= mkBaseModule FSLIT("GHC.Show")
gHC_READ	= mkBaseModule FSLIT("GHC.Read")
gHC_NUM		= mkBaseModule FSLIT("GHC.Num")
gHC_INTEGER	= mkIntegerModule FSLIT("GHC.Integer")
gHC_LIST	= mkBaseModule FSLIT("GHC.List")
gHC_PARR	= mkBaseModule FSLIT("GHC.PArr")
dATA_TUP	= mkBaseModule FSLIT("Data.Tuple")
dATA_EITHER	= mkBaseModule FSLIT("Data.Either")
dATA_STRING	= mkBaseModule FSLIT("Data.String")
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
lEX		    = mkBaseModule FSLIT("Text.Read.Lex")
gHC_INT		= mkBaseModule FSLIT("GHC.Int")
gHC_WORD	= mkBaseModule FSLIT("GHC.Word")
mONAD		= mkBaseModule FSLIT("Control.Monad")
mONAD_FIX	= mkBaseModule FSLIT("Control.Monad.Fix")
aRROW		= mkBaseModule FSLIT("Control.Arrow")
gHC_DESUGAR = mkBaseModule FSLIT("GHC.Desugar")
rANDOM		= mkBaseModule FSLIT("System.Random")
gHC_EXTS	= mkBaseModule FSLIT("GHC.Exts")

mAIN, rOOT_MAIN :: Module
mAIN	        = mkMainModule_ mAIN_NAME
rOOT_MAIN	= mkMainModule FSLIT(":Main") -- Root module for initialisation 

	-- The ':xxx' makes a module name that the user can never
	-- use himself.  The z-encoding for ':' is "ZC", so the z-encoded
	-- module name still starts with a capital letter, which keeps
	-- the z-encoded version consistent.
iNTERACTIVE, thFAKE :: Module
iNTERACTIVE    = mkMainModule FSLIT(":Interactive")
thFAKE         = mkMainModule FSLIT(":THFake")

pRELUDE_NAME, mAIN_NAME :: ModuleName
pRELUDE_NAME   = mkModuleNameFS FSLIT("Prelude")
mAIN_NAME      = mkModuleNameFS FSLIT("Main")

mkPrimModule :: FastString -> Module
mkPrimModule m = mkModule primPackageId (mkModuleNameFS m)

mkIntegerModule :: FastString -> Module
mkIntegerModule m = mkModule integerPackageId (mkModuleNameFS m)

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
main_RDR_Unqual    :: RdrName
main_RDR_Unqual	= mkUnqual varName FSLIT("main")
	-- We definitely don't want an Orig RdrName, because
	-- main might, in principle, be imported into module Main

forall_tv_RDR, dot_tv_RDR :: RdrName
forall_tv_RDR = mkUnqual tvName FSLIT("forall")
dot_tv_RDR    = mkUnqual tvName FSLIT(".")

eq_RDR, ge_RDR, ne_RDR, le_RDR, lt_RDR, gt_RDR, compare_RDR,
    ltTag_RDR, eqTag_RDR, gtTag_RDR :: RdrName
eq_RDR 			= nameRdrName eqName
ge_RDR 			= nameRdrName geName
ne_RDR 			= varQual_RDR  gHC_BASE FSLIT("/=")
le_RDR 			= varQual_RDR  gHC_BASE FSLIT("<=") 
lt_RDR 			= varQual_RDR  gHC_BASE FSLIT("<") 
gt_RDR 			= varQual_RDR  gHC_BASE FSLIT(">")  
compare_RDR		= varQual_RDR  gHC_BASE FSLIT("compare") 
ltTag_RDR		= dataQual_RDR gHC_ORDERING FSLIT("LT") 
eqTag_RDR		= dataQual_RDR gHC_ORDERING FSLIT("EQ")
gtTag_RDR		= dataQual_RDR gHC_ORDERING FSLIT("GT")

eqClass_RDR, numClass_RDR, ordClass_RDR, enumClass_RDR, monadClass_RDR
    :: RdrName
eqClass_RDR		= nameRdrName eqClassName
numClass_RDR 		= nameRdrName numClassName
ordClass_RDR 		= nameRdrName ordClassName
enumClass_RDR		= nameRdrName enumClassName
monadClass_RDR		= nameRdrName monadClassName

map_RDR, append_RDR :: RdrName
map_RDR 		= varQual_RDR gHC_BASE FSLIT("map")
append_RDR 		= varQual_RDR gHC_BASE FSLIT("++")

foldr_RDR, build_RDR, returnM_RDR, bindM_RDR, failM_RDR :: RdrName
foldr_RDR 		= nameRdrName foldrName
build_RDR 		= nameRdrName buildName
returnM_RDR 		= nameRdrName returnMName
bindM_RDR 		= nameRdrName bindMName
failM_RDR 		= nameRdrName failMName

and_RDR :: RdrName
and_RDR			= nameRdrName andName

left_RDR, right_RDR :: RdrName
left_RDR		= nameRdrName leftDataConName
right_RDR		= nameRdrName rightDataConName

fromEnum_RDR, toEnum_RDR :: RdrName
fromEnum_RDR		= varQual_RDR gHC_ENUM FSLIT("fromEnum")
toEnum_RDR		= varQual_RDR gHC_ENUM FSLIT("toEnum")

enumFrom_RDR, enumFromTo_RDR, enumFromThen_RDR, enumFromThenTo_RDR :: RdrName
enumFrom_RDR		= nameRdrName enumFromName
enumFromTo_RDR 		= nameRdrName enumFromToName
enumFromThen_RDR	= nameRdrName enumFromThenName
enumFromThenTo_RDR	= nameRdrName enumFromThenToName

ratioDataCon_RDR, plusInteger_RDR, timesInteger_RDR :: RdrName
ratioDataCon_RDR	= nameRdrName ratioDataConName
plusInteger_RDR		= nameRdrName plusIntegerName
timesInteger_RDR	= nameRdrName timesIntegerName

ioDataCon_RDR :: RdrName
ioDataCon_RDR		= nameRdrName ioDataConName

eqString_RDR, unpackCString_RDR, unpackCStringFoldr_RDR,
    unpackCStringUtf8_RDR :: RdrName
eqString_RDR		= nameRdrName eqStringName
unpackCString_RDR      	= nameRdrName unpackCStringName
unpackCStringFoldr_RDR 	= nameRdrName unpackCStringFoldrName
unpackCStringUtf8_RDR  	= nameRdrName unpackCStringUtf8Name

newStablePtr_RDR, wordDataCon_RDR :: RdrName
newStablePtr_RDR 	= nameRdrName newStablePtrName
wordDataCon_RDR		= dataQual_RDR gHC_WORD FSLIT("W#")

bindIO_RDR, returnIO_RDR :: RdrName
bindIO_RDR	  	= nameRdrName bindIOName
returnIO_RDR	  	= nameRdrName returnIOName

fromInteger_RDR, fromRational_RDR, minus_RDR, times_RDR, plus_RDR :: RdrName
fromInteger_RDR		= nameRdrName fromIntegerName
fromRational_RDR	= nameRdrName fromRationalName
minus_RDR		= nameRdrName minusName
times_RDR		= varQual_RDR  gHC_NUM FSLIT("*")
plus_RDR                = varQual_RDR gHC_NUM FSLIT("+")

fromString_RDR :: RdrName
fromString_RDR		= nameRdrName fromStringName

compose_RDR :: RdrName
compose_RDR		= varQual_RDR gHC_BASE FSLIT(".")

not_RDR, getTag_RDR, succ_RDR, pred_RDR, minBound_RDR, maxBound_RDR,
    range_RDR, inRange_RDR, index_RDR,
    unsafeIndex_RDR, unsafeRangeSize_RDR :: RdrName
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

readList_RDR, readListDefault_RDR, readListPrec_RDR, readListPrecDefault_RDR,
    readPrec_RDR, parens_RDR, choose_RDR, lexP_RDR :: RdrName
readList_RDR            = varQual_RDR gHC_READ FSLIT("readList")
readListDefault_RDR     = varQual_RDR gHC_READ FSLIT("readListDefault")
readListPrec_RDR        = varQual_RDR gHC_READ FSLIT("readListPrec")
readListPrecDefault_RDR = varQual_RDR gHC_READ FSLIT("readListPrecDefault")
readPrec_RDR            = varQual_RDR gHC_READ FSLIT("readPrec")
parens_RDR              = varQual_RDR gHC_READ FSLIT("parens")
choose_RDR              = varQual_RDR gHC_READ FSLIT("choose")
lexP_RDR                = varQual_RDR gHC_READ FSLIT("lexP")

punc_RDR, ident_RDR, symbol_RDR :: RdrName
punc_RDR                = dataQual_RDR lEX FSLIT("Punc")
ident_RDR               = dataQual_RDR lEX FSLIT("Ident")
symbol_RDR              = dataQual_RDR lEX FSLIT("Symbol")

step_RDR, alt_RDR, reset_RDR, prec_RDR :: RdrName
step_RDR                = varQual_RDR  rEAD_PREC FSLIT("step")
alt_RDR                 = varQual_RDR  rEAD_PREC FSLIT("+++") 
reset_RDR               = varQual_RDR  rEAD_PREC FSLIT("reset")
prec_RDR                = varQual_RDR  rEAD_PREC FSLIT("prec")

showList_RDR, showList___RDR, showsPrec_RDR, showString_RDR,
    showSpace_RDR, showParen_RDR :: RdrName
showList_RDR            = varQual_RDR gHC_SHOW FSLIT("showList")
showList___RDR          = varQual_RDR gHC_SHOW FSLIT("showList__")
showsPrec_RDR           = varQual_RDR gHC_SHOW FSLIT("showsPrec") 
showString_RDR          = varQual_RDR gHC_SHOW FSLIT("showString")
showSpace_RDR           = varQual_RDR gHC_SHOW FSLIT("showSpace") 
showParen_RDR           = varQual_RDR gHC_SHOW FSLIT("showParen") 

typeOf_RDR, mkTypeRep_RDR, mkTyConRep_RDR :: RdrName
typeOf_RDR     = varQual_RDR tYPEABLE FSLIT("typeOf")
mkTypeRep_RDR  = varQual_RDR tYPEABLE FSLIT("mkTyConApp")
mkTyConRep_RDR = varQual_RDR tYPEABLE FSLIT("mkTyCon")

undefined_RDR :: RdrName
undefined_RDR = varQual_RDR gHC_ERR FSLIT("undefined")

crossDataCon_RDR, inlDataCon_RDR, inrDataCon_RDR, genUnitDataCon_RDR :: RdrName
crossDataCon_RDR   = dataQual_RDR gHC_GENERICS FSLIT(":*:")
inlDataCon_RDR     = dataQual_RDR gHC_GENERICS FSLIT("Inl")
inrDataCon_RDR     = dataQual_RDR gHC_GENERICS FSLIT("Inr")
genUnitDataCon_RDR = dataQual_RDR gHC_GENERICS FSLIT("Unit")

----------------------
varQual_RDR, tcQual_RDR, clsQual_RDR, dataQual_RDR
    :: Module -> FastString -> RdrName
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
runMainIOName :: Name
runMainIOName = varQual gHC_TOP_HANDLER FSLIT("runMainIO") runMainKey

orderingTyConName :: Name
orderingTyConName = tcQual   gHC_ORDERING FSLIT("Ordering") orderingTyConKey

eitherTyConName, leftDataConName, rightDataConName :: Name
eitherTyConName	  = tcQual  dATA_EITHER FSLIT("Either") eitherTyConKey
leftDataConName   = conName dATA_EITHER FSLIT("Left")   leftDataConKey
rightDataConName  = conName dATA_EITHER FSLIT("Right")  rightDataConKey

-- Generics
crossTyConName, plusTyConName, genUnitTyConName :: Name
crossTyConName     = tcQual   gHC_GENERICS FSLIT(":*:") crossTyConKey
plusTyConName      = tcQual   gHC_GENERICS FSLIT(":+:") plusTyConKey
genUnitTyConName   = tcQual   gHC_GENERICS FSLIT("Unit") genUnitTyConKey

-- Base strings Strings
unpackCStringName, unpackCStringAppendName, unpackCStringFoldrName,
    unpackCStringUtf8Name, eqStringName, stringTyConName :: Name
unpackCStringName       = varQual gHC_BASE FSLIT("unpackCString#") unpackCStringIdKey
unpackCStringAppendName = varQual gHC_BASE FSLIT("unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual gHC_BASE FSLIT("unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual gHC_BASE FSLIT("unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName	 	= varQual gHC_BASE FSLIT("eqString")  eqStringIdKey
stringTyConName         = tcQual  gHC_BASE FSLIT("String") stringTyConKey

-- The 'inline' function
inlineIdName :: Name
inlineIdName	 	= varQual gHC_BASE FSLIT("inline") inlineIdKey

-- Base classes (Eq, Ord, Functor)
eqClassName, eqName, ordClassName, geName, functorClassName :: Name
eqClassName	  = clsQual  gHC_BASE FSLIT("Eq")      eqClassKey
eqName		  = methName gHC_BASE FSLIT("==")      eqClassOpKey
ordClassName	  = clsQual  gHC_BASE FSLIT("Ord")     ordClassKey
geName		  = methName gHC_BASE FSLIT(">=")      geClassOpKey
functorClassName  = clsQual  gHC_BASE FSLIT("Functor") functorClassKey

-- Class Monad
monadClassName, thenMName, bindMName, returnMName, failMName :: Name
monadClassName	   = clsQual  gHC_BASE FSLIT("Monad")  monadClassKey
thenMName	   = methName gHC_BASE FSLIT(">>")     thenMClassOpKey
bindMName	   = methName gHC_BASE FSLIT(">>=")    bindMClassOpKey
returnMName	   = methName gHC_BASE FSLIT("return") returnMClassOpKey
failMName	   = methName gHC_BASE FSLIT("fail")   failMClassOpKey

-- Functions for GHC extensions
groupWithName :: Name
groupWithName = varQual gHC_EXTS FSLIT("groupWith") groupWithIdKey

-- Random PrelBase functions
fromStringName, otherwiseIdName, foldrName, buildName, augmentName,
    mapName, appendName, andName, orName, assertName,
    breakpointName, breakpointCondName, breakpointAutoName,
    opaqueTyConName :: Name
fromStringName = methName dATA_STRING FSLIT("fromString") fromStringClassOpKey
otherwiseIdName   = varQual gHC_BASE FSLIT("otherwise")  otherwiseIdKey
foldrName	  = varQual gHC_BASE FSLIT("foldr")      foldrIdKey
buildName	  = varQual gHC_BASE FSLIT("build")      buildIdKey
augmentName	  = varQual gHC_BASE FSLIT("augment")    augmentIdKey
mapName       = varQual gHC_BASE FSLIT("map")        mapIdKey
appendName	  = varQual gHC_BASE FSLIT("++")         appendIdKey
andName		  = varQual gHC_BASE FSLIT("&&")	 andIdKey
orName		  = varQual gHC_BASE FSLIT("||")	 orIdKey
assertName        = varQual gHC_BASE FSLIT("assert")     assertIdKey
breakpointName    = varQual gHC_BASE FSLIT("breakpoint") breakpointIdKey
breakpointCondName= varQual gHC_BASE FSLIT("breakpointCond") breakpointCondIdKey
breakpointAutoName= varQual gHC_BASE FSLIT("breakpointAuto") breakpointAutoIdKey
opaqueTyConName   = tcQual  gHC_BASE FSLIT("Opaque")   opaqueTyConKey

+breakpointJumpName :: Name
breakpointJumpName
    = mkInternalName
        breakpointJumpIdKey
        (mkOccNameFS varName FSLIT("breakpointJump"))
        noSrcSpan
breakpointCondJumpName :: Name
breakpointCondJumpName
    = mkInternalName
        breakpointCondJumpIdKey
        (mkOccNameFS varName FSLIT("breakpointCondJump"))
        noSrcSpan
breakpointAutoJumpName :: Name
breakpointAutoJumpName
    = mkInternalName
        breakpointAutoJumpIdKey
        (mkOccNameFS varName FSLIT("breakpointAutoJump"))
        noSrcSpan

-- PrelTup
fstName, sndName :: Name
fstName		  = varQual dATA_TUP FSLIT("fst") fstIdKey
sndName		  = varQual dATA_TUP FSLIT("snd") sndIdKey

-- Module PrelNum
numClassName, fromIntegerName, minusName, negateName, plusIntegerName,
    timesIntegerName, integerTyConName, smallIntegerName :: Name
numClassName	  = clsQual  gHC_NUM FSLIT("Num") numClassKey
fromIntegerName   = methName gHC_NUM FSLIT("fromInteger") fromIntegerClassOpKey
minusName	  = methName gHC_NUM FSLIT("-") minusClassOpKey
negateName	  = methName gHC_NUM FSLIT("negate") negateClassOpKey
plusIntegerName   = varQual  gHC_INTEGER FSLIT("plusInteger") plusIntegerIdKey
timesIntegerName  = varQual  gHC_INTEGER FSLIT("timesInteger") timesIntegerIdKey
integerTyConName  = tcQual   gHC_INTEGER FSLIT("Integer") integerTyConKey
smallIntegerName = varQual gHC_INTEGER FSLIT("smallInteger") smallIntegerIdKey

-- PrelReal types and classes
rationalTyConName, ratioTyConName, ratioDataConName, realClassName,
    integralClassName, realFracClassName, fractionalClassName,
    fromRationalName :: Name
rationalTyConName   = tcQual  gHC_REAL FSLIT("Rational") rationalTyConKey
ratioTyConName	    = tcQual  gHC_REAL FSLIT("Ratio") ratioTyConKey
ratioDataConName    = conName gHC_REAL FSLIT(":%") ratioDataConKey
realClassName	    = clsQual gHC_REAL FSLIT("Real") realClassKey
integralClassName   = clsQual gHC_REAL FSLIT("Integral") integralClassKey
realFracClassName   = clsQual gHC_REAL FSLIT("RealFrac") realFracClassKey
fractionalClassName = clsQual gHC_REAL FSLIT("Fractional") fractionalClassKey
fromRationalName    = methName gHC_REAL  FSLIT("fromRational") fromRationalClassOpKey

-- PrelFloat classes
floatingClassName, realFloatClassName :: Name
floatingClassName  = clsQual  gHC_FLOAT FSLIT("Floating") floatingClassKey
realFloatClassName = clsQual  gHC_FLOAT FSLIT("RealFloat") realFloatClassKey

-- Class Ix
ixClassName :: Name
ixClassName = clsQual gHC_ARR FSLIT("Ix") ixClassKey

-- Class Typeable
typeableClassName, typeable1ClassName, typeable2ClassName,
    typeable3ClassName, typeable4ClassName, typeable5ClassName,
    typeable6ClassName, typeable7ClassName :: Name
typeableClassName  = clsQual tYPEABLE FSLIT("Typeable") typeableClassKey
typeable1ClassName = clsQual tYPEABLE FSLIT("Typeable1") typeable1ClassKey
typeable2ClassName = clsQual tYPEABLE FSLIT("Typeable2") typeable2ClassKey
typeable3ClassName = clsQual tYPEABLE FSLIT("Typeable3") typeable3ClassKey
typeable4ClassName = clsQual tYPEABLE FSLIT("Typeable4") typeable4ClassKey
typeable5ClassName = clsQual tYPEABLE FSLIT("Typeable5") typeable5ClassKey
typeable6ClassName = clsQual tYPEABLE FSLIT("Typeable6") typeable6ClassKey
typeable7ClassName = clsQual tYPEABLE FSLIT("Typeable7") typeable7ClassKey

typeableClassNames :: [Name]
typeableClassNames = 	[ typeableClassName, typeable1ClassName, typeable2ClassName
		 	, typeable3ClassName, typeable4ClassName, typeable5ClassName
			, typeable6ClassName, typeable7ClassName ]

-- Class Data
dataClassName :: Name
dataClassName = clsQual gENERICS FSLIT("Data") dataClassKey

-- Error module
assertErrorName    :: Name
assertErrorName	  = varQual gHC_ERR FSLIT("assertError") assertErrorIdKey

-- Enum module (Enum, Bounded)
enumClassName, enumFromName, enumFromToName, enumFromThenName,
    enumFromThenToName, boundedClassName :: Name
enumClassName 	   = clsQual gHC_ENUM FSLIT("Enum") enumClassKey
enumFromName	   = methName gHC_ENUM FSLIT("enumFrom") enumFromClassOpKey
enumFromToName	   = methName gHC_ENUM FSLIT("enumFromTo") enumFromToClassOpKey
enumFromThenName   = methName gHC_ENUM FSLIT("enumFromThen") enumFromThenClassOpKey
enumFromThenToName = methName gHC_ENUM FSLIT("enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual gHC_ENUM FSLIT("Bounded") boundedClassKey

-- List functions
concatName, filterName, zipName :: Name
concatName	  = varQual gHC_LIST FSLIT("concat") concatIdKey
filterName	  = varQual gHC_LIST FSLIT("filter") filterIdKey
zipName	   	  = varQual gHC_LIST FSLIT("zip") zipIdKey

-- Class Show
showClassName :: Name
showClassName	  = clsQual gHC_SHOW FSLIT("Show")       showClassKey

-- Class Read
readClassName :: Name
readClassName	   = clsQual gHC_READ FSLIT("Read") readClassKey

-- parallel array types and functions
enumFromToPName, enumFromThenToPName, nullPName, lengthPName,
    singletonPName, replicatePName, mapPName, filterPName,
    zipPName, crossMapPName, indexPName, toPName, bpermutePName,
    bpermuteDftPName, indexOfPName :: Name
enumFromToPName	   = varQual gHC_PARR FSLIT("enumFromToP") enumFromToPIdKey
enumFromThenToPName= varQual gHC_PARR FSLIT("enumFromThenToP") enumFromThenToPIdKey
nullPName	  = varQual gHC_PARR FSLIT("nullP")      	 nullPIdKey
lengthPName	  = varQual gHC_PARR FSLIT("lengthP")    	 lengthPIdKey
singletonPName    = varQual gHC_PARR FSLIT("singletonP")         singletonPIdKey
replicatePName	  = varQual gHC_PARR FSLIT("replicateP") 	 replicatePIdKey
mapPName	  = varQual gHC_PARR FSLIT("mapP")       	 mapPIdKey
filterPName	  = varQual gHC_PARR FSLIT("filterP")    	 filterPIdKey
zipPName	  = varQual gHC_PARR FSLIT("zipP")       	 zipPIdKey
crossMapPName	  = varQual gHC_PARR FSLIT("crossMapP")     	 crossMapPIdKey
indexPName	  = varQual gHC_PARR FSLIT("!:")	       	 indexPIdKey
toPName	          = varQual gHC_PARR FSLIT("toP")	       	 toPIdKey
bpermutePName     = varQual gHC_PARR FSLIT("bpermuteP")    bpermutePIdKey
bpermuteDftPName  = varQual gHC_PARR FSLIT("bpermuteDftP") bpermuteDftPIdKey
indexOfPName      = varQual gHC_PARR FSLIT("indexOfP")     indexOfPIdKey

-- IOBase things
ioTyConName, ioDataConName, thenIOName, bindIOName, returnIOName,
    failIOName :: Name
ioTyConName	  = tcQual  gHC_IO_BASE FSLIT("IO") ioTyConKey
ioDataConName     = conName gHC_IO_BASE FSLIT("IO") ioDataConKey
thenIOName	  = varQual gHC_IO_BASE FSLIT("thenIO") thenIOIdKey
bindIOName	  = varQual gHC_IO_BASE FSLIT("bindIO") bindIOIdKey
returnIOName	  = varQual gHC_IO_BASE FSLIT("returnIO") returnIOIdKey
failIOName	  = varQual gHC_IO_BASE FSLIT("failIO") failIOIdKey

-- IO things
printName :: Name
printName	  = varQual sYSTEM_IO FSLIT("print") printIdKey

-- Int, Word, and Addr things
int8TyConName, int16TyConName, int32TyConName, int64TyConName :: Name
int8TyConName     = tcQual gHC_INT  FSLIT("Int8") int8TyConKey
int16TyConName    = tcQual gHC_INT  FSLIT("Int16") int16TyConKey
int32TyConName    = tcQual gHC_INT  FSLIT("Int32") int32TyConKey
int64TyConName    = tcQual gHC_INT  FSLIT("Int64") int64TyConKey

-- Word module
word8TyConName, word16TyConName, word32TyConName, word64TyConName,
    wordTyConName, wordDataConName :: Name
word8TyConName    = tcQual  gHC_WORD FSLIT("Word8")  word8TyConKey
word16TyConName   = tcQual  gHC_WORD FSLIT("Word16") word16TyConKey
word32TyConName   = tcQual  gHC_WORD FSLIT("Word32") word32TyConKey
word64TyConName   = tcQual  gHC_WORD FSLIT("Word64") word64TyConKey
wordTyConName     = tcQual  gHC_WORD FSLIT("Word")   wordTyConKey
wordDataConName   = conName gHC_WORD FSLIT("W#") wordDataConKey

-- PrelPtr module
ptrTyConName, funPtrTyConName :: Name
ptrTyConName	  = tcQual   gHC_PTR FSLIT("Ptr") ptrTyConKey
funPtrTyConName	  = tcQual   gHC_PTR FSLIT("FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName, newStablePtrName :: Name
stablePtrTyConName    = tcQual   gHC_STABLE FSLIT("StablePtr") stablePtrTyConKey
newStablePtrName      = varQual  gHC_STABLE FSLIT("newStablePtr") newStablePtrIdKey

-- PrelST module
runSTRepName :: Name
runSTRepName	   = varQual gHC_ST  FSLIT("runSTRep") runSTRepIdKey

-- Recursive-do notation
monadFixClassName, mfixName :: Name
monadFixClassName  = clsQual mONAD_FIX FSLIT("MonadFix") monadFixClassKey
mfixName	   = methName mONAD_FIX FSLIT("mfix") mfixIdKey

-- Arrow notation
arrAName, composeAName, firstAName, appAName, choiceAName, loopAName :: Name
arrAName	   = varQual aRROW FSLIT("arr")	  arrAIdKey
composeAName	   = varQual gHC_DESUGAR FSLIT(">>>") composeAIdKey
firstAName	   = varQual aRROW FSLIT("first") firstAIdKey
appAName	   = varQual aRROW FSLIT("app")	  appAIdKey
choiceAName	   = varQual aRROW FSLIT("|||")	  choiceAIdKey
loopAName	   = varQual aRROW FSLIT("loop")  loopAIdKey

-- Other classes, needed for type defaulting
monadPlusClassName, randomClassName, randomGenClassName,
    isStringClassName :: Name
monadPlusClassName  = clsQual mONAD FSLIT("MonadPlus")  monadPlusClassKey
randomClassName     = clsQual rANDOM FSLIT("Random")    randomClassKey
randomGenClassName  = clsQual rANDOM FSLIT("RandomGen") randomGenClassKey
isStringClassName   = clsQual dATA_STRING FSLIT("IsString") isStringClassKey

-- dotnet interop
objectTyConName :: Name
objectTyConName	    = tcQual   dOTNET FSLIT("Object") objectTyConKey
	-- objectTyConName was "wTcQual", but that's gone now, and
	-- I can't see why it was wired in anyway...
unmarshalObjectName, marshalObjectName, marshalStringName,
    unmarshalStringName, checkDotnetResName :: Name
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
varQual, tcQual, clsQual :: Module -> FastString -> Unique -> Name
varQual  = mk_known_key_name varName
tcQual   = mk_known_key_name tcName
clsQual  = mk_known_key_name clsName

mk_known_key_name :: NameSpace -> Module -> FastString -> Unique -> Name
mk_known_key_name space modu str unique 
  = mkExternalName unique modu (mkOccNameFS space str) noSrcSpan

conName :: Module -> FastString -> Unique -> Name
conName modu occ unique
  = mkExternalName unique modu (mkOccNameFS dataName occ) noSrcSpan

methName :: Module -> FastString -> Unique -> Name
methName modu occ unique
  = mkExternalName unique modu (mkVarOccFS occ) noSrcSpan
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}
%*									*
%************************************************************************
--MetaHaskell extension hand allocate keys here

\begin{code}
boundedClassKey, enumClassKey, eqClassKey, floatingClassKey,
    fractionalClassKey, integralClassKey, monadClassKey, dataClassKey,
    functorClassKey, numClassKey, ordClassKey, readClassKey, realClassKey,
    realFloatClassKey, realFracClassKey, showClassKey, ixClassKey :: Unique
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

typeableClassKey, typeable1ClassKey, typeable2ClassKey, typeable3ClassKey,
    typeable4ClassKey, typeable5ClassKey, typeable6ClassKey, typeable7ClassKey
    :: Unique
typeableClassKey	= mkPreludeClassUnique 20
typeable1ClassKey	= mkPreludeClassUnique 21
typeable2ClassKey	= mkPreludeClassUnique 22
typeable3ClassKey	= mkPreludeClassUnique 23
typeable4ClassKey	= mkPreludeClassUnique 24
typeable5ClassKey	= mkPreludeClassUnique 25
typeable6ClassKey	= mkPreludeClassUnique 26
typeable7ClassKey	= mkPreludeClassUnique 27

monadFixClassKey :: Unique
monadFixClassKey	= mkPreludeClassUnique 28

monadPlusClassKey, randomClassKey, randomGenClassKey :: Unique
monadPlusClassKey	= mkPreludeClassUnique 30
randomClassKey		= mkPreludeClassUnique 31
randomGenClassKey	= mkPreludeClassUnique 32

isStringClassKey :: Unique
isStringClassKey	= mkPreludeClassUnique 33
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
%*									*
%************************************************************************

\begin{code}
addrPrimTyConKey, arrayPrimTyConKey, boolTyConKey, byteArrayPrimTyConKey,
    charPrimTyConKey, charTyConKey, doublePrimTyConKey, doubleTyConKey,
    floatPrimTyConKey, floatTyConKey, funTyConKey, intPrimTyConKey,
    intTyConKey, int8TyConKey, int16TyConKey, int32PrimTyConKey,
    int32TyConKey, int64PrimTyConKey, int64TyConKey, integerTyConKey,
    listTyConKey, foreignObjPrimTyConKey, weakPrimTyConKey,
    mutableArrayPrimTyConKey, mutableByteArrayPrimTyConKey,
    orderingTyConKey, mVarPrimTyConKey, ratioTyConKey, rationalTyConKey,
    realWorldTyConKey, stablePtrPrimTyConKey, stablePtrTyConKey :: Unique
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

anyPrimTyConKey, anyPrimTyCon1Key :: Unique
anyPrimTyConKey				= mkPreludeTyConUnique 37
anyPrimTyCon1Key			= mkPreludeTyConUnique 38

statePrimTyConKey, stableNamePrimTyConKey, stableNameTyConKey,
    mutVarPrimTyConKey, ioTyConKey,
    wordPrimTyConKey, wordTyConKey, word8TyConKey, word16TyConKey,
    word32PrimTyConKey, word32TyConKey, word64PrimTyConKey, word64TyConKey,
    liftedConKey, unliftedConKey, anyBoxConKey, kindConKey, boxityConKey,
    typeConKey, threadIdPrimTyConKey, bcoPrimTyConKey, ptrTyConKey,
    funPtrTyConKey, tVarPrimTyConKey :: Unique
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
crossTyConKey, plusTyConKey, genUnitTyConKey :: Unique
crossTyConKey		      		= mkPreludeTyConUnique 79
plusTyConKey		      		= mkPreludeTyConUnique 80
genUnitTyConKey				= mkPreludeTyConUnique 81

-- Parallel array type constructor
parrTyConKey :: Unique
parrTyConKey				= mkPreludeTyConUnique 82

-- dotnet interop
objectTyConKey :: Unique
objectTyConKey				= mkPreludeTyConUnique 83

eitherTyConKey :: Unique
eitherTyConKey				= mkPreludeTyConUnique 84

-- Super Kinds constructors
tySuperKindTyConKey, coSuperKindTyConKey :: Unique
tySuperKindTyConKey                    = mkPreludeTyConUnique 85
coSuperKindTyConKey                    = mkPreludeTyConUnique 86

-- Kind constructors
liftedTypeKindTyConKey, openTypeKindTyConKey, unliftedTypeKindTyConKey,
    ubxTupleKindTyConKey, argTypeKindTyConKey :: Unique
liftedTypeKindTyConKey                  = mkPreludeTyConUnique 87
openTypeKindTyConKey                    = mkPreludeTyConUnique 88
unliftedTypeKindTyConKey                = mkPreludeTyConUnique 89
ubxTupleKindTyConKey                    = mkPreludeTyConUnique 90
argTypeKindTyConKey                     = mkPreludeTyConUnique 91

-- Coercion constructors
symCoercionTyConKey, transCoercionTyConKey, leftCoercionTyConKey,
    rightCoercionTyConKey, instCoercionTyConKey, unsafeCoercionTyConKey
    :: Unique
symCoercionTyConKey                     = mkPreludeTyConUnique 93
transCoercionTyConKey                   = mkPreludeTyConUnique 94
leftCoercionTyConKey                    = mkPreludeTyConUnique 95
rightCoercionTyConKey                   = mkPreludeTyConUnique 96
instCoercionTyConKey                    = mkPreludeTyConUnique 97
unsafeCoercionTyConKey                  = mkPreludeTyConUnique 98

unknownTyConKey, unknown1TyConKey, unknown2TyConKey, unknown3TyConKey,
    opaqueTyConKey :: Unique
unknownTyConKey				= mkPreludeTyConUnique 99
unknown1TyConKey			= mkPreludeTyConUnique 130
unknown2TyConKey			= mkPreludeTyConUnique 131
unknown3TyConKey			= mkPreludeTyConUnique 132
opaqueTyConKey                          = mkPreludeTyConUnique 133

stringTyConKey :: Unique
stringTyConKey				= mkPreludeTyConUnique 134

---------------- Template Haskell -------------------
--	USES TyConUniques 100-129
-----------------------------------------------------

unitTyConKey :: Unique
unitTyConKey = mkTupleTyConUnique Boxed 0
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
%*									*
%************************************************************************

\begin{code}
charDataConKey, consDataConKey, doubleDataConKey, falseDataConKey,
    floatDataConKey, intDataConKey, nilDataConKey, ratioDataConKey,
    stableNameDataConKey, trueDataConKey, wordDataConKey,
    ioDataConKey :: Unique
charDataConKey				= mkPreludeDataConUnique  1
consDataConKey				= mkPreludeDataConUnique  2
doubleDataConKey			= mkPreludeDataConUnique  3
falseDataConKey				= mkPreludeDataConUnique  4
floatDataConKey				= mkPreludeDataConUnique  5
intDataConKey				= mkPreludeDataConUnique  6
nilDataConKey				= mkPreludeDataConUnique 11
ratioDataConKey				= mkPreludeDataConUnique 12
stableNameDataConKey			= mkPreludeDataConUnique 14
trueDataConKey				= mkPreludeDataConUnique 15
wordDataConKey				= mkPreludeDataConUnique 16
ioDataConKey				= mkPreludeDataConUnique 17

-- Generic data constructors
crossDataConKey, inlDataConKey, inrDataConKey, genUnitDataConKey :: Unique
crossDataConKey		      		= mkPreludeDataConUnique 20
inlDataConKey		      		= mkPreludeDataConUnique 21
inrDataConKey		      		= mkPreludeDataConUnique 22
genUnitDataConKey			= mkPreludeDataConUnique 23

-- Data constructor for parallel arrays
parrDataConKey :: Unique
parrDataConKey				= mkPreludeDataConUnique 24

leftDataConKey, rightDataConKey :: Unique
leftDataConKey				= mkPreludeDataConUnique 25
rightDataConKey				= mkPreludeDataConUnique 26
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
%*									*
%************************************************************************

\begin{code}
absentErrorIdKey, augmentIdKey, appendIdKey, buildIdKey, errorIdKey,
    foldlIdKey, foldrIdKey, recSelErrorIdKey,
    integerMinusOneIdKey, integerPlusOneIdKey,
    integerPlusTwoIdKey, integerZeroIdKey,
    int2IntegerIdKey, seqIdKey, irrefutPatErrorIdKey, eqStringIdKey,
    noMethodBindingErrorIdKey, nonExhaustiveGuardsErrorIdKey,
    runtimeErrorIdKey, parErrorIdKey, parIdKey, patErrorIdKey,
    realWorldPrimIdKey, recConErrorIdKey, recUpdErrorIdKey,
    traceIdKey,
    unpackCStringUtf8IdKey, unpackCStringAppendIdKey,
    unpackCStringFoldrIdKey, unpackCStringIdKey :: Unique
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

unsafeCoerceIdKey, concatIdKey, filterIdKey, zipIdKey, bindIOIdKey,
    returnIOIdKey, deRefStablePtrIdKey, newStablePtrIdKey,
    smallIntegerIdKey, plusIntegerIdKey, timesIntegerIdKey,
    printIdKey, failIOIdKey, nullAddrIdKey, voidArgIdKey,
    fstIdKey, sndIdKey, otherwiseIdKey, assertIdKey, runSTRepIdKey :: Unique
unsafeCoerceIdKey	      = mkPreludeMiscIdUnique 32
concatIdKey		      = mkPreludeMiscIdUnique 33
filterIdKey		      = mkPreludeMiscIdUnique 34
zipIdKey		      = mkPreludeMiscIdUnique 35
bindIOIdKey		      = mkPreludeMiscIdUnique 36
returnIOIdKey		      = mkPreludeMiscIdUnique 37
deRefStablePtrIdKey	      = mkPreludeMiscIdUnique 38
newStablePtrIdKey	      = mkPreludeMiscIdUnique 39
smallIntegerIdKey	      = mkPreludeMiscIdUnique 40
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

rootMainKey, runMainKey :: Unique
rootMainKey		      = mkPreludeMiscIdUnique 55
runMainKey		      = mkPreludeMiscIdUnique 56

andIdKey, orIdKey, thenIOIdKey, lazyIdKey, assertErrorIdKey :: Unique
andIdKey		      = mkPreludeMiscIdUnique 57
orIdKey			      = mkPreludeMiscIdUnique 58
thenIOIdKey		      = mkPreludeMiscIdUnique 59
lazyIdKey		      = mkPreludeMiscIdUnique 60
assertErrorIdKey	      = mkPreludeMiscIdUnique 61

breakpointIdKey, breakpointCondIdKey, breakpointAutoIdKey,
    breakpointJumpIdKey, breakpointCondJumpIdKey,
    breakpointAutoJumpIdKey :: Unique
breakpointIdKey               = mkPreludeMiscIdUnique 62
breakpointCondIdKey           = mkPreludeMiscIdUnique 63
breakpointAutoIdKey           = mkPreludeMiscIdUnique 64
breakpointJumpIdKey           = mkPreludeMiscIdUnique 65
breakpointCondJumpIdKey       = mkPreludeMiscIdUnique 66
breakpointAutoJumpIdKey       = mkPreludeMiscIdUnique 67

inlineIdKey :: Unique
inlineIdKey		      = mkPreludeMiscIdUnique 68

mapIdKey, groupWithIdKey :: Unique
mapIdKey		      = mkPreludeMiscIdUnique 69
groupWithIdKey        = mkPreludeMiscIdUnique 70

-- Parallel array functions
singletonPIdKey, nullPIdKey, lengthPIdKey, replicatePIdKey, mapPIdKey,
    filterPIdKey, zipPIdKey, crossMapPIdKey, indexPIdKey, toPIdKey,
    enumFromToPIdKey, enumFromThenToPIdKey,
    bpermutePIdKey, bpermuteDftPIdKey, indexOfPIdKey :: Unique
singletonPIdKey               = mkPreludeMiscIdUnique 79
nullPIdKey	              = mkPreludeMiscIdUnique 80
lengthPIdKey		      = mkPreludeMiscIdUnique 81
replicatePIdKey		      = mkPreludeMiscIdUnique 82
mapPIdKey		      = mkPreludeMiscIdUnique 83
filterPIdKey		      = mkPreludeMiscIdUnique 84
zipPIdKey		      = mkPreludeMiscIdUnique 85
crossMapPIdKey		      = mkPreludeMiscIdUnique 86
indexPIdKey		      = mkPreludeMiscIdUnique 87
toPIdKey		      = mkPreludeMiscIdUnique 88
enumFromToPIdKey              = mkPreludeMiscIdUnique 89
enumFromThenToPIdKey          = mkPreludeMiscIdUnique 90
bpermutePIdKey		      = mkPreludeMiscIdUnique 91
bpermuteDftPIdKey	      = mkPreludeMiscIdUnique 92
indexOfPIdKey		      = mkPreludeMiscIdUnique 93

-- dotnet interop
unmarshalObjectIdKey, marshalObjectIdKey, marshalStringIdKey,
    unmarshalStringIdKey, checkDotnetResNameIdKey :: Unique
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
unboundKey :: Unique
unboundKey		      = mkPreludeMiscIdUnique 101 

fromIntegerClassOpKey, minusClassOpKey, fromRationalClassOpKey,
    enumFromClassOpKey, enumFromThenClassOpKey, enumFromToClassOpKey,
    enumFromThenToClassOpKey, eqClassOpKey, geClassOpKey, negateClassOpKey,
    failMClassOpKey, bindMClassOpKey, thenMClassOpKey, returnMClassOpKey
    :: Unique
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
mfixIdKey :: Unique
mfixIdKey	= mkPreludeMiscIdUnique 118

-- Arrow notation
arrAIdKey, composeAIdKey, firstAIdKey, appAIdKey, choiceAIdKey,
    loopAIdKey :: Unique
arrAIdKey	= mkPreludeMiscIdUnique 119
composeAIdKey	= mkPreludeMiscIdUnique 120 -- >>>
firstAIdKey	= mkPreludeMiscIdUnique 121
appAIdKey	= mkPreludeMiscIdUnique 122
choiceAIdKey	= mkPreludeMiscIdUnique 123 --  |||
loopAIdKey	= mkPreludeMiscIdUnique 124

fromStringClassOpKey :: Unique
fromStringClassOpKey	      = mkPreludeMiscIdUnique 125

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
numericTyKeys :: [Unique]
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
numericClassKeys :: [Unique]
numericClassKeys =
	[ numClassKey
    	, realClassKey
    	, integralClassKey
	]
	++ fractionalClassKeys

fractionalClassKeys :: [Unique]
fractionalClassKeys = 
    	[ fractionalClassKey
    	, floatingClassKey
    	, realFracClassKey
    	, realFloatClassKey
    	]

	-- the strictness analyser needs to know about numeric types
	-- (see SaAbsInt.lhs)
needsDataDeclCtxtClassKeys :: [Unique]
needsDataDeclCtxtClassKeys = -- see comments in TcDeriv
  	[ readClassKey
    	]

-- The "standard classes" are used in defaulting (Haskell 98 report 4.3.4),
-- and are: "classes defined in the Prelude or a standard library"
standardClassKeys :: [Unique]
standardClassKeys = derivableClassKeys ++ numericClassKeys
		  ++ [randomClassKey, randomGenClassKey,
		      functorClassKey, 
		      monadClassKey, monadPlusClassKey,
		      isStringClassKey
		     ]
\end{code}

@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).

\begin{code}
derivableClassKeys :: [Unique]
derivableClassKeys
  = [ eqClassKey, ordClassKey, enumClassKey, ixClassKey,
      boundedClassKey, showClassKey, readClassKey ]
\end{code}


%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelNames]{Definitions of prelude modules and names}


Nota Bene: all Names defined in here should come from the base package

 - ModuleNames for prelude modules, 
	e.g.	pREL_BASE_Name :: ModuleName

 - Modules for prelude modules
	e.g.	pREL_Base :: Module

 - Uniques for Ids, DataCons, TyCons and Classes that the compiler 
   "knows about" in some way
	e.g.	intTyConKey :: Unique
		minusClassOpKey :: Unique

 - Names for Ids, DataCons, TyCons and Classes that the compiler 
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

 - RdrNames for Ids, DataCons etc that the compiler may emit into
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
import Name       ( Name, mkInternalName, mkExternalName, mkSystemVarName )
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
itName uniq = mkInternalName uniq (mkOccNameFS varName (fsLit "it")) noSrcSpan
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
	applicativeClassName,
	foldableClassName,
	traversableClassName,

	-- Numeric stuff
        negateName, minusName, geName, eqName,

        -- Conversion functions
        fromRationalName, fromIntegerName,
        toIntegerName, toRationalName,
        fromIntegralName, realToFracName,

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

        dollarName,	    -- The ($) apply function

        -- Parallel array operations
	nullPName, lengthPName, replicatePName,	singletonPName, mapPName,
	filterPName, zipPName, crossMapPName, indexPName,
	toPName, emptyPName, appPName,

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

        -- Annotation type checking
        toAnnotationWrapperName

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

gHC_PRIM, gHC_TYPES, gHC_UNIT, gHC_ORDERING, gHC_GENERICS,
    gHC_MAGIC,
    gHC_CLASSES, gHC_BASE, gHC_ENUM,
    gHC_SHOW, gHC_READ, gHC_NUM, gHC_INTEGER, gHC_INTEGER_TYPE, gHC_LIST, gHC_PARR,
    gHC_TUPLE, dATA_TUPLE, dATA_EITHER, dATA_STRING, dATA_FOLDABLE, dATA_TRAVERSABLE,
    gHC_PACK, gHC_CONC, gHC_IO, gHC_IO_Exception,
    gHC_ST, gHC_ARR, gHC_STABLE, gHC_ADDR, gHC_PTR, gHC_ERR, gHC_REAL,
    gHC_FLOAT, gHC_TOP_HANDLER, sYSTEM_IO, dYNAMIC, tYPEABLE, gENERICS,
    dOTNET, rEAD_PREC, lEX, gHC_INT, gHC_WORD, mONAD, mONAD_FIX, aRROW, cONTROL_APPLICATIVE,
    gHC_DESUGAR, rANDOM, gHC_EXTS, cONTROL_EXCEPTION_BASE :: Module

gHC_PRIM	= mkPrimModule (fsLit "GHC.Prim")   -- Primitive types and values
gHC_TYPES       = mkPrimModule (fsLit "GHC.Types")
gHC_UNIT	= mkPrimModule (fsLit "GHC.Unit")
gHC_ORDERING	= mkPrimModule (fsLit "GHC.Ordering")
gHC_GENERICS	= mkPrimModule (fsLit "GHC.Generics")
gHC_MAGIC	= mkPrimModule (fsLit "GHC.Magic")

gHC_CLASSES	= mkBaseModule (fsLit "GHC.Classes")
gHC_BASE	= mkBaseModule (fsLit "GHC.Base")
gHC_ENUM	= mkBaseModule (fsLit "GHC.Enum")
gHC_SHOW	= mkBaseModule (fsLit "GHC.Show")
gHC_READ	= mkBaseModule (fsLit "GHC.Read")
gHC_NUM		= mkBaseModule (fsLit "GHC.Num")
gHC_INTEGER	= mkIntegerModule (fsLit "GHC.Integer")
gHC_INTEGER_TYPE= mkIntegerModule (fsLit "GHC.Integer.Type")
gHC_LIST	= mkBaseModule (fsLit "GHC.List")
gHC_PARR	= mkBaseModule (fsLit "GHC.PArr")
gHC_TUPLE	= mkPrimModule (fsLit "GHC.Tuple")
dATA_TUPLE	= mkBaseModule (fsLit "Data.Tuple")
dATA_EITHER	= mkBaseModule (fsLit "Data.Either")
dATA_STRING	= mkBaseModule (fsLit "Data.String")
dATA_FOLDABLE	= mkBaseModule (fsLit "Data.Foldable")
dATA_TRAVERSABLE= mkBaseModule (fsLit "Data.Traversable")
gHC_PACK	= mkBaseModule (fsLit "GHC.Pack")
gHC_CONC	= mkBaseModule (fsLit "GHC.Conc")
gHC_IO    	= mkBaseModule (fsLit "GHC.IO")
gHC_IO_Exception = mkBaseModule (fsLit "GHC.IO.Exception")
gHC_ST		= mkBaseModule (fsLit "GHC.ST")
gHC_ARR		= mkBaseModule (fsLit "GHC.Arr")
gHC_STABLE	= mkBaseModule (fsLit "GHC.Stable")
gHC_ADDR	= mkBaseModule (fsLit "GHC.Addr")
gHC_PTR		= mkBaseModule (fsLit "GHC.Ptr")
gHC_ERR		= mkBaseModule (fsLit "GHC.Err")
gHC_REAL	= mkBaseModule (fsLit "GHC.Real")
gHC_FLOAT	= mkBaseModule (fsLit "GHC.Float")
gHC_TOP_HANDLER	= mkBaseModule (fsLit "GHC.TopHandler")
sYSTEM_IO	= mkBaseModule (fsLit "System.IO")
dYNAMIC		= mkBaseModule (fsLit "Data.Dynamic")
tYPEABLE	= mkBaseModule (fsLit "Data.Typeable")
gENERICS        = mkBaseModule (fsLit "Data.Data")
dOTNET		= mkBaseModule (fsLit "GHC.Dotnet")
rEAD_PREC	= mkBaseModule (fsLit "Text.ParserCombinators.ReadPrec")
lEX		= mkBaseModule (fsLit "Text.Read.Lex")
gHC_INT		= mkBaseModule (fsLit "GHC.Int")
gHC_WORD	= mkBaseModule (fsLit "GHC.Word")
mONAD		= mkBaseModule (fsLit "Control.Monad")
mONAD_FIX	= mkBaseModule (fsLit "Control.Monad.Fix")
aRROW		= mkBaseModule (fsLit "Control.Arrow")
cONTROL_APPLICATIVE = mkBaseModule (fsLit "Control.Applicative")
gHC_DESUGAR = mkBaseModule (fsLit "GHC.Desugar")
rANDOM		= mkBaseModule (fsLit "System.Random")
gHC_EXTS	= mkBaseModule (fsLit "GHC.Exts")
cONTROL_EXCEPTION_BASE = mkBaseModule (fsLit "Control.Exception.Base")

mAIN, rOOT_MAIN :: Module
mAIN	        = mkMainModule_ mAIN_NAME
rOOT_MAIN	= mkMainModule (fsLit ":Main") -- Root module for initialisation 

	-- The ':xxx' makes a module name that the user can never
	-- use himself.  The z-encoding for ':' is "ZC", so the z-encoded
	-- module name still starts with a capital letter, which keeps
	-- the z-encoded version consistent.
iNTERACTIVE :: Module
iNTERACTIVE    = mkMainModule (fsLit ":Interactive")

pRELUDE_NAME, mAIN_NAME :: ModuleName
pRELUDE_NAME   = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME      = mkModuleNameFS (fsLit "Main")

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
mkTupleModule Boxed   0 = gHC_UNIT
mkTupleModule Boxed   _ = gHC_TUPLE
mkTupleModule Unboxed _ = gHC_PRIM
\end{code}


%************************************************************************
%*									*
			RdrNames
%*									*
%************************************************************************

\begin{code}
main_RDR_Unqual    :: RdrName
main_RDR_Unqual	= mkUnqual varName (fsLit "main")
	-- We definitely don't want an Orig RdrName, because
	-- main might, in principle, be imported into module Main

forall_tv_RDR, dot_tv_RDR :: RdrName
forall_tv_RDR = mkUnqual tvName (fsLit "forall")
dot_tv_RDR    = mkUnqual tvName (fsLit ".")

eq_RDR, ge_RDR, ne_RDR, le_RDR, lt_RDR, gt_RDR, compare_RDR,
    ltTag_RDR, eqTag_RDR, gtTag_RDR :: RdrName
eq_RDR 			= nameRdrName eqName
ge_RDR 			= nameRdrName geName
ne_RDR 			= varQual_RDR  gHC_CLASSES (fsLit "/=")
le_RDR 			= varQual_RDR  gHC_CLASSES (fsLit "<=") 
lt_RDR 			= varQual_RDR  gHC_CLASSES (fsLit "<") 
gt_RDR 			= varQual_RDR  gHC_CLASSES (fsLit ">")  
compare_RDR		= varQual_RDR  gHC_CLASSES (fsLit "compare") 
ltTag_RDR		= dataQual_RDR gHC_ORDERING (fsLit "LT") 
eqTag_RDR		= dataQual_RDR gHC_ORDERING (fsLit "EQ")
gtTag_RDR		= dataQual_RDR gHC_ORDERING (fsLit "GT")

eqClass_RDR, numClass_RDR, ordClass_RDR, enumClass_RDR, monadClass_RDR
    :: RdrName
eqClass_RDR		= nameRdrName eqClassName
numClass_RDR 		= nameRdrName numClassName
ordClass_RDR 		= nameRdrName ordClassName
enumClass_RDR		= nameRdrName enumClassName
monadClass_RDR		= nameRdrName monadClassName

map_RDR, append_RDR :: RdrName
map_RDR 		= varQual_RDR gHC_BASE (fsLit "map")
append_RDR 		= varQual_RDR gHC_BASE (fsLit "++")

foldr_RDR, build_RDR, returnM_RDR, bindM_RDR, failM_RDR :: RdrName
foldr_RDR 		= nameRdrName foldrName
build_RDR 		= nameRdrName buildName
returnM_RDR 		= nameRdrName returnMName
bindM_RDR 		= nameRdrName bindMName
failM_RDR 		= nameRdrName failMName

left_RDR, right_RDR :: RdrName
left_RDR		= nameRdrName leftDataConName
right_RDR		= nameRdrName rightDataConName

fromEnum_RDR, toEnum_RDR :: RdrName
fromEnum_RDR		= varQual_RDR gHC_ENUM (fsLit "fromEnum")
toEnum_RDR		= varQual_RDR gHC_ENUM (fsLit "toEnum")

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
wordDataCon_RDR		= dataQual_RDR gHC_WORD (fsLit "W#")

bindIO_RDR, returnIO_RDR :: RdrName
bindIO_RDR	  	= nameRdrName bindIOName
returnIO_RDR	  	= nameRdrName returnIOName

fromInteger_RDR, fromRational_RDR, minus_RDR, times_RDR, plus_RDR :: RdrName
fromInteger_RDR		= nameRdrName fromIntegerName
fromRational_RDR	= nameRdrName fromRationalName
minus_RDR		= nameRdrName minusName
times_RDR		= varQual_RDR  gHC_NUM (fsLit "*")
plus_RDR                = varQual_RDR gHC_NUM (fsLit "+")

fromString_RDR :: RdrName
fromString_RDR		= nameRdrName fromStringName

compose_RDR :: RdrName
compose_RDR		= varQual_RDR gHC_BASE (fsLit ".")

not_RDR, getTag_RDR, succ_RDR, pred_RDR, minBound_RDR, maxBound_RDR,
    and_RDR, range_RDR, inRange_RDR, index_RDR,
    unsafeIndex_RDR, unsafeRangeSize_RDR :: RdrName
and_RDR			= varQual_RDR gHC_CLASSES (fsLit "&&")
not_RDR 		= varQual_RDR gHC_CLASSES (fsLit "not")
getTag_RDR	 	= varQual_RDR gHC_BASE (fsLit "getTag")
succ_RDR 		= varQual_RDR gHC_ENUM (fsLit "succ")
pred_RDR                = varQual_RDR gHC_ENUM (fsLit "pred")
minBound_RDR            = varQual_RDR gHC_ENUM (fsLit "minBound")
maxBound_RDR            = varQual_RDR gHC_ENUM (fsLit "maxBound")
range_RDR               = varQual_RDR gHC_ARR (fsLit "range")
inRange_RDR             = varQual_RDR gHC_ARR (fsLit "inRange")
index_RDR		= varQual_RDR gHC_ARR (fsLit "index")
unsafeIndex_RDR		= varQual_RDR gHC_ARR (fsLit "unsafeIndex")
unsafeRangeSize_RDR	= varQual_RDR gHC_ARR (fsLit "unsafeRangeSize")

readList_RDR, readListDefault_RDR, readListPrec_RDR, readListPrecDefault_RDR,
    readPrec_RDR, parens_RDR, choose_RDR, lexP_RDR :: RdrName
readList_RDR            = varQual_RDR gHC_READ (fsLit "readList")
readListDefault_RDR     = varQual_RDR gHC_READ (fsLit "readListDefault")
readListPrec_RDR        = varQual_RDR gHC_READ (fsLit "readListPrec")
readListPrecDefault_RDR = varQual_RDR gHC_READ (fsLit "readListPrecDefault")
readPrec_RDR            = varQual_RDR gHC_READ (fsLit "readPrec")
parens_RDR              = varQual_RDR gHC_READ (fsLit "parens")
choose_RDR              = varQual_RDR gHC_READ (fsLit "choose")
lexP_RDR                = varQual_RDR gHC_READ (fsLit "lexP")

punc_RDR, ident_RDR, symbol_RDR :: RdrName
punc_RDR                = dataQual_RDR lEX (fsLit "Punc")
ident_RDR               = dataQual_RDR lEX (fsLit "Ident")
symbol_RDR              = dataQual_RDR lEX (fsLit "Symbol")

step_RDR, alt_RDR, reset_RDR, prec_RDR :: RdrName
step_RDR                = varQual_RDR  rEAD_PREC (fsLit "step")
alt_RDR                 = varQual_RDR  rEAD_PREC (fsLit "+++") 
reset_RDR               = varQual_RDR  rEAD_PREC (fsLit "reset")
prec_RDR                = varQual_RDR  rEAD_PREC (fsLit "prec")

showList_RDR, showList___RDR, showsPrec_RDR, showString_RDR,
    showSpace_RDR, showParen_RDR :: RdrName
showList_RDR            = varQual_RDR gHC_SHOW (fsLit "showList")
showList___RDR          = varQual_RDR gHC_SHOW (fsLit "showList__")
showsPrec_RDR           = varQual_RDR gHC_SHOW (fsLit "showsPrec") 
showString_RDR          = varQual_RDR gHC_SHOW (fsLit "showString")
showSpace_RDR           = varQual_RDR gHC_SHOW (fsLit "showSpace") 
showParen_RDR           = varQual_RDR gHC_SHOW (fsLit "showParen") 

typeOf_RDR, mkTypeRep_RDR, mkTyConRep_RDR :: RdrName
typeOf_RDR     = varQual_RDR tYPEABLE (fsLit "typeOf")
mkTypeRep_RDR  = varQual_RDR tYPEABLE (fsLit "mkTyConApp")
mkTyConRep_RDR = varQual_RDR tYPEABLE (fsLit "mkTyCon")

undefined_RDR :: RdrName
undefined_RDR = varQual_RDR gHC_ERR (fsLit "undefined")

crossDataCon_RDR, inlDataCon_RDR, inrDataCon_RDR, genUnitDataCon_RDR :: RdrName
crossDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit ":*:")
inlDataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "Inl")
inrDataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "Inr")
genUnitDataCon_RDR = dataQual_RDR gHC_GENERICS (fsLit "Unit")

fmap_RDR, pure_RDR, ap_RDR, foldable_foldr_RDR, traverse_RDR :: RdrName
fmap_RDR 		= varQual_RDR gHC_BASE (fsLit "fmap")
pure_RDR 		= varQual_RDR cONTROL_APPLICATIVE (fsLit "pure")
ap_RDR 			= varQual_RDR cONTROL_APPLICATIVE (fsLit "<*>")
foldable_foldr_RDR 	= varQual_RDR dATA_FOLDABLE       (fsLit "foldr")
traverse_RDR 		= varQual_RDR dATA_TRAVERSABLE    (fsLit "traverse")

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
wildCardName :: Name
wildCardName = mkSystemVarName wildCardKey (fsLit "wild")

runMainIOName :: Name
runMainIOName = varQual gHC_TOP_HANDLER (fsLit "runMainIO") runMainKey

orderingTyConName :: Name
orderingTyConName = tcQual   gHC_ORDERING (fsLit "Ordering") orderingTyConKey

eitherTyConName, leftDataConName, rightDataConName :: Name
eitherTyConName	  = tcQual  dATA_EITHER (fsLit "Either") eitherTyConKey
leftDataConName   = conName dATA_EITHER (fsLit "Left")   leftDataConKey
rightDataConName  = conName dATA_EITHER (fsLit "Right")  rightDataConKey

-- Generics
crossTyConName, plusTyConName, genUnitTyConName :: Name
crossTyConName     = tcQual   gHC_GENERICS (fsLit ":*:") crossTyConKey
plusTyConName      = tcQual   gHC_GENERICS (fsLit ":+:") plusTyConKey
genUnitTyConName   = tcQual   gHC_GENERICS (fsLit "Unit") genUnitTyConKey

-- Base strings Strings
unpackCStringName, unpackCStringAppendName, unpackCStringFoldrName,
    unpackCStringUtf8Name, eqStringName, stringTyConName :: Name
unpackCStringName       = varQual gHC_BASE (fsLit "unpackCString#") unpackCStringIdKey
unpackCStringAppendName = varQual gHC_BASE (fsLit "unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual gHC_BASE (fsLit "unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual gHC_BASE (fsLit "unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName	 	= varQual gHC_BASE (fsLit "eqString")  eqStringIdKey
stringTyConName         = tcQual  gHC_BASE (fsLit "String") stringTyConKey

-- The 'inline' function
inlineIdName :: Name
inlineIdName	 	= varQual gHC_MAGIC (fsLit "inline") inlineIdKey

-- Base classes (Eq, Ord, Functor)
eqClassName, eqName, ordClassName, geName, functorClassName :: Name
eqClassName	  = clsQual  gHC_CLASSES (fsLit "Eq")      eqClassKey
eqName		  = methName gHC_CLASSES (fsLit "==")      eqClassOpKey
ordClassName	  = clsQual  gHC_CLASSES (fsLit "Ord")     ordClassKey
geName		  = methName gHC_CLASSES (fsLit ">=")      geClassOpKey
functorClassName  = clsQual  gHC_BASE (fsLit "Functor") functorClassKey

-- Class Monad
monadClassName, thenMName, bindMName, returnMName, failMName :: Name
monadClassName	   = clsQual  gHC_BASE (fsLit "Monad")  monadClassKey
thenMName	   = methName gHC_BASE (fsLit ">>")     thenMClassOpKey
bindMName	   = methName gHC_BASE (fsLit ">>=")    bindMClassOpKey
returnMName	   = methName gHC_BASE (fsLit "return") returnMClassOpKey
failMName	   = methName gHC_BASE (fsLit "fail")   failMClassOpKey

-- Classes (Applicative, Foldable, Traversable)
applicativeClassName, foldableClassName, traversableClassName :: Name
applicativeClassName  = clsQual  cONTROL_APPLICATIVE (fsLit "Applicative") applicativeClassKey
foldableClassName     = clsQual  dATA_FOLDABLE       (fsLit "Foldable")    foldableClassKey
traversableClassName  = clsQual  dATA_TRAVERSABLE    (fsLit "Traversable") traversableClassKey

-- Functions for GHC extensions
groupWithName :: Name
groupWithName = varQual gHC_EXTS (fsLit "groupWith") groupWithIdKey

-- Random PrelBase functions
fromStringName, otherwiseIdName, foldrName, buildName, augmentName,
    mapName, appendName, assertName,
    breakpointName, breakpointCondName, breakpointAutoName,
    dollarName, opaqueTyConName :: Name
fromStringName = methName dATA_STRING (fsLit "fromString") fromStringClassOpKey
otherwiseIdName   = varQual gHC_BASE (fsLit "otherwise")  otherwiseIdKey
foldrName	  = varQual gHC_BASE (fsLit "foldr")      foldrIdKey
buildName	  = varQual gHC_BASE (fsLit "build")      buildIdKey
augmentName	  = varQual gHC_BASE (fsLit "augment")    augmentIdKey
mapName           = varQual gHC_BASE (fsLit "map")        mapIdKey
appendName	  = varQual gHC_BASE (fsLit "++")         appendIdKey
dollarName	  = varQual gHC_BASE (fsLit "$")          dollarIdKey
assertName        = varQual gHC_BASE (fsLit "assert")     assertIdKey
breakpointName    = varQual gHC_BASE (fsLit "breakpoint") breakpointIdKey
breakpointCondName= varQual gHC_BASE (fsLit "breakpointCond") breakpointCondIdKey
breakpointAutoName= varQual gHC_BASE (fsLit "breakpointAuto") breakpointAutoIdKey
opaqueTyConName   = tcQual  gHC_BASE (fsLit "Opaque")   opaqueTyConKey

breakpointJumpName :: Name
breakpointJumpName
    = mkInternalName
        breakpointJumpIdKey
        (mkOccNameFS varName (fsLit "breakpointJump"))
        noSrcSpan
breakpointCondJumpName :: Name
breakpointCondJumpName
    = mkInternalName
        breakpointCondJumpIdKey
        (mkOccNameFS varName (fsLit "breakpointCondJump"))
        noSrcSpan
breakpointAutoJumpName :: Name
breakpointAutoJumpName
    = mkInternalName
        breakpointAutoJumpIdKey
        (mkOccNameFS varName (fsLit "breakpointAutoJump"))
        noSrcSpan

-- PrelTup
fstName, sndName :: Name
fstName		  = varQual dATA_TUPLE (fsLit "fst") fstIdKey
sndName		  = varQual dATA_TUPLE (fsLit "snd") sndIdKey

-- Module GHC.Num
numClassName, fromIntegerName, minusName, negateName, plusIntegerName,
    timesIntegerName,
    integerTyConName, smallIntegerName :: Name
numClassName	  = clsQual  gHC_NUM (fsLit "Num") numClassKey
fromIntegerName   = methName gHC_NUM (fsLit "fromInteger") fromIntegerClassOpKey
minusName	  = methName gHC_NUM (fsLit "-") minusClassOpKey
negateName	  = methName gHC_NUM (fsLit "negate") negateClassOpKey
plusIntegerName   = varQual  gHC_INTEGER (fsLit "plusInteger") plusIntegerIdKey
timesIntegerName  = varQual  gHC_INTEGER (fsLit "timesInteger") timesIntegerIdKey
integerTyConName  = tcQual   gHC_INTEGER_TYPE (fsLit "Integer") integerTyConKey
smallIntegerName = varQual gHC_INTEGER (fsLit "smallInteger") smallIntegerIdKey

-- GHC.Real types and classes
rationalTyConName, ratioTyConName, ratioDataConName, realClassName,
    integralClassName, realFracClassName, fractionalClassName,
    fromRationalName, toIntegerName, toRationalName, fromIntegralName,
    realToFracName :: Name
rationalTyConName   = tcQual  gHC_REAL (fsLit "Rational") rationalTyConKey
ratioTyConName	    = tcQual  gHC_REAL (fsLit "Ratio") ratioTyConKey
ratioDataConName    = conName gHC_REAL (fsLit ":%") ratioDataConKey
realClassName	    = clsQual gHC_REAL (fsLit "Real") realClassKey
integralClassName   = clsQual gHC_REAL (fsLit "Integral") integralClassKey
realFracClassName   = clsQual gHC_REAL (fsLit "RealFrac") realFracClassKey
fractionalClassName = clsQual gHC_REAL (fsLit "Fractional") fractionalClassKey
fromRationalName    = methName gHC_REAL (fsLit "fromRational") fromRationalClassOpKey
toIntegerName       = methName gHC_REAL (fsLit "toInteger") toIntegerClassOpKey
toRationalName      = methName gHC_REAL (fsLit "toRational") toRationalClassOpKey
fromIntegralName    = varQual  gHC_REAL (fsLit "fromIntegral") fromIntegralIdKey
realToFracName      = varQual  gHC_REAL (fsLit "realToFrac") realToFracIdKey

-- PrelFloat classes
floatingClassName, realFloatClassName :: Name
floatingClassName  = clsQual  gHC_FLOAT (fsLit "Floating") floatingClassKey
realFloatClassName = clsQual  gHC_FLOAT (fsLit "RealFloat") realFloatClassKey

-- Class Ix
ixClassName :: Name
ixClassName = clsQual gHC_ARR (fsLit "Ix") ixClassKey

-- Class Typeable
typeableClassName, typeable1ClassName, typeable2ClassName,
    typeable3ClassName, typeable4ClassName, typeable5ClassName,
    typeable6ClassName, typeable7ClassName :: Name
typeableClassName  = clsQual tYPEABLE (fsLit "Typeable") typeableClassKey
typeable1ClassName = clsQual tYPEABLE (fsLit "Typeable1") typeable1ClassKey
typeable2ClassName = clsQual tYPEABLE (fsLit "Typeable2") typeable2ClassKey
typeable3ClassName = clsQual tYPEABLE (fsLit "Typeable3") typeable3ClassKey
typeable4ClassName = clsQual tYPEABLE (fsLit "Typeable4") typeable4ClassKey
typeable5ClassName = clsQual tYPEABLE (fsLit "Typeable5") typeable5ClassKey
typeable6ClassName = clsQual tYPEABLE (fsLit "Typeable6") typeable6ClassKey
typeable7ClassName = clsQual tYPEABLE (fsLit "Typeable7") typeable7ClassKey

typeableClassNames :: [Name]
typeableClassNames = 	[ typeableClassName, typeable1ClassName, typeable2ClassName
		 	, typeable3ClassName, typeable4ClassName, typeable5ClassName
			, typeable6ClassName, typeable7ClassName ]

-- Class Data
dataClassName :: Name
dataClassName = clsQual gENERICS (fsLit "Data") dataClassKey

-- Error module
assertErrorName    :: Name
assertErrorName	  = varQual gHC_IO_Exception (fsLit "assertError") assertErrorIdKey

-- Enum module (Enum, Bounded)
enumClassName, enumFromName, enumFromToName, enumFromThenName,
    enumFromThenToName, boundedClassName :: Name
enumClassName 	   = clsQual gHC_ENUM (fsLit "Enum") enumClassKey
enumFromName	   = methName gHC_ENUM (fsLit "enumFrom") enumFromClassOpKey
enumFromToName	   = methName gHC_ENUM (fsLit "enumFromTo") enumFromToClassOpKey
enumFromThenName   = methName gHC_ENUM (fsLit "enumFromThen") enumFromThenClassOpKey
enumFromThenToName = methName gHC_ENUM (fsLit "enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual gHC_ENUM (fsLit "Bounded") boundedClassKey

-- List functions
concatName, filterName, zipName :: Name
concatName	  = varQual gHC_LIST (fsLit "concat") concatIdKey
filterName	  = varQual gHC_LIST (fsLit "filter") filterIdKey
zipName	   	  = varQual gHC_LIST (fsLit "zip") zipIdKey

-- Class Show
showClassName :: Name
showClassName	  = clsQual gHC_SHOW (fsLit "Show")       showClassKey

-- Class Read
readClassName :: Name
readClassName	   = clsQual gHC_READ (fsLit "Read") readClassKey

-- parallel array types and functions
enumFromToPName, enumFromThenToPName, nullPName, lengthPName,
    singletonPName, replicatePName, mapPName, filterPName,
    zipPName, crossMapPName, indexPName, toPName,
    emptyPName, appPName :: Name
enumFromToPName	   = varQual gHC_PARR (fsLit "enumFromToP") enumFromToPIdKey
enumFromThenToPName= varQual gHC_PARR (fsLit "enumFromThenToP") enumFromThenToPIdKey
nullPName	  = varQual gHC_PARR (fsLit "nullP")      	 nullPIdKey
lengthPName	  = varQual gHC_PARR (fsLit "lengthP")    	 lengthPIdKey
singletonPName    = varQual gHC_PARR (fsLit "singletonP")         singletonPIdKey
replicatePName	  = varQual gHC_PARR (fsLit "replicateP") 	 replicatePIdKey
mapPName	  = varQual gHC_PARR (fsLit "mapP")       	 mapPIdKey
filterPName	  = varQual gHC_PARR (fsLit "filterP")    	 filterPIdKey
zipPName	  = varQual gHC_PARR (fsLit "zipP")       	 zipPIdKey
crossMapPName	  = varQual gHC_PARR (fsLit "crossMapP")     	 crossMapPIdKey
indexPName	  = varQual gHC_PARR (fsLit "!:")	       	 indexPIdKey
toPName	          = varQual gHC_PARR (fsLit "toP")	       	 toPIdKey
emptyPName        = varQual gHC_PARR (fsLit "emptyP")            emptyPIdKey
appPName          = varQual gHC_PARR (fsLit "+:+")               appPIdKey

-- IO things
ioTyConName, ioDataConName, thenIOName, bindIOName, returnIOName,
    failIOName :: Name
ioTyConName	  = tcQual  gHC_TYPES (fsLit "IO") ioTyConKey
ioDataConName     = conName gHC_TYPES (fsLit "IO") ioDataConKey
thenIOName	  = varQual gHC_BASE (fsLit "thenIO") thenIOIdKey
bindIOName	  = varQual gHC_BASE (fsLit "bindIO") bindIOIdKey
returnIOName	  = varQual gHC_BASE (fsLit "returnIO") returnIOIdKey
failIOName	  = varQual gHC_IO (fsLit "failIO") failIOIdKey

-- IO things
printName :: Name
printName	  = varQual sYSTEM_IO (fsLit "print") printIdKey

-- Int, Word, and Addr things
int8TyConName, int16TyConName, int32TyConName, int64TyConName :: Name
int8TyConName     = tcQual gHC_INT  (fsLit "Int8") int8TyConKey
int16TyConName    = tcQual gHC_INT  (fsLit "Int16") int16TyConKey
int32TyConName    = tcQual gHC_INT  (fsLit "Int32") int32TyConKey
int64TyConName    = tcQual gHC_INT  (fsLit "Int64") int64TyConKey

-- Word module
word8TyConName, word16TyConName, word32TyConName, word64TyConName,
    wordTyConName, wordDataConName :: Name
word8TyConName    = tcQual  gHC_WORD (fsLit "Word8")  word8TyConKey
word16TyConName   = tcQual  gHC_WORD (fsLit "Word16") word16TyConKey
word32TyConName   = tcQual  gHC_WORD (fsLit "Word32") word32TyConKey
word64TyConName   = tcQual  gHC_WORD (fsLit "Word64") word64TyConKey
wordTyConName     = tcQual  gHC_WORD (fsLit "Word")   wordTyConKey
wordDataConName   = conName gHC_WORD (fsLit "W#") wordDataConKey

-- PrelPtr module
ptrTyConName, funPtrTyConName :: Name
ptrTyConName	  = tcQual   gHC_PTR (fsLit "Ptr") ptrTyConKey
funPtrTyConName	  = tcQual   gHC_PTR (fsLit "FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName, newStablePtrName :: Name
stablePtrTyConName    = tcQual   gHC_STABLE (fsLit "StablePtr") stablePtrTyConKey
newStablePtrName      = varQual  gHC_STABLE (fsLit "newStablePtr") newStablePtrIdKey

-- PrelST module
runSTRepName :: Name
runSTRepName	   = varQual gHC_ST  (fsLit "runSTRep") runSTRepIdKey

-- Recursive-do notation
monadFixClassName, mfixName :: Name
monadFixClassName  = clsQual mONAD_FIX (fsLit "MonadFix") monadFixClassKey
mfixName	   = methName mONAD_FIX (fsLit "mfix") mfixIdKey

-- Arrow notation
arrAName, composeAName, firstAName, appAName, choiceAName, loopAName :: Name
arrAName	   = varQual aRROW (fsLit "arr")	  arrAIdKey
composeAName	   = varQual gHC_DESUGAR (fsLit ">>>") composeAIdKey
firstAName	   = varQual aRROW (fsLit "first") firstAIdKey
appAName	   = varQual aRROW (fsLit "app")	  appAIdKey
choiceAName	   = varQual aRROW (fsLit "|||")	  choiceAIdKey
loopAName	   = varQual aRROW (fsLit "loop")  loopAIdKey

-- Annotation type checking
toAnnotationWrapperName :: Name
toAnnotationWrapperName = varQual gHC_DESUGAR (fsLit "toAnnotationWrapper") toAnnotationWrapperIdKey

-- Other classes, needed for type defaulting
monadPlusClassName, randomClassName, randomGenClassName,
    isStringClassName :: Name
monadPlusClassName  = clsQual mONAD (fsLit "MonadPlus")  monadPlusClassKey
randomClassName     = clsQual rANDOM (fsLit "Random")    randomClassKey
randomGenClassName  = clsQual rANDOM (fsLit "RandomGen") randomGenClassKey
isStringClassName   = clsQual dATA_STRING (fsLit "IsString") isStringClassKey

-- dotnet interop
objectTyConName :: Name
objectTyConName	    = tcQual   dOTNET (fsLit "Object") objectTyConKey
	-- objectTyConName was "wTcQual", but that's gone now, and
	-- I can't see why it was wired in anyway...
unmarshalObjectName, marshalObjectName, marshalStringName,
    unmarshalStringName, checkDotnetResName :: Name
unmarshalObjectName = varQual  dOTNET (fsLit "unmarshalObject") unmarshalObjectIdKey
marshalObjectName   = varQual  dOTNET (fsLit "marshalObject") marshalObjectIdKey
marshalStringName   = varQual  dOTNET (fsLit "marshalString") marshalStringIdKey
unmarshalStringName = varQual  dOTNET (fsLit "unmarshalString") unmarshalStringIdKey
checkDotnetResName  = varQual  dOTNET (fsLit "checkResult")     checkDotnetResNameIdKey
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

applicativeClassKey, foldableClassKey, traversableClassKey :: Unique
applicativeClassKey	= mkPreludeClassUnique 34
foldableClassKey	= mkPreludeClassUnique 35
traversableClassKey	= mkPreludeClassUnique 36
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
    realWorldTyConKey, stablePtrPrimTyConKey, stablePtrTyConKey,
    anyTyConKey :: Unique
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
anyTyConKey				= mkPreludeTyConUnique 37

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
    ubxTupleKindTyConKey, argTypeKindTyConKey, natKindTyConKey :: Unique
liftedTypeKindTyConKey                  = mkPreludeTyConUnique 87
openTypeKindTyConKey                    = mkPreludeTyConUnique 88
unliftedTypeKindTyConKey                = mkPreludeTyConUnique 89
ubxTupleKindTyConKey                    = mkPreludeTyConUnique 90
argTypeKindTyConKey                     = mkPreludeTyConUnique 91
natKindTyConKey                         = mkPreludeTyConUnique 92

-- Coercion constructors
symCoercionTyConKey, transCoercionTyConKey, leftCoercionTyConKey,
    rightCoercionTyConKey, instCoercionTyConKey, unsafeCoercionTyConKey,
    csel1CoercionTyConKey, csel2CoercionTyConKey, cselRCoercionTyConKey
    :: Unique
symCoercionTyConKey                     = mkPreludeTyConUnique 93
transCoercionTyConKey                   = mkPreludeTyConUnique 94
leftCoercionTyConKey                    = mkPreludeTyConUnique 95
rightCoercionTyConKey                   = mkPreludeTyConUnique 96
instCoercionTyConKey                    = mkPreludeTyConUnique 97
unsafeCoercionTyConKey                  = mkPreludeTyConUnique 98
csel1CoercionTyConKey                   = mkPreludeTyConUnique 99
csel2CoercionTyConKey                   = mkPreludeTyConUnique 100
cselRCoercionTyConKey                   = mkPreludeTyConUnique 101

unknownTyConKey, unknown1TyConKey, unknown2TyConKey, unknown3TyConKey,
    opaqueTyConKey :: Unique
unknownTyConKey				= mkPreludeTyConUnique 129
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
    ioDataConKey, integerDataConKey :: Unique
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
integerDataConKey			= mkPreludeDataConUnique 18

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
    traceIdKey, wildCardKey,
    unpackCStringUtf8IdKey, unpackCStringAppendIdKey,
    unpackCStringFoldrIdKey, unpackCStringIdKey :: Unique
wildCardKey                   = mkPreludeMiscIdUnique  0  -- See Note [WildCard]
absentErrorIdKey              = mkPreludeMiscIdUnique  1
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

thenIOIdKey, lazyIdKey, assertErrorIdKey :: Unique
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

mapIdKey, groupWithIdKey, dollarIdKey :: Unique
mapIdKey	      = mkPreludeMiscIdUnique 69
groupWithIdKey        = mkPreludeMiscIdUnique 70
dollarIdKey           = mkPreludeMiscIdUnique 71

-- Parallel array functions
singletonPIdKey, nullPIdKey, lengthPIdKey, replicatePIdKey, mapPIdKey,
    filterPIdKey, zipPIdKey, crossMapPIdKey, indexPIdKey, toPIdKey,
    enumFromToPIdKey, enumFromThenToPIdKey, emptyPIdKey, appPIdKey :: Unique
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
emptyPIdKey                   = mkPreludeMiscIdUnique 91
appPIdKey                     = mkPreludeMiscIdUnique 92

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

-- Annotation type checking
toAnnotationWrapperIdKey :: Unique
toAnnotationWrapperIdKey      = mkPreludeMiscIdUnique 126

-- Conversion functions
fromIntegralIdKey, realToFracIdKey, toIntegerClassOpKey, toRationalClassOpKey :: Unique
fromIntegralIdKey    = mkPreludeMiscIdUnique 127
realToFracIdKey      = mkPreludeMiscIdUnique 128
toIntegerClassOpKey  = mkPreludeMiscIdUnique 129
toRationalClassOpKey = mkPreludeMiscIdUnique 130

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

kindKeys :: [Unique] 
kindKeys = [ liftedTypeKindTyConKey
	   , openTypeKindTyConKey
	   , unliftedTypeKindTyConKey
	   , ubxTupleKindTyConKey 
	   , argTypeKindTyConKey
           , natKindTyConKey
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
		      isStringClassKey,
		      applicativeClassKey, foldableClassKey, traversableClassKey
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


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

import Module	  ( ModuleName, mkBasePkgModule, mkHomeModule, mkModuleName )
import OccName	  ( UserFS, dataName, tcName, clsName, varName,
		    mkKindOccFS, mkOccFS
		  )
		  
import RdrName	  ( RdrName, nameRdrName, mkOrig, rdrNameOcc )
import Unique	  ( Unique, Uniquable(..), hasKey,
		    mkPreludeMiscIdUnique, mkPreludeDataConUnique,
		    mkPreludeTyConUnique, mkPreludeClassUnique,
		    mkTupleTyConUnique, isTupleKey
		  ) 
import BasicTypes ( Boxity(..) )
import Name	  ( Name, mkInternalName, mkKnownKeyExternalName, mkWiredInName, nameUnique )
import SrcLoc     ( noSrcLoc )
import Util	  ( nOfThem )
import Panic	  ( panic )
import FastString


\end{code}


%************************************************************************
%*									*
\subsection{Local Names}
%*									*
%************************************************************************

This *local* name is used by the interactive stuff

\begin{code}
itName uniq = mkInternalName uniq (mkOccFS varName FSLIT("it")) noSrcLoc
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
\subsection{Built-in-syntax names
%*									*
%************************************************************************

Built-in syntax names are parsed directly into Exact RdrNames.
This predicate just identifies them. 

\begin{code}
isBuiltInSyntaxName :: Name -> Bool
isBuiltInSyntaxName n
  =  isTupleKey uniq
  || uniq `elem` [listTyConKey, nilDataConKey, consDataConKey,
		  funTyConKey, parrTyConKey]
  where
     uniq = nameUnique n
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
 =  [	-- Type constructors (synonyms especially)
	ioTyConName, ioDataConName,
	runIOName,
	orderingTyConName,
	rationalTyConName,
	ratioDataConName,
	ratioTyConName,
	byteArrayTyConName,
	mutableByteArrayTyConName,
	bcoPrimTyConName,

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
	typeableClassName,

	-- Numeric stuff
	negateName, minusName, 
	fromRationalName, fromIntegerName, 
	geName, eqName, 
	
	-- Enum stuff
	enumFromName, enumFromThenName,	
	enumFromThenToName, enumFromToName,
	enumFromToPName, enumFromThenToPName,

	-- Monad stuff
	thenMName, bindMName, returnMName, failMName,
	thenIOName, bindIOName, returnIOName, failIOName,

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
	stablePtrTyConName, ptrTyConName, funPtrTyConName, addrTyConName,
	int8TyConName, int16TyConName, int32TyConName, int64TyConName,
	word8TyConName, word16TyConName, word32TyConName, word64TyConName,

	-- Others
	unsafeCoerceName, otherwiseIdName, 
	plusIntegerName, timesIntegerName,
	eqStringName, assertName, assertErrorName, runSTRepName,
	printName, splitName, fstName, sndName,
	errorName,

	-- Booleans
	andName, orName
	
	-- The Either type
	, eitherTyConName, leftDataConName, rightDataConName

	-- dotnet interop
	, objectTyConName, marshalObjectName, unmarshalObjectName
	, marshalStringName, unmarshalStringName, checkDotnetResName
    ]

monadNames :: [Name]	-- The monad ops need by a HsDo
monadNames = [returnMName, failMName, bindMName, thenMName]
\end{code}


%************************************************************************
%*									*
\subsection{Module names}
%*									*
%************************************************************************


--MetaHaskell Extension Add a new module here
\begin{code}
pRELUDE_Name      = mkModuleName "Prelude"
gHC_PRIM_Name     = mkModuleName "GHC.Prim"	   -- Primitive types and values
pREL_BASE_Name    = mkModuleName "GHC.Base"
pREL_ENUM_Name    = mkModuleName "GHC.Enum"
pREL_SHOW_Name    = mkModuleName "GHC.Show"
pREL_READ_Name    = mkModuleName "GHC.Read"
pREL_NUM_Name     = mkModuleName "GHC.Num"
pREL_LIST_Name    = mkModuleName "GHC.List"
pREL_PARR_Name    = mkModuleName "GHC.PArr"
pREL_TUP_Name     = mkModuleName "Data.Tuple"
pREL_EITHER_Name  = mkModuleName "Data.Either"
pREL_PACK_Name    = mkModuleName "GHC.Pack"
pREL_CONC_Name    = mkModuleName "GHC.Conc"
pREL_IO_BASE_Name = mkModuleName "GHC.IOBase"
pREL_ST_Name	  = mkModuleName "GHC.ST"
pREL_ARR_Name     = mkModuleName "GHC.Arr"
pREL_BYTEARR_Name = mkModuleName "PrelByteArr"
pREL_STABLE_Name  = mkModuleName "GHC.Stable"
pREL_ADDR_Name    = mkModuleName "GHC.Addr"
pREL_PTR_Name     = mkModuleName "GHC.Ptr"
pREL_ERR_Name     = mkModuleName "GHC.Err"
pREL_REAL_Name    = mkModuleName "GHC.Real"
pREL_FLOAT_Name   = mkModuleName "GHC.Float"
pREL_TOP_HANDLER_Name = mkModuleName "GHC.TopHandler"
sYSTEM_IO_Name	  = mkModuleName "System.IO"
dYNAMIC_Name	  = mkModuleName "Data.Dynamic"
tYPEABLE_Name	  = mkModuleName "Data.Typeable"
gENERICS_Name	  = mkModuleName "Data.Generics.Basics"
dOTNET_Name       = mkModuleName "GHC.Dotnet"

rEAD_PREC_Name = mkModuleName "Text.ParserCombinators.ReadPrec"
lEX_Name       = mkModuleName "Text.Read.Lex"

mAIN_Name	  = mkModuleName "Main"
pREL_INT_Name	  = mkModuleName "GHC.Int"
pREL_WORD_Name	  = mkModuleName "GHC.Word"
mONAD_FIX_Name	  = mkModuleName "Control.Monad.Fix"
aRROW_Name	  = mkModuleName "Control.Arrow"
aDDR_Name	  = mkModuleName "Addr"

gLA_EXTS_Name   = mkModuleName "GHC.Exts"

gHC_PRIM     	= mkBasePkgModule gHC_PRIM_Name
pREL_BASE    	= mkBasePkgModule pREL_BASE_Name
pREL_ADDR    	= mkBasePkgModule pREL_ADDR_Name
pREL_PTR    	= mkBasePkgModule pREL_PTR_Name
pREL_STABLE  	= mkBasePkgModule pREL_STABLE_Name
pREL_IO_BASE 	= mkBasePkgModule pREL_IO_BASE_Name
pREL_PACK    	= mkBasePkgModule pREL_PACK_Name
pREL_ERR     	= mkBasePkgModule pREL_ERR_Name
pREL_NUM     	= mkBasePkgModule pREL_NUM_Name
pREL_REAL    	= mkBasePkgModule pREL_REAL_Name
pREL_FLOAT   	= mkBasePkgModule pREL_FLOAT_Name
pRELUDE		= mkBasePkgModule pRELUDE_Name

-- MetaHaskell Extension  text2 from Meta/work/gen.hs
mETA_META_Name   = mkModuleName "Language.Haskell.THSyntax"

rOOT_MAIN_Name = mkModuleName ":Main"		-- Root module for initialisation 
rOOT_MAIN      = mkHomeModule rOOT_MAIN_Name	
	-- The ':xxx' makes a moudle name that the user can never
	-- use himself.  The z-encoding for ':' is "ZC", so the z-encoded
	-- module name still starts with a capital letter, which keeps
	-- the z-encoded version consistent.
iNTERACTIVE    = mkHomeModule (mkModuleName ":Interactive")
\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupNameStr :: Boxity -> Int -> (ModuleName, UserFS)

mkTupNameStr Boxed 0 = (pREL_BASE_Name, FSLIT("()"))
mkTupNameStr Boxed 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr Boxed 2 = (pREL_TUP_Name, mkFastString "(,)")   -- not strictly necessary
mkTupNameStr Boxed 3 = (pREL_TUP_Name, mkFastString "(,,)")  -- ditto
mkTupNameStr Boxed 4 = (pREL_TUP_Name, mkFastString "(,,,)") -- ditto
mkTupNameStr Boxed n = (pREL_TUP_Name, mkFastString ("(" ++ nOfThem (n-1) ',' ++ ")"))

mkTupNameStr Unboxed 0 = (gHC_PRIM_Name, mkFastString "(# #)") -- 1 and 0 both make sense!!!
--panic "Name.mkUbxTupNameStr: 0 ???"
mkTupNameStr Unboxed 1 = (gHC_PRIM_Name, mkFastString "(# #)") -- 1 and 0 both make sense!!!
mkTupNameStr Unboxed 2 = (gHC_PRIM_Name, mkFastString "(#,#)")
mkTupNameStr Unboxed 3 = (gHC_PRIM_Name, mkFastString "(#,,#)")
mkTupNameStr Unboxed 4 = (gHC_PRIM_Name, mkFastString "(#,,,#)")
mkTupNameStr Unboxed n = (gHC_PRIM_Name, mkFastString ("(#" ++ nOfThem (n-1) ',' ++ "#)"))
\end{code}


%************************************************************************
%*									*
			RdrNames
%*									*
%************************************************************************

\begin{code}
eq_RDR 			= nameRdrName eqName
ge_RDR 			= nameRdrName geName
ne_RDR 			= varQual_RDR  pREL_BASE_Name FSLIT("/=")
le_RDR 			= varQual_RDR  pREL_BASE_Name FSLIT("<=") 
gt_RDR 			= varQual_RDR  pREL_BASE_Name FSLIT(">")  
compare_RDR		= varQual_RDR  pREL_BASE_Name FSLIT("compare") 
ltTag_RDR		= dataQual_RDR pREL_BASE_Name FSLIT("LT") 
eqTag_RDR		= dataQual_RDR pREL_BASE_Name FSLIT("EQ")
gtTag_RDR		= dataQual_RDR pREL_BASE_Name FSLIT("GT")

eqClass_RDR		= nameRdrName eqClassName
numClass_RDR 		= nameRdrName numClassName
ordClass_RDR 		= nameRdrName ordClassName
enumClass_RDR		= nameRdrName enumClassName
monadClass_RDR		= nameRdrName monadClassName

map_RDR 		= varQual_RDR pREL_BASE_Name FSLIT("map")
append_RDR 		= varQual_RDR pREL_BASE_Name FSLIT("++")

foldr_RDR 		= nameRdrName foldrName
build_RDR 		= nameRdrName buildName
returnM_RDR 		= nameRdrName returnMName
bindM_RDR 		= nameRdrName bindMName
failM_RDR 		= nameRdrName failMName

false_RDR		= nameRdrName falseDataConName
true_RDR		= nameRdrName trueDataConName
and_RDR			= nameRdrName andName

left_RDR		= nameRdrName leftDataConName
right_RDR		= nameRdrName rightDataConName

error_RDR		= nameRdrName errorName

fromEnum_RDR		= varQual_RDR pREL_ENUM_Name FSLIT("fromEnum")
toEnum_RDR		= varQual_RDR pREL_ENUM_Name FSLIT("toEnum")
mkInt_RDR		= nameRdrName intDataConName

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
addrDataCon_RDR		= dataQual_RDR aDDR_Name FSLIT("A#")

bindIO_RDR	  	= nameRdrName bindIOName
returnIO_RDR	  	= nameRdrName returnIOName

fromInteger_RDR		= nameRdrName fromIntegerName
fromRational_RDR	= nameRdrName fromRationalName
minus_RDR		= nameRdrName minusName
times_RDR		= varQual_RDR  pREL_NUM_Name FSLIT("*")
plus_RDR                = varQual_RDR pREL_NUM_Name FSLIT("+")

compose_RDR		= varQual_RDR pREL_BASE_Name FSLIT(".")

not_RDR 		= varQual_RDR pREL_BASE_Name FSLIT("not")
getTag_RDR	 	= varQual_RDR pREL_BASE_Name FSLIT("getTag")
succ_RDR 		= varQual_RDR pREL_ENUM_Name FSLIT("succ")
pred_RDR                = varQual_RDR pREL_ENUM_Name FSLIT("pred")
minBound_RDR            = varQual_RDR pREL_ENUM_Name FSLIT("minBound")
maxBound_RDR            = varQual_RDR pREL_ENUM_Name FSLIT("maxBound")
range_RDR               = varQual_RDR pREL_ARR_Name FSLIT("range")
inRange_RDR             = varQual_RDR pREL_ARR_Name FSLIT("inRange")
index_RDR		= varQual_RDR pREL_ARR_Name FSLIT("index")

readList_RDR            = varQual_RDR pREL_READ_Name FSLIT("readList")
readListDefault_RDR     = varQual_RDR pREL_READ_Name FSLIT("readListDefault")
readListPrec_RDR        = varQual_RDR pREL_READ_Name FSLIT("readListPrec")
readListPrecDefault_RDR = varQual_RDR pREL_READ_Name FSLIT("readListPrecDefault")
readPrec_RDR            = varQual_RDR pREL_READ_Name FSLIT("readPrec")
parens_RDR              = varQual_RDR pREL_READ_Name FSLIT("parens")
choose_RDR              = varQual_RDR pREL_READ_Name FSLIT("choose")
lexP_RDR                = varQual_RDR pREL_READ_Name FSLIT("lexP")

punc_RDR                = dataQual_RDR lEX_Name FSLIT("Punc")
ident_RDR               = dataQual_RDR lEX_Name FSLIT("Ident")
symbol_RDR              = dataQual_RDR lEX_Name FSLIT("Symbol")

step_RDR                = varQual_RDR  rEAD_PREC_Name FSLIT("step")
alt_RDR                 = varQual_RDR  rEAD_PREC_Name FSLIT("+++") 
reset_RDR               = varQual_RDR  rEAD_PREC_Name FSLIT("reset")
prec_RDR                = varQual_RDR  rEAD_PREC_Name FSLIT("prec")

showList_RDR            = varQual_RDR pREL_SHOW_Name FSLIT("showList")
showList___RDR          = varQual_RDR pREL_SHOW_Name FSLIT("showList__")
showsPrec_RDR           = varQual_RDR pREL_SHOW_Name FSLIT("showsPrec") 
showString_RDR          = varQual_RDR pREL_SHOW_Name FSLIT("showString")
showSpace_RDR           = varQual_RDR pREL_SHOW_Name FSLIT("showSpace") 
showParen_RDR           = varQual_RDR pREL_SHOW_Name FSLIT("showParen") 

typeOf_RDR     = varQual_RDR tYPEABLE_Name FSLIT("typeOf")
mkTypeRep_RDR  = varQual_RDR tYPEABLE_Name FSLIT("mkAppTy")
mkTyConRep_RDR = varQual_RDR tYPEABLE_Name FSLIT("mkTyCon")

undefined_RDR = varQual_RDR pREL_ERR_Name FSLIT("undefined")
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
rootMainName = varQual rOOT_MAIN_Name FSLIT("main") rootMainKey
runIOName    = varQual pREL_TOP_HANDLER_Name FSLIT("runIO") runMainKey

-- Stuff from GHC.Prim
superKindName    = kindQual FSLIT("KX") kindConKey
superBoxityName  = kindQual FSLIT("BX") boxityConKey
liftedConName    = kindQual FSLIT("*") liftedConKey
unliftedConName  = kindQual FSLIT("#") unliftedConKey
openKindConName  = kindQual FSLIT("?") anyBoxConKey
typeConName	 = kindQual FSLIT("Type") typeConKey

funTyConName	    	      = tcQual  gHC_PRIM_Name FSLIT("(->)")  funTyConKey
charPrimTyConName    	      = tcQual  gHC_PRIM_Name FSLIT("Char#") charPrimTyConKey 
intPrimTyConName     	      = tcQual  gHC_PRIM_Name FSLIT("Int#") intPrimTyConKey 
int32PrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("Int32#") int32PrimTyConKey 
int64PrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("Int64#") int64PrimTyConKey 
wordPrimTyConName    	      = tcQual  gHC_PRIM_Name FSLIT("Word#") wordPrimTyConKey 
word32PrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Word32#") word32PrimTyConKey 
word64PrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Word64#") word64PrimTyConKey 
addrPrimTyConName    	      = tcQual  gHC_PRIM_Name FSLIT("Addr#") addrPrimTyConKey 
floatPrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("Float#") floatPrimTyConKey 
doublePrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Double#") doublePrimTyConKey 
statePrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("State#") statePrimTyConKey 
realWorldTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("RealWorld") realWorldTyConKey 
arrayPrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("Array#") arrayPrimTyConKey 
byteArrayPrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("ByteArray#") byteArrayPrimTyConKey 
mutableArrayPrimTyConName     = tcQual  gHC_PRIM_Name FSLIT("MutableArray#") mutableArrayPrimTyConKey 
mutableByteArrayPrimTyConName = tcQual  gHC_PRIM_Name FSLIT("MutableByteArray#") mutableByteArrayPrimTyConKey 
mutVarPrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("MutVar#") mutVarPrimTyConKey 
mVarPrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("MVar#") mVarPrimTyConKey 
stablePtrPrimTyConName        = tcQual  gHC_PRIM_Name FSLIT("StablePtr#") stablePtrPrimTyConKey 
stableNamePrimTyConName       = tcQual  gHC_PRIM_Name FSLIT("StableName#") stableNamePrimTyConKey 
foreignObjPrimTyConName       = tcQual  gHC_PRIM_Name FSLIT("ForeignObj#") foreignObjPrimTyConKey 
bcoPrimTyConName 	      = tcQual  gHC_PRIM_Name FSLIT("BCO#") bcoPrimTyConKey 
weakPrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Weak#") weakPrimTyConKey 
threadIdPrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("ThreadId#") threadIdPrimTyConKey 

unsafeCoerceName = wVarQual gHC_PRIM_Name FSLIT("unsafeCoerce#") unsafeCoerceIdKey 
nullAddrName     = wVarQual gHC_PRIM_Name FSLIT("nullAddr#")	nullAddrIdKey
seqName		 = wVarQual gHC_PRIM_Name FSLIT("seq")		seqIdKey
realWorldName	 = wVarQual gHC_PRIM_Name FSLIT("realWorld#")	realWorldPrimIdKey

-- PrelBase data types and constructors
charTyConName	  = wTcQual   pREL_BASE_Name FSLIT("Char") charTyConKey
charDataConName   = wDataQual pREL_BASE_Name FSLIT("C#") charDataConKey
intTyConName	  = wTcQual   pREL_BASE_Name FSLIT("Int") intTyConKey
intDataConName	  = wDataQual pREL_BASE_Name FSLIT("I#") intDataConKey
orderingTyConName = tcQual   pREL_BASE_Name FSLIT("Ordering") orderingTyConKey
boolTyConName	  = wTcQual   pREL_BASE_Name FSLIT("Bool") boolTyConKey
falseDataConName  = wDataQual pREL_BASE_Name FSLIT("False") falseDataConKey
trueDataConName	  = wDataQual pREL_BASE_Name FSLIT("True") trueDataConKey
listTyConName	  = wTcQual   pREL_BASE_Name FSLIT("[]") listTyConKey
nilDataConName 	  = wDataQual pREL_BASE_Name FSLIT("[]") nilDataConKey
consDataConName	  = wDataQual pREL_BASE_Name FSLIT(":") consDataConKey
eqName		  = varQual  pREL_BASE_Name FSLIT("==") eqClassOpKey
geName		  = varQual  pREL_BASE_Name FSLIT(">=") geClassOpKey

eitherTyConName	  = tcQual   pREL_EITHER_Name FSLIT("Either") eitherTyConKey
leftDataConName   = dataQual pREL_EITHER_Name FSLIT("Left")   leftDataConKey
rightDataConName  = dataQual pREL_EITHER_Name FSLIT("Right")  rightDataConKey

-- Generics
crossTyConName     = tcQual   pREL_BASE_Name FSLIT(":*:") crossTyConKey
crossDataConName   = dataQual pREL_BASE_Name FSLIT(":*:") crossDataConKey
plusTyConName      = wTcQual   pREL_BASE_Name FSLIT(":+:") plusTyConKey
inlDataConName     = wDataQual pREL_BASE_Name FSLIT("Inl") inlDataConKey
inrDataConName     = wDataQual pREL_BASE_Name FSLIT("Inr") inrDataConKey
genUnitTyConName   = wTcQual   pREL_BASE_Name FSLIT("Unit") genUnitTyConKey
genUnitDataConName = wDataQual pREL_BASE_Name FSLIT("Unit") genUnitDataConKey

-- Base strings Strings
unpackCStringName       = varQual pREL_BASE_Name FSLIT("unpackCString#") unpackCStringIdKey
unpackCStringAppendName = varQual pREL_BASE_Name FSLIT("unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual pREL_BASE_Name FSLIT("unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual pREL_BASE_Name FSLIT("unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName	 	= varQual pREL_BASE_Name FSLIT("eqString")  eqStringIdKey

-- Base classes (Eq, Ord, Functor)
eqClassName	  = clsQual pREL_BASE_Name FSLIT("Eq") eqClassKey
functorClassName  = clsQual pREL_BASE_Name FSLIT("Functor") functorClassKey
ordClassName	  = clsQual pREL_BASE_Name FSLIT("Ord") ordClassKey

-- Class Monad
monadClassName	   = clsQual pREL_BASE_Name FSLIT("Monad") monadClassKey
thenMName	   = varQual pREL_BASE_Name FSLIT(">>")  thenMClassOpKey
bindMName	   = varQual pREL_BASE_Name FSLIT(">>=") bindMClassOpKey
returnMName	   = varQual pREL_BASE_Name FSLIT("return") returnMClassOpKey
failMName	   = varQual pREL_BASE_Name FSLIT("fail") failMClassOpKey


-- Random PrelBase functions
otherwiseIdName   = varQual pREL_BASE_Name FSLIT("otherwise") otherwiseIdKey
foldrName	  = varQual pREL_BASE_Name FSLIT("foldr")     foldrIdKey
buildName	  = varQual pREL_BASE_Name FSLIT("build")     buildIdKey
augmentName	  = varQual pREL_BASE_Name FSLIT("augment")   augmentIdKey
appendName	  = varQual pREL_BASE_Name FSLIT("++")        appendIdKey
andName		  = varQual pREL_BASE_Name FSLIT("&&")	      andIdKey
orName		  = varQual pREL_BASE_Name FSLIT("||")	      orIdKey
assertName        = varQual pREL_BASE_Name FSLIT("assert")    assertIdKey
lazyIdName	  = wVarQual pREL_BASE_Name FSLIT("lazy")     lazyIdKey

-- PrelTup
fstName		  = varQual pREL_TUP_Name FSLIT("fst") fstIdKey
sndName		  = varQual pREL_TUP_Name FSLIT("snd") sndIdKey

-- Module PrelNum
numClassName	  = clsQual pREL_NUM_Name FSLIT("Num") numClassKey
fromIntegerName   = varQual pREL_NUM_Name FSLIT("fromInteger") fromIntegerClassOpKey
minusName	  = varQual pREL_NUM_Name FSLIT("-") minusClassOpKey
negateName	  = varQual pREL_NUM_Name FSLIT("negate") negateClassOpKey
plusIntegerName   = varQual pREL_NUM_Name FSLIT("plusInteger") plusIntegerIdKey
timesIntegerName  = varQual pREL_NUM_Name FSLIT("timesInteger") timesIntegerIdKey
integerTyConName  = wTcQual  pREL_NUM_Name FSLIT("Integer") integerTyConKey
smallIntegerDataConName = wDataQual pREL_NUM_Name FSLIT("S#") smallIntegerDataConKey
largeIntegerDataConName = wDataQual pREL_NUM_Name FSLIT("J#") largeIntegerDataConKey

-- PrelReal types and classes
rationalTyConName   = tcQual   pREL_REAL_Name  FSLIT("Rational") rationalTyConKey
ratioTyConName	    = tcQual   pREL_REAL_Name  FSLIT("Ratio") ratioTyConKey
ratioDataConName    = dataQual pREL_REAL_Name  FSLIT(":%") ratioDataConKey
realClassName	    = clsQual  pREL_REAL_Name  FSLIT("Real") realClassKey
integralClassName   = clsQual  pREL_REAL_Name  FSLIT("Integral") integralClassKey
realFracClassName   = clsQual  pREL_REAL_Name  FSLIT("RealFrac") realFracClassKey
fractionalClassName = clsQual  pREL_REAL_Name  FSLIT("Fractional") fractionalClassKey
fromRationalName    = varQual  pREL_REAL_Name  FSLIT("fromRational") fromRationalClassOpKey

-- PrelFloat classes
floatTyConName	   = wTcQual   pREL_FLOAT_Name FSLIT("Float") floatTyConKey
floatDataConName   = wDataQual pREL_FLOAT_Name FSLIT("F#") floatDataConKey
doubleTyConName    = wTcQual   pREL_FLOAT_Name FSLIT("Double") doubleTyConKey
doubleDataConName  = wDataQual pREL_FLOAT_Name FSLIT("D#") doubleDataConKey
floatingClassName  = clsQual  pREL_FLOAT_Name FSLIT("Floating") floatingClassKey
realFloatClassName = clsQual  pREL_FLOAT_Name FSLIT("RealFloat") realFloatClassKey

-- Class Ix
ixClassName	   = clsQual pREL_ARR_Name FSLIT("Ix") ixClassKey

-- Class Typeable and Data
typeableClassName = clsQual tYPEABLE_Name FSLIT("Typeable") typeableClassKey
dataClassName     = clsQual gENERICS_Name FSLIT("Data")     dataClassKey

-- Enum module (Enum, Bounded)
enumClassName 	   = clsQual pREL_ENUM_Name FSLIT("Enum") enumClassKey
enumFromName	   = varQual pREL_ENUM_Name FSLIT("enumFrom") enumFromClassOpKey
enumFromToName	   = varQual pREL_ENUM_Name FSLIT("enumFromTo") enumFromToClassOpKey
enumFromThenName   = varQual pREL_ENUM_Name FSLIT("enumFromThen") enumFromThenClassOpKey
enumFromThenToName = varQual pREL_ENUM_Name FSLIT("enumFromThenTo") enumFromThenToClassOpKey
boundedClassName  = clsQual pREL_ENUM_Name FSLIT("Bounded") boundedClassKey

-- List functions
concatName	  = varQual pREL_LIST_Name FSLIT("concat") concatIdKey
filterName	  = varQual pREL_LIST_Name FSLIT("filter") filterIdKey
zipName	   	  = varQual pREL_LIST_Name FSLIT("zip") zipIdKey

-- Class Show
showClassName	  = clsQual pREL_SHOW_Name FSLIT("Show")       showClassKey

-- Class Read
readClassName	   = clsQual pREL_READ_Name FSLIT("Read") readClassKey

-- parallel array types and functions
enumFromToPName	   = varQual pREL_PARR_Name FSLIT("enumFromToP") enumFromToPIdKey
enumFromThenToPName= varQual pREL_PARR_Name FSLIT("enumFromThenToP") enumFromThenToPIdKey
parrTyConName	  = wTcQual  pREL_PARR_Name FSLIT("[::]")      	 parrTyConKey
parrDataConName   = wDataQual pREL_PARR_Name FSLIT("PArr")     	 parrDataConKey
nullPName	  = varQual pREL_PARR_Name FSLIT("nullP")      	 nullPIdKey
lengthPName	  = varQual pREL_PARR_Name FSLIT("lengthP")    	 lengthPIdKey
replicatePName	  = varQual pREL_PARR_Name FSLIT("replicateP") 	 replicatePIdKey
mapPName	  = varQual pREL_PARR_Name FSLIT("mapP")       	 mapPIdKey
filterPName	  = varQual pREL_PARR_Name FSLIT("filterP")    	 filterPIdKey
zipPName	  = varQual pREL_PARR_Name FSLIT("zipP")       	 zipPIdKey
crossPName	  = varQual pREL_PARR_Name FSLIT("crossP")     	 crossPIdKey
indexPName	  = varQual pREL_PARR_Name FSLIT("!:")	       	 indexPIdKey
toPName	          = varQual pREL_PARR_Name FSLIT("toP")	       	 toPIdKey
bpermutePName     = varQual pREL_PARR_Name FSLIT("bpermuteP")    bpermutePIdKey
bpermuteDftPName  = varQual pREL_PARR_Name FSLIT("bpermuteDftP") bpermuteDftPIdKey
indexOfPName      = varQual pREL_PARR_Name FSLIT("indexOfP")     indexOfPIdKey

-- IOBase things
ioTyConName	  = tcQual   pREL_IO_BASE_Name FSLIT("IO") ioTyConKey
ioDataConName     = dataQual pREL_IO_BASE_Name FSLIT("IO") ioDataConKey
thenIOName	  = varQual  pREL_IO_BASE_Name FSLIT("thenIO") thenIOIdKey
bindIOName	  = varQual  pREL_IO_BASE_Name FSLIT("bindIO") bindIOIdKey
returnIOName	  = varQual  pREL_IO_BASE_Name FSLIT("returnIO") returnIOIdKey
failIOName	  = varQual  pREL_IO_BASE_Name FSLIT("failIO") failIOIdKey

-- IO things
printName	  = varQual sYSTEM_IO_Name FSLIT("print") printIdKey

-- Int, Word, and Addr things
int8TyConName     = tcQual pREL_INT_Name  FSLIT("Int8") int8TyConKey
int16TyConName    = tcQual pREL_INT_Name  FSLIT("Int16") int16TyConKey
int32TyConName    = tcQual pREL_INT_Name  FSLIT("Int32") int32TyConKey
int64TyConName    = tcQual pREL_INT_Name  FSLIT("Int64") int64TyConKey

-- Word module
word8TyConName    = tcQual   pREL_WORD_Name FSLIT("Word8")  word8TyConKey
word16TyConName   = tcQual   pREL_WORD_Name FSLIT("Word16") word16TyConKey
word32TyConName   = tcQual   pREL_WORD_Name FSLIT("Word32") word32TyConKey
word64TyConName   = tcQual   pREL_WORD_Name FSLIT("Word64") word64TyConKey
wordTyConName     = wTcQual   pREL_WORD_Name FSLIT("Word")   wordTyConKey
wordDataConName   = wDataQual pREL_WORD_Name FSLIT("W#")     wordDataConKey

-- Addr module
addrTyConName	  = tcQual   aDDR_Name FSLIT("Addr") addrTyConKey

-- PrelPtr module
ptrTyConName	  = tcQual   pREL_PTR_Name FSLIT("Ptr") ptrTyConKey
funPtrTyConName	  = tcQual   pREL_PTR_Name FSLIT("FunPtr") funPtrTyConKey

-- Byte array types
byteArrayTyConName	  = tcQual pREL_BYTEARR_Name  FSLIT("ByteArray") byteArrayTyConKey
mutableByteArrayTyConName = tcQual pREL_BYTEARR_Name  FSLIT("MutableByteArray") mutableByteArrayTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName    = tcQual   pREL_STABLE_Name FSLIT("StablePtr") stablePtrTyConKey
newStablePtrName      = varQual  pREL_STABLE_Name FSLIT("newStablePtr") newStablePtrIdKey

-- Error module
errorName		 = wVarQual pREL_ERR_Name FSLIT("error")	        errorIdKey
assertErrorName		 = wVarQual pREL_ERR_Name FSLIT("assertError")	        assertErrorIdKey
recSelErrorName		 = wVarQual pREL_ERR_Name FSLIT("recSelError") 	        recSelErrorIdKey
runtimeErrorName	 = wVarQual pREL_ERR_Name FSLIT("runtimeError")         runtimeErrorIdKey
irrefutPatErrorName	 = wVarQual pREL_ERR_Name FSLIT("irrefutPatError")      irrefutPatErrorIdKey
recConErrorName		 = wVarQual pREL_ERR_Name FSLIT("recConError")          recConErrorIdKey
patErrorName 		 = wVarQual pREL_ERR_Name FSLIT("patError") 	        patErrorIdKey
noMethodBindingErrorName = wVarQual pREL_ERR_Name FSLIT("noMethodBindingError") noMethodBindingErrorIdKey
nonExhaustiveGuardsErrorName 
  = wVarQual pREL_ERR_Name FSLIT("nonExhaustiveGuardsError") nonExhaustiveGuardsErrorIdKey

-- PrelST module
runSTRepName	   = varQual pREL_ST_Name  FSLIT("runSTRep") runSTRepIdKey

-- The "split" Id for splittable implicit parameters
splitName          = varQual gLA_EXTS_Name FSLIT("split") splitIdKey

-- Recursive-do notation
mfixName	   = varQual mONAD_FIX_Name FSLIT("mfix") mfixIdKey

-- Arrow notation
arrAName	   = varQual aRROW_Name FSLIT("arr")	arrAIdKey
composeAName	   = varQual aRROW_Name FSLIT(">>>")	composeAIdKey
firstAName	   = varQual aRROW_Name FSLIT("first")	firstAIdKey
appAName	   = varQual aRROW_Name FSLIT("app")	appAIdKey
choiceAName	   = varQual aRROW_Name FSLIT("|||")	choiceAIdKey
loopAName	   = varQual aRROW_Name FSLIT("loop")	loopAIdKey

-- dotnet interop
objectTyConName	    = wTcQual  dOTNET_Name FSLIT("Object") objectTyConKey
unmarshalObjectName = varQual  dOTNET_Name FSLIT("unmarshalObject") unmarshalObjectIdKey
marshalObjectName   = varQual  dOTNET_Name FSLIT("marshalObject") marshalObjectIdKey
marshalStringName   = varQual  dOTNET_Name FSLIT("marshalString") marshalStringIdKey
unmarshalStringName = varQual  dOTNET_Name FSLIT("unmarshalString") unmarshalStringIdKey
checkDotnetResName  = varQual  dOTNET_Name FSLIT("checkResult")     checkDotnetResNameIdKey

\end{code}

%************************************************************************
%*									*
\subsection{Local helpers}
%*									*
%************************************************************************

All these are original names; hence mkOrig

\begin{code}
varQual  = mk_known_key_name varName
dataQual = mk_known_key_name dataName	-- All the constructor names here are for the DataCon
					-- itself, which lives in the VarName name space
tcQual   = mk_known_key_name tcName
clsQual  = mk_known_key_name clsName

wVarQual  = mk_wired_in_name varName	-- The wired-in analogues
wDataQual = mk_wired_in_name dataName		
wTcQual   = mk_wired_in_name tcName

varQual_RDR  mod str = mkOrig mod (mkOccFS varName str)	-- The RDR analogues
dataQual_RDR mod str = mkOrig mod (mkOccFS dataName str)
tcQual_RDR   mod str = mkOrig mod (mkOccFS tcName str)
clsQual_RDR  mod str = mkOrig mod (mkOccFS clsName str)

mk_known_key_name space mod str uniq 
  = mkKnownKeyExternalName (mkBasePkgModule mod) (mkOccFS space str) uniq 
mk_wired_in_name space mod str uniq 
  = mkWiredInName (mkBasePkgModule mod) (mkOccFS space str) uniq

kindQual str uq = mkInternalName uq (mkKindOccFS tcName str) noSrcLoc
	-- Kinds are not z-encoded in interface file, hence mkKindOccFS
	-- And they don't come from any particular module; indeed we always
	-- want to print them unqualified.  Hence the LocalName
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
typeableClassKey	= mkPreludeClassUnique 4
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
ixClassKey		= mkPreludeClassUnique 20
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
%*									*
%************************************************************************

\begin{code}
addrPrimTyConKey			= mkPreludeTyConUnique	1
addrTyConKey				= mkPreludeTyConUnique	2
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
mutableByteArrayTyConKey		= mkPreludeTyConUnique 53
mutVarPrimTyConKey			= mkPreludeTyConUnique 55
ioTyConKey				= mkPreludeTyConUnique 56
byteArrayTyConKey			= mkPreludeTyConUnique 57
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
--	USES TyConUniques 100-119
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
choiceAIdKey	= mkPreludeMiscIdUnique 123 -- |||
loopAIdKey	= mkPreludeMiscIdUnique 124

---------------- Template Haskell -------------------
--	USES IdUniques 200-299
-----------------------------------------------------
\end{code}


%************************************************************************
%*									*
\subsection{Standard groups of types}
%*									*
%************************************************************************

\begin{code}
numericTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, intTyConKey
	, integerTyConKey
	, doubleTyConKey
	, floatTyConKey
	]

	-- Renamer always imports these data decls replete with constructors
	-- so that desugarer can always see their constructors.  Ugh!
cCallishTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, byteArrayTyConKey
	, mutableByteArrayTyConKey
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

standardClassKeys = derivableClassKeys ++ numericClassKeys

noDictClassKeys = [] -- ToDo: remove?
\end{code}

@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).

\begin{code}
derivableClassKeys
  = [ eqClassKey, ordClassKey, enumClassKey, ixClassKey,
      boundedClassKey, showClassKey, readClassKey ]
\end{code}


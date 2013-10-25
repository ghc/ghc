%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelNames]{Definitions of prelude modules and names}


Nota Bene: all Names defined in here should come from the base package

 - ModuleNames for prelude modules,
        e.g.    pREL_BASE_Name :: ModuleName

 - Modules for prelude modules
        e.g.    pREL_Base :: Module

 - Uniques for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    intTyConKey :: Unique
                minusClassOpKey :: Unique

 - Names for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    intTyConName :: Name
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


Note [Known-key names]
~~~~~~~~~~~~~~~~~~~~~~

It is *very* important that the compiler gives wired-in things and things with "known-key" names
the correct Uniques wherever they occur. We have to be careful about this in exactly two places:

  1. When we parse some source code, renaming the AST better yield an AST whose Names have the
     correct uniques

  2. When we read an interface file, the read-in gubbins better have the right uniques

This is accomplished through a combination of mechanisms:

  1. When parsing source code, the RdrName-decorated AST has some RdrNames which are Exact. These are
     wired-in RdrNames where the we could directly tell from the parsed syntax what Name to use. For
     example, when we parse a [] in a type we can just insert an Exact RdrName Name with the listTyConKey.

     Currently, I believe this is just an optimisation: it would be equally valid to just output Orig
     RdrNames that correctly record the module etc we expect the final Name to come from. However,
     were we to eliminate isTupleOcc_maybe it would become essential (see point 3).

  2. The knownKeyNames (which consist of the basicKnownKeyNames from the module, and those names reachable
     via the wired-in stuff from TysWiredIn) are used to initialise the "original name cache" in IfaceEnv.
     This initialization ensures that when the type checker or renamer (both of which use IfaceEnv) look up
     an original name (i.e. a pair of a Module and an OccName) for a known-key name they get the correct Unique.

     This is the most important mechanism for ensuring that known-key stuff gets the right Unique, and is why
     it is so important to place your known-key names in the appropriate lists.

  3. For "infinite families" of known-key names (i.e. tuples, Any tycons and implicit parameter TyCons), we
     have to be extra careful. Because there are an infinite number of these things, we cannot add them to
     the list of known-key names used to initialise the original name cache. Instead, we have to rely on
     never having to look them up in that cache.

     This is accomplished through a variety of mechanisms:

       a) The known infinite families of names are specially serialised by BinIface.putName, with that special treatment
          detected when we read back to ensure that we get back to the correct uniques.

       b) Most of the infinite families cannot occur in source code, so mechanism a) sufficies to ensure that they
          always have the right Unique. In particular, implicit param TyCon names, constraint tuples and Any TyCons
          cannot be mentioned by the user.

       c) Tuple TyCon/DataCon names have a special hack (isTupleOcc_maybe) that is used by the original name cache
          lookup routine to detect tuple names and give them the right Unique. You might think that this is unnecessary
          because tuple TyCon/DataCons are parsed as Exact RdrNames and *don't* appear as original names in interface files
          (because serialization gives them special treatment), so we will never look them up in the original name cache.

          However, there is a subtle reason why this is not the case: if you use setRdrNameSpace on an Exact RdrName
          it may be turned into an Orig RdrName. So if the original name was an Exact tuple Name we might end up with
          an Orig instead, which *will* lead to an original name cache query.
\begin{code}
module PrelNames (
        Unique, Uniquable(..), hasKey,  -- Re-exported for convenience

        -----------------------------------------------------------
        module PrelNames,       -- A huge bunch of (a) Names,  e.g. intTyConName
                                --                 (b) Uniques e.g. intTyConKey
                                --                 (c) Groups of classes and types
                                --                 (d) miscellaneous things
                                -- So many that we export them all
    ) where

#include "HsVersions.h"

import Module
import OccName
import RdrName
import Unique
import BasicTypes
import Name
import SrcLoc
import FastString
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Local Names}
%*                                                                      *
%************************************************************************

This *local* name is used by the interactive stuff

\begin{code}
itName :: Unique -> SrcSpan -> Name
itName uniq loc = mkInternalName uniq (mkOccNameFS varName (fsLit "it")) loc
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
%*                                                                      *
\subsection{Known key Names}
%*                                                                      *
%************************************************************************

This section tells what the compiler knows about the association of
names with uniques.  These ones are the *non* wired-in ones.  The
wired in ones are defined in TysWiredIn etc.

The names for DPH can come from one of multiple backend packages. At the point where
'basicKnownKeyNames' is used, we don't know which backend it will be.  Hence, we list
the names for multiple backends.  That works out fine, although they use the same uniques,
as we are guaranteed to only load one backend; hence, only one of the different names
sharing a unique will be used.

\begin{code}
basicKnownKeyNames :: [Name]
basicKnownKeyNames
 = genericTyConNames
 ++ oldTypeableClassNames
 ++ [   -- Type constructors (synonyms especially)
        ioTyConName, ioDataConName,
        runMainIOName,
        rationalTyConName,
        stringTyConName,
        ratioDataConName,
        ratioTyConName,
        integerTyConName,

        --  Classes.  *Must* include:
        --      classes that are grabbed by key (e.g., eqClassKey)
        --      classes in "Class.standardClassKeys" (quite a few)
        eqClassName,                    -- mentioned, derivable
        ordClassName,                   -- derivable
        boundedClassName,               -- derivable
        numClassName,                   -- mentioned, numeric
        enumClassName,                  -- derivable
        monadClassName,
        functorClassName,
        realClassName,                  -- numeric
        integralClassName,              -- numeric
        fractionalClassName,            -- numeric
        floatingClassName,              -- numeric
        realFracClassName,              -- numeric
        realFloatClassName,             -- numeric
        dataClassName,
        isStringClassName,
        applicativeClassName,
        alternativeClassName,
        foldableClassName,
        traversableClassName,
        typeableClassName,              -- derivable

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

        -- Applicative/Alternative stuff
        pureAName,
        apAName,

        -- Monad stuff
        thenIOName, bindIOName, returnIOName, failIOName,
        failMName, bindMName, thenMName, returnMName,
        fmapName,
        joinMName,

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
        unpackCStringName,
        unpackCStringFoldrName, unpackCStringUtf8Name,

        -- Overloaded lists
        isListClassName,
        fromListName,
        fromListNName,
        toListName,

        -- List operations
        concatName, filterName, mapName,
        zipName, foldrName, buildName, augmentName, appendName,

        dollarName,         -- The ($) apply function

        -- FFI primitive types that are not wired-in.
        stablePtrTyConName, ptrTyConName, funPtrTyConName,
        int8TyConName, int16TyConName, int32TyConName, int64TyConName,
        word8TyConName, word16TyConName, word32TyConName, word64TyConName,

        -- Others
        otherwiseIdName, inlineIdName,
        eqStringName, assertName, breakpointName, breakpointCondName,
        breakpointAutoName,  opaqueTyConName,
        assertErrorName, runSTRepName,
        printName, fstName, sndName,

        -- Integer
        integerTyConName, mkIntegerName,
        integerToWord64Name, integerToInt64Name,
        word64ToIntegerName, int64ToIntegerName,
        plusIntegerName, timesIntegerName, smallIntegerName,
        wordToIntegerName,
        integerToWordName, integerToIntName, minusIntegerName,
        negateIntegerName, eqIntegerPrimName, neqIntegerPrimName,
        absIntegerName, signumIntegerName,
        leIntegerPrimName, gtIntegerPrimName, ltIntegerPrimName, geIntegerPrimName,
        compareIntegerName, quotRemIntegerName, divModIntegerName,
        quotIntegerName, remIntegerName, divIntegerName, modIntegerName,
        floatFromIntegerName, doubleFromIntegerName,
        encodeFloatIntegerName, encodeDoubleIntegerName,
        decodeDoubleIntegerName,
        gcdIntegerName, lcmIntegerName,
        andIntegerName, orIntegerName, xorIntegerName, complementIntegerName,
        shiftLIntegerName, shiftRIntegerName,

        -- Float/Double
        rationalToFloatName,
        rationalToDoubleName,

        -- MonadFix
        monadFixClassName, mfixName,

        -- Other classes
        randomClassName, randomGenClassName, monadPlusClassName,

        -- Type-level naturals
        knownNatClassName, knownSymbolClassName,

        -- Implicit parameters
        ipClassName,

        -- Annotation type checking
        toAnnotationWrapperName

        -- The Ordering type
        , orderingTyConName, ltDataConName, eqDataConName, gtDataConName

        -- The SPEC type for SpecConstr
        , specTyConName

        -- The Either type
        , eitherTyConName, leftDataConName, rightDataConName

        -- Plugins
        , pluginTyConName

        -- dotnet interop
        , objectTyConName, marshalObjectName, unmarshalObjectName
        , marshalStringName, unmarshalStringName, checkDotnetResName

        -- Generics
        , genClassName, gen1ClassName
        , datatypeClassName, constructorClassName, selectorClassName

        -- Monad comprehensions
        , guardMName
        , liftMName
        , mzipName

        -- GHCi Sandbox
        , ghciIoClassName, ghciStepIoMName
    ]

genericTyConNames :: [Name]
genericTyConNames = [
    v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
    k1TyConName, m1TyConName, sumTyConName, prodTyConName,
    compTyConName, rTyConName, pTyConName, dTyConName,
    cTyConName, sTyConName, rec0TyConName, par0TyConName,
    d1TyConName, c1TyConName, s1TyConName, noSelTyConName,
    repTyConName, rep1TyConName
  ]
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Module names}
%*                                                                      *
%************************************************************************


--MetaHaskell Extension Add a new module here
\begin{code}
pRELUDE :: Module
pRELUDE         = mkBaseModule_ pRELUDE_NAME

gHC_PRIM, gHC_TYPES, gHC_GENERICS, gHC_MAGIC, gHC_COERCIBLE,
    gHC_CLASSES, gHC_BASE, gHC_ENUM, gHC_GHCI, gHC_CSTRING,
    gHC_SHOW, gHC_READ, gHC_NUM, gHC_INTEGER_TYPE, gHC_LIST,
    gHC_TUPLE, dATA_TUPLE, dATA_EITHER, dATA_STRING, dATA_FOLDABLE, dATA_TRAVERSABLE, dATA_MONOID,
    gHC_CONC, gHC_IO, gHC_IO_Exception,
    gHC_ST, gHC_ARR, gHC_STABLE, gHC_PTR, gHC_ERR, gHC_REAL,
    gHC_FLOAT, gHC_TOP_HANDLER, sYSTEM_IO, dYNAMIC,
    tYPEABLE, tYPEABLE_INTERNAL, oLDTYPEABLE, oLDTYPEABLE_INTERNAL, gENERICS,
    dOTNET, rEAD_PREC, lEX, gHC_INT, gHC_WORD, mONAD, mONAD_FIX, mONAD_ZIP,
    aRROW, cONTROL_APPLICATIVE, gHC_DESUGAR, rANDOM, gHC_EXTS,
    cONTROL_EXCEPTION_BASE, gHC_TYPELITS, gHC_IP :: Module

gHC_PRIM        = mkPrimModule (fsLit "GHC.Prim")   -- Primitive types and values
gHC_TYPES       = mkPrimModule (fsLit "GHC.Types")
gHC_MAGIC       = mkPrimModule (fsLit "GHC.Magic")
gHC_CSTRING     = mkPrimModule (fsLit "GHC.CString")
gHC_CLASSES     = mkPrimModule (fsLit "GHC.Classes")
gHC_COERCIBLE   = mkPrimModule (fsLit "GHC.Coercible")

gHC_BASE        = mkBaseModule (fsLit "GHC.Base")
gHC_ENUM        = mkBaseModule (fsLit "GHC.Enum")
gHC_GHCI        = mkBaseModule (fsLit "GHC.GHCi")
gHC_SHOW        = mkBaseModule (fsLit "GHC.Show")
gHC_READ        = mkBaseModule (fsLit "GHC.Read")
gHC_NUM         = mkBaseModule (fsLit "GHC.Num")
gHC_INTEGER_TYPE= mkIntegerModule (fsLit "GHC.Integer.Type")
gHC_LIST        = mkBaseModule (fsLit "GHC.List")
gHC_TUPLE       = mkPrimModule (fsLit "GHC.Tuple")
dATA_TUPLE      = mkBaseModule (fsLit "Data.Tuple")
dATA_EITHER     = mkBaseModule (fsLit "Data.Either")
dATA_STRING     = mkBaseModule (fsLit "Data.String")
dATA_FOLDABLE   = mkBaseModule (fsLit "Data.Foldable")
dATA_TRAVERSABLE= mkBaseModule (fsLit "Data.Traversable")
dATA_MONOID     = mkBaseModule (fsLit "Data.Monoid")
gHC_CONC        = mkBaseModule (fsLit "GHC.Conc")
gHC_IO          = mkBaseModule (fsLit "GHC.IO")
gHC_IO_Exception = mkBaseModule (fsLit "GHC.IO.Exception")
gHC_ST          = mkBaseModule (fsLit "GHC.ST")
gHC_ARR         = mkBaseModule (fsLit "GHC.Arr")
gHC_STABLE      = mkBaseModule (fsLit "GHC.Stable")
gHC_PTR         = mkBaseModule (fsLit "GHC.Ptr")
gHC_ERR         = mkBaseModule (fsLit "GHC.Err")
gHC_REAL        = mkBaseModule (fsLit "GHC.Real")
gHC_FLOAT       = mkBaseModule (fsLit "GHC.Float")
gHC_TOP_HANDLER = mkBaseModule (fsLit "GHC.TopHandler")
sYSTEM_IO       = mkBaseModule (fsLit "System.IO")
dYNAMIC         = mkBaseModule (fsLit "Data.Dynamic")
tYPEABLE        = mkBaseModule (fsLit "Data.Typeable")
tYPEABLE_INTERNAL = mkBaseModule (fsLit "Data.Typeable.Internal")
oLDTYPEABLE     = mkBaseModule (fsLit "Data.OldTypeable")
oLDTYPEABLE_INTERNAL = mkBaseModule (fsLit "Data.OldTypeable.Internal")
gENERICS        = mkBaseModule (fsLit "Data.Data")
dOTNET          = mkBaseModule (fsLit "GHC.Dotnet")
rEAD_PREC       = mkBaseModule (fsLit "Text.ParserCombinators.ReadPrec")
lEX             = mkBaseModule (fsLit "Text.Read.Lex")
gHC_INT         = mkBaseModule (fsLit "GHC.Int")
gHC_WORD        = mkBaseModule (fsLit "GHC.Word")
mONAD           = mkBaseModule (fsLit "Control.Monad")
mONAD_FIX       = mkBaseModule (fsLit "Control.Monad.Fix")
mONAD_ZIP       = mkBaseModule (fsLit "Control.Monad.Zip")
aRROW           = mkBaseModule (fsLit "Control.Arrow")
cONTROL_APPLICATIVE = mkBaseModule (fsLit "Control.Applicative")
gHC_DESUGAR = mkBaseModule (fsLit "GHC.Desugar")
rANDOM          = mkBaseModule (fsLit "System.Random")
gHC_EXTS        = mkBaseModule (fsLit "GHC.Exts")
cONTROL_EXCEPTION_BASE = mkBaseModule (fsLit "Control.Exception.Base")
gHC_GENERICS    = mkBaseModule (fsLit "GHC.Generics")
gHC_TYPELITS    = mkBaseModule (fsLit "GHC.TypeLits")
gHC_IP          = mkBaseModule (fsLit "GHC.IP")

gHC_PARR' :: Module
gHC_PARR' = mkBaseModule (fsLit "GHC.PArr")

mAIN, rOOT_MAIN :: Module
mAIN            = mkMainModule_ mAIN_NAME
rOOT_MAIN       = mkMainModule (fsLit ":Main") -- Root module for initialisation

        -- The ':xxx' makes a module name that the user can never
        -- use himself.  The z-encoding for ':' is "ZC", so the z-encoded
        -- module name still starts with a capital letter, which keeps
        -- the z-encoded version consistent.
iNTERACTIVE :: Module
iNTERACTIVE    = mkMainModule (fsLit ":Interactive")

pRELUDE_NAME, mAIN_NAME :: ModuleName
pRELUDE_NAME   = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME      = mkModuleNameFS (fsLit "Main")

dATA_ARRAY_PARALLEL_NAME, dATA_ARRAY_PARALLEL_PRIM_NAME :: ModuleName
dATA_ARRAY_PARALLEL_NAME      = mkModuleNameFS (fsLit "Data.Array.Parallel")
dATA_ARRAY_PARALLEL_PRIM_NAME = mkModuleNameFS (fsLit "Data.Array.Parallel.Prim")

mkPrimModule :: FastString -> Module
mkPrimModule m = mkModule primPackageId (mkModuleNameFS m)

mkIntegerModule :: FastString -> Module
mkIntegerModule m = mkModule integerPackageId (mkModuleNameFS m)

mkBaseModule :: FastString -> Module
mkBaseModule m = mkModule basePackageId (mkModuleNameFS m)

mkBaseModule_ :: ModuleName -> Module
mkBaseModule_ m = mkModule basePackageId m

mkThisGhcModule :: FastString -> Module
mkThisGhcModule m = mkModule thisGhcPackageId (mkModuleNameFS m)

mkThisGhcModule_ :: ModuleName -> Module
mkThisGhcModule_ m = mkModule thisGhcPackageId m

mkMainModule :: FastString -> Module
mkMainModule m = mkModule mainPackageId (mkModuleNameFS m)

mkMainModule_ :: ModuleName -> Module
mkMainModule_ m = mkModule mainPackageId m
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Constructing the names of tuples
%*                                                                      *
%************************************************************************

\begin{code}
mkTupleModule :: TupleSort -> Arity -> Module
mkTupleModule BoxedTuple   _ = gHC_TUPLE
mkTupleModule ConstraintTuple    _ = gHC_TUPLE
mkTupleModule UnboxedTuple _ = gHC_PRIM
\end{code}


%************************************************************************
%*                                                                      *
                        RdrNames
%*                                                                      *
%************************************************************************

\begin{code}
main_RDR_Unqual    :: RdrName
main_RDR_Unqual = mkUnqual varName (fsLit "main")
        -- We definitely don't want an Orig RdrName, because
        -- main might, in principle, be imported into module Main

forall_tv_RDR, dot_tv_RDR :: RdrName
forall_tv_RDR = mkUnqual tvName (fsLit "forall")
dot_tv_RDR    = mkUnqual tvName (fsLit ".")

eq_RDR, ge_RDR, ne_RDR, le_RDR, lt_RDR, gt_RDR, compare_RDR,
    ltTag_RDR, eqTag_RDR, gtTag_RDR :: RdrName
eq_RDR                  = nameRdrName eqName
ge_RDR                  = nameRdrName geName
ne_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "/=")
le_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "<=")
lt_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "<")
gt_RDR                  = varQual_RDR  gHC_CLASSES (fsLit ">")
compare_RDR             = varQual_RDR  gHC_CLASSES (fsLit "compare")
ltTag_RDR               = dataQual_RDR gHC_TYPES (fsLit "LT")
eqTag_RDR               = dataQual_RDR gHC_TYPES (fsLit "EQ")
gtTag_RDR               = dataQual_RDR gHC_TYPES (fsLit "GT")

eqClass_RDR, numClass_RDR, ordClass_RDR, enumClass_RDR, monadClass_RDR
    :: RdrName
eqClass_RDR             = nameRdrName eqClassName
numClass_RDR            = nameRdrName numClassName
ordClass_RDR            = nameRdrName ordClassName
enumClass_RDR           = nameRdrName enumClassName
monadClass_RDR          = nameRdrName monadClassName

map_RDR, append_RDR :: RdrName
map_RDR                 = varQual_RDR gHC_BASE (fsLit "map")
append_RDR              = varQual_RDR gHC_BASE (fsLit "++")

foldr_RDR, build_RDR, returnM_RDR, bindM_RDR, failM_RDR :: RdrName
foldr_RDR               = nameRdrName foldrName
build_RDR               = nameRdrName buildName
returnM_RDR             = nameRdrName returnMName
bindM_RDR               = nameRdrName bindMName
failM_RDR               = nameRdrName failMName

left_RDR, right_RDR :: RdrName
left_RDR                = nameRdrName leftDataConName
right_RDR               = nameRdrName rightDataConName

fromEnum_RDR, toEnum_RDR :: RdrName
fromEnum_RDR            = varQual_RDR gHC_ENUM (fsLit "fromEnum")
toEnum_RDR              = varQual_RDR gHC_ENUM (fsLit "toEnum")

enumFrom_RDR, enumFromTo_RDR, enumFromThen_RDR, enumFromThenTo_RDR :: RdrName
enumFrom_RDR            = nameRdrName enumFromName
enumFromTo_RDR          = nameRdrName enumFromToName
enumFromThen_RDR        = nameRdrName enumFromThenName
enumFromThenTo_RDR      = nameRdrName enumFromThenToName

ratioDataCon_RDR, plusInteger_RDR, timesInteger_RDR :: RdrName
ratioDataCon_RDR        = nameRdrName ratioDataConName
plusInteger_RDR         = nameRdrName plusIntegerName
timesInteger_RDR        = nameRdrName timesIntegerName

ioDataCon_RDR :: RdrName
ioDataCon_RDR           = nameRdrName ioDataConName

eqString_RDR, unpackCString_RDR, unpackCStringFoldr_RDR,
    unpackCStringUtf8_RDR :: RdrName
eqString_RDR            = nameRdrName eqStringName
unpackCString_RDR       = nameRdrName unpackCStringName
unpackCStringFoldr_RDR  = nameRdrName unpackCStringFoldrName
unpackCStringUtf8_RDR   = nameRdrName unpackCStringUtf8Name

newStablePtr_RDR :: RdrName
newStablePtr_RDR        = nameRdrName newStablePtrName

bindIO_RDR, returnIO_RDR :: RdrName
bindIO_RDR              = nameRdrName bindIOName
returnIO_RDR            = nameRdrName returnIOName

fromInteger_RDR, fromRational_RDR, minus_RDR, times_RDR, plus_RDR :: RdrName
fromInteger_RDR         = nameRdrName fromIntegerName
fromRational_RDR        = nameRdrName fromRationalName
minus_RDR               = nameRdrName minusName
times_RDR               = varQual_RDR  gHC_NUM (fsLit "*")
plus_RDR                = varQual_RDR gHC_NUM (fsLit "+")

fromString_RDR :: RdrName
fromString_RDR          = nameRdrName fromStringName

fromList_RDR, fromListN_RDR, toList_RDR :: RdrName
fromList_RDR = nameRdrName fromListName
fromListN_RDR = nameRdrName fromListNName
toList_RDR = nameRdrName toListName

compose_RDR :: RdrName
compose_RDR             = varQual_RDR gHC_BASE (fsLit ".")

not_RDR, getTag_RDR, succ_RDR, pred_RDR, minBound_RDR, maxBound_RDR,
    and_RDR, range_RDR, inRange_RDR, index_RDR,
    unsafeIndex_RDR, unsafeRangeSize_RDR :: RdrName
and_RDR                 = varQual_RDR gHC_CLASSES (fsLit "&&")
not_RDR                 = varQual_RDR gHC_CLASSES (fsLit "not")
getTag_RDR              = varQual_RDR gHC_BASE (fsLit "getTag")
succ_RDR                = varQual_RDR gHC_ENUM (fsLit "succ")
pred_RDR                = varQual_RDR gHC_ENUM (fsLit "pred")
minBound_RDR            = varQual_RDR gHC_ENUM (fsLit "minBound")
maxBound_RDR            = varQual_RDR gHC_ENUM (fsLit "maxBound")
range_RDR               = varQual_RDR gHC_ARR (fsLit "range")
inRange_RDR             = varQual_RDR gHC_ARR (fsLit "inRange")
index_RDR               = varQual_RDR gHC_ARR (fsLit "index")
unsafeIndex_RDR         = varQual_RDR gHC_ARR (fsLit "unsafeIndex")
unsafeRangeSize_RDR     = varQual_RDR gHC_ARR (fsLit "unsafeRangeSize")

readList_RDR, readListDefault_RDR, readListPrec_RDR, readListPrecDefault_RDR,
    readPrec_RDR, parens_RDR, choose_RDR, lexP_RDR, expectP_RDR :: RdrName
readList_RDR            = varQual_RDR gHC_READ (fsLit "readList")
readListDefault_RDR     = varQual_RDR gHC_READ (fsLit "readListDefault")
readListPrec_RDR        = varQual_RDR gHC_READ (fsLit "readListPrec")
readListPrecDefault_RDR = varQual_RDR gHC_READ (fsLit "readListPrecDefault")
readPrec_RDR            = varQual_RDR gHC_READ (fsLit "readPrec")
parens_RDR              = varQual_RDR gHC_READ (fsLit "parens")
choose_RDR              = varQual_RDR gHC_READ (fsLit "choose")
lexP_RDR                = varQual_RDR gHC_READ (fsLit "lexP")
expectP_RDR             = varQual_RDR gHC_READ (fsLit "expectP")

punc_RDR, ident_RDR, symbol_RDR :: RdrName
punc_RDR                = dataQual_RDR lEX (fsLit "Punc")
ident_RDR               = dataQual_RDR lEX (fsLit "Ident")
symbol_RDR              = dataQual_RDR lEX (fsLit "Symbol")

step_RDR, alt_RDR, reset_RDR, prec_RDR, pfail_RDR :: RdrName
step_RDR                = varQual_RDR  rEAD_PREC (fsLit "step")
alt_RDR                 = varQual_RDR  rEAD_PREC (fsLit "+++")
reset_RDR               = varQual_RDR  rEAD_PREC (fsLit "reset")
prec_RDR                = varQual_RDR  rEAD_PREC (fsLit "prec")
pfail_RDR               = varQual_RDR  rEAD_PREC (fsLit "pfail")

showList_RDR, showList___RDR, showsPrec_RDR, showString_RDR,
    showSpace_RDR, showParen_RDR :: RdrName
showList_RDR            = varQual_RDR gHC_SHOW (fsLit "showList")
showList___RDR          = varQual_RDR gHC_SHOW (fsLit "showList__")
showsPrec_RDR           = varQual_RDR gHC_SHOW (fsLit "showsPrec")
showString_RDR          = varQual_RDR gHC_SHOW (fsLit "showString")
showSpace_RDR           = varQual_RDR gHC_SHOW (fsLit "showSpace")
showParen_RDR           = varQual_RDR gHC_SHOW (fsLit "showParen")

typeRep_RDR, mkTyCon_RDR, mkTyConApp_RDR,
    oldTypeOf_RDR, oldMkTyCon_RDR, oldMkTyConApp_RDR :: RdrName
typeRep_RDR       = varQual_RDR tYPEABLE_INTERNAL    (fsLit "typeRep#")
mkTyCon_RDR       = varQual_RDR tYPEABLE_INTERNAL    (fsLit "mkTyCon")
mkTyConApp_RDR    = varQual_RDR tYPEABLE_INTERNAL    (fsLit "mkTyConApp")
oldTypeOf_RDR     = varQual_RDR oLDTYPEABLE_INTERNAL (fsLit "typeOf")
oldMkTyCon_RDR    = varQual_RDR oLDTYPEABLE_INTERNAL (fsLit "mkTyCon")
oldMkTyConApp_RDR = varQual_RDR oLDTYPEABLE_INTERNAL (fsLit "mkTyConApp")

undefined_RDR :: RdrName
undefined_RDR = varQual_RDR gHC_ERR (fsLit "undefined")

error_RDR :: RdrName
error_RDR = varQual_RDR gHC_ERR (fsLit "error")

-- Generics (constructors and functions)
u1DataCon_RDR, par1DataCon_RDR, rec1DataCon_RDR,
  k1DataCon_RDR, m1DataCon_RDR, l1DataCon_RDR, r1DataCon_RDR,
  prodDataCon_RDR, comp1DataCon_RDR,
  unPar1_RDR, unRec1_RDR, unK1_RDR, unComp1_RDR,
  from_RDR, from1_RDR, to_RDR, to1_RDR,
  datatypeName_RDR, moduleName_RDR, isNewtypeName_RDR,
  conName_RDR, conFixity_RDR, conIsRecord_RDR,
  noArityDataCon_RDR, arityDataCon_RDR, selName_RDR,
  prefixDataCon_RDR, infixDataCon_RDR, leftAssocDataCon_RDR,
  rightAssocDataCon_RDR, notAssocDataCon_RDR :: RdrName

u1DataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "U1")
par1DataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "Par1")
rec1DataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "Rec1")
k1DataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "K1")
m1DataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "M1")

l1DataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "L1")
r1DataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "R1")

prodDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit ":*:")
comp1DataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "Comp1")

unPar1_RDR  = varQual_RDR gHC_GENERICS (fsLit "unPar1")
unRec1_RDR  = varQual_RDR gHC_GENERICS (fsLit "unRec1")
unK1_RDR    = varQual_RDR gHC_GENERICS (fsLit "unK1")
unComp1_RDR = varQual_RDR gHC_GENERICS (fsLit "unComp1")

from_RDR  = varQual_RDR gHC_GENERICS (fsLit "from")
from1_RDR = varQual_RDR gHC_GENERICS (fsLit "from1")
to_RDR    = varQual_RDR gHC_GENERICS (fsLit "to")
to1_RDR   = varQual_RDR gHC_GENERICS (fsLit "to1")

datatypeName_RDR  = varQual_RDR gHC_GENERICS (fsLit "datatypeName")
moduleName_RDR    = varQual_RDR gHC_GENERICS (fsLit "moduleName")
isNewtypeName_RDR = varQual_RDR gHC_GENERICS (fsLit "isNewtype")
selName_RDR       = varQual_RDR gHC_GENERICS (fsLit "selName")
conName_RDR       = varQual_RDR gHC_GENERICS (fsLit "conName")
conFixity_RDR     = varQual_RDR gHC_GENERICS (fsLit "conFixity")
conIsRecord_RDR   = varQual_RDR gHC_GENERICS (fsLit "conIsRecord")

noArityDataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "NoArity")
arityDataCon_RDR      = dataQual_RDR gHC_GENERICS (fsLit "Arity")
prefixDataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "Prefix")
infixDataCon_RDR      = dataQual_RDR gHC_GENERICS (fsLit "Infix")
leftAssocDataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "LeftAssociative")
rightAssocDataCon_RDR = dataQual_RDR gHC_GENERICS (fsLit "RightAssociative")
notAssocDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit "NotAssociative")


fmap_RDR, pure_RDR, ap_RDR, foldable_foldr_RDR, foldMap_RDR,
    traverse_RDR, mempty_RDR, mappend_RDR :: RdrName
fmap_RDR                = varQual_RDR gHC_BASE (fsLit "fmap")
pure_RDR                = nameRdrName pureAName
ap_RDR                  = nameRdrName apAName
foldable_foldr_RDR      = varQual_RDR dATA_FOLDABLE       (fsLit "foldr")
foldMap_RDR             = varQual_RDR dATA_FOLDABLE       (fsLit "foldMap")
traverse_RDR            = varQual_RDR dATA_TRAVERSABLE    (fsLit "traverse")
mempty_RDR              = varQual_RDR dATA_MONOID         (fsLit "mempty")
mappend_RDR             = varQual_RDR dATA_MONOID         (fsLit "mappend")

----------------------
varQual_RDR, tcQual_RDR, clsQual_RDR, dataQual_RDR
    :: Module -> FastString -> RdrName
varQual_RDR  mod str = mkOrig mod (mkOccNameFS varName str)
tcQual_RDR   mod str = mkOrig mod (mkOccNameFS tcName str)
clsQual_RDR  mod str = mkOrig mod (mkOccNameFS clsName str)
dataQual_RDR mod str = mkOrig mod (mkOccNameFS dataName str)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Known-key names}
%*                                                                      *
%************************************************************************

Many of these Names are not really "built in", but some parts of the
compiler (notably the deriving mechanism) need to mention their names,
and it's convenient to write them all down in one place.

--MetaHaskell Extension  add the constrs and the lower case case
-- guys as well (perhaps) e.g. see  trueDataConName     below


\begin{code}
wildCardName :: Name
wildCardName = mkSystemVarName wildCardKey (fsLit "wild")

runMainIOName :: Name
runMainIOName = varQual gHC_TOP_HANDLER (fsLit "runMainIO") runMainKey

orderingTyConName, ltDataConName, eqDataConName, gtDataConName :: Name
orderingTyConName = tcQual   gHC_TYPES (fsLit "Ordering") orderingTyConKey
ltDataConName     = conName gHC_TYPES (fsLit "LT") ltDataConKey
eqDataConName     = conName gHC_TYPES (fsLit "EQ") eqDataConKey
gtDataConName     = conName gHC_TYPES (fsLit "GT") gtDataConKey

specTyConName :: Name
specTyConName     = tcQual gHC_TYPES (fsLit "SPEC") specTyConKey

eitherTyConName, leftDataConName, rightDataConName :: Name
eitherTyConName   = tcQual  dATA_EITHER (fsLit "Either") eitherTyConKey
leftDataConName   = conName dATA_EITHER (fsLit "Left")   leftDataConKey
rightDataConName  = conName dATA_EITHER (fsLit "Right")  rightDataConKey

-- Generics (types)
v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
  k1TyConName, m1TyConName, sumTyConName, prodTyConName,
  compTyConName, rTyConName, pTyConName, dTyConName,
  cTyConName, sTyConName, rec0TyConName, par0TyConName,
  d1TyConName, c1TyConName, s1TyConName, noSelTyConName,
  repTyConName, rep1TyConName :: Name

v1TyConName  = tcQual gHC_GENERICS (fsLit "V1") v1TyConKey
u1TyConName  = tcQual gHC_GENERICS (fsLit "U1") u1TyConKey
par1TyConName  = tcQual gHC_GENERICS (fsLit "Par1") par1TyConKey
rec1TyConName  = tcQual gHC_GENERICS (fsLit "Rec1") rec1TyConKey
k1TyConName  = tcQual gHC_GENERICS (fsLit "K1") k1TyConKey
m1TyConName  = tcQual gHC_GENERICS (fsLit "M1") m1TyConKey

sumTyConName    = tcQual gHC_GENERICS (fsLit ":+:") sumTyConKey
prodTyConName   = tcQual gHC_GENERICS (fsLit ":*:") prodTyConKey
compTyConName   = tcQual gHC_GENERICS (fsLit ":.:") compTyConKey

rTyConName  = tcQual gHC_GENERICS (fsLit "R") rTyConKey
pTyConName  = tcQual gHC_GENERICS (fsLit "P") pTyConKey
dTyConName  = tcQual gHC_GENERICS (fsLit "D") dTyConKey
cTyConName  = tcQual gHC_GENERICS (fsLit "C") cTyConKey
sTyConName  = tcQual gHC_GENERICS (fsLit "S") sTyConKey

rec0TyConName  = tcQual gHC_GENERICS (fsLit "Rec0") rec0TyConKey
par0TyConName  = tcQual gHC_GENERICS (fsLit "Par0") par0TyConKey
d1TyConName  = tcQual gHC_GENERICS (fsLit "D1") d1TyConKey
c1TyConName  = tcQual gHC_GENERICS (fsLit "C1") c1TyConKey
s1TyConName  = tcQual gHC_GENERICS (fsLit "S1") s1TyConKey
noSelTyConName = tcQual gHC_GENERICS (fsLit "NoSelector") noSelTyConKey

repTyConName  = tcQual gHC_GENERICS (fsLit "Rep")  repTyConKey
rep1TyConName = tcQual gHC_GENERICS (fsLit "Rep1") rep1TyConKey

-- Base strings Strings
unpackCStringName, unpackCStringFoldrName,
    unpackCStringUtf8Name, eqStringName, stringTyConName :: Name
unpackCStringName       = varQual gHC_CSTRING (fsLit "unpackCString#") unpackCStringIdKey
unpackCStringFoldrName  = varQual gHC_CSTRING (fsLit "unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual gHC_CSTRING (fsLit "unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName            = varQual gHC_BASE (fsLit "eqString")  eqStringIdKey
stringTyConName         = tcQual  gHC_BASE (fsLit "String") stringTyConKey

-- The 'inline' function
inlineIdName :: Name
inlineIdName            = varQual gHC_MAGIC (fsLit "inline") inlineIdKey

-- Base classes (Eq, Ord, Functor)
fmapName, eqClassName, eqName, ordClassName, geName, functorClassName :: Name
eqClassName       = clsQual  gHC_CLASSES (fsLit "Eq")      eqClassKey
eqName            = methName gHC_CLASSES (fsLit "==")      eqClassOpKey
ordClassName      = clsQual  gHC_CLASSES (fsLit "Ord")     ordClassKey
geName            = methName gHC_CLASSES (fsLit ">=")      geClassOpKey
functorClassName  = clsQual  gHC_BASE (fsLit "Functor") functorClassKey
fmapName          = methName gHC_BASE (fsLit "fmap")    fmapClassOpKey

-- Class Monad
monadClassName, thenMName, bindMName, returnMName, failMName :: Name
monadClassName     = clsQual  gHC_BASE (fsLit "Monad")  monadClassKey
thenMName          = methName gHC_BASE (fsLit ">>")     thenMClassOpKey
bindMName          = methName gHC_BASE (fsLit ">>=")    bindMClassOpKey
returnMName        = methName gHC_BASE (fsLit "return") returnMClassOpKey
failMName          = methName gHC_BASE (fsLit "fail")   failMClassOpKey

-- Classes (Applicative, Foldable, Traversable)
applicativeClassName, foldableClassName, traversableClassName :: Name
applicativeClassName  = clsQual  cONTROL_APPLICATIVE (fsLit "Applicative") applicativeClassKey
foldableClassName     = clsQual  dATA_FOLDABLE       (fsLit "Foldable")    foldableClassKey
traversableClassName  = clsQual  dATA_TRAVERSABLE    (fsLit "Traversable") traversableClassKey



-- AMP additions

joinMName,  apAName, pureAName, alternativeClassName :: Name
joinMName            = methName mONAD               (fsLit "join")        joinMIdKey
apAName              = methName cONTROL_APPLICATIVE (fsLit "<*>")         apAClassOpKey
pureAName            = methName cONTROL_APPLICATIVE (fsLit "pure")        pureAClassOpKey
alternativeClassName = clsQual  cONTROL_APPLICATIVE (fsLit "Alternative") alternativeClassKey

joinMIdKey, apAClassOpKey, pureAClassOpKey, alternativeClassKey :: Unique
joinMIdKey          = mkPreludeMiscIdUnique 750
apAClassOpKey       = mkPreludeMiscIdUnique 751 -- <*>
pureAClassOpKey     = mkPreludeMiscIdUnique 752
alternativeClassKey = mkPreludeMiscIdUnique 753


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
foldrName         = varQual gHC_BASE (fsLit "foldr")      foldrIdKey
buildName         = varQual gHC_BASE (fsLit "build")      buildIdKey
augmentName       = varQual gHC_BASE (fsLit "augment")    augmentIdKey
mapName           = varQual gHC_BASE (fsLit "map")        mapIdKey
appendName        = varQual gHC_BASE (fsLit "++")         appendIdKey
dollarName        = varQual gHC_BASE (fsLit "$")          dollarIdKey
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
fstName           = varQual dATA_TUPLE (fsLit "fst") fstIdKey
sndName           = varQual dATA_TUPLE (fsLit "snd") sndIdKey

-- Module GHC.Num
numClassName, fromIntegerName, minusName, negateName :: Name
numClassName      = clsQual  gHC_NUM (fsLit "Num") numClassKey
fromIntegerName   = methName gHC_NUM (fsLit "fromInteger") fromIntegerClassOpKey
minusName         = methName gHC_NUM (fsLit "-") minusClassOpKey
negateName        = methName gHC_NUM (fsLit "negate") negateClassOpKey

integerTyConName, mkIntegerName,
    integerToWord64Name, integerToInt64Name,
    word64ToIntegerName, int64ToIntegerName,
    plusIntegerName, timesIntegerName, smallIntegerName,
    wordToIntegerName,
    integerToWordName, integerToIntName, minusIntegerName,
    negateIntegerName, eqIntegerPrimName, neqIntegerPrimName,
    absIntegerName, signumIntegerName,
    leIntegerPrimName, gtIntegerPrimName, ltIntegerPrimName, geIntegerPrimName,
    compareIntegerName, quotRemIntegerName, divModIntegerName,
    quotIntegerName, remIntegerName, divIntegerName, modIntegerName,
    floatFromIntegerName, doubleFromIntegerName,
    encodeFloatIntegerName, encodeDoubleIntegerName,
    decodeDoubleIntegerName,
    gcdIntegerName, lcmIntegerName,
    andIntegerName, orIntegerName, xorIntegerName, complementIntegerName,
    shiftLIntegerName, shiftRIntegerName :: Name
integerTyConName      = tcQual  gHC_INTEGER_TYPE (fsLit "Integer")           integerTyConKey
mkIntegerName         = varQual gHC_INTEGER_TYPE (fsLit "mkInteger")         mkIntegerIdKey
integerToWord64Name   = varQual gHC_INTEGER_TYPE (fsLit "integerToWord64")   integerToWord64IdKey
integerToInt64Name    = varQual gHC_INTEGER_TYPE (fsLit "integerToInt64")    integerToInt64IdKey
word64ToIntegerName   = varQual gHC_INTEGER_TYPE (fsLit "word64ToInteger")   word64ToIntegerIdKey
int64ToIntegerName    = varQual gHC_INTEGER_TYPE (fsLit "int64ToInteger")    int64ToIntegerIdKey
plusIntegerName       = varQual gHC_INTEGER_TYPE (fsLit "plusInteger")       plusIntegerIdKey
timesIntegerName      = varQual gHC_INTEGER_TYPE (fsLit "timesInteger")      timesIntegerIdKey
smallIntegerName      = varQual gHC_INTEGER_TYPE (fsLit "smallInteger")      smallIntegerIdKey
wordToIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "wordToInteger")     wordToIntegerIdKey
integerToWordName     = varQual gHC_INTEGER_TYPE (fsLit "integerToWord")     integerToWordIdKey
integerToIntName      = varQual gHC_INTEGER_TYPE (fsLit "integerToInt")      integerToIntIdKey
minusIntegerName      = varQual gHC_INTEGER_TYPE (fsLit "minusInteger")      minusIntegerIdKey
negateIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "negateInteger")     negateIntegerIdKey
eqIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "eqInteger#")        eqIntegerPrimIdKey
neqIntegerPrimName    = varQual gHC_INTEGER_TYPE (fsLit "neqInteger#")       neqIntegerPrimIdKey
absIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "absInteger")        absIntegerIdKey
signumIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "signumInteger")     signumIntegerIdKey
leIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "leInteger#")        leIntegerPrimIdKey
gtIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "gtInteger#")        gtIntegerPrimIdKey
ltIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "ltInteger#")        ltIntegerPrimIdKey
geIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "geInteger#")        geIntegerPrimIdKey
compareIntegerName    = varQual gHC_INTEGER_TYPE (fsLit "compareInteger")    compareIntegerIdKey
quotRemIntegerName    = varQual gHC_INTEGER_TYPE (fsLit "quotRemInteger")    quotRemIntegerIdKey
divModIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "divModInteger")     divModIntegerIdKey
quotIntegerName       = varQual gHC_INTEGER_TYPE (fsLit "quotInteger")       quotIntegerIdKey
remIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "remInteger")        remIntegerIdKey
divIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "divInteger")        divIntegerIdKey
modIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "modInteger")        modIntegerIdKey
floatFromIntegerName  = varQual gHC_INTEGER_TYPE (fsLit "floatFromInteger")      floatFromIntegerIdKey
doubleFromIntegerName = varQual gHC_INTEGER_TYPE (fsLit "doubleFromInteger")     doubleFromIntegerIdKey
encodeFloatIntegerName  = varQual gHC_INTEGER_TYPE (fsLit "encodeFloatInteger")  encodeFloatIntegerIdKey
encodeDoubleIntegerName = varQual gHC_INTEGER_TYPE (fsLit "encodeDoubleInteger") encodeDoubleIntegerIdKey
decodeDoubleIntegerName = varQual gHC_INTEGER_TYPE (fsLit "decodeDoubleInteger") decodeDoubleIntegerIdKey
gcdIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "gcdInteger")        gcdIntegerIdKey
lcmIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "lcmInteger")        lcmIntegerIdKey
andIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "andInteger")        andIntegerIdKey
orIntegerName         = varQual gHC_INTEGER_TYPE (fsLit "orInteger")         orIntegerIdKey
xorIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "xorInteger")        xorIntegerIdKey
complementIntegerName = varQual gHC_INTEGER_TYPE (fsLit "complementInteger") complementIntegerIdKey
shiftLIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "shiftLInteger")     shiftLIntegerIdKey
shiftRIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "shiftRInteger")     shiftRIntegerIdKey

-- GHC.Real types and classes
rationalTyConName, ratioTyConName, ratioDataConName, realClassName,
    integralClassName, realFracClassName, fractionalClassName,
    fromRationalName, toIntegerName, toRationalName, fromIntegralName,
    realToFracName :: Name
rationalTyConName   = tcQual  gHC_REAL (fsLit "Rational") rationalTyConKey
ratioTyConName      = tcQual  gHC_REAL (fsLit "Ratio") ratioTyConKey
ratioDataConName    = conName gHC_REAL (fsLit ":%") ratioDataConKey
realClassName       = clsQual gHC_REAL (fsLit "Real") realClassKey
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

-- other GHC.Float functions
rationalToFloatName, rationalToDoubleName :: Name
rationalToFloatName  = varQual gHC_FLOAT (fsLit "rationalToFloat") rationalToFloatIdKey
rationalToDoubleName = varQual gHC_FLOAT (fsLit "rationalToDouble") rationalToDoubleIdKey

-- Class Ix
ixClassName :: Name
ixClassName = clsQual gHC_ARR (fsLit "Ix") ixClassKey

-- Class Typeable
typeableClassName,
    oldTypeableClassName, oldTypeable1ClassName, oldTypeable2ClassName,
    oldTypeable3ClassName, oldTypeable4ClassName, oldTypeable5ClassName,
    oldTypeable6ClassName, oldTypeable7ClassName :: Name
typeableClassName  = clsQual tYPEABLE_INTERNAL (fsLit "Typeable")  typeableClassKey
oldTypeableClassName  = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable")  oldTypeableClassKey
oldTypeable1ClassName = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable1") oldTypeable1ClassKey
oldTypeable2ClassName = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable2") oldTypeable2ClassKey
oldTypeable3ClassName = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable3") oldTypeable3ClassKey
oldTypeable4ClassName = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable4") oldTypeable4ClassKey
oldTypeable5ClassName = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable5") oldTypeable5ClassKey
oldTypeable6ClassName = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable6") oldTypeable6ClassKey
oldTypeable7ClassName = clsQual oLDTYPEABLE_INTERNAL (fsLit "Typeable7") oldTypeable7ClassKey

oldTypeableClassNames :: [Name]
oldTypeableClassNames = [ oldTypeableClassName, oldTypeable1ClassName, oldTypeable2ClassName
                        , oldTypeable3ClassName, oldTypeable4ClassName, oldTypeable5ClassName
                        , oldTypeable6ClassName, oldTypeable7ClassName ]

-- Class Data
dataClassName :: Name
dataClassName = clsQual gENERICS (fsLit "Data") dataClassKey

-- Error module
assertErrorName    :: Name
assertErrorName   = varQual gHC_IO_Exception (fsLit "assertError") assertErrorIdKey

-- Enum module (Enum, Bounded)
enumClassName, enumFromName, enumFromToName, enumFromThenName,
    enumFromThenToName, boundedClassName :: Name
enumClassName      = clsQual gHC_ENUM (fsLit "Enum") enumClassKey
enumFromName       = methName gHC_ENUM (fsLit "enumFrom") enumFromClassOpKey
enumFromToName     = methName gHC_ENUM (fsLit "enumFromTo") enumFromToClassOpKey
enumFromThenName   = methName gHC_ENUM (fsLit "enumFromThen") enumFromThenClassOpKey
enumFromThenToName = methName gHC_ENUM (fsLit "enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual gHC_ENUM (fsLit "Bounded") boundedClassKey

-- List functions
concatName, filterName, zipName :: Name
concatName        = varQual gHC_LIST (fsLit "concat") concatIdKey
filterName        = varQual gHC_LIST (fsLit "filter") filterIdKey
zipName           = varQual gHC_LIST (fsLit "zip") zipIdKey

-- Overloaded lists
isListClassName, fromListName, fromListNName, toListName :: Name
isListClassName = clsQual gHC_EXTS (fsLit "IsList") isListClassKey
fromListName = methName gHC_EXTS (fsLit "fromList") fromListClassOpKey
fromListNName = methName gHC_EXTS (fsLit "fromListN") fromListNClassOpKey
toListName = methName gHC_EXTS (fsLit "toList") toListClassOpKey

-- Class Show
showClassName :: Name
showClassName     = clsQual gHC_SHOW (fsLit "Show")       showClassKey

-- Class Read
readClassName :: Name
readClassName      = clsQual gHC_READ (fsLit "Read") readClassKey

-- Classes Generic and Generic1, Datatype, Constructor and Selector
genClassName, gen1ClassName, datatypeClassName, constructorClassName,
  selectorClassName :: Name
genClassName  = clsQual gHC_GENERICS (fsLit "Generic")  genClassKey
gen1ClassName = clsQual gHC_GENERICS (fsLit "Generic1") gen1ClassKey

datatypeClassName = clsQual gHC_GENERICS (fsLit "Datatype") datatypeClassKey
constructorClassName = clsQual gHC_GENERICS (fsLit "Constructor") constructorClassKey
selectorClassName = clsQual gHC_GENERICS (fsLit "Selector") selectorClassKey

-- GHCi things
ghciIoClassName, ghciStepIoMName :: Name
ghciIoClassName = clsQual gHC_GHCI (fsLit "GHCiSandboxIO") ghciIoClassKey
ghciStepIoMName = methName gHC_GHCI (fsLit "ghciStepIO") ghciStepIoMClassOpKey

-- IO things
ioTyConName, ioDataConName, thenIOName, bindIOName, returnIOName,
    failIOName :: Name
ioTyConName       = tcQual  gHC_TYPES (fsLit "IO") ioTyConKey
ioDataConName     = conName gHC_TYPES (fsLit "IO") ioDataConKey
thenIOName        = varQual gHC_BASE (fsLit "thenIO") thenIOIdKey
bindIOName        = varQual gHC_BASE (fsLit "bindIO") bindIOIdKey
returnIOName      = varQual gHC_BASE (fsLit "returnIO") returnIOIdKey
failIOName        = varQual gHC_IO (fsLit "failIO") failIOIdKey

-- IO things
printName :: Name
printName         = varQual sYSTEM_IO (fsLit "print") printIdKey

-- Int, Word, and Addr things
int8TyConName, int16TyConName, int32TyConName, int64TyConName :: Name
int8TyConName     = tcQual gHC_INT  (fsLit "Int8") int8TyConKey
int16TyConName    = tcQual gHC_INT  (fsLit "Int16") int16TyConKey
int32TyConName    = tcQual gHC_INT  (fsLit "Int32") int32TyConKey
int64TyConName    = tcQual gHC_INT  (fsLit "Int64") int64TyConKey

-- Word module
word8TyConName, word16TyConName, word32TyConName, word64TyConName :: Name
word8TyConName    = tcQual  gHC_WORD (fsLit "Word8")  word8TyConKey
word16TyConName   = tcQual  gHC_WORD (fsLit "Word16") word16TyConKey
word32TyConName   = tcQual  gHC_WORD (fsLit "Word32") word32TyConKey
word64TyConName   = tcQual  gHC_WORD (fsLit "Word64") word64TyConKey

-- PrelPtr module
ptrTyConName, funPtrTyConName :: Name
ptrTyConName      = tcQual   gHC_PTR (fsLit "Ptr") ptrTyConKey
funPtrTyConName   = tcQual   gHC_PTR (fsLit "FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName, newStablePtrName :: Name
stablePtrTyConName    = tcQual   gHC_STABLE (fsLit "StablePtr") stablePtrTyConKey
newStablePtrName      = varQual  gHC_STABLE (fsLit "newStablePtr") newStablePtrIdKey

-- PrelST module
runSTRepName :: Name
runSTRepName       = varQual gHC_ST  (fsLit "runSTRep") runSTRepIdKey

-- Recursive-do notation
monadFixClassName, mfixName :: Name
monadFixClassName  = clsQual mONAD_FIX (fsLit "MonadFix") monadFixClassKey
mfixName           = methName mONAD_FIX (fsLit "mfix") mfixIdKey

-- Arrow notation
arrAName, composeAName, firstAName, appAName, choiceAName, loopAName :: Name
arrAName           = varQual aRROW (fsLit "arr")          arrAIdKey
composeAName       = varQual gHC_DESUGAR (fsLit ">>>") composeAIdKey
firstAName         = varQual aRROW (fsLit "first") firstAIdKey
appAName           = varQual aRROW (fsLit "app")          appAIdKey
choiceAName        = varQual aRROW (fsLit "|||")          choiceAIdKey
loopAName          = varQual aRROW (fsLit "loop")  loopAIdKey

-- Monad comprehensions
guardMName, liftMName, mzipName :: Name
guardMName         = varQual mONAD (fsLit "guard") guardMIdKey
liftMName          = varQual mONAD (fsLit "liftM") liftMIdKey
mzipName           = varQual mONAD_ZIP (fsLit "mzip") mzipIdKey


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

-- Type-level naturals
knownNatClassName :: Name
knownNatClassName     = clsQual gHC_TYPELITS (fsLit "KnownNat") knownNatClassNameKey
knownSymbolClassName :: Name
knownSymbolClassName  = clsQual gHC_TYPELITS (fsLit "KnownSymbol") knownSymbolClassNameKey

-- Implicit parameters
ipClassName :: Name
ipClassName         = clsQual gHC_IP (fsLit "IP")      ipClassNameKey



-- dotnet interop
objectTyConName :: Name
objectTyConName     = tcQual   dOTNET (fsLit "Object") objectTyConKey
        -- objectTyConName was "wTcQual", but that's gone now, and
        -- I can't see why it was wired in anyway...
unmarshalObjectName, marshalObjectName, marshalStringName,
    unmarshalStringName, checkDotnetResName :: Name
unmarshalObjectName = varQual  dOTNET (fsLit "unmarshalObject") unmarshalObjectIdKey
marshalObjectName   = varQual  dOTNET (fsLit "marshalObject") marshalObjectIdKey
marshalStringName   = varQual  dOTNET (fsLit "marshalString") marshalStringIdKey
unmarshalStringName = varQual  dOTNET (fsLit "unmarshalString") unmarshalStringIdKey
checkDotnetResName  = varQual  dOTNET (fsLit "checkResult")     checkDotnetResNameIdKey

-- plugins
cORE_MONAD :: Module
cORE_MONAD = mkThisGhcModule (fsLit "CoreMonad")
pluginTyConName :: Name
pluginTyConName = tcQual cORE_MONAD (fsLit "Plugin") pluginTyConKey
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Local helpers}
%*                                                                      *
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
%*                                                                      *
\subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}
%*                                                                      *
%************************************************************************
--MetaHaskell extension hand allocate keys here

\begin{code}
boundedClassKey, enumClassKey, eqClassKey, floatingClassKey,
    fractionalClassKey, integralClassKey, monadClassKey, dataClassKey,
    functorClassKey, numClassKey, ordClassKey, readClassKey, realClassKey,
    realFloatClassKey, realFracClassKey, showClassKey, ixClassKey :: Unique
boundedClassKey         = mkPreludeClassUnique 1
enumClassKey            = mkPreludeClassUnique 2
eqClassKey              = mkPreludeClassUnique 3
floatingClassKey        = mkPreludeClassUnique 5
fractionalClassKey      = mkPreludeClassUnique 6
integralClassKey        = mkPreludeClassUnique 7
monadClassKey           = mkPreludeClassUnique 8
dataClassKey            = mkPreludeClassUnique 9
functorClassKey         = mkPreludeClassUnique 10
numClassKey             = mkPreludeClassUnique 11
ordClassKey             = mkPreludeClassUnique 12
readClassKey            = mkPreludeClassUnique 13
realClassKey            = mkPreludeClassUnique 14
realFloatClassKey       = mkPreludeClassUnique 15
realFracClassKey        = mkPreludeClassUnique 16
showClassKey            = mkPreludeClassUnique 17
ixClassKey              = mkPreludeClassUnique 18

typeableClassKey, typeable1ClassKey, typeable2ClassKey, typeable3ClassKey,
    typeable4ClassKey, typeable5ClassKey, typeable6ClassKey, typeable7ClassKey
    :: Unique
typeableClassKey        = mkPreludeClassUnique 20
typeable1ClassKey       = mkPreludeClassUnique 21
typeable2ClassKey       = mkPreludeClassUnique 22
typeable3ClassKey       = mkPreludeClassUnique 23
typeable4ClassKey       = mkPreludeClassUnique 24
typeable5ClassKey       = mkPreludeClassUnique 25
typeable6ClassKey       = mkPreludeClassUnique 26
typeable7ClassKey       = mkPreludeClassUnique 27

monadFixClassKey :: Unique
monadFixClassKey        = mkPreludeClassUnique 28

monadPlusClassKey, randomClassKey, randomGenClassKey :: Unique
monadPlusClassKey       = mkPreludeClassUnique 30
randomClassKey          = mkPreludeClassUnique 31
randomGenClassKey       = mkPreludeClassUnique 32

isStringClassKey :: Unique
isStringClassKey        = mkPreludeClassUnique 33

applicativeClassKey, foldableClassKey, traversableClassKey :: Unique
applicativeClassKey     = mkPreludeClassUnique 34
foldableClassKey        = mkPreludeClassUnique 35
traversableClassKey     = mkPreludeClassUnique 36

genClassKey, gen1ClassKey, datatypeClassKey, constructorClassKey,
  selectorClassKey :: Unique
genClassKey   = mkPreludeClassUnique 37
gen1ClassKey  = mkPreludeClassUnique 38

datatypeClassKey    = mkPreludeClassUnique 39
constructorClassKey = mkPreludeClassUnique 40
selectorClassKey    = mkPreludeClassUnique 41

-- KnownNat: see Note [KnowNat & KnownSymbol and EvLit] in TcEvidence
knownNatClassNameKey :: Unique
knownNatClassNameKey = mkPreludeClassUnique 42

-- KnownSymbol: see Note [KnownNat & KnownSymbol and EvLit] in TcEvidence
knownSymbolClassNameKey :: Unique
knownSymbolClassNameKey = mkPreludeClassUnique 43

ghciIoClassKey :: Unique
ghciIoClassKey = mkPreludeClassUnique 44

ipClassNameKey :: Unique
ipClassNameKey = mkPreludeClassUnique 45

oldTypeableClassKey, oldTypeable1ClassKey, oldTypeable2ClassKey,
    oldTypeable3ClassKey, oldTypeable4ClassKey, oldTypeable5ClassKey,
    oldTypeable6ClassKey, oldTypeable7ClassKey :: Unique
oldTypeableClassKey        = mkPreludeClassUnique 46
oldTypeable1ClassKey       = mkPreludeClassUnique 47
oldTypeable2ClassKey       = mkPreludeClassUnique 48
oldTypeable3ClassKey       = mkPreludeClassUnique 49
oldTypeable4ClassKey       = mkPreludeClassUnique 50
oldTypeable5ClassKey       = mkPreludeClassUnique 51
oldTypeable6ClassKey       = mkPreludeClassUnique 52
oldTypeable7ClassKey       = mkPreludeClassUnique 53
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
%*                                                                      *
%************************************************************************

\begin{code}
addrPrimTyConKey, arrayPrimTyConKey, arrayArrayPrimTyConKey, boolTyConKey, byteArrayPrimTyConKey,
    charPrimTyConKey, charTyConKey, doublePrimTyConKey, doubleTyConKey,
    floatPrimTyConKey, floatTyConKey, funTyConKey, intPrimTyConKey,
    intTyConKey, int8TyConKey, int16TyConKey, int32PrimTyConKey,
    int32TyConKey, int64PrimTyConKey, int64TyConKey,
    integerTyConKey, digitsTyConKey,
    listTyConKey, foreignObjPrimTyConKey, weakPrimTyConKey,
    mutableArrayPrimTyConKey, mutableArrayArrayPrimTyConKey, mutableByteArrayPrimTyConKey,
    orderingTyConKey, mVarPrimTyConKey, ratioTyConKey, rationalTyConKey,
    realWorldTyConKey, stablePtrPrimTyConKey, stablePtrTyConKey,
    anyTyConKey, eqTyConKey :: Unique
addrPrimTyConKey                        = mkPreludeTyConUnique  1
arrayPrimTyConKey                       = mkPreludeTyConUnique  3
boolTyConKey                            = mkPreludeTyConUnique  4
byteArrayPrimTyConKey                   = mkPreludeTyConUnique  5
charPrimTyConKey                        = mkPreludeTyConUnique  7
charTyConKey                            = mkPreludeTyConUnique  8
doublePrimTyConKey                      = mkPreludeTyConUnique  9
doubleTyConKey                          = mkPreludeTyConUnique 10
floatPrimTyConKey                       = mkPreludeTyConUnique 11
floatTyConKey                           = mkPreludeTyConUnique 12
funTyConKey                             = mkPreludeTyConUnique 13
intPrimTyConKey                         = mkPreludeTyConUnique 14
intTyConKey                             = mkPreludeTyConUnique 15
int8TyConKey                            = mkPreludeTyConUnique 16
int16TyConKey                           = mkPreludeTyConUnique 17
int32PrimTyConKey                       = mkPreludeTyConUnique 18
int32TyConKey                           = mkPreludeTyConUnique 19
int64PrimTyConKey                       = mkPreludeTyConUnique 20
int64TyConKey                           = mkPreludeTyConUnique 21
integerTyConKey                         = mkPreludeTyConUnique 22
digitsTyConKey                          = mkPreludeTyConUnique 23
listTyConKey                            = mkPreludeTyConUnique 24
foreignObjPrimTyConKey                  = mkPreludeTyConUnique 25
weakPrimTyConKey                        = mkPreludeTyConUnique 27
mutableArrayPrimTyConKey                = mkPreludeTyConUnique 28
mutableByteArrayPrimTyConKey            = mkPreludeTyConUnique 29
orderingTyConKey                        = mkPreludeTyConUnique 30
mVarPrimTyConKey                        = mkPreludeTyConUnique 31
ratioTyConKey                           = mkPreludeTyConUnique 32
rationalTyConKey                        = mkPreludeTyConUnique 33
realWorldTyConKey                       = mkPreludeTyConUnique 34
stablePtrPrimTyConKey                   = mkPreludeTyConUnique 35
stablePtrTyConKey                       = mkPreludeTyConUnique 36
anyTyConKey                             = mkPreludeTyConUnique 37
eqTyConKey                              = mkPreludeTyConUnique 38
arrayArrayPrimTyConKey                  = mkPreludeTyConUnique 39
mutableArrayArrayPrimTyConKey           = mkPreludeTyConUnique 40

statePrimTyConKey, stableNamePrimTyConKey, stableNameTyConKey,
    mutVarPrimTyConKey, ioTyConKey,
    wordPrimTyConKey, wordTyConKey, word8TyConKey, word16TyConKey,
    word32PrimTyConKey, word32TyConKey, word64PrimTyConKey, word64TyConKey,
    liftedConKey, unliftedConKey, anyBoxConKey, kindConKey, boxityConKey,
    typeConKey, threadIdPrimTyConKey, bcoPrimTyConKey, ptrTyConKey,
    funPtrTyConKey, tVarPrimTyConKey, eqPrimTyConKey,
    eqReprPrimTyConKey :: Unique
statePrimTyConKey                       = mkPreludeTyConUnique 50
stableNamePrimTyConKey                  = mkPreludeTyConUnique 51
stableNameTyConKey                      = mkPreludeTyConUnique 52
eqPrimTyConKey                          = mkPreludeTyConUnique 53
eqReprPrimTyConKey                      = mkPreludeTyConUnique 54
mutVarPrimTyConKey                      = mkPreludeTyConUnique 55
ioTyConKey                              = mkPreludeTyConUnique 56
wordPrimTyConKey                        = mkPreludeTyConUnique 58
wordTyConKey                            = mkPreludeTyConUnique 59
word8TyConKey                           = mkPreludeTyConUnique 60
word16TyConKey                          = mkPreludeTyConUnique 61
word32PrimTyConKey                      = mkPreludeTyConUnique 62
word32TyConKey                          = mkPreludeTyConUnique 63
word64PrimTyConKey                      = mkPreludeTyConUnique 64
word64TyConKey                          = mkPreludeTyConUnique 65
liftedConKey                            = mkPreludeTyConUnique 66
unliftedConKey                          = mkPreludeTyConUnique 67
anyBoxConKey                            = mkPreludeTyConUnique 68
kindConKey                              = mkPreludeTyConUnique 69
boxityConKey                            = mkPreludeTyConUnique 70
typeConKey                              = mkPreludeTyConUnique 71
threadIdPrimTyConKey                    = mkPreludeTyConUnique 72
bcoPrimTyConKey                         = mkPreludeTyConUnique 73
ptrTyConKey                             = mkPreludeTyConUnique 74
funPtrTyConKey                          = mkPreludeTyConUnique 75
tVarPrimTyConKey                        = mkPreludeTyConUnique 76

-- Parallel array type constructor
parrTyConKey :: Unique
parrTyConKey                            = mkPreludeTyConUnique 82

-- dotnet interop
objectTyConKey :: Unique
objectTyConKey                          = mkPreludeTyConUnique 83

eitherTyConKey :: Unique
eitherTyConKey                          = mkPreludeTyConUnique 84

-- Super Kinds constructors
superKindTyConKey :: Unique
superKindTyConKey                     = mkPreludeTyConUnique 85

-- Kind constructors
liftedTypeKindTyConKey, anyKindTyConKey, openTypeKindTyConKey,
  unliftedTypeKindTyConKey, constraintKindTyConKey :: Unique
anyKindTyConKey                         = mkPreludeTyConUnique 86
liftedTypeKindTyConKey                  = mkPreludeTyConUnique 87
openTypeKindTyConKey                    = mkPreludeTyConUnique 88
unliftedTypeKindTyConKey                = mkPreludeTyConUnique 89
constraintKindTyConKey                  = mkPreludeTyConUnique 92

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

pluginTyConKey :: Unique
pluginTyConKey                          = mkPreludeTyConUnique 102

unknownTyConKey, unknown1TyConKey, unknown2TyConKey, unknown3TyConKey,
    opaqueTyConKey :: Unique
unknownTyConKey                         = mkPreludeTyConUnique 129
unknown1TyConKey                        = mkPreludeTyConUnique 130
unknown2TyConKey                        = mkPreludeTyConUnique 131
unknown3TyConKey                        = mkPreludeTyConUnique 132
opaqueTyConKey                          = mkPreludeTyConUnique 133

stringTyConKey :: Unique
stringTyConKey                          = mkPreludeTyConUnique 134

-- Generics (Unique keys)
v1TyConKey, u1TyConKey, par1TyConKey, rec1TyConKey,
  k1TyConKey, m1TyConKey, sumTyConKey, prodTyConKey,
  compTyConKey, rTyConKey, pTyConKey, dTyConKey,
  cTyConKey, sTyConKey, rec0TyConKey, par0TyConKey,
  d1TyConKey, c1TyConKey, s1TyConKey, noSelTyConKey,
  repTyConKey, rep1TyConKey :: Unique

v1TyConKey    = mkPreludeTyConUnique 135
u1TyConKey    = mkPreludeTyConUnique 136
par1TyConKey  = mkPreludeTyConUnique 137
rec1TyConKey  = mkPreludeTyConUnique 138
k1TyConKey    = mkPreludeTyConUnique 139
m1TyConKey    = mkPreludeTyConUnique 140

sumTyConKey   = mkPreludeTyConUnique 141
prodTyConKey  = mkPreludeTyConUnique 142
compTyConKey  = mkPreludeTyConUnique 143

rTyConKey = mkPreludeTyConUnique 144
pTyConKey = mkPreludeTyConUnique 145
dTyConKey = mkPreludeTyConUnique 146
cTyConKey = mkPreludeTyConUnique 147
sTyConKey = mkPreludeTyConUnique 148

rec0TyConKey  = mkPreludeTyConUnique 149
par0TyConKey  = mkPreludeTyConUnique 150
d1TyConKey    = mkPreludeTyConUnique 151
c1TyConKey    = mkPreludeTyConUnique 152
s1TyConKey    = mkPreludeTyConUnique 153
noSelTyConKey = mkPreludeTyConUnique 154

repTyConKey  = mkPreludeTyConUnique 155
rep1TyConKey = mkPreludeTyConUnique 156

-- Type-level naturals
typeNatKindConNameKey, typeSymbolKindConNameKey,
  typeNatAddTyFamNameKey, typeNatMulTyFamNameKey, typeNatExpTyFamNameKey,
  typeNatLeqTyFamNameKey, typeNatSubTyFamNameKey
  :: Unique
typeNatKindConNameKey     = mkPreludeTyConUnique 160
typeSymbolKindConNameKey  = mkPreludeTyConUnique 161
typeNatAddTyFamNameKey    = mkPreludeTyConUnique 162
typeNatMulTyFamNameKey    = mkPreludeTyConUnique 163
typeNatExpTyFamNameKey    = mkPreludeTyConUnique 164
typeNatLeqTyFamNameKey    = mkPreludeTyConUnique 165
typeNatSubTyFamNameKey    = mkPreludeTyConUnique 166

ntTyConKey:: Unique
ntTyConKey = mkPreludeTyConUnique 174
coercibleTyConKey :: Unique
coercibleTyConKey = mkPreludeTyConUnique 175

proxyPrimTyConKey :: Unique
proxyPrimTyConKey = mkPreludeTyConUnique 176

specTyConKey :: Unique
specTyConKey = mkPreludeTyConUnique 177

---------------- Template Haskell -------------------
--      USES TyConUniques 200-299
-----------------------------------------------------

----------------------- SIMD ------------------------
--      USES TyConUniques 300-399
-----------------------------------------------------

#include "primop-vector-uniques.hs-incl"

unitTyConKey :: Unique
unitTyConKey = mkTupleTyConUnique BoxedTuple 0
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
%*                                                                      *
%************************************************************************

\begin{code}
charDataConKey, consDataConKey, doubleDataConKey, falseDataConKey,
    floatDataConKey, intDataConKey, nilDataConKey, ratioDataConKey,
    stableNameDataConKey, trueDataConKey, wordDataConKey,
    ioDataConKey, integerDataConKey, eqBoxDataConKey, coercibleDataConKey :: Unique
charDataConKey                          = mkPreludeDataConUnique  1
consDataConKey                          = mkPreludeDataConUnique  2
doubleDataConKey                        = mkPreludeDataConUnique  3
falseDataConKey                         = mkPreludeDataConUnique  4
floatDataConKey                         = mkPreludeDataConUnique  5
intDataConKey                           = mkPreludeDataConUnique  6
nilDataConKey                           = mkPreludeDataConUnique 11
ratioDataConKey                         = mkPreludeDataConUnique 12
stableNameDataConKey                    = mkPreludeDataConUnique 14
trueDataConKey                          = mkPreludeDataConUnique 15
wordDataConKey                          = mkPreludeDataConUnique 16
ioDataConKey                            = mkPreludeDataConUnique 17
integerDataConKey                       = mkPreludeDataConUnique 18
eqBoxDataConKey                         = mkPreludeDataConUnique 19

-- Generic data constructors
crossDataConKey, inlDataConKey, inrDataConKey, genUnitDataConKey :: Unique
crossDataConKey                         = mkPreludeDataConUnique 20
inlDataConKey                           = mkPreludeDataConUnique 21
inrDataConKey                           = mkPreludeDataConUnique 22
genUnitDataConKey                       = mkPreludeDataConUnique 23

-- Data constructor for parallel arrays
parrDataConKey :: Unique
parrDataConKey                          = mkPreludeDataConUnique 24

leftDataConKey, rightDataConKey :: Unique
leftDataConKey                          = mkPreludeDataConUnique 25
rightDataConKey                         = mkPreludeDataConUnique 26

ltDataConKey, eqDataConKey, gtDataConKey :: Unique
ltDataConKey                            = mkPreludeDataConUnique 27
eqDataConKey                            = mkPreludeDataConUnique 28
gtDataConKey                            = mkPreludeDataConUnique 29

-- For integer-gmp only
integerGmpSDataConKey, integerGmpJDataConKey :: Unique
integerGmpSDataConKey                   = mkPreludeDataConUnique 30
integerGmpJDataConKey                   = mkPreludeDataConUnique 31

coercibleDataConKey                     = mkPreludeDataConUnique 32
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
%*                                                                      *
%************************************************************************

\begin{code}
wildCardKey, absentErrorIdKey, augmentIdKey, appendIdKey,
    buildIdKey, errorIdKey, foldrIdKey, recSelErrorIdKey,
    seqIdKey, irrefutPatErrorIdKey, eqStringIdKey,
    noMethodBindingErrorIdKey, nonExhaustiveGuardsErrorIdKey,
    runtimeErrorIdKey, patErrorIdKey,
    realWorldPrimIdKey, recConErrorIdKey,
    unpackCStringUtf8IdKey, unpackCStringAppendIdKey,
    unpackCStringFoldrIdKey, unpackCStringIdKey :: Unique
wildCardKey                   = mkPreludeMiscIdUnique  0  -- See Note [WildCard]
absentErrorIdKey              = mkPreludeMiscIdUnique  1
augmentIdKey                  = mkPreludeMiscIdUnique  2
appendIdKey                   = mkPreludeMiscIdUnique  3
buildIdKey                    = mkPreludeMiscIdUnique  4
errorIdKey                    = mkPreludeMiscIdUnique  5
foldrIdKey                    = mkPreludeMiscIdUnique  6
recSelErrorIdKey              = mkPreludeMiscIdUnique  7
seqIdKey                      = mkPreludeMiscIdUnique  8
irrefutPatErrorIdKey          = mkPreludeMiscIdUnique  9
eqStringIdKey                 = mkPreludeMiscIdUnique 10
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 11
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 12
runtimeErrorIdKey             = mkPreludeMiscIdUnique 13
patErrorIdKey                 = mkPreludeMiscIdUnique 14
realWorldPrimIdKey            = mkPreludeMiscIdUnique 15
recConErrorIdKey              = mkPreludeMiscIdUnique 16
unpackCStringUtf8IdKey        = mkPreludeMiscIdUnique 17
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 18
unpackCStringFoldrIdKey       = mkPreludeMiscIdUnique 19
unpackCStringIdKey            = mkPreludeMiscIdUnique 20

unsafeCoerceIdKey, concatIdKey, filterIdKey, zipIdKey, bindIOIdKey,
    returnIOIdKey, newStablePtrIdKey,
    printIdKey, failIOIdKey, nullAddrIdKey, voidArgIdKey,
    fstIdKey, sndIdKey, otherwiseIdKey, assertIdKey, runSTRepIdKey :: Unique
unsafeCoerceIdKey             = mkPreludeMiscIdUnique 30
concatIdKey                   = mkPreludeMiscIdUnique 31
filterIdKey                   = mkPreludeMiscIdUnique 32
zipIdKey                      = mkPreludeMiscIdUnique 33
bindIOIdKey                   = mkPreludeMiscIdUnique 34
returnIOIdKey                 = mkPreludeMiscIdUnique 35
newStablePtrIdKey             = mkPreludeMiscIdUnique 36
printIdKey                    = mkPreludeMiscIdUnique 37
failIOIdKey                   = mkPreludeMiscIdUnique 38
nullAddrIdKey                 = mkPreludeMiscIdUnique 39
voidArgIdKey                  = mkPreludeMiscIdUnique 40
fstIdKey                      = mkPreludeMiscIdUnique 41
sndIdKey                      = mkPreludeMiscIdUnique 42
otherwiseIdKey                = mkPreludeMiscIdUnique 43
assertIdKey                   = mkPreludeMiscIdUnique 44
runSTRepIdKey                 = mkPreludeMiscIdUnique 45

mkIntegerIdKey, smallIntegerIdKey, wordToIntegerIdKey,
    integerToWordIdKey, integerToIntIdKey,
    integerToWord64IdKey, integerToInt64IdKey,
    word64ToIntegerIdKey, int64ToIntegerIdKey,
    plusIntegerIdKey, timesIntegerIdKey, minusIntegerIdKey,
    negateIntegerIdKey,
    eqIntegerPrimIdKey, neqIntegerPrimIdKey, absIntegerIdKey, signumIntegerIdKey,
    leIntegerPrimIdKey, gtIntegerPrimIdKey, ltIntegerPrimIdKey, geIntegerPrimIdKey,
    compareIntegerIdKey, quotRemIntegerIdKey, divModIntegerIdKey,
    quotIntegerIdKey, remIntegerIdKey, divIntegerIdKey, modIntegerIdKey,
    floatFromIntegerIdKey, doubleFromIntegerIdKey,
    encodeFloatIntegerIdKey, encodeDoubleIntegerIdKey,
    decodeDoubleIntegerIdKey,
    gcdIntegerIdKey, lcmIntegerIdKey,
    andIntegerIdKey, orIntegerIdKey, xorIntegerIdKey, complementIntegerIdKey,
    shiftLIntegerIdKey, shiftRIntegerIdKey :: Unique
mkIntegerIdKey                = mkPreludeMiscIdUnique 60
smallIntegerIdKey             = mkPreludeMiscIdUnique 61
integerToWordIdKey            = mkPreludeMiscIdUnique 62
integerToIntIdKey             = mkPreludeMiscIdUnique 63
integerToWord64IdKey          = mkPreludeMiscIdUnique 64
integerToInt64IdKey           = mkPreludeMiscIdUnique 65
plusIntegerIdKey              = mkPreludeMiscIdUnique 66
timesIntegerIdKey             = mkPreludeMiscIdUnique 67
minusIntegerIdKey             = mkPreludeMiscIdUnique 68
negateIntegerIdKey            = mkPreludeMiscIdUnique 69
eqIntegerPrimIdKey            = mkPreludeMiscIdUnique 70
neqIntegerPrimIdKey           = mkPreludeMiscIdUnique 71
absIntegerIdKey               = mkPreludeMiscIdUnique 72
signumIntegerIdKey            = mkPreludeMiscIdUnique 73
leIntegerPrimIdKey            = mkPreludeMiscIdUnique 74
gtIntegerPrimIdKey            = mkPreludeMiscIdUnique 75
ltIntegerPrimIdKey            = mkPreludeMiscIdUnique 76
geIntegerPrimIdKey            = mkPreludeMiscIdUnique 77
compareIntegerIdKey           = mkPreludeMiscIdUnique 78
quotIntegerIdKey              = mkPreludeMiscIdUnique 79
remIntegerIdKey               = mkPreludeMiscIdUnique 80
divIntegerIdKey               = mkPreludeMiscIdUnique 81
modIntegerIdKey               = mkPreludeMiscIdUnique 82
divModIntegerIdKey            = mkPreludeMiscIdUnique 83
quotRemIntegerIdKey           = mkPreludeMiscIdUnique 84
floatFromIntegerIdKey         = mkPreludeMiscIdUnique 85
doubleFromIntegerIdKey        = mkPreludeMiscIdUnique 86
encodeFloatIntegerIdKey       = mkPreludeMiscIdUnique 87
encodeDoubleIntegerIdKey      = mkPreludeMiscIdUnique 88
gcdIntegerIdKey               = mkPreludeMiscIdUnique 89
lcmIntegerIdKey               = mkPreludeMiscIdUnique 90
andIntegerIdKey               = mkPreludeMiscIdUnique 91
orIntegerIdKey                = mkPreludeMiscIdUnique 92
xorIntegerIdKey               = mkPreludeMiscIdUnique 93
complementIntegerIdKey        = mkPreludeMiscIdUnique 94
shiftLIntegerIdKey            = mkPreludeMiscIdUnique 95
shiftRIntegerIdKey            = mkPreludeMiscIdUnique 96
wordToIntegerIdKey            = mkPreludeMiscIdUnique 97
word64ToIntegerIdKey          = mkPreludeMiscIdUnique 98
int64ToIntegerIdKey           = mkPreludeMiscIdUnique 99
decodeDoubleIntegerIdKey      = mkPreludeMiscIdUnique 100

rootMainKey, runMainKey :: Unique
rootMainKey                   = mkPreludeMiscIdUnique 101
runMainKey                    = mkPreludeMiscIdUnique 102

thenIOIdKey, lazyIdKey, assertErrorIdKey :: Unique
thenIOIdKey                   = mkPreludeMiscIdUnique 103
lazyIdKey                     = mkPreludeMiscIdUnique 104
assertErrorIdKey              = mkPreludeMiscIdUnique 105

breakpointIdKey, breakpointCondIdKey, breakpointAutoIdKey,
    breakpointJumpIdKey, breakpointCondJumpIdKey,
    breakpointAutoJumpIdKey :: Unique
breakpointIdKey               = mkPreludeMiscIdUnique 110
breakpointCondIdKey           = mkPreludeMiscIdUnique 111
breakpointAutoIdKey           = mkPreludeMiscIdUnique 112
breakpointJumpIdKey           = mkPreludeMiscIdUnique 113
breakpointCondJumpIdKey       = mkPreludeMiscIdUnique 114
breakpointAutoJumpIdKey       = mkPreludeMiscIdUnique 115

inlineIdKey :: Unique
inlineIdKey                   = mkPreludeMiscIdUnique 120

mapIdKey, groupWithIdKey, dollarIdKey :: Unique
mapIdKey              = mkPreludeMiscIdUnique 121
groupWithIdKey        = mkPreludeMiscIdUnique 122
dollarIdKey           = mkPreludeMiscIdUnique 123

coercionTokenIdKey :: Unique
coercionTokenIdKey    = mkPreludeMiscIdUnique 124

rationalToFloatIdKey, rationalToDoubleIdKey :: Unique
rationalToFloatIdKey   = mkPreludeMiscIdUnique 130
rationalToDoubleIdKey  = mkPreludeMiscIdUnique 131

-- dotnet interop
unmarshalObjectIdKey, marshalObjectIdKey, marshalStringIdKey,
    unmarshalStringIdKey, checkDotnetResNameIdKey :: Unique
unmarshalObjectIdKey          = mkPreludeMiscIdUnique 150
marshalObjectIdKey            = mkPreludeMiscIdUnique 151
marshalStringIdKey            = mkPreludeMiscIdUnique 152
unmarshalStringIdKey          = mkPreludeMiscIdUnique 153
checkDotnetResNameIdKey       = mkPreludeMiscIdUnique 154

undefinedKey :: Unique
undefinedKey                  = mkPreludeMiscIdUnique 155

magicDictKey :: Unique
magicDictKey                  = mkPreludeMiscIdUnique 156

coerceKey :: Unique
coerceKey                     = mkPreludeMiscIdUnique 157
\end{code}

Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.

\begin{code}
        -- Just a place holder for  unbound variables  produced by the renamer:
unboundKey :: Unique
unboundKey                    = mkPreludeMiscIdUnique 160

fromIntegerClassOpKey, minusClassOpKey, fromRationalClassOpKey,
    enumFromClassOpKey, enumFromThenClassOpKey, enumFromToClassOpKey,
    enumFromThenToClassOpKey, eqClassOpKey, geClassOpKey, negateClassOpKey,
    failMClassOpKey, bindMClassOpKey, thenMClassOpKey, returnMClassOpKey,
    fmapClassOpKey
    :: Unique
fromIntegerClassOpKey         = mkPreludeMiscIdUnique 160
minusClassOpKey               = mkPreludeMiscIdUnique 161
fromRationalClassOpKey        = mkPreludeMiscIdUnique 162
enumFromClassOpKey            = mkPreludeMiscIdUnique 163
enumFromThenClassOpKey        = mkPreludeMiscIdUnique 164
enumFromToClassOpKey          = mkPreludeMiscIdUnique 165
enumFromThenToClassOpKey      = mkPreludeMiscIdUnique 166
eqClassOpKey                  = mkPreludeMiscIdUnique 167
geClassOpKey                  = mkPreludeMiscIdUnique 168
negateClassOpKey              = mkPreludeMiscIdUnique 169
failMClassOpKey               = mkPreludeMiscIdUnique 170
bindMClassOpKey               = mkPreludeMiscIdUnique 171 -- (>>=)
thenMClassOpKey               = mkPreludeMiscIdUnique 172 -- (>>)
fmapClassOpKey                = mkPreludeMiscIdUnique 173
returnMClassOpKey             = mkPreludeMiscIdUnique 174

-- Recursive do notation
mfixIdKey :: Unique
mfixIdKey       = mkPreludeMiscIdUnique 175

-- Arrow notation
arrAIdKey, composeAIdKey, firstAIdKey, appAIdKey, choiceAIdKey,
    loopAIdKey :: Unique
arrAIdKey       = mkPreludeMiscIdUnique 180
composeAIdKey   = mkPreludeMiscIdUnique 181 -- >>>
firstAIdKey     = mkPreludeMiscIdUnique 182
appAIdKey       = mkPreludeMiscIdUnique 183
choiceAIdKey    = mkPreludeMiscIdUnique 184 --  |||
loopAIdKey      = mkPreludeMiscIdUnique 185

fromStringClassOpKey :: Unique
fromStringClassOpKey          = mkPreludeMiscIdUnique 186

-- Annotation type checking
toAnnotationWrapperIdKey :: Unique
toAnnotationWrapperIdKey      = mkPreludeMiscIdUnique 187

-- Conversion functions
fromIntegralIdKey, realToFracIdKey, toIntegerClassOpKey, toRationalClassOpKey :: Unique
fromIntegralIdKey    = mkPreludeMiscIdUnique 190
realToFracIdKey      = mkPreludeMiscIdUnique 191
toIntegerClassOpKey  = mkPreludeMiscIdUnique 192
toRationalClassOpKey = mkPreludeMiscIdUnique 193

-- Monad comprehensions
guardMIdKey, liftMIdKey, mzipIdKey :: Unique
guardMIdKey     = mkPreludeMiscIdUnique 194
liftMIdKey      = mkPreludeMiscIdUnique 195
mzipIdKey       = mkPreludeMiscIdUnique 196

-- GHCi
ghciStepIoMClassOpKey :: Unique
ghciStepIoMClassOpKey = mkPreludeMiscIdUnique 197

-- Overloaded lists
isListClassKey, fromListClassOpKey, fromListNClassOpKey, toListClassOpKey :: Unique
isListClassKey = mkPreludeMiscIdUnique 198
fromListClassOpKey = mkPreludeMiscIdUnique 199
fromListNClassOpKey = mkPreludeMiscIdUnique 500
toListClassOpKey = mkPreludeMiscIdUnique 501

proxyHashKey :: Unique
proxyHashKey = mkPreludeMiscIdUnique 502

---------------- Template Haskell -------------------
--      USES IdUniques 200-499
-----------------------------------------------------
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Class-std-groups]{Standard groups of Prelude classes}
%*                                                                      *
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

-- The "standard classes" are used in defaulting (Haskell 98 report 4.3.4),
-- and are: "classes defined in the Prelude or a standard library"
standardClassKeys :: [Unique]
standardClassKeys = derivableClassKeys ++ numericClassKeys
                  ++ [randomClassKey, randomGenClassKey,
                      functorClassKey,
                      monadClassKey, monadPlusClassKey,
                      isStringClassKey,
                      applicativeClassKey, foldableClassKey,
                      traversableClassKey, alternativeClassKey
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


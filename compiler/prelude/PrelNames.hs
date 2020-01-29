{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

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
   The way the compiler "knows about" one of these things is
   where the type checker or desugarer needs to look it up. For
   example, when desugaring list comprehensions the desugarer
   needs to conjure up 'foldr'.  It does this by looking up
   foldrName in the environment.

 - RdrNames for Ids, DataCons etc that the compiler may emit into
   generated code (e.g. for deriving).  It's not necessary to know
   the uniques for these guys, only their names


Note [Known-key names]
~~~~~~~~~~~~~~~~~~~~~~
It is *very* important that the compiler gives wired-in things and
things with "known-key" names the correct Uniques wherever they
occur. We have to be careful about this in exactly two places:

  1. When we parse some source code, renaming the AST better yield an
     AST whose Names have the correct uniques

  2. When we read an interface file, the read-in gubbins better have
     the right uniques

This is accomplished through a combination of mechanisms:

  1. When parsing source code, the RdrName-decorated AST has some
     RdrNames which are Exact. These are wired-in RdrNames where the
     we could directly tell from the parsed syntax what Name to
     use. For example, when we parse a [] in a type we can just insert
     an Exact RdrName Name with the listTyConKey.

     Currently, I believe this is just an optimisation: it would be
     equally valid to just output Orig RdrNames that correctly record
     the module etc we expect the final Name to come from. However,
     were we to eliminate isBuiltInOcc_maybe it would become essential
     (see point 3).

  2. The knownKeyNames (which consist of the basicKnownKeyNames from
     the module, and those names reachable via the wired-in stuff from
     TysWiredIn) are used to initialise the "OrigNameCache" in
     GHC.Iface.Env.  This initialization ensures that when the type checker
     or renamer (both of which use GHC.Iface.Env) look up an original name
     (i.e. a pair of a Module and an OccName) for a known-key name
     they get the correct Unique.

     This is the most important mechanism for ensuring that known-key
     stuff gets the right Unique, and is why it is so important to
     place your known-key names in the appropriate lists.

  3. For "infinite families" of known-key names (i.e. tuples and sums), we
     have to be extra careful. Because there are an infinite number of
     these things, we cannot add them to the list of known-key names
     used to initialise the OrigNameCache. Instead, we have to
     rely on never having to look them up in that cache. See
     Note [Infinite families of known-key names] for details.


Note [Infinite families of known-key names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Infinite families of known-key things (e.g. tuples and sums) pose a tricky
problem: we can't add them to the knownKeyNames finite map which we use to
ensure that, e.g., a reference to (,) gets assigned the right unique (if this
doesn't sound familiar see Note [Known-key names] above).

We instead handle tuples and sums separately from the "vanilla" known-key
things,

  a) The parser recognises them specially and generates an Exact Name (hence not
     looked up in the orig-name cache)

  b) The known infinite families of names are specially serialised by
     GHC.Iface.Binary.putName, with that special treatment detected when we read
     back to ensure that we get back to the correct uniques. See Note [Symbol
     table representation of names] in GHC.Iface.Binary and Note [How tuples
     work] in TysWiredIn.

Most of the infinite families cannot occur in source code, so mechanisms (a) and (b)
suffice to ensure that they always have the right Unique. In particular,
implicit param TyCon names, constraint tuples and Any TyCons cannot be mentioned
by the user. For those things that *can* appear in source programs,

  c) GHC.Iface.Env.lookupOrigNameCache uses isBuiltInOcc_maybe to map built-in syntax
     directly onto the corresponding name, rather than trying to find it in the
     original-name cache.

     See also Note [Built-in syntax and the OrigNameCache]


Note [The integer library]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Clearly, we need to know the names of various definitions of the integer
library, e.g. the type itself, `mkInteger` etc. But there are two possible
implementations of the integer library:

 * integer-gmp (fast, but uses libgmp, which may not be available on all
   targets and is GPL licensed)
 * integer-simple (slow, but pure Haskell and BSD-licensed)

We want the compiler to work with either one. The way we achieve this is:

 * When compiling the integer-{gmp,simple} library, we pass
     -this-unit-id  integer-wired-in
   to GHC (see the cabal file libraries/integer-{gmp,simple}.
 * This way, GHC can use just this UnitID (see Module.integerUnitId) when
   generating code, and the linker will succeed.

Unfortuately, the abstraction is not complete: When using integer-gmp, we
really want to use the S# constructor directly. This is controlled by
the `integerLibrary` field of `DynFlags`: If it is IntegerGMP, we use
this constructor directly (see  CorePrep.lookupIntegerSDataConName)

When GHC reads the package data base, it (internally only) pretends it has UnitId
`integer-wired-in` instead of the actual UnitId (which includes the version
number); just like for `base` and other packages, as described in
Note [Wired-in packages] in GHC.Types.Module. This is done in Packages.findWiredInPackages.
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

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

import GhcPrelude

import GHC.Types.Module
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.Unique
import GHC.Types.Name
import GHC.Types.SrcLoc
import FastString

{-
************************************************************************
*                                                                      *
     allNameStrings
*                                                                      *
************************************************************************
-}

allNameStrings :: [String]
-- Infinite list of a,b,c...z, aa, ab, ac, ... etc
allNameStrings = [ c:cs | cs <- "" : allNameStrings, c <- ['a'..'z'] ]

{-
************************************************************************
*                                                                      *
\subsection{Local Names}
*                                                                      *
************************************************************************

This *local* name is used by the interactive stuff
-}

itName :: Unique -> SrcSpan -> Name
itName uniq loc = mkInternalName uniq (mkOccNameFS varName (fsLit "it")) loc

-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: OccName -> Name
mkUnboundName occ = mkInternalName unboundKey occ noSrcSpan

isUnboundName :: Name -> Bool
isUnboundName name = name `hasKey` unboundKey

{-
************************************************************************
*                                                                      *
\subsection{Known key Names}
*                                                                      *
************************************************************************

This section tells what the compiler knows about the association of
names with uniques.  These ones are the *non* wired-in ones.  The
wired in ones are defined in TysWiredIn etc.
-}

basicKnownKeyNames :: [Name]  -- See Note [Known-key names]
basicKnownKeyNames
 = genericTyConNames
 ++ [   --  Classes.  *Must* include:
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
        semigroupClassName, sappendName,
        monoidClassName, memptyName, mappendName, mconcatName,

        -- The IO type
        -- See Note [TyConRepNames for non-wired-in TyCons]
        ioTyConName, ioDataConName,
        runMainIOName,
        runRWName,

        -- Type representation types
        trModuleTyConName, trModuleDataConName,
        trNameTyConName, trNameSDataConName, trNameDDataConName,
        trTyConTyConName, trTyConDataConName,

        -- Typeable
        typeableClassName,
        typeRepTyConName,
        someTypeRepTyConName,
        someTypeRepDataConName,
        kindRepTyConName,
        kindRepTyConAppDataConName,
        kindRepVarDataConName,
        kindRepAppDataConName,
        kindRepFunDataConName,
        kindRepTYPEDataConName,
        kindRepTypeLitSDataConName,
        kindRepTypeLitDDataConName,
        typeLitSortTyConName,
        typeLitSymbolDataConName,
        typeLitNatDataConName,
        typeRepIdName,
        mkTrTypeName,
        mkTrConName,
        mkTrAppName,
        mkTrFunName,
        typeSymbolTypeRepName, typeNatTypeRepName,
        trGhcPrimModuleName,

        -- KindReps for common cases
        starKindRepName,
        starArrStarKindRepName,
        starArrStarArrStarKindRepName,

        -- Dynamic
        toDynName,

        -- Numeric stuff
        negateName, minusName, geName, eqName,

        -- Conversion functions
        rationalTyConName,
        ratioTyConName, ratioDataConName,
        fromRationalName, fromIntegerName,
        toIntegerName, toRationalName,
        fromIntegralName, realToFracName,

        -- Int# stuff
        divIntName, modIntName,

        -- String stuff
        fromStringName,

        -- Enum stuff
        enumFromName, enumFromThenName,
        enumFromThenToName, enumFromToName,

        -- Applicative stuff
        pureAName, apAName, thenAName,

        -- Functor stuff
        fmapName,

        -- Monad stuff
        thenIOName, bindIOName, returnIOName, failIOName, bindMName, thenMName,
        returnMName, joinMName,

        -- MonadFail
        monadFailClassName, failMName,

        -- MonadFix
        monadFixClassName, mfixName,

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
        unpackCStringName, unpackCStringUtf8Name,
        unpackCStringFoldrName, unpackCStringFoldrUtf8Name,

        -- Overloaded lists
        isListClassName,
        fromListName,
        fromListNName,
        toListName,

        -- List operations
        concatName, filterName, mapName,
        zipName, foldrName, buildName, augmentName, appendName,
        elemName,

        -- FFI primitive types that are not wired-in.
        stablePtrTyConName, ptrTyConName, funPtrTyConName,
        int8TyConName, int16TyConName, int32TyConName, int64TyConName,
        word16TyConName, word32TyConName, word64TyConName,

        -- Others
        otherwiseIdName, inlineIdName,
        eqStringName, assertName, breakpointName, breakpointCondName,
        opaqueTyConName,
        assertErrorName, traceName,
        printName, fstName, sndName,
        dollarName,

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
        shiftLIntegerName, shiftRIntegerName, bitIntegerName,
        integerSDataConName,naturalSDataConName,

        -- Natural
        naturalTyConName,
        naturalFromIntegerName, naturalToIntegerName,
        plusNaturalName, minusNaturalName, timesNaturalName, mkNaturalName,
        wordToNaturalName,

        -- Float/Double
        rationalToFloatName,
        rationalToDoubleName,

        -- Other classes
        randomClassName, randomGenClassName, monadPlusClassName,

        -- Type-level naturals
        knownNatClassName, knownSymbolClassName,

        -- Overloaded labels
        isLabelClassName,

        -- Implicit Parameters
        ipClassName,

        -- Overloaded record fields
        hasFieldClassName,

        -- Call Stacks
        callStackTyConName,
        emptyCallStackName, pushCallStackName,

        -- Source Locations
        srcLocDataConName,

        -- Annotation type checking
        toAnnotationWrapperName

        -- The Ordering type
        , orderingTyConName
        , ordLTDataConName, ordEQDataConName, ordGTDataConName

        -- The SPEC type for SpecConstr
        , specTyConName

        -- The Either type
        , eitherTyConName, leftDataConName, rightDataConName

        -- Plugins
        , pluginTyConName
        , frontendPluginTyConName

        -- Generics
        , genClassName, gen1ClassName
        , datatypeClassName, constructorClassName, selectorClassName

        -- Monad comprehensions
        , guardMName
        , liftMName
        , mzipName

        -- GHCi Sandbox
        , ghciIoClassName, ghciStepIoMName

        -- StaticPtr
        , makeStaticName
        , staticPtrTyConName
        , staticPtrDataConName, staticPtrInfoDataConName
        , fromStaticPtrName

        -- Fingerprint
        , fingerprintDataConName

        -- Custom type errors
        , errorMessageTypeErrorFamName
        , typeErrorTextDataConName
        , typeErrorAppendDataConName
        , typeErrorVAppendDataConName
        , typeErrorShowTypeDataConName

        -- Unsafe coercion proofs
        , unsafeEqualityProofName
        , unsafeEqualityTyConName
        , unsafeReflDataConName
        , unsafeCoercePrimName
        , unsafeCoerceName
    ]

genericTyConNames :: [Name]
genericTyConNames = [
    v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
    k1TyConName, m1TyConName, sumTyConName, prodTyConName,
    compTyConName, rTyConName, dTyConName,
    cTyConName, sTyConName, rec0TyConName,
    d1TyConName, c1TyConName, s1TyConName, noSelTyConName,
    repTyConName, rep1TyConName, uRecTyConName,
    uAddrTyConName, uCharTyConName, uDoubleTyConName,
    uFloatTyConName, uIntTyConName, uWordTyConName,
    prefixIDataConName, infixIDataConName, leftAssociativeDataConName,
    rightAssociativeDataConName, notAssociativeDataConName,
    sourceUnpackDataConName, sourceNoUnpackDataConName,
    noSourceUnpackednessDataConName, sourceLazyDataConName,
    sourceStrictDataConName, noSourceStrictnessDataConName,
    decidedLazyDataConName, decidedStrictDataConName, decidedUnpackDataConName,
    metaDataDataConName, metaConsDataConName, metaSelDataConName
  ]

{-
************************************************************************
*                                                                      *
\subsection{Module names}
*                                                                      *
************************************************************************


--MetaHaskell Extension Add a new module here
-}

pRELUDE :: Module
pRELUDE         = mkBaseModule_ pRELUDE_NAME

gHC_PRIM, gHC_TYPES, gHC_GENERICS, gHC_MAGIC,
    gHC_CLASSES, gHC_PRIMOPWRAPPERS, gHC_BASE, gHC_ENUM,
    gHC_GHCI, gHC_GHCI_HELPERS, gHC_CSTRING,
    gHC_SHOW, gHC_READ, gHC_NUM, gHC_MAYBE, gHC_INTEGER_TYPE, gHC_NATURAL,
    gHC_LIST, gHC_TUPLE, dATA_TUPLE, dATA_EITHER, dATA_LIST, dATA_STRING,
    dATA_FOLDABLE, dATA_TRAVERSABLE,
    gHC_CONC, gHC_IO, gHC_IO_Exception,
    gHC_ST, gHC_IX, gHC_STABLE, gHC_PTR, gHC_ERR, gHC_REAL,
    gHC_FLOAT, gHC_TOP_HANDLER, sYSTEM_IO, dYNAMIC,
    tYPEABLE, tYPEABLE_INTERNAL, gENERICS,
    rEAD_PREC, lEX, gHC_INT, gHC_WORD, mONAD, mONAD_FIX, mONAD_ZIP, mONAD_FAIL,
    aRROW, cONTROL_APPLICATIVE, gHC_DESUGAR, rANDOM, gHC_EXTS,
    cONTROL_EXCEPTION_BASE, gHC_TYPELITS, gHC_TYPENATS, dATA_TYPE_EQUALITY,
    dATA_COERCE, dEBUG_TRACE, uNSAFE_COERCE :: Module

gHC_PRIM        = mkPrimModule (fsLit "GHC.Prim")   -- Primitive types and values
gHC_TYPES       = mkPrimModule (fsLit "GHC.Types")
gHC_MAGIC       = mkPrimModule (fsLit "GHC.Magic")
gHC_CSTRING     = mkPrimModule (fsLit "GHC.CString")
gHC_CLASSES     = mkPrimModule (fsLit "GHC.Classes")
gHC_PRIMOPWRAPPERS = mkPrimModule (fsLit "GHC.PrimopWrappers")

gHC_BASE        = mkBaseModule (fsLit "GHC.Base")
gHC_ENUM        = mkBaseModule (fsLit "GHC.Enum")
gHC_GHCI        = mkBaseModule (fsLit "GHC.GHCi")
gHC_GHCI_HELPERS= mkBaseModule (fsLit "GHC.GHCi.Helpers")
gHC_SHOW        = mkBaseModule (fsLit "GHC.Show")
gHC_READ        = mkBaseModule (fsLit "GHC.Read")
gHC_NUM         = mkBaseModule (fsLit "GHC.Num")
gHC_MAYBE       = mkBaseModule (fsLit "GHC.Maybe")
gHC_INTEGER_TYPE= mkIntegerModule (fsLit "GHC.Integer.Type")
gHC_NATURAL     = mkBaseModule (fsLit "GHC.Natural")
gHC_LIST        = mkBaseModule (fsLit "GHC.List")
gHC_TUPLE       = mkPrimModule (fsLit "GHC.Tuple")
dATA_TUPLE      = mkBaseModule (fsLit "Data.Tuple")
dATA_EITHER     = mkBaseModule (fsLit "Data.Either")
dATA_LIST       = mkBaseModule (fsLit "Data.List")
dATA_STRING     = mkBaseModule (fsLit "Data.String")
dATA_FOLDABLE   = mkBaseModule (fsLit "Data.Foldable")
dATA_TRAVERSABLE= mkBaseModule (fsLit "Data.Traversable")
gHC_CONC        = mkBaseModule (fsLit "GHC.Conc")
gHC_IO          = mkBaseModule (fsLit "GHC.IO")
gHC_IO_Exception = mkBaseModule (fsLit "GHC.IO.Exception")
gHC_ST          = mkBaseModule (fsLit "GHC.ST")
gHC_IX          = mkBaseModule (fsLit "GHC.Ix")
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
gENERICS        = mkBaseModule (fsLit "Data.Data")
rEAD_PREC       = mkBaseModule (fsLit "Text.ParserCombinators.ReadPrec")
lEX             = mkBaseModule (fsLit "Text.Read.Lex")
gHC_INT         = mkBaseModule (fsLit "GHC.Int")
gHC_WORD        = mkBaseModule (fsLit "GHC.Word")
mONAD           = mkBaseModule (fsLit "Control.Monad")
mONAD_FIX       = mkBaseModule (fsLit "Control.Monad.Fix")
mONAD_ZIP       = mkBaseModule (fsLit "Control.Monad.Zip")
mONAD_FAIL      = mkBaseModule (fsLit "Control.Monad.Fail")
aRROW           = mkBaseModule (fsLit "Control.Arrow")
cONTROL_APPLICATIVE = mkBaseModule (fsLit "Control.Applicative")
gHC_DESUGAR = mkBaseModule (fsLit "GHC.Desugar")
rANDOM          = mkBaseModule (fsLit "System.Random")
gHC_EXTS        = mkBaseModule (fsLit "GHC.Exts")
cONTROL_EXCEPTION_BASE = mkBaseModule (fsLit "Control.Exception.Base")
gHC_GENERICS    = mkBaseModule (fsLit "GHC.Generics")
gHC_TYPELITS    = mkBaseModule (fsLit "GHC.TypeLits")
gHC_TYPENATS    = mkBaseModule (fsLit "GHC.TypeNats")
dATA_TYPE_EQUALITY = mkBaseModule (fsLit "Data.Type.Equality")
dATA_COERCE     = mkBaseModule (fsLit "Data.Coerce")
dEBUG_TRACE     = mkBaseModule (fsLit "Debug.Trace")
uNSAFE_COERCE   = mkBaseModule (fsLit "Unsafe.Coerce")

gHC_SRCLOC :: Module
gHC_SRCLOC = mkBaseModule (fsLit "GHC.SrcLoc")

gHC_STACK, gHC_STACK_TYPES :: Module
gHC_STACK = mkBaseModule (fsLit "GHC.Stack")
gHC_STACK_TYPES = mkBaseModule (fsLit "GHC.Stack.Types")

gHC_STATICPTR :: Module
gHC_STATICPTR = mkBaseModule (fsLit "GHC.StaticPtr")

gHC_STATICPTR_INTERNAL :: Module
gHC_STATICPTR_INTERNAL = mkBaseModule (fsLit "GHC.StaticPtr.Internal")

gHC_FINGERPRINT_TYPE :: Module
gHC_FINGERPRINT_TYPE = mkBaseModule (fsLit "GHC.Fingerprint.Type")

gHC_OVER_LABELS :: Module
gHC_OVER_LABELS = mkBaseModule (fsLit "GHC.OverloadedLabels")

gHC_RECORDS :: Module
gHC_RECORDS = mkBaseModule (fsLit "GHC.Records")

mAIN, rOOT_MAIN :: Module
mAIN            = mkMainModule_ mAIN_NAME
rOOT_MAIN       = mkMainModule (fsLit ":Main") -- Root module for initialisation

mkInteractiveModule :: Int -> Module
-- (mkInteractiveMoudule 9) makes module 'interactive:M9'
mkInteractiveModule n = mkModule interactiveUnitId (mkModuleName ("Ghci" ++ show n))

pRELUDE_NAME, mAIN_NAME :: ModuleName
pRELUDE_NAME   = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME      = mkModuleNameFS (fsLit "Main")

dATA_ARRAY_PARALLEL_NAME, dATA_ARRAY_PARALLEL_PRIM_NAME :: ModuleName
dATA_ARRAY_PARALLEL_NAME      = mkModuleNameFS (fsLit "Data.Array.Parallel")
dATA_ARRAY_PARALLEL_PRIM_NAME = mkModuleNameFS (fsLit "Data.Array.Parallel.Prim")

mkPrimModule :: FastString -> Module
mkPrimModule m = mkModule primUnitId (mkModuleNameFS m)

mkIntegerModule :: FastString -> Module
mkIntegerModule m = mkModule integerUnitId (mkModuleNameFS m)

mkBaseModule :: FastString -> Module
mkBaseModule m = mkModule baseUnitId (mkModuleNameFS m)

mkBaseModule_ :: ModuleName -> Module
mkBaseModule_ m = mkModule baseUnitId m

mkThisGhcModule :: FastString -> Module
mkThisGhcModule m = mkModule thisGhcUnitId (mkModuleNameFS m)

mkThisGhcModule_ :: ModuleName -> Module
mkThisGhcModule_ m = mkModule thisGhcUnitId m

mkMainModule :: FastString -> Module
mkMainModule m = mkModule mainUnitId (mkModuleNameFS m)

mkMainModule_ :: ModuleName -> Module
mkMainModule_ m = mkModule mainUnitId m

{-
************************************************************************
*                                                                      *
                        RdrNames
*                                                                      *
************************************************************************
-}

main_RDR_Unqual    :: RdrName
main_RDR_Unqual = mkUnqual varName (fsLit "main")
        -- We definitely don't want an Orig RdrName, because
        -- main might, in principle, be imported into module Main

eq_RDR, ge_RDR, le_RDR, lt_RDR, gt_RDR, compare_RDR,
    ltTag_RDR, eqTag_RDR, gtTag_RDR :: RdrName
eq_RDR                  = nameRdrName eqName
ge_RDR                  = nameRdrName geName
le_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "<=")
lt_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "<")
gt_RDR                  = varQual_RDR  gHC_CLASSES (fsLit ">")
compare_RDR             = varQual_RDR  gHC_CLASSES (fsLit "compare")
ltTag_RDR               = nameRdrName  ordLTDataConName
eqTag_RDR               = nameRdrName  ordEQDataConName
gtTag_RDR               = nameRdrName  ordGTDataConName

eqClass_RDR, numClass_RDR, ordClass_RDR, enumClass_RDR, monadClass_RDR
    :: RdrName
eqClass_RDR             = nameRdrName eqClassName
numClass_RDR            = nameRdrName numClassName
ordClass_RDR            = nameRdrName ordClassName
enumClass_RDR           = nameRdrName enumClassName
monadClass_RDR          = nameRdrName monadClassName

map_RDR, append_RDR, elem_RDR :: RdrName
map_RDR                 = nameRdrName mapName
append_RDR              = nameRdrName appendName
elem_RDR                = nameRdrName elemName

foldr_RDR, build_RDR, returnM_RDR, bindM_RDR, failM_RDR
    :: RdrName
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
    unpackCStringFoldrUtf8_RDR, unpackCStringUtf8_RDR :: RdrName
eqString_RDR            = nameRdrName eqStringName
unpackCString_RDR       = nameRdrName unpackCStringName
unpackCStringFoldr_RDR  = nameRdrName unpackCStringFoldrName
unpackCStringUtf8_RDR   = nameRdrName unpackCStringUtf8Name
unpackCStringFoldrUtf8_RDR  = nameRdrName unpackCStringFoldrUtf8Name

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

toInteger_RDR, toRational_RDR, fromIntegral_RDR :: RdrName
toInteger_RDR           = nameRdrName toIntegerName
toRational_RDR          = nameRdrName toRationalName
fromIntegral_RDR        = nameRdrName fromIntegralName

stringTy_RDR, fromString_RDR :: RdrName
stringTy_RDR            = tcQual_RDR gHC_BASE (fsLit "String")
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
range_RDR               = varQual_RDR gHC_IX (fsLit "range")
inRange_RDR             = varQual_RDR gHC_IX (fsLit "inRange")
index_RDR               = varQual_RDR gHC_IX (fsLit "index")
unsafeIndex_RDR         = varQual_RDR gHC_IX (fsLit "unsafeIndex")
unsafeRangeSize_RDR     = varQual_RDR gHC_IX (fsLit "unsafeRangeSize")

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

readField_RDR, readFieldHash_RDR, readSymField_RDR :: RdrName
readField_RDR           = varQual_RDR gHC_READ (fsLit "readField")
readFieldHash_RDR       = varQual_RDR gHC_READ (fsLit "readFieldHash")
readSymField_RDR        = varQual_RDR gHC_READ (fsLit "readSymField")

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

showsPrec_RDR, shows_RDR, showString_RDR,
    showSpace_RDR, showCommaSpace_RDR, showParen_RDR :: RdrName
showsPrec_RDR           = varQual_RDR gHC_SHOW (fsLit "showsPrec")
shows_RDR               = varQual_RDR gHC_SHOW (fsLit "shows")
showString_RDR          = varQual_RDR gHC_SHOW (fsLit "showString")
showSpace_RDR           = varQual_RDR gHC_SHOW (fsLit "showSpace")
showCommaSpace_RDR      = varQual_RDR gHC_SHOW (fsLit "showCommaSpace")
showParen_RDR           = varQual_RDR gHC_SHOW (fsLit "showParen")

error_RDR :: RdrName
error_RDR = varQual_RDR gHC_ERR (fsLit "error")

-- Generics (constructors and functions)
u1DataCon_RDR, par1DataCon_RDR, rec1DataCon_RDR,
  k1DataCon_RDR, m1DataCon_RDR, l1DataCon_RDR, r1DataCon_RDR,
  prodDataCon_RDR, comp1DataCon_RDR,
  unPar1_RDR, unRec1_RDR, unK1_RDR, unComp1_RDR,
  from_RDR, from1_RDR, to_RDR, to1_RDR,
  datatypeName_RDR, moduleName_RDR, packageName_RDR, isNewtypeName_RDR,
  conName_RDR, conFixity_RDR, conIsRecord_RDR, selName_RDR,
  prefixDataCon_RDR, infixDataCon_RDR, leftAssocDataCon_RDR,
  rightAssocDataCon_RDR, notAssocDataCon_RDR,
  uAddrDataCon_RDR, uCharDataCon_RDR, uDoubleDataCon_RDR,
  uFloatDataCon_RDR, uIntDataCon_RDR, uWordDataCon_RDR,
  uAddrHash_RDR, uCharHash_RDR, uDoubleHash_RDR,
  uFloatHash_RDR, uIntHash_RDR, uWordHash_RDR :: RdrName

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
packageName_RDR   = varQual_RDR gHC_GENERICS (fsLit "packageName")
isNewtypeName_RDR = varQual_RDR gHC_GENERICS (fsLit "isNewtype")
selName_RDR       = varQual_RDR gHC_GENERICS (fsLit "selName")
conName_RDR       = varQual_RDR gHC_GENERICS (fsLit "conName")
conFixity_RDR     = varQual_RDR gHC_GENERICS (fsLit "conFixity")
conIsRecord_RDR   = varQual_RDR gHC_GENERICS (fsLit "conIsRecord")

prefixDataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "Prefix")
infixDataCon_RDR      = dataQual_RDR gHC_GENERICS (fsLit "Infix")
leftAssocDataCon_RDR  = nameRdrName leftAssociativeDataConName
rightAssocDataCon_RDR = nameRdrName rightAssociativeDataConName
notAssocDataCon_RDR   = nameRdrName notAssociativeDataConName

uAddrDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit "UAddr")
uCharDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit "UChar")
uDoubleDataCon_RDR = dataQual_RDR gHC_GENERICS (fsLit "UDouble")
uFloatDataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "UFloat")
uIntDataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "UInt")
uWordDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit "UWord")

uAddrHash_RDR   = varQual_RDR gHC_GENERICS (fsLit "uAddr#")
uCharHash_RDR   = varQual_RDR gHC_GENERICS (fsLit "uChar#")
uDoubleHash_RDR = varQual_RDR gHC_GENERICS (fsLit "uDouble#")
uFloatHash_RDR  = varQual_RDR gHC_GENERICS (fsLit "uFloat#")
uIntHash_RDR    = varQual_RDR gHC_GENERICS (fsLit "uInt#")
uWordHash_RDR   = varQual_RDR gHC_GENERICS (fsLit "uWord#")

fmap_RDR, replace_RDR, pure_RDR, ap_RDR, liftA2_RDR, foldable_foldr_RDR,
    foldMap_RDR, null_RDR, all_RDR, traverse_RDR, mempty_RDR,
    mappend_RDR :: RdrName
fmap_RDR                = nameRdrName fmapName
replace_RDR             = varQual_RDR gHC_BASE (fsLit "<$")
pure_RDR                = nameRdrName pureAName
ap_RDR                  = nameRdrName apAName
liftA2_RDR              = varQual_RDR gHC_BASE (fsLit "liftA2")
foldable_foldr_RDR      = varQual_RDR dATA_FOLDABLE       (fsLit "foldr")
foldMap_RDR             = varQual_RDR dATA_FOLDABLE       (fsLit "foldMap")
null_RDR                = varQual_RDR dATA_FOLDABLE       (fsLit "null")
all_RDR                 = varQual_RDR dATA_FOLDABLE       (fsLit "all")
traverse_RDR            = varQual_RDR dATA_TRAVERSABLE    (fsLit "traverse")
mempty_RDR              = nameRdrName memptyName
mappend_RDR             = nameRdrName mappendName

----------------------
varQual_RDR, tcQual_RDR, clsQual_RDR, dataQual_RDR
    :: Module -> FastString -> RdrName
varQual_RDR  mod str = mkOrig mod (mkOccNameFS varName str)
tcQual_RDR   mod str = mkOrig mod (mkOccNameFS tcName str)
clsQual_RDR  mod str = mkOrig mod (mkOccNameFS clsName str)
dataQual_RDR mod str = mkOrig mod (mkOccNameFS dataName str)

{-
************************************************************************
*                                                                      *
\subsection{Known-key names}
*                                                                      *
************************************************************************

Many of these Names are not really "built in", but some parts of the
compiler (notably the deriving mechanism) need to mention their names,
and it's convenient to write them all down in one place.
-}

wildCardName :: Name
wildCardName = mkSystemVarName wildCardKey (fsLit "wild")

runMainIOName, runRWName :: Name
runMainIOName = varQual gHC_TOP_HANDLER (fsLit "runMainIO") runMainKey
runRWName     = varQual gHC_MAGIC       (fsLit "runRW#")    runRWKey

orderingTyConName, ordLTDataConName, ordEQDataConName, ordGTDataConName :: Name
orderingTyConName = tcQual  gHC_TYPES (fsLit "Ordering") orderingTyConKey
ordLTDataConName     = dcQual gHC_TYPES (fsLit "LT") ordLTDataConKey
ordEQDataConName     = dcQual gHC_TYPES (fsLit "EQ") ordEQDataConKey
ordGTDataConName     = dcQual gHC_TYPES (fsLit "GT") ordGTDataConKey

specTyConName :: Name
specTyConName     = tcQual gHC_TYPES (fsLit "SPEC") specTyConKey

eitherTyConName, leftDataConName, rightDataConName :: Name
eitherTyConName   = tcQual  dATA_EITHER (fsLit "Either") eitherTyConKey
leftDataConName   = dcQual dATA_EITHER (fsLit "Left")   leftDataConKey
rightDataConName  = dcQual dATA_EITHER (fsLit "Right")  rightDataConKey

-- Generics (types)
v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
  k1TyConName, m1TyConName, sumTyConName, prodTyConName,
  compTyConName, rTyConName, dTyConName,
  cTyConName, sTyConName, rec0TyConName,
  d1TyConName, c1TyConName, s1TyConName, noSelTyConName,
  repTyConName, rep1TyConName, uRecTyConName,
  uAddrTyConName, uCharTyConName, uDoubleTyConName,
  uFloatTyConName, uIntTyConName, uWordTyConName,
  prefixIDataConName, infixIDataConName, leftAssociativeDataConName,
  rightAssociativeDataConName, notAssociativeDataConName,
  sourceUnpackDataConName, sourceNoUnpackDataConName,
  noSourceUnpackednessDataConName, sourceLazyDataConName,
  sourceStrictDataConName, noSourceStrictnessDataConName,
  decidedLazyDataConName, decidedStrictDataConName, decidedUnpackDataConName,
  metaDataDataConName, metaConsDataConName, metaSelDataConName :: Name

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
dTyConName  = tcQual gHC_GENERICS (fsLit "D") dTyConKey
cTyConName  = tcQual gHC_GENERICS (fsLit "C") cTyConKey
sTyConName  = tcQual gHC_GENERICS (fsLit "S") sTyConKey

rec0TyConName  = tcQual gHC_GENERICS (fsLit "Rec0") rec0TyConKey
d1TyConName  = tcQual gHC_GENERICS (fsLit "D1") d1TyConKey
c1TyConName  = tcQual gHC_GENERICS (fsLit "C1") c1TyConKey
s1TyConName  = tcQual gHC_GENERICS (fsLit "S1") s1TyConKey
noSelTyConName = tcQual gHC_GENERICS (fsLit "NoSelector") noSelTyConKey

repTyConName  = tcQual gHC_GENERICS (fsLit "Rep")  repTyConKey
rep1TyConName = tcQual gHC_GENERICS (fsLit "Rep1") rep1TyConKey

uRecTyConName      = tcQual gHC_GENERICS (fsLit "URec") uRecTyConKey
uAddrTyConName     = tcQual gHC_GENERICS (fsLit "UAddr") uAddrTyConKey
uCharTyConName     = tcQual gHC_GENERICS (fsLit "UChar") uCharTyConKey
uDoubleTyConName   = tcQual gHC_GENERICS (fsLit "UDouble") uDoubleTyConKey
uFloatTyConName    = tcQual gHC_GENERICS (fsLit "UFloat") uFloatTyConKey
uIntTyConName      = tcQual gHC_GENERICS (fsLit "UInt") uIntTyConKey
uWordTyConName     = tcQual gHC_GENERICS (fsLit "UWord") uWordTyConKey

prefixIDataConName = dcQual gHC_GENERICS (fsLit "PrefixI")  prefixIDataConKey
infixIDataConName  = dcQual gHC_GENERICS (fsLit "InfixI")   infixIDataConKey
leftAssociativeDataConName  = dcQual gHC_GENERICS (fsLit "LeftAssociative")   leftAssociativeDataConKey
rightAssociativeDataConName = dcQual gHC_GENERICS (fsLit "RightAssociative")  rightAssociativeDataConKey
notAssociativeDataConName   = dcQual gHC_GENERICS (fsLit "NotAssociative")    notAssociativeDataConKey

sourceUnpackDataConName         = dcQual gHC_GENERICS (fsLit "SourceUnpack")         sourceUnpackDataConKey
sourceNoUnpackDataConName       = dcQual gHC_GENERICS (fsLit "SourceNoUnpack")       sourceNoUnpackDataConKey
noSourceUnpackednessDataConName = dcQual gHC_GENERICS (fsLit "NoSourceUnpackedness") noSourceUnpackednessDataConKey
sourceLazyDataConName           = dcQual gHC_GENERICS (fsLit "SourceLazy")           sourceLazyDataConKey
sourceStrictDataConName         = dcQual gHC_GENERICS (fsLit "SourceStrict")         sourceStrictDataConKey
noSourceStrictnessDataConName   = dcQual gHC_GENERICS (fsLit "NoSourceStrictness")   noSourceStrictnessDataConKey
decidedLazyDataConName          = dcQual gHC_GENERICS (fsLit "DecidedLazy")          decidedLazyDataConKey
decidedStrictDataConName        = dcQual gHC_GENERICS (fsLit "DecidedStrict")        decidedStrictDataConKey
decidedUnpackDataConName        = dcQual gHC_GENERICS (fsLit "DecidedUnpack")        decidedUnpackDataConKey

metaDataDataConName  = dcQual gHC_GENERICS (fsLit "MetaData")  metaDataDataConKey
metaConsDataConName  = dcQual gHC_GENERICS (fsLit "MetaCons")  metaConsDataConKey
metaSelDataConName   = dcQual gHC_GENERICS (fsLit "MetaSel")   metaSelDataConKey

-- Primitive Int
divIntName, modIntName :: Name
divIntName = varQual gHC_CLASSES (fsLit "divInt#") divIntIdKey
modIntName = varQual gHC_CLASSES (fsLit "modInt#") modIntIdKey

-- Base strings Strings
unpackCStringName, unpackCStringFoldrName,
    unpackCStringUtf8Name, unpackCStringFoldrUtf8Name,eqStringName :: Name
unpackCStringName       = varQual gHC_CSTRING (fsLit "unpackCString#") unpackCStringIdKey
unpackCStringFoldrName  = varQual gHC_CSTRING (fsLit "unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual gHC_CSTRING (fsLit "unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName            = varQual gHC_BASE (fsLit "eqString")  eqStringIdKey
unpackCStringFoldrUtf8Name  = varQual gHC_CSTRING (fsLit "unpackFoldrCStringUtf8#") unpackCStringFoldrUtf8IdKey

-- The 'inline' function
inlineIdName :: Name
inlineIdName            = varQual gHC_MAGIC (fsLit "inline") inlineIdKey

-- Base classes (Eq, Ord, Functor)
fmapName, eqClassName, eqName, ordClassName, geName, functorClassName :: Name
eqClassName       = clsQual gHC_CLASSES (fsLit "Eq")      eqClassKey
eqName            = varQual gHC_CLASSES (fsLit "==")      eqClassOpKey
ordClassName      = clsQual gHC_CLASSES (fsLit "Ord")     ordClassKey
geName            = varQual gHC_CLASSES (fsLit ">=")      geClassOpKey
functorClassName  = clsQual gHC_BASE    (fsLit "Functor") functorClassKey
fmapName          = varQual gHC_BASE    (fsLit "fmap")    fmapClassOpKey

-- Class Monad
monadClassName, thenMName, bindMName, returnMName :: Name
monadClassName     = clsQual gHC_BASE (fsLit "Monad")  monadClassKey
thenMName          = varQual gHC_BASE (fsLit ">>")     thenMClassOpKey
bindMName          = varQual gHC_BASE (fsLit ">>=")    bindMClassOpKey
returnMName        = varQual gHC_BASE (fsLit "return") returnMClassOpKey

-- Class MonadFail
monadFailClassName, failMName :: Name
monadFailClassName = clsQual mONAD_FAIL (fsLit "MonadFail") monadFailClassKey
failMName          = varQual mONAD_FAIL (fsLit "fail")      failMClassOpKey

-- Class Applicative
applicativeClassName, pureAName, apAName, thenAName :: Name
applicativeClassName = clsQual gHC_BASE (fsLit "Applicative") applicativeClassKey
apAName              = varQual gHC_BASE (fsLit "<*>")         apAClassOpKey
pureAName            = varQual gHC_BASE (fsLit "pure")        pureAClassOpKey
thenAName            = varQual gHC_BASE (fsLit "*>")          thenAClassOpKey

-- Classes (Foldable, Traversable)
foldableClassName, traversableClassName :: Name
foldableClassName     = clsQual  dATA_FOLDABLE       (fsLit "Foldable")    foldableClassKey
traversableClassName  = clsQual  dATA_TRAVERSABLE    (fsLit "Traversable") traversableClassKey

-- Classes (Semigroup, Monoid)
semigroupClassName, sappendName :: Name
semigroupClassName = clsQual gHC_BASE       (fsLit "Semigroup") semigroupClassKey
sappendName        = varQual gHC_BASE       (fsLit "<>")        sappendClassOpKey
monoidClassName, memptyName, mappendName, mconcatName :: Name
monoidClassName    = clsQual gHC_BASE       (fsLit "Monoid")    monoidClassKey
memptyName         = varQual gHC_BASE       (fsLit "mempty")    memptyClassOpKey
mappendName        = varQual gHC_BASE       (fsLit "mappend")   mappendClassOpKey
mconcatName        = varQual gHC_BASE       (fsLit "mconcat")   mconcatClassOpKey



-- AMP additions

joinMName, alternativeClassName :: Name
joinMName            = varQual gHC_BASE (fsLit "join")        joinMIdKey
alternativeClassName = clsQual mONAD (fsLit "Alternative") alternativeClassKey

--
joinMIdKey, apAClassOpKey, pureAClassOpKey, thenAClassOpKey,
    alternativeClassKey :: Unique
joinMIdKey          = mkPreludeMiscIdUnique 750
apAClassOpKey       = mkPreludeMiscIdUnique 751 -- <*>
pureAClassOpKey     = mkPreludeMiscIdUnique 752
thenAClassOpKey     = mkPreludeMiscIdUnique 753
alternativeClassKey = mkPreludeMiscIdUnique 754


-- Functions for GHC extensions
groupWithName :: Name
groupWithName = varQual gHC_EXTS (fsLit "groupWith") groupWithIdKey

-- Random PrelBase functions
fromStringName, otherwiseIdName, foldrName, buildName, augmentName,
    mapName, appendName, elemName, assertName,
    breakpointName, breakpointCondName,
    opaqueTyConName, dollarName :: Name
dollarName        = varQual gHC_BASE (fsLit "$")          dollarIdKey
otherwiseIdName   = varQual gHC_BASE (fsLit "otherwise")  otherwiseIdKey
foldrName         = varQual gHC_BASE (fsLit "foldr")      foldrIdKey
buildName         = varQual gHC_BASE (fsLit "build")      buildIdKey
augmentName       = varQual gHC_BASE (fsLit "augment")    augmentIdKey
mapName           = varQual gHC_BASE (fsLit "map")        mapIdKey
appendName        = varQual gHC_BASE (fsLit "++")         appendIdKey
assertName        = varQual gHC_BASE (fsLit "assert")     assertIdKey
breakpointName    = varQual gHC_BASE (fsLit "breakpoint") breakpointIdKey
breakpointCondName= varQual gHC_BASE (fsLit "breakpointCond") breakpointCondIdKey
opaqueTyConName   = tcQual  gHC_BASE (fsLit "Opaque")     opaqueTyConKey
elemName          = varQual gHC_LIST (fsLit "elem")       elemIdKey
fromStringName = varQual dATA_STRING (fsLit "fromString") fromStringClassOpKey

-- PrelTup
fstName, sndName :: Name
fstName           = varQual dATA_TUPLE (fsLit "fst") fstIdKey
sndName           = varQual dATA_TUPLE (fsLit "snd") sndIdKey

-- Module GHC.Num
numClassName, fromIntegerName, minusName, negateName :: Name
numClassName      = clsQual gHC_NUM (fsLit "Num")         numClassKey
fromIntegerName   = varQual gHC_NUM (fsLit "fromInteger") fromIntegerClassOpKey
minusName         = varQual gHC_NUM (fsLit "-")           minusClassOpKey
negateName        = varQual gHC_NUM (fsLit "negate")      negateClassOpKey

integerTyConName, mkIntegerName, integerSDataConName,
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
    shiftLIntegerName, shiftRIntegerName, bitIntegerName :: Name
integerTyConName      = tcQual gHC_INTEGER_TYPE (fsLit "Integer")           integerTyConKey
integerSDataConName   = dcQual gHC_INTEGER_TYPE (fsLit "S#")                integerSDataConKey
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
bitIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "bitInteger")        bitIntegerIdKey

-- GHC.Natural types
naturalTyConName, naturalSDataConName :: Name
naturalTyConName     = tcQual gHC_NATURAL (fsLit "Natural") naturalTyConKey
naturalSDataConName  = dcQual gHC_NATURAL (fsLit "NatS#")   naturalSDataConKey

naturalFromIntegerName :: Name
naturalFromIntegerName = varQual gHC_NATURAL (fsLit "naturalFromInteger") naturalFromIntegerIdKey

naturalToIntegerName, plusNaturalName, minusNaturalName, timesNaturalName,
   mkNaturalName, wordToNaturalName :: Name
naturalToIntegerName  = varQual gHC_NATURAL (fsLit "naturalToInteger")  naturalToIntegerIdKey
plusNaturalName       = varQual gHC_NATURAL (fsLit "plusNatural")       plusNaturalIdKey
minusNaturalName      = varQual gHC_NATURAL (fsLit "minusNatural")      minusNaturalIdKey
timesNaturalName      = varQual gHC_NATURAL (fsLit "timesNatural")      timesNaturalIdKey
mkNaturalName         = varQual gHC_NATURAL (fsLit "mkNatural")         mkNaturalIdKey
wordToNaturalName     = varQual gHC_NATURAL (fsLit "wordToNatural#")    wordToNaturalIdKey

-- GHC.Real types and classes
rationalTyConName, ratioTyConName, ratioDataConName, realClassName,
    integralClassName, realFracClassName, fractionalClassName,
    fromRationalName, toIntegerName, toRationalName, fromIntegralName,
    realToFracName :: Name
rationalTyConName   = tcQual  gHC_REAL (fsLit "Rational")     rationalTyConKey
ratioTyConName      = tcQual  gHC_REAL (fsLit "Ratio")        ratioTyConKey
ratioDataConName    = dcQual  gHC_REAL (fsLit ":%")           ratioDataConKey
realClassName       = clsQual gHC_REAL (fsLit "Real")         realClassKey
integralClassName   = clsQual gHC_REAL (fsLit "Integral")     integralClassKey
realFracClassName   = clsQual gHC_REAL (fsLit "RealFrac")     realFracClassKey
fractionalClassName = clsQual gHC_REAL (fsLit "Fractional")   fractionalClassKey
fromRationalName    = varQual gHC_REAL (fsLit "fromRational") fromRationalClassOpKey
toIntegerName       = varQual gHC_REAL (fsLit "toInteger")    toIntegerClassOpKey
toRationalName      = varQual gHC_REAL (fsLit "toRational")   toRationalClassOpKey
fromIntegralName    = varQual  gHC_REAL (fsLit "fromIntegral")fromIntegralIdKey
realToFracName      = varQual  gHC_REAL (fsLit "realToFrac")  realToFracIdKey

-- PrelFloat classes
floatingClassName, realFloatClassName :: Name
floatingClassName  = clsQual gHC_FLOAT (fsLit "Floating")  floatingClassKey
realFloatClassName = clsQual gHC_FLOAT (fsLit "RealFloat") realFloatClassKey

-- other GHC.Float functions
rationalToFloatName, rationalToDoubleName :: Name
rationalToFloatName  = varQual gHC_FLOAT (fsLit "rationalToFloat") rationalToFloatIdKey
rationalToDoubleName = varQual gHC_FLOAT (fsLit "rationalToDouble") rationalToDoubleIdKey

-- Class Ix
ixClassName :: Name
ixClassName = clsQual gHC_IX (fsLit "Ix") ixClassKey

-- Typeable representation types
trModuleTyConName
  , trModuleDataConName
  , trNameTyConName
  , trNameSDataConName
  , trNameDDataConName
  , trTyConTyConName
  , trTyConDataConName
  :: Name
trModuleTyConName     = tcQual gHC_TYPES          (fsLit "Module")         trModuleTyConKey
trModuleDataConName   = dcQual gHC_TYPES          (fsLit "Module")         trModuleDataConKey
trNameTyConName       = tcQual gHC_TYPES          (fsLit "TrName")         trNameTyConKey
trNameSDataConName    = dcQual gHC_TYPES          (fsLit "TrNameS")        trNameSDataConKey
trNameDDataConName    = dcQual gHC_TYPES          (fsLit "TrNameD")        trNameDDataConKey
trTyConTyConName      = tcQual gHC_TYPES          (fsLit "TyCon")          trTyConTyConKey
trTyConDataConName    = dcQual gHC_TYPES          (fsLit "TyCon")          trTyConDataConKey

kindRepTyConName
  , kindRepTyConAppDataConName
  , kindRepVarDataConName
  , kindRepAppDataConName
  , kindRepFunDataConName
  , kindRepTYPEDataConName
  , kindRepTypeLitSDataConName
  , kindRepTypeLitDDataConName
  :: Name
kindRepTyConName      = tcQual gHC_TYPES          (fsLit "KindRep")        kindRepTyConKey
kindRepTyConAppDataConName = dcQual gHC_TYPES     (fsLit "KindRepTyConApp") kindRepTyConAppDataConKey
kindRepVarDataConName = dcQual gHC_TYPES          (fsLit "KindRepVar")     kindRepVarDataConKey
kindRepAppDataConName = dcQual gHC_TYPES          (fsLit "KindRepApp")     kindRepAppDataConKey
kindRepFunDataConName = dcQual gHC_TYPES          (fsLit "KindRepFun")     kindRepFunDataConKey
kindRepTYPEDataConName = dcQual gHC_TYPES         (fsLit "KindRepTYPE")    kindRepTYPEDataConKey
kindRepTypeLitSDataConName = dcQual gHC_TYPES     (fsLit "KindRepTypeLitS") kindRepTypeLitSDataConKey
kindRepTypeLitDDataConName = dcQual gHC_TYPES     (fsLit "KindRepTypeLitD") kindRepTypeLitDDataConKey

typeLitSortTyConName
  , typeLitSymbolDataConName
  , typeLitNatDataConName
  :: Name
typeLitSortTyConName     = tcQual gHC_TYPES       (fsLit "TypeLitSort")    typeLitSortTyConKey
typeLitSymbolDataConName = dcQual gHC_TYPES       (fsLit "TypeLitSymbol")  typeLitSymbolDataConKey
typeLitNatDataConName    = dcQual gHC_TYPES       (fsLit "TypeLitNat")     typeLitNatDataConKey

-- Class Typeable, and functions for constructing `Typeable` dictionaries
typeableClassName
  , typeRepTyConName
  , someTypeRepTyConName
  , someTypeRepDataConName
  , mkTrTypeName
  , mkTrConName
  , mkTrAppName
  , mkTrFunName
  , typeRepIdName
  , typeNatTypeRepName
  , typeSymbolTypeRepName
  , trGhcPrimModuleName
  :: Name
typeableClassName     = clsQual tYPEABLE_INTERNAL (fsLit "Typeable")       typeableClassKey
typeRepTyConName      = tcQual  tYPEABLE_INTERNAL (fsLit "TypeRep")        typeRepTyConKey
someTypeRepTyConName   = tcQual tYPEABLE_INTERNAL (fsLit "SomeTypeRep")    someTypeRepTyConKey
someTypeRepDataConName = dcQual tYPEABLE_INTERNAL (fsLit "SomeTypeRep")    someTypeRepDataConKey
typeRepIdName         = varQual tYPEABLE_INTERNAL (fsLit "typeRep#")       typeRepIdKey
mkTrTypeName          = varQual tYPEABLE_INTERNAL (fsLit "mkTrType")       mkTrTypeKey
mkTrConName           = varQual tYPEABLE_INTERNAL (fsLit "mkTrCon")        mkTrConKey
mkTrAppName           = varQual tYPEABLE_INTERNAL (fsLit "mkTrApp")        mkTrAppKey
mkTrFunName           = varQual tYPEABLE_INTERNAL (fsLit "mkTrFun")        mkTrFunKey
typeNatTypeRepName    = varQual tYPEABLE_INTERNAL (fsLit "typeNatTypeRep") typeNatTypeRepKey
typeSymbolTypeRepName = varQual tYPEABLE_INTERNAL (fsLit "typeSymbolTypeRep") typeSymbolTypeRepKey
-- this is the Typeable 'Module' for GHC.Prim (which has no code, so we place in GHC.Types)
-- See Note [Grand plan for Typeable] in TcTypeable.
trGhcPrimModuleName   = varQual gHC_TYPES         (fsLit "tr$ModuleGHCPrim")  trGhcPrimModuleKey

-- Typeable KindReps for some common cases
starKindRepName, starArrStarKindRepName, starArrStarArrStarKindRepName :: Name
starKindRepName        = varQual gHC_TYPES         (fsLit "krep$*")         starKindRepKey
starArrStarKindRepName = varQual gHC_TYPES         (fsLit "krep$*Arr*")     starArrStarKindRepKey
starArrStarArrStarKindRepName = varQual gHC_TYPES  (fsLit "krep$*->*->*")   starArrStarArrStarKindRepKey

-- Custom type errors
errorMessageTypeErrorFamName
  , typeErrorTextDataConName
  , typeErrorAppendDataConName
  , typeErrorVAppendDataConName
  , typeErrorShowTypeDataConName
  :: Name

errorMessageTypeErrorFamName =
  tcQual gHC_TYPELITS (fsLit "TypeError") errorMessageTypeErrorFamKey

typeErrorTextDataConName =
  dcQual gHC_TYPELITS (fsLit "Text") typeErrorTextDataConKey

typeErrorAppendDataConName =
  dcQual gHC_TYPELITS (fsLit ":<>:") typeErrorAppendDataConKey

typeErrorVAppendDataConName =
  dcQual gHC_TYPELITS (fsLit ":$$:") typeErrorVAppendDataConKey

typeErrorShowTypeDataConName =
  dcQual gHC_TYPELITS (fsLit "ShowType") typeErrorShowTypeDataConKey

-- Unsafe coercion proofs
unsafeEqualityProofName, unsafeEqualityTyConName, unsafeCoercePrimName,
  unsafeCoerceName, unsafeReflDataConName :: Name
unsafeEqualityProofName = varQual uNSAFE_COERCE (fsLit "unsafeEqualityProof") unsafeEqualityProofIdKey
unsafeEqualityTyConName = tcQual uNSAFE_COERCE (fsLit "UnsafeEquality") unsafeEqualityTyConKey
unsafeReflDataConName   = dcQual uNSAFE_COERCE (fsLit "UnsafeRefl")     unsafeReflDataConKey
unsafeCoercePrimName    = varQual uNSAFE_COERCE (fsLit "unsafeCoerce#") unsafeCoercePrimIdKey
unsafeCoerceName        = varQual uNSAFE_COERCE (fsLit "unsafeCoerce")  unsafeCoerceIdKey

-- Dynamic
toDynName :: Name
toDynName = varQual dYNAMIC (fsLit "toDyn") toDynIdKey

-- Class Data
dataClassName :: Name
dataClassName = clsQual gENERICS (fsLit "Data") dataClassKey

-- Error module
assertErrorName    :: Name
assertErrorName   = varQual gHC_IO_Exception (fsLit "assertError") assertErrorIdKey

-- Debug.Trace
traceName          :: Name
traceName         = varQual dEBUG_TRACE (fsLit "trace") traceKey

-- Enum module (Enum, Bounded)
enumClassName, enumFromName, enumFromToName, enumFromThenName,
    enumFromThenToName, boundedClassName :: Name
enumClassName      = clsQual gHC_ENUM (fsLit "Enum")           enumClassKey
enumFromName       = varQual gHC_ENUM (fsLit "enumFrom")       enumFromClassOpKey
enumFromToName     = varQual gHC_ENUM (fsLit "enumFromTo")     enumFromToClassOpKey
enumFromThenName   = varQual gHC_ENUM (fsLit "enumFromThen")   enumFromThenClassOpKey
enumFromThenToName = varQual gHC_ENUM (fsLit "enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual gHC_ENUM (fsLit "Bounded")        boundedClassKey

-- List functions
concatName, filterName, zipName :: Name
concatName        = varQual gHC_LIST (fsLit "concat") concatIdKey
filterName        = varQual gHC_LIST (fsLit "filter") filterIdKey
zipName           = varQual gHC_LIST (fsLit "zip")    zipIdKey

-- Overloaded lists
isListClassName, fromListName, fromListNName, toListName :: Name
isListClassName = clsQual gHC_EXTS (fsLit "IsList")    isListClassKey
fromListName    = varQual gHC_EXTS (fsLit "fromList")  fromListClassOpKey
fromListNName   = varQual gHC_EXTS (fsLit "fromListN") fromListNClassOpKey
toListName      = varQual gHC_EXTS (fsLit "toList")    toListClassOpKey

-- Class Show
showClassName :: Name
showClassName   = clsQual gHC_SHOW (fsLit "Show")      showClassKey

-- Class Read
readClassName :: Name
readClassName   = clsQual gHC_READ (fsLit "Read")      readClassKey

-- Classes Generic and Generic1, Datatype, Constructor and Selector
genClassName, gen1ClassName, datatypeClassName, constructorClassName,
  selectorClassName :: Name
genClassName  = clsQual gHC_GENERICS (fsLit "Generic")  genClassKey
gen1ClassName = clsQual gHC_GENERICS (fsLit "Generic1") gen1ClassKey

datatypeClassName    = clsQual gHC_GENERICS (fsLit "Datatype")    datatypeClassKey
constructorClassName = clsQual gHC_GENERICS (fsLit "Constructor") constructorClassKey
selectorClassName    = clsQual gHC_GENERICS (fsLit "Selector")    selectorClassKey

genericClassNames :: [Name]
genericClassNames = [genClassName, gen1ClassName]

-- GHCi things
ghciIoClassName, ghciStepIoMName :: Name
ghciIoClassName = clsQual gHC_GHCI (fsLit "GHCiSandboxIO") ghciIoClassKey
ghciStepIoMName = varQual gHC_GHCI (fsLit "ghciStepIO") ghciStepIoMClassOpKey

-- IO things
ioTyConName, ioDataConName,
  thenIOName, bindIOName, returnIOName, failIOName :: Name
ioTyConName       = tcQual  gHC_TYPES (fsLit "IO")       ioTyConKey
ioDataConName     = dcQual  gHC_TYPES (fsLit "IO")       ioDataConKey
thenIOName        = varQual gHC_BASE  (fsLit "thenIO")   thenIOIdKey
bindIOName        = varQual gHC_BASE  (fsLit "bindIO")   bindIOIdKey
returnIOName      = varQual gHC_BASE  (fsLit "returnIO") returnIOIdKey
failIOName        = varQual gHC_IO    (fsLit "failIO")   failIOIdKey

-- IO things
printName :: Name
printName         = varQual sYSTEM_IO (fsLit "print") printIdKey

-- Int, Word, and Addr things
int8TyConName, int16TyConName, int32TyConName, int64TyConName :: Name
int8TyConName     = tcQual gHC_INT  (fsLit "Int8")  int8TyConKey
int16TyConName    = tcQual gHC_INT  (fsLit "Int16") int16TyConKey
int32TyConName    = tcQual gHC_INT  (fsLit "Int32") int32TyConKey
int64TyConName    = tcQual gHC_INT  (fsLit "Int64") int64TyConKey

-- Word module
word16TyConName, word32TyConName, word64TyConName :: Name
word16TyConName   = tcQual  gHC_WORD (fsLit "Word16") word16TyConKey
word32TyConName   = tcQual  gHC_WORD (fsLit "Word32") word32TyConKey
word64TyConName   = tcQual  gHC_WORD (fsLit "Word64") word64TyConKey

-- PrelPtr module
ptrTyConName, funPtrTyConName :: Name
ptrTyConName      = tcQual   gHC_PTR (fsLit "Ptr")    ptrTyConKey
funPtrTyConName   = tcQual   gHC_PTR (fsLit "FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName, newStablePtrName :: Name
stablePtrTyConName    = tcQual   gHC_STABLE (fsLit "StablePtr")    stablePtrTyConKey
newStablePtrName      = varQual  gHC_STABLE (fsLit "newStablePtr") newStablePtrIdKey

-- Recursive-do notation
monadFixClassName, mfixName :: Name
monadFixClassName  = clsQual mONAD_FIX (fsLit "MonadFix") monadFixClassKey
mfixName           = varQual mONAD_FIX (fsLit "mfix")     mfixIdKey

-- Arrow notation
arrAName, composeAName, firstAName, appAName, choiceAName, loopAName :: Name
arrAName           = varQual aRROW (fsLit "arr")       arrAIdKey
composeAName       = varQual gHC_DESUGAR (fsLit ">>>") composeAIdKey
firstAName         = varQual aRROW (fsLit "first")     firstAIdKey
appAName           = varQual aRROW (fsLit "app")       appAIdKey
choiceAName        = varQual aRROW (fsLit "|||")       choiceAIdKey
loopAName          = varQual aRROW (fsLit "loop")      loopAIdKey

-- Monad comprehensions
guardMName, liftMName, mzipName :: Name
guardMName         = varQual mONAD (fsLit "guard")    guardMIdKey
liftMName          = varQual mONAD (fsLit "liftM")    liftMIdKey
mzipName           = varQual mONAD_ZIP (fsLit "mzip") mzipIdKey


-- Annotation type checking
toAnnotationWrapperName :: Name
toAnnotationWrapperName = varQual gHC_DESUGAR (fsLit "toAnnotationWrapper") toAnnotationWrapperIdKey

-- Other classes, needed for type defaulting
monadPlusClassName, randomClassName, randomGenClassName,
    isStringClassName :: Name
monadPlusClassName  = clsQual mONAD (fsLit "MonadPlus")      monadPlusClassKey
randomClassName     = clsQual rANDOM (fsLit "Random")        randomClassKey
randomGenClassName  = clsQual rANDOM (fsLit "RandomGen")     randomGenClassKey
isStringClassName   = clsQual dATA_STRING (fsLit "IsString") isStringClassKey

-- Type-level naturals
knownNatClassName :: Name
knownNatClassName     = clsQual gHC_TYPENATS (fsLit "KnownNat") knownNatClassNameKey
knownSymbolClassName :: Name
knownSymbolClassName  = clsQual gHC_TYPELITS (fsLit "KnownSymbol") knownSymbolClassNameKey

-- Overloaded labels
isLabelClassName :: Name
isLabelClassName
 = clsQual gHC_OVER_LABELS (fsLit "IsLabel") isLabelClassNameKey

-- Implicit Parameters
ipClassName :: Name
ipClassName
  = clsQual gHC_CLASSES (fsLit "IP") ipClassKey

-- Overloaded record fields
hasFieldClassName :: Name
hasFieldClassName
 = clsQual gHC_RECORDS (fsLit "HasField") hasFieldClassNameKey

-- Source Locations
callStackTyConName, emptyCallStackName, pushCallStackName,
  srcLocDataConName :: Name
callStackTyConName
  = tcQual gHC_STACK_TYPES  (fsLit "CallStack") callStackTyConKey
emptyCallStackName
  = varQual gHC_STACK_TYPES (fsLit "emptyCallStack") emptyCallStackKey
pushCallStackName
  = varQual gHC_STACK_TYPES (fsLit "pushCallStack") pushCallStackKey
srcLocDataConName
  = dcQual gHC_STACK_TYPES  (fsLit "SrcLoc")    srcLocDataConKey

-- plugins
pLUGINS :: Module
pLUGINS = mkThisGhcModule (fsLit "GHC.Driver.Plugins")
pluginTyConName :: Name
pluginTyConName = tcQual pLUGINS (fsLit "Plugin") pluginTyConKey
frontendPluginTyConName :: Name
frontendPluginTyConName = tcQual pLUGINS (fsLit "FrontendPlugin") frontendPluginTyConKey

-- Static pointers
makeStaticName :: Name
makeStaticName =
    varQual gHC_STATICPTR_INTERNAL (fsLit "makeStatic") makeStaticKey

staticPtrInfoTyConName :: Name
staticPtrInfoTyConName =
    tcQual gHC_STATICPTR (fsLit "StaticPtrInfo") staticPtrInfoTyConKey

staticPtrInfoDataConName :: Name
staticPtrInfoDataConName =
    dcQual gHC_STATICPTR (fsLit "StaticPtrInfo") staticPtrInfoDataConKey

staticPtrTyConName :: Name
staticPtrTyConName =
    tcQual gHC_STATICPTR (fsLit "StaticPtr") staticPtrTyConKey

staticPtrDataConName :: Name
staticPtrDataConName =
    dcQual gHC_STATICPTR (fsLit "StaticPtr") staticPtrDataConKey

fromStaticPtrName :: Name
fromStaticPtrName =
    varQual gHC_STATICPTR (fsLit "fromStaticPtr") fromStaticPtrClassOpKey

fingerprintDataConName :: Name
fingerprintDataConName =
    dcQual gHC_FINGERPRINT_TYPE (fsLit "Fingerprint") fingerprintDataConKey

{-
************************************************************************
*                                                                      *
\subsection{Local helpers}
*                                                                      *
************************************************************************

All these are original names; hence mkOrig
-}

varQual, tcQual, clsQual, dcQual :: Module -> FastString -> Unique -> Name
varQual  = mk_known_key_name varName
tcQual   = mk_known_key_name tcName
clsQual  = mk_known_key_name clsName
dcQual   = mk_known_key_name dataName

mk_known_key_name :: NameSpace -> Module -> FastString -> Unique -> Name
mk_known_key_name space modu str unique
  = mkExternalName unique modu (mkOccNameFS space str) noSrcSpan


{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}
*                                                                      *
************************************************************************
--MetaHaskell extension hand allocate keys here
-}

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

typeableClassKey :: Unique
typeableClassKey        = mkPreludeClassUnique 20

monadFixClassKey :: Unique
monadFixClassKey        = mkPreludeClassUnique 28

monadFailClassKey :: Unique
monadFailClassKey       = mkPreludeClassUnique 29

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

isLabelClassNameKey :: Unique
isLabelClassNameKey = mkPreludeClassUnique 45

semigroupClassKey, monoidClassKey :: Unique
semigroupClassKey = mkPreludeClassUnique 46
monoidClassKey    = mkPreludeClassUnique 47

-- Implicit Parameters
ipClassKey :: Unique
ipClassKey = mkPreludeClassUnique 48

-- Overloaded record fields
hasFieldClassNameKey :: Unique
hasFieldClassNameKey = mkPreludeClassUnique 49


---------------- Template Haskell -------------------
--      THNames.hs: USES ClassUniques 200-299
-----------------------------------------------------

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
*                                                                      *
************************************************************************
-}

addrPrimTyConKey, arrayPrimTyConKey, arrayArrayPrimTyConKey, boolTyConKey,
    byteArrayPrimTyConKey, charPrimTyConKey, charTyConKey, doublePrimTyConKey,
    doubleTyConKey, floatPrimTyConKey, floatTyConKey, funTyConKey,
    intPrimTyConKey, intTyConKey, int8TyConKey, int16TyConKey,
    int8PrimTyConKey, int16PrimTyConKey, int32PrimTyConKey, int32TyConKey,
    int64PrimTyConKey, int64TyConKey,
    integerTyConKey, naturalTyConKey,
    listTyConKey, foreignObjPrimTyConKey, maybeTyConKey,
    weakPrimTyConKey, mutableArrayPrimTyConKey, mutableArrayArrayPrimTyConKey,
    mutableByteArrayPrimTyConKey, orderingTyConKey, mVarPrimTyConKey,
    ratioTyConKey, rationalTyConKey, realWorldTyConKey, stablePtrPrimTyConKey,
    stablePtrTyConKey, eqTyConKey, heqTyConKey,
    smallArrayPrimTyConKey, smallMutableArrayPrimTyConKey :: Unique
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
int8PrimTyConKey                        = mkPreludeTyConUnique 16
int8TyConKey                            = mkPreludeTyConUnique 17
int16PrimTyConKey                       = mkPreludeTyConUnique 18
int16TyConKey                           = mkPreludeTyConUnique 19
int32PrimTyConKey                       = mkPreludeTyConUnique 20
int32TyConKey                           = mkPreludeTyConUnique 21
int64PrimTyConKey                       = mkPreludeTyConUnique 22
int64TyConKey                           = mkPreludeTyConUnique 23
integerTyConKey                         = mkPreludeTyConUnique 24
naturalTyConKey                         = mkPreludeTyConUnique 25

listTyConKey                            = mkPreludeTyConUnique 26
foreignObjPrimTyConKey                  = mkPreludeTyConUnique 27
maybeTyConKey                           = mkPreludeTyConUnique 28
weakPrimTyConKey                        = mkPreludeTyConUnique 29
mutableArrayPrimTyConKey                = mkPreludeTyConUnique 30
mutableByteArrayPrimTyConKey            = mkPreludeTyConUnique 31
orderingTyConKey                        = mkPreludeTyConUnique 32
mVarPrimTyConKey                        = mkPreludeTyConUnique 33
ratioTyConKey                           = mkPreludeTyConUnique 34
rationalTyConKey                        = mkPreludeTyConUnique 35
realWorldTyConKey                       = mkPreludeTyConUnique 36
stablePtrPrimTyConKey                   = mkPreludeTyConUnique 37
stablePtrTyConKey                       = mkPreludeTyConUnique 38
eqTyConKey                              = mkPreludeTyConUnique 40
heqTyConKey                             = mkPreludeTyConUnique 41
arrayArrayPrimTyConKey                  = mkPreludeTyConUnique 42
mutableArrayArrayPrimTyConKey           = mkPreludeTyConUnique 43

statePrimTyConKey, stableNamePrimTyConKey, stableNameTyConKey,
    mutVarPrimTyConKey, ioTyConKey,
    wordPrimTyConKey, wordTyConKey, word8PrimTyConKey, word8TyConKey,
    word16PrimTyConKey, word16TyConKey, word32PrimTyConKey, word32TyConKey,
    word64PrimTyConKey, word64TyConKey,
    liftedConKey, unliftedConKey, anyBoxConKey, kindConKey, boxityConKey,
    typeConKey, threadIdPrimTyConKey, bcoPrimTyConKey, ptrTyConKey,
    funPtrTyConKey, tVarPrimTyConKey, eqPrimTyConKey,
    eqReprPrimTyConKey, eqPhantPrimTyConKey, voidPrimTyConKey,
    compactPrimTyConKey :: Unique
statePrimTyConKey                       = mkPreludeTyConUnique 50
stableNamePrimTyConKey                  = mkPreludeTyConUnique 51
stableNameTyConKey                      = mkPreludeTyConUnique 52
eqPrimTyConKey                          = mkPreludeTyConUnique 53
eqReprPrimTyConKey                      = mkPreludeTyConUnique 54
eqPhantPrimTyConKey                     = mkPreludeTyConUnique 55
mutVarPrimTyConKey                      = mkPreludeTyConUnique 56
ioTyConKey                              = mkPreludeTyConUnique 57
voidPrimTyConKey                        = mkPreludeTyConUnique 58
wordPrimTyConKey                        = mkPreludeTyConUnique 59
wordTyConKey                            = mkPreludeTyConUnique 60
word8PrimTyConKey                       = mkPreludeTyConUnique 61
word8TyConKey                           = mkPreludeTyConUnique 62
word16PrimTyConKey                      = mkPreludeTyConUnique 63
word16TyConKey                          = mkPreludeTyConUnique 64
word32PrimTyConKey                      = mkPreludeTyConUnique 65
word32TyConKey                          = mkPreludeTyConUnique 66
word64PrimTyConKey                      = mkPreludeTyConUnique 67
word64TyConKey                          = mkPreludeTyConUnique 68
liftedConKey                            = mkPreludeTyConUnique 69
unliftedConKey                          = mkPreludeTyConUnique 70
anyBoxConKey                            = mkPreludeTyConUnique 71
kindConKey                              = mkPreludeTyConUnique 72
boxityConKey                            = mkPreludeTyConUnique 73
typeConKey                              = mkPreludeTyConUnique 74
threadIdPrimTyConKey                    = mkPreludeTyConUnique 75
bcoPrimTyConKey                         = mkPreludeTyConUnique 76
ptrTyConKey                             = mkPreludeTyConUnique 77
funPtrTyConKey                          = mkPreludeTyConUnique 78
tVarPrimTyConKey                        = mkPreludeTyConUnique 79
compactPrimTyConKey                     = mkPreludeTyConUnique 80

eitherTyConKey :: Unique
eitherTyConKey                          = mkPreludeTyConUnique 84

-- Kind constructors
liftedTypeKindTyConKey, tYPETyConKey,
  constraintKindTyConKey, runtimeRepTyConKey,
  vecCountTyConKey, vecElemTyConKey :: Unique
liftedTypeKindTyConKey                  = mkPreludeTyConUnique 87
tYPETyConKey                            = mkPreludeTyConUnique 88
constraintKindTyConKey                  = mkPreludeTyConUnique 92
runtimeRepTyConKey                      = mkPreludeTyConUnique 95
vecCountTyConKey                        = mkPreludeTyConUnique 96
vecElemTyConKey                         = mkPreludeTyConUnique 97

pluginTyConKey, frontendPluginTyConKey :: Unique
pluginTyConKey                          = mkPreludeTyConUnique 102
frontendPluginTyConKey                  = mkPreludeTyConUnique 103

unknownTyConKey, unknown1TyConKey, unknown2TyConKey, unknown3TyConKey,
    opaqueTyConKey :: Unique
unknownTyConKey                         = mkPreludeTyConUnique 129
unknown1TyConKey                        = mkPreludeTyConUnique 130
unknown2TyConKey                        = mkPreludeTyConUnique 131
unknown3TyConKey                        = mkPreludeTyConUnique 132
opaqueTyConKey                          = mkPreludeTyConUnique 133

-- Generics (Unique keys)
v1TyConKey, u1TyConKey, par1TyConKey, rec1TyConKey,
  k1TyConKey, m1TyConKey, sumTyConKey, prodTyConKey,
  compTyConKey, rTyConKey, dTyConKey,
  cTyConKey, sTyConKey, rec0TyConKey,
  d1TyConKey, c1TyConKey, s1TyConKey, noSelTyConKey,
  repTyConKey, rep1TyConKey, uRecTyConKey,
  uAddrTyConKey, uCharTyConKey, uDoubleTyConKey,
  uFloatTyConKey, uIntTyConKey, uWordTyConKey :: Unique

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
dTyConKey = mkPreludeTyConUnique 146
cTyConKey = mkPreludeTyConUnique 147
sTyConKey = mkPreludeTyConUnique 148

rec0TyConKey  = mkPreludeTyConUnique 149
d1TyConKey    = mkPreludeTyConUnique 151
c1TyConKey    = mkPreludeTyConUnique 152
s1TyConKey    = mkPreludeTyConUnique 153
noSelTyConKey = mkPreludeTyConUnique 154

repTyConKey  = mkPreludeTyConUnique 155
rep1TyConKey = mkPreludeTyConUnique 156

uRecTyConKey    = mkPreludeTyConUnique 157
uAddrTyConKey   = mkPreludeTyConUnique 158
uCharTyConKey   = mkPreludeTyConUnique 159
uDoubleTyConKey = mkPreludeTyConUnique 160
uFloatTyConKey  = mkPreludeTyConUnique 161
uIntTyConKey    = mkPreludeTyConUnique 162
uWordTyConKey   = mkPreludeTyConUnique 163

-- Type-level naturals
typeNatKindConNameKey, typeSymbolKindConNameKey,
  typeNatAddTyFamNameKey, typeNatMulTyFamNameKey, typeNatExpTyFamNameKey,
  typeNatLeqTyFamNameKey, typeNatSubTyFamNameKey
  , typeSymbolCmpTyFamNameKey, typeNatCmpTyFamNameKey
  , typeNatDivTyFamNameKey
  , typeNatModTyFamNameKey
  , typeNatLogTyFamNameKey
  :: Unique
typeNatKindConNameKey     = mkPreludeTyConUnique 164
typeSymbolKindConNameKey  = mkPreludeTyConUnique 165
typeNatAddTyFamNameKey    = mkPreludeTyConUnique 166
typeNatMulTyFamNameKey    = mkPreludeTyConUnique 167
typeNatExpTyFamNameKey    = mkPreludeTyConUnique 168
typeNatLeqTyFamNameKey    = mkPreludeTyConUnique 169
typeNatSubTyFamNameKey    = mkPreludeTyConUnique 170
typeSymbolCmpTyFamNameKey = mkPreludeTyConUnique 171
typeNatCmpTyFamNameKey    = mkPreludeTyConUnique 172
typeNatDivTyFamNameKey  = mkPreludeTyConUnique 173
typeNatModTyFamNameKey  = mkPreludeTyConUnique 174
typeNatLogTyFamNameKey  = mkPreludeTyConUnique 175

-- Custom user type-errors
errorMessageTypeErrorFamKey :: Unique
errorMessageTypeErrorFamKey =  mkPreludeTyConUnique 176



ntTyConKey:: Unique
ntTyConKey = mkPreludeTyConUnique 177
coercibleTyConKey :: Unique
coercibleTyConKey = mkPreludeTyConUnique 178

proxyPrimTyConKey :: Unique
proxyPrimTyConKey = mkPreludeTyConUnique 179

specTyConKey :: Unique
specTyConKey = mkPreludeTyConUnique 180

anyTyConKey :: Unique
anyTyConKey = mkPreludeTyConUnique 181

smallArrayPrimTyConKey        = mkPreludeTyConUnique  182
smallMutableArrayPrimTyConKey = mkPreludeTyConUnique  183

staticPtrTyConKey  :: Unique
staticPtrTyConKey  = mkPreludeTyConUnique 184

staticPtrInfoTyConKey :: Unique
staticPtrInfoTyConKey = mkPreludeTyConUnique 185

callStackTyConKey :: Unique
callStackTyConKey = mkPreludeTyConUnique 186

-- Typeables
typeRepTyConKey, someTypeRepTyConKey, someTypeRepDataConKey :: Unique
typeRepTyConKey       = mkPreludeTyConUnique 187
someTypeRepTyConKey   = mkPreludeTyConUnique 188
someTypeRepDataConKey = mkPreludeTyConUnique 189


typeSymbolAppendFamNameKey :: Unique
typeSymbolAppendFamNameKey = mkPreludeTyConUnique 190

-- Unsafe equality
unsafeEqualityTyConKey :: Unique
unsafeEqualityTyConKey = mkPreludeTyConUnique 191


---------------- Template Haskell -------------------
--      THNames.hs: USES TyConUniques 200-299
-----------------------------------------------------

----------------------- SIMD ------------------------
--      USES TyConUniques 300-399
-----------------------------------------------------

#include "primop-vector-uniques.hs-incl"

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
*                                                                      *
************************************************************************
-}

charDataConKey, consDataConKey, doubleDataConKey, falseDataConKey,
    floatDataConKey, intDataConKey, integerSDataConKey, nilDataConKey,
    ratioDataConKey, stableNameDataConKey, trueDataConKey, wordDataConKey,
    word8DataConKey, ioDataConKey, integerDataConKey, heqDataConKey,
    coercibleDataConKey, eqDataConKey, nothingDataConKey, justDataConKey :: Unique

charDataConKey                          = mkPreludeDataConUnique  1
consDataConKey                          = mkPreludeDataConUnique  2
doubleDataConKey                        = mkPreludeDataConUnique  3
falseDataConKey                         = mkPreludeDataConUnique  4
floatDataConKey                         = mkPreludeDataConUnique  5
intDataConKey                           = mkPreludeDataConUnique  6
integerSDataConKey                      = mkPreludeDataConUnique  7
nothingDataConKey                       = mkPreludeDataConUnique  8
justDataConKey                          = mkPreludeDataConUnique  9
eqDataConKey                            = mkPreludeDataConUnique 10
nilDataConKey                           = mkPreludeDataConUnique 11
ratioDataConKey                         = mkPreludeDataConUnique 12
word8DataConKey                         = mkPreludeDataConUnique 13
stableNameDataConKey                    = mkPreludeDataConUnique 14
trueDataConKey                          = mkPreludeDataConUnique 15
wordDataConKey                          = mkPreludeDataConUnique 16
ioDataConKey                            = mkPreludeDataConUnique 17
integerDataConKey                       = mkPreludeDataConUnique 18
heqDataConKey                           = mkPreludeDataConUnique 19

-- Generic data constructors
crossDataConKey, inlDataConKey, inrDataConKey, genUnitDataConKey :: Unique
crossDataConKey                         = mkPreludeDataConUnique 20
inlDataConKey                           = mkPreludeDataConUnique 21
inrDataConKey                           = mkPreludeDataConUnique 22
genUnitDataConKey                       = mkPreludeDataConUnique 23

leftDataConKey, rightDataConKey :: Unique
leftDataConKey                          = mkPreludeDataConUnique 25
rightDataConKey                         = mkPreludeDataConUnique 26

ordLTDataConKey, ordEQDataConKey, ordGTDataConKey :: Unique
ordLTDataConKey                         = mkPreludeDataConUnique 27
ordEQDataConKey                         = mkPreludeDataConUnique 28
ordGTDataConKey                         = mkPreludeDataConUnique 29


coercibleDataConKey                     = mkPreludeDataConUnique 32

staticPtrDataConKey :: Unique
staticPtrDataConKey                     = mkPreludeDataConUnique 33

staticPtrInfoDataConKey :: Unique
staticPtrInfoDataConKey                 = mkPreludeDataConUnique 34

fingerprintDataConKey :: Unique
fingerprintDataConKey                   = mkPreludeDataConUnique 35

srcLocDataConKey :: Unique
srcLocDataConKey                        = mkPreludeDataConUnique 37

trTyConTyConKey, trTyConDataConKey,
  trModuleTyConKey, trModuleDataConKey,
  trNameTyConKey, trNameSDataConKey, trNameDDataConKey,
  trGhcPrimModuleKey, kindRepTyConKey,
  typeLitSortTyConKey :: Unique
trTyConTyConKey                         = mkPreludeDataConUnique 40
trTyConDataConKey                       = mkPreludeDataConUnique 41
trModuleTyConKey                        = mkPreludeDataConUnique 42
trModuleDataConKey                      = mkPreludeDataConUnique 43
trNameTyConKey                          = mkPreludeDataConUnique 44
trNameSDataConKey                       = mkPreludeDataConUnique 45
trNameDDataConKey                       = mkPreludeDataConUnique 46
trGhcPrimModuleKey                      = mkPreludeDataConUnique 47
kindRepTyConKey                         = mkPreludeDataConUnique 48
typeLitSortTyConKey                     = mkPreludeDataConUnique 49

typeErrorTextDataConKey,
  typeErrorAppendDataConKey,
  typeErrorVAppendDataConKey,
  typeErrorShowTypeDataConKey
  :: Unique
typeErrorTextDataConKey                 = mkPreludeDataConUnique 50
typeErrorAppendDataConKey               = mkPreludeDataConUnique 51
typeErrorVAppendDataConKey              = mkPreludeDataConUnique 52
typeErrorShowTypeDataConKey             = mkPreludeDataConUnique 53

prefixIDataConKey, infixIDataConKey, leftAssociativeDataConKey,
    rightAssociativeDataConKey, notAssociativeDataConKey,
    sourceUnpackDataConKey, sourceNoUnpackDataConKey,
    noSourceUnpackednessDataConKey, sourceLazyDataConKey,
    sourceStrictDataConKey, noSourceStrictnessDataConKey,
    decidedLazyDataConKey, decidedStrictDataConKey, decidedUnpackDataConKey,
    metaDataDataConKey, metaConsDataConKey, metaSelDataConKey :: Unique
prefixIDataConKey                       = mkPreludeDataConUnique 54
infixIDataConKey                        = mkPreludeDataConUnique 55
leftAssociativeDataConKey               = mkPreludeDataConUnique 56
rightAssociativeDataConKey              = mkPreludeDataConUnique 57
notAssociativeDataConKey                = mkPreludeDataConUnique 58
sourceUnpackDataConKey                  = mkPreludeDataConUnique 59
sourceNoUnpackDataConKey                = mkPreludeDataConUnique 60
noSourceUnpackednessDataConKey          = mkPreludeDataConUnique 61
sourceLazyDataConKey                    = mkPreludeDataConUnique 62
sourceStrictDataConKey                  = mkPreludeDataConUnique 63
noSourceStrictnessDataConKey            = mkPreludeDataConUnique 64
decidedLazyDataConKey                   = mkPreludeDataConUnique 65
decidedStrictDataConKey                 = mkPreludeDataConUnique 66
decidedUnpackDataConKey                 = mkPreludeDataConUnique 67
metaDataDataConKey                      = mkPreludeDataConUnique 68
metaConsDataConKey                      = mkPreludeDataConUnique 69
metaSelDataConKey                       = mkPreludeDataConUnique 70

vecRepDataConKey, tupleRepDataConKey, sumRepDataConKey :: Unique
vecRepDataConKey                        = mkPreludeDataConUnique 71
tupleRepDataConKey                      = mkPreludeDataConUnique 72
sumRepDataConKey                        = mkPreludeDataConUnique 73

-- See Note [Wiring in RuntimeRep] in TysWiredIn
runtimeRepSimpleDataConKeys, unliftedSimpleRepDataConKeys, unliftedRepDataConKeys :: [Unique]
liftedRepDataConKey :: Unique
runtimeRepSimpleDataConKeys@(liftedRepDataConKey : unliftedSimpleRepDataConKeys)
  = map mkPreludeDataConUnique [74..88]

unliftedRepDataConKeys = vecRepDataConKey :
                         tupleRepDataConKey :
                         sumRepDataConKey :
                         unliftedSimpleRepDataConKeys

-- See Note [Wiring in RuntimeRep] in TysWiredIn
-- VecCount
vecCountDataConKeys :: [Unique]
vecCountDataConKeys = map mkPreludeDataConUnique [89..94]

-- See Note [Wiring in RuntimeRep] in TysWiredIn
-- VecElem
vecElemDataConKeys :: [Unique]
vecElemDataConKeys = map mkPreludeDataConUnique [95..104]

-- Typeable things
kindRepTyConAppDataConKey, kindRepVarDataConKey, kindRepAppDataConKey,
    kindRepFunDataConKey, kindRepTYPEDataConKey,
    kindRepTypeLitSDataConKey, kindRepTypeLitDDataConKey
    :: Unique
kindRepTyConAppDataConKey = mkPreludeDataConUnique 105
kindRepVarDataConKey      = mkPreludeDataConUnique 106
kindRepAppDataConKey      = mkPreludeDataConUnique 107
kindRepFunDataConKey      = mkPreludeDataConUnique 108
kindRepTYPEDataConKey     = mkPreludeDataConUnique 109
kindRepTypeLitSDataConKey = mkPreludeDataConUnique 110
kindRepTypeLitDDataConKey = mkPreludeDataConUnique 111

typeLitSymbolDataConKey, typeLitNatDataConKey :: Unique
typeLitSymbolDataConKey   = mkPreludeDataConUnique 112
typeLitNatDataConKey      = mkPreludeDataConUnique 113

-- Unsafe equality
unsafeReflDataConKey :: Unique
unsafeReflDataConKey      = mkPreludeDataConUnique 114

---------------- Template Haskell -------------------
--      THNames.hs: USES DataUniques 200-250
-----------------------------------------------------


{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
*                                                                      *
************************************************************************
-}

wildCardKey, absentErrorIdKey, augmentIdKey, appendIdKey,
    elemIdKey,
    buildIdKey, errorIdKey, foldrIdKey, recSelErrorIdKey,
    seqIdKey, eqStringIdKey,
    noMethodBindingErrorIdKey, nonExhaustiveGuardsErrorIdKey,
    runtimeErrorIdKey, patErrorIdKey, voidPrimIdKey,
    realWorldPrimIdKey, recConErrorIdKey,
    unpackCStringUtf8IdKey, unpackCStringAppendIdKey,
    unpackCStringFoldrIdKey, unpackCStringFoldrUtf8IdKey,
    unpackCStringIdKey,
    typeErrorIdKey, divIntIdKey, modIntIdKey,
    absentSumFieldErrorIdKey :: Unique

wildCardKey                   = mkPreludeMiscIdUnique  0  -- See Note [WildCard binders]
absentErrorIdKey              = mkPreludeMiscIdUnique  1
augmentIdKey                  = mkPreludeMiscIdUnique  2
appendIdKey                   = mkPreludeMiscIdUnique  3
elemIdKey                     = mkPreludeMiscIdUnique  4
buildIdKey                    = mkPreludeMiscIdUnique  5
errorIdKey                    = mkPreludeMiscIdUnique  6
foldrIdKey                    = mkPreludeMiscIdUnique  7
recSelErrorIdKey              = mkPreludeMiscIdUnique  8
seqIdKey                      = mkPreludeMiscIdUnique  9
absentSumFieldErrorIdKey      = mkPreludeMiscIdUnique 10
eqStringIdKey                 = mkPreludeMiscIdUnique 11
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 12
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 13
runtimeErrorIdKey             = mkPreludeMiscIdUnique 14
patErrorIdKey                 = mkPreludeMiscIdUnique 15
realWorldPrimIdKey            = mkPreludeMiscIdUnique 16
recConErrorIdKey              = mkPreludeMiscIdUnique 17
unpackCStringUtf8IdKey        = mkPreludeMiscIdUnique 18
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 19
unpackCStringFoldrIdKey       = mkPreludeMiscIdUnique 20
unpackCStringFoldrUtf8IdKey   = mkPreludeMiscIdUnique 21
unpackCStringIdKey            = mkPreludeMiscIdUnique 22
voidPrimIdKey                 = mkPreludeMiscIdUnique 23
typeErrorIdKey                = mkPreludeMiscIdUnique 24
divIntIdKey                   = mkPreludeMiscIdUnique 25
modIntIdKey                   = mkPreludeMiscIdUnique 26

concatIdKey, filterIdKey, zipIdKey,
    bindIOIdKey, returnIOIdKey, newStablePtrIdKey,
    printIdKey, failIOIdKey, nullAddrIdKey, voidArgIdKey,
    fstIdKey, sndIdKey, otherwiseIdKey, assertIdKey :: Unique
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

thenIOIdKey, lazyIdKey, assertErrorIdKey, oneShotKey, runRWKey :: Unique
thenIOIdKey                   = mkPreludeMiscIdUnique 103
lazyIdKey                     = mkPreludeMiscIdUnique 104
assertErrorIdKey              = mkPreludeMiscIdUnique 105
oneShotKey                    = mkPreludeMiscIdUnique 106
runRWKey                      = mkPreludeMiscIdUnique 107

traceKey :: Unique
traceKey                      = mkPreludeMiscIdUnique 108

breakpointIdKey, breakpointCondIdKey :: Unique
breakpointIdKey               = mkPreludeMiscIdUnique 110
breakpointCondIdKey           = mkPreludeMiscIdUnique 111

inlineIdKey, noinlineIdKey :: Unique
inlineIdKey                   = mkPreludeMiscIdUnique 120
-- see below

mapIdKey, groupWithIdKey, dollarIdKey :: Unique
mapIdKey              = mkPreludeMiscIdUnique 121
groupWithIdKey        = mkPreludeMiscIdUnique 122
dollarIdKey           = mkPreludeMiscIdUnique 123

coercionTokenIdKey :: Unique
coercionTokenIdKey    = mkPreludeMiscIdUnique 124

noinlineIdKey                 = mkPreludeMiscIdUnique 125

rationalToFloatIdKey, rationalToDoubleIdKey :: Unique
rationalToFloatIdKey   = mkPreludeMiscIdUnique 130
rationalToDoubleIdKey  = mkPreludeMiscIdUnique 131

magicDictKey :: Unique
magicDictKey                  = mkPreludeMiscIdUnique 156

coerceKey :: Unique
coerceKey                     = mkPreludeMiscIdUnique 157

{-
Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.
-}

-- Just a placeholder for unbound variables produced by the renamer:
unboundKey :: Unique
unboundKey                    = mkPreludeMiscIdUnique 158

fromIntegerClassOpKey, minusClassOpKey, fromRationalClassOpKey,
    enumFromClassOpKey, enumFromThenClassOpKey, enumFromToClassOpKey,
    enumFromThenToClassOpKey, eqClassOpKey, geClassOpKey, negateClassOpKey,
    bindMClassOpKey, thenMClassOpKey, returnMClassOpKey, fmapClassOpKey
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
bindMClassOpKey               = mkPreludeMiscIdUnique 171 -- (>>=)
thenMClassOpKey               = mkPreludeMiscIdUnique 172 -- (>>)
fmapClassOpKey                = mkPreludeMiscIdUnique 173
returnMClassOpKey             = mkPreludeMiscIdUnique 174

-- Recursive do notation
mfixIdKey :: Unique
mfixIdKey       = mkPreludeMiscIdUnique 175

-- MonadFail operations
failMClassOpKey :: Unique
failMClassOpKey = mkPreludeMiscIdUnique 176

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
--      THNames.hs: USES IdUniques 200-499
-----------------------------------------------------

-- Used to make `Typeable` dictionaries
mkTyConKey
  , mkTrTypeKey
  , mkTrConKey
  , mkTrAppKey
  , mkTrFunKey
  , typeNatTypeRepKey
  , typeSymbolTypeRepKey
  , typeRepIdKey
  :: Unique
mkTyConKey            = mkPreludeMiscIdUnique 503
mkTrTypeKey           = mkPreludeMiscIdUnique 504
mkTrConKey            = mkPreludeMiscIdUnique 505
mkTrAppKey            = mkPreludeMiscIdUnique 506
typeNatTypeRepKey     = mkPreludeMiscIdUnique 507
typeSymbolTypeRepKey  = mkPreludeMiscIdUnique 508
typeRepIdKey          = mkPreludeMiscIdUnique 509
mkTrFunKey            = mkPreludeMiscIdUnique 510

-- Representations for primitive types
trTYPEKey
  ,trTYPE'PtrRepLiftedKey
  , trRuntimeRepKey
  , tr'PtrRepLiftedKey
  :: Unique
trTYPEKey              = mkPreludeMiscIdUnique 511
trTYPE'PtrRepLiftedKey = mkPreludeMiscIdUnique 512
trRuntimeRepKey        = mkPreludeMiscIdUnique 513
tr'PtrRepLiftedKey     = mkPreludeMiscIdUnique 514

-- KindReps for common cases
starKindRepKey, starArrStarKindRepKey, starArrStarArrStarKindRepKey :: Unique
starKindRepKey        = mkPreludeMiscIdUnique 520
starArrStarKindRepKey = mkPreludeMiscIdUnique 521
starArrStarArrStarKindRepKey = mkPreludeMiscIdUnique 522

-- Dynamic
toDynIdKey :: Unique
toDynIdKey            = mkPreludeMiscIdUnique 523


bitIntegerIdKey :: Unique
bitIntegerIdKey       = mkPreludeMiscIdUnique 550

heqSCSelIdKey, eqSCSelIdKey, coercibleSCSelIdKey :: Unique
eqSCSelIdKey        = mkPreludeMiscIdUnique 551
heqSCSelIdKey       = mkPreludeMiscIdUnique 552
coercibleSCSelIdKey = mkPreludeMiscIdUnique 553

sappendClassOpKey :: Unique
sappendClassOpKey = mkPreludeMiscIdUnique 554

memptyClassOpKey, mappendClassOpKey, mconcatClassOpKey :: Unique
memptyClassOpKey  = mkPreludeMiscIdUnique 555
mappendClassOpKey = mkPreludeMiscIdUnique 556
mconcatClassOpKey = mkPreludeMiscIdUnique 557

emptyCallStackKey, pushCallStackKey :: Unique
emptyCallStackKey = mkPreludeMiscIdUnique 558
pushCallStackKey  = mkPreludeMiscIdUnique 559

fromStaticPtrClassOpKey :: Unique
fromStaticPtrClassOpKey = mkPreludeMiscIdUnique 560

makeStaticKey :: Unique
makeStaticKey = mkPreludeMiscIdUnique 561

-- Natural
naturalFromIntegerIdKey, naturalToIntegerIdKey, plusNaturalIdKey,
   minusNaturalIdKey, timesNaturalIdKey, mkNaturalIdKey,
   naturalSDataConKey, wordToNaturalIdKey :: Unique
naturalFromIntegerIdKey = mkPreludeMiscIdUnique 562
naturalToIntegerIdKey   = mkPreludeMiscIdUnique 563
plusNaturalIdKey        = mkPreludeMiscIdUnique 564
minusNaturalIdKey       = mkPreludeMiscIdUnique 565
timesNaturalIdKey       = mkPreludeMiscIdUnique 566
mkNaturalIdKey          = mkPreludeMiscIdUnique 567
naturalSDataConKey      = mkPreludeMiscIdUnique 568
wordToNaturalIdKey      = mkPreludeMiscIdUnique 569

-- Unsafe coercion proofs
unsafeEqualityProofIdKey, unsafeCoercePrimIdKey, unsafeCoerceIdKey :: Unique
unsafeEqualityProofIdKey = mkPreludeMiscIdUnique 570
unsafeCoercePrimIdKey    = mkPreludeMiscIdUnique 571
unsafeCoerceIdKey        = mkPreludeMiscIdUnique 572

{-
************************************************************************
*                                                                      *
\subsection[Class-std-groups]{Standard groups of Prelude classes}
*                                                                      *
************************************************************************

NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.
-}

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
                      monadClassKey, monadPlusClassKey, monadFailClassKey,
                      semigroupClassKey, monoidClassKey,
                      isStringClassKey,
                      applicativeClassKey, foldableClassKey,
                      traversableClassKey, alternativeClassKey
                     ]

{-
@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).
-}

derivableClassKeys :: [Unique]
derivableClassKeys
  = [ eqClassKey, ordClassKey, enumClassKey, ixClassKey,
      boundedClassKey, showClassKey, readClassKey ]


-- These are the "interactive classes" that are consulted when doing
-- defaulting. Does not include Num or IsString, which have special
-- handling.
interactiveClassNames :: [Name]
interactiveClassNames
  = [ showClassName, eqClassName, ordClassName, foldableClassName
    , traversableClassName ]

interactiveClassKeys :: [Unique]
interactiveClassKeys = map getUnique interactiveClassNames

{-
************************************************************************
*                                                                      *
   Semi-builtin names
*                                                                      *
************************************************************************

The following names should be considered by GHCi to be in scope always.

-}

pretendNameIsInScope :: Name -> Bool
pretendNameIsInScope n
  = any (n `hasKey`)
    [ liftedTypeKindTyConKey, tYPETyConKey
    , runtimeRepTyConKey, liftedRepDataConKey ]

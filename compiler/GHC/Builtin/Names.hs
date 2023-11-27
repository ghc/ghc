{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[GHC.Builtin.Names]{Definitions of prelude modules and names}


Nota Bene: all Names defined in here should come from the base package,
the big-num package or (for plugins) the ghc package.

 - ModuleNames for prelude modules,
        e.g.    pRELUDE_NAME :: ModuleName

 - Modules for prelude modules
        e.g.    pRELUDE :: Module

 - Uniques for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    orderingTyConKey :: Unique
                minusClassOpKey :: Unique

 - Names for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    orderingTyConName :: Name
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
   generated code (e.g. for deriving).
        e.g.    and_RDR :: RdrName
   It's not necessary to know the uniques for these guys, only their names


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
     RdrNames which are Exact. These are wired-in RdrNames where
     we could directly tell from the parsed syntax what Name to
     use. For example, when we parse a [] in a type and ListTuplePuns
     are enabled, we can just insert (Exact listTyConName :: RdrName).

     This is just an optimisation: it would be equally valid to output
     Orig RdrNames that correctly record the module (and package) that
     we expect the final Name to come from. The name would be looked up
     in the OrigNameCache (see point 3).

  2. The knownKeyNames (which consist of the basicKnownKeyNames from
     the module, and those names reachable via the wired-in stuff from
     GHC.Builtin.Types) are used to initialise the "OrigNameCache" in
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
     used to initialise the OrigNameCache. Instead, lookupOrigNameCache pretends
     that these names are in the cache by using isInfiniteFamilyOrigName_maybe
     before the actual lookup.
     See Note [Infinite families of known-key names] for details.


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
     back to ensure that we get back to the correct uniques.
     See Note [Symbol table representation of names] in GHC.Iface.Binary and
     Note [How tuples work] in GHC.Builtin.Types.

  c) GHC.Iface.Env.lookupOrigNameCache uses isInfiniteFamilyOrigName_maybe to
     map tuples and sums onto their exact names, rather than trying to find them
     in the original-name cache.
     See also Note [Built-in syntax and the OrigNameCache]

-}

{-# LANGUAGE CPP #-}

module GHC.Builtin.Names
   ( Unique, Uniquable(..), hasKey,  -- Re-exported for convenience

   -----------------------------------------------------------
   module GHC.Builtin.Names, -- A huge bunch of (a) Names,  e.g. intTyConName
                             --                 (b) Uniques e.g. intTyConKey
                             --                 (c) Groups of classes and types
                             --                 (d) miscellaneous things
                             -- So many that we export them all
   )
where

import GHC.Prelude

import GHC.Unit.Types
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.Unique
import GHC.Builtin.Uniques
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Data.FastString
import GHC.Data.List.Infinite (Infinite (..))
import qualified GHC.Data.List.Infinite as Inf

import Language.Haskell.Syntax.Module.Name

{-
************************************************************************
*                                                                      *
     allNameStrings
*                                                                      *
************************************************************************
-}

allNameStrings :: Infinite String
-- Infinite list of a,b,c...z, aa, ab, ac, ... etc
allNameStrings = Inf.allListsOf ['a'..'z']

allNameStringList :: [String]
-- Infinite list of a,b,c...z, aa, ab, ac, ... etc
allNameStringList = Inf.toList allNameStrings

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
wired in ones are defined in GHC.Builtin.Types etc.
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
        typeLitCharDataConName,
        typeRepIdName,
        mkTrTypeName,
        mkTrConName,
        mkTrAppName,
        mkTrFunName,
        typeSymbolTypeRepName, typeNatTypeRepName, typeCharTypeRepName,
        trGhcPrimModuleName,

        -- KindReps for common cases
        starKindRepName,
        starArrStarKindRepName,
        starArrStarArrStarKindRepName,
        constraintKindRepName,

        -- WithDict
        withDictClassName,

        -- TagToEnum
        tagToEnumClassName,

        -- DataToTag
        dataToTagClassName,

        -- seq#
        seqHashName,

        -- Dynamic
        toDynName,

        -- Numeric stuff
        negateName, minusName, geName, eqName,
        mkRationalBase2Name, mkRationalBase10Name,

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
        considerAccessibleName,

        -- Strings and lists
        unpackCStringName, unpackCStringUtf8Name,
        unpackCStringAppendName, unpackCStringAppendUtf8Name,
        unpackCStringFoldrName, unpackCStringFoldrUtf8Name,
        cstringLengthName,

        -- Overloaded lists
        isListClassName,
        fromListName,
        fromListNName,
        toListName,

        -- Non-empty lists
        nonEmptyTyConName,

        -- Overloaded record dot, record update
        getFieldName, setFieldName,

        -- List operations
        concatName, filterName, mapName,
        zipName, foldrName, buildName, augmentName, appendName,

        -- FFI primitive types that are not wired-in.
        stablePtrTyConName, ptrTyConName, funPtrTyConName, constPtrConName,
        int8TyConName, int16TyConName, int32TyConName, int64TyConName,
        word8TyConName, word16TyConName, word32TyConName, word64TyConName,
        jsvalTyConName,

        -- Others
        otherwiseIdName, inlineIdName,
        eqStringName, assertName,
        assertErrorName, traceName,
        printName,
        dollarName,

        -- ghc-bignum
        integerFromNaturalName,
        integerToNaturalClampName,
        integerToNaturalThrowName,
        integerToNaturalName,
        integerToWordName,
        integerToIntName,
        integerToWord64Name,
        integerToInt64Name,
        integerFromWordName,
        integerFromWord64Name,
        integerFromInt64Name,
        integerAddName,
        integerMulName,
        integerSubName,
        integerNegateName,
        integerAbsName,
        integerPopCountName,
        integerQuotName,
        integerRemName,
        integerDivName,
        integerModName,
        integerDivModName,
        integerQuotRemName,
        integerEncodeFloatName,
        integerEncodeDoubleName,
        integerGcdName,
        integerLcmName,
        integerAndName,
        integerOrName,
        integerXorName,
        integerComplementName,
        integerBitName,
        integerTestBitName,
        integerShiftLName,
        integerShiftRName,

        naturalToWordName,
        naturalPopCountName,
        naturalShiftRName,
        naturalShiftLName,
        naturalAddName,
        naturalSubName,
        naturalSubThrowName,
        naturalSubUnsafeName,
        naturalMulName,
        naturalQuotRemName,
        naturalQuotName,
        naturalRemName,
        naturalAndName,
        naturalAndNotName,
        naturalOrName,
        naturalXorName,
        naturalTestBitName,
        naturalBitName,
        naturalGcdName,
        naturalLcmName,
        naturalLog2Name,
        naturalLogBaseWordName,
        naturalLogBaseName,
        naturalPowModName,
        naturalSizeInBaseName,

        bignatEqName,

        -- Float/Double
        integerToFloatName,
        integerToDoubleName,
        naturalToFloatName,
        naturalToDoubleName,
        rationalToFloatName,
        rationalToDoubleName,

        -- Other classes
        monadPlusClassName,

        -- Type-level naturals
        knownNatClassName, knownSymbolClassName, knownCharClassName,

        -- Overloaded labels
        fromLabelClassOpName,

        -- Implicit Parameters
        ipClassName,

        -- Overloaded record fields
        hasFieldClassName,

        -- ExceptionContext
        exceptionContextTyConName,
        emptyExceptionContextName,

        -- Call Stacks
        callStackTyConName,
        emptyCallStackName, pushCallStackName,

        -- Source Locations
        srcLocDataConName,

        -- Annotation type checking
        toAnnotationWrapperName

        -- The SPEC type for SpecConstr
        , specTyConName

        -- The Either type
        , eitherTyConName, leftDataConName, rightDataConName

        -- The Void type
        , voidTyConName

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

        -- "Unsatisfiable" constraint
        , unsatisfiableClassName
        , unsatisfiableIdName

        -- Unsafe coercion proofs
        , unsafeEqualityProofName
        , unsafeEqualityTyConName
        , unsafeReflDataConName
        , unsafeCoercePrimName

        , unsafeUnpackJSStringUtf8ShShName
    ]

genericTyConNames :: [Name]
genericTyConNames = [
    v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
    k1TyConName, m1TyConName, sumTyConName, prodTyConName,
    compTyConName, rTyConName, dTyConName,
    cTyConName, sTyConName, rec0TyConName,
    d1TyConName, c1TyConName, s1TyConName,
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

gHC_PRIM, gHC_PRIM_PANIC,
    gHC_TYPES, gHC_INTERNAL_DATA_DATA,
    gHC_MAGIC, gHC_MAGIC_DICT, gHC_MAGIC_TAGTOENUM,
    gHC_CLASSES, gHC_PRIMOPWRAPPERS :: Module
gHC_PRIM           = mkGhcInternalModule (fsLit "GHC.Internal.Prim")   -- Primitive types and values
gHC_PRIM_PANIC     = mkGhcInternalModule (fsLit "GHC.Internal.Prim.Panic")
gHC_TYPES          = mkGhcInternalModule (fsLit "GHC.Internal.Types")
gHC_MAGIC          = mkGhcInternalModule (fsLit "GHC.Internal.Magic")
gHC_MAGIC_DICT     = mkGhcInternalModule (fsLit "GHC.Internal.Magic.Dict")
gHC_MAGIC_TAGTOENUM = mkGhcInternalModule (fsLit "GHC.Internal.Magic.TagToEnum")
gHC_CSTRING        = mkGhcInternalModule (fsLit "GHC.Internal.CString")
gHC_CLASSES        = mkGhcInternalModule (fsLit "GHC.Internal.Classes")
gHC_PRIMOPWRAPPERS = mkGhcInternalModule (fsLit "GHC.Internal.PrimopWrappers")
gHC_INTERNAL_TUPLE = mkGhcInternalModule (fsLit "GHC.Internal.Tuple")

gHC_INTERNAL_CONTROL_MONAD_ZIP :: Module
gHC_INTERNAL_CONTROL_MONAD_ZIP  = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad.Zip")

gHC_INTERNAL_NUM_INTEGER, gHC_INTERNAL_NUM_NATURAL, gHC_INTERNAL_NUM_BIGNAT :: Module
gHC_INTERNAL_NUM_INTEGER            = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.Integer")
gHC_INTERNAL_NUM_NATURAL            = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.Natural")
gHC_INTERNAL_NUM_BIGNAT             = mkGhcInternalModule (fsLit "GHC.Internal.Bignum.BigNat")

gHC_INTERNAL_BASE, gHC_INTERNAL_ENUM,
    gHC_INTERNAL_GHCI, gHC_INTERNAL_GHCI_HELPERS, gHC_CSTRING, gHC_INTERNAL_DATA_STRING,
    gHC_INTERNAL_SHOW, gHC_INTERNAL_READ, gHC_INTERNAL_NUM, gHC_INTERNAL_MAYBE,
    gHC_INTERNAL_LIST, gHC_INTERNAL_TUPLE, gHC_INTERNAL_DATA_EITHER,
    gHC_INTERNAL_DATA_FOLDABLE, gHC_INTERNAL_DATA_TRAVERSABLE,
    gHC_INTERNAL_EXCEPTION_CONTEXT,
    gHC_INTERNAL_CONC, gHC_INTERNAL_IO, gHC_INTERNAL_IO_Exception,
    gHC_INTERNAL_ST, gHC_INTERNAL_IX, gHC_INTERNAL_STABLE, gHC_INTERNAL_PTR, gHC_INTERNAL_ERR, gHC_INTERNAL_REAL,
    gHC_INTERNAL_FLOAT, gHC_INTERNAL_TOP_HANDLER, gHC_INTERNAL_SYSTEM_IO, gHC_INTERNAL_DYNAMIC,
    gHC_INTERNAL_TYPEABLE, gHC_INTERNAL_TYPEABLE_INTERNAL, gHC_INTERNAL_GENERICS,
    gHC_INTERNAL_READ_PREC, gHC_INTERNAL_LEX, gHC_INTERNAL_INT, gHC_INTERNAL_WORD, gHC_INTERNAL_MONAD, gHC_INTERNAL_MONAD_FIX,  gHC_INTERNAL_MONAD_FAIL,
    gHC_INTERNAL_ARROW, gHC_INTERNAL_DESUGAR, gHC_INTERNAL_RANDOM, gHC_INTERNAL_EXTS, gHC_INTERNAL_IS_LIST,
    gHC_INTERNAL_CONTROL_EXCEPTION_BASE, gHC_INTERNAL_TYPEERROR, gHC_INTERNAL_TYPELITS, gHC_INTERNAL_TYPELITS_INTERNAL,
    gHC_INTERNAL_TYPENATS, gHC_INTERNAL_TYPENATS_INTERNAL,
    gHC_INTERNAL_DATA_COERCE, gHC_INTERNAL_DEBUG_TRACE, gHC_INTERNAL_UNSAFE_COERCE, gHC_INTERNAL_FOREIGN_C_CONSTPTR,
    gHC_INTERNAL_JS_PRIM, gHC_INTERNAL_WASM_PRIM_TYPES :: Module
gHC_INTERNAL_BASE                   = mkGhcInternalModule (fsLit "GHC.Internal.Base")
gHC_INTERNAL_ENUM                   = mkGhcInternalModule (fsLit "GHC.Internal.Enum")
gHC_INTERNAL_GHCI                   = mkGhcInternalModule (fsLit "GHC.Internal.GHCi")
gHC_INTERNAL_GHCI_HELPERS           = mkGhcInternalModule (fsLit "GHC.Internal.GHCi.Helpers")
gHC_INTERNAL_SHOW                   = mkGhcInternalModule (fsLit "GHC.Internal.Show")
gHC_INTERNAL_READ                   = mkGhcInternalModule (fsLit "GHC.Internal.Read")
gHC_INTERNAL_NUM                    = mkGhcInternalModule (fsLit "GHC.Internal.Num")
gHC_INTERNAL_MAYBE                  = mkGhcInternalModule (fsLit "GHC.Internal.Maybe")
gHC_INTERNAL_LIST                   = mkGhcInternalModule (fsLit "GHC.Internal.List")
gHC_INTERNAL_DATA_EITHER            = mkGhcInternalModule (fsLit "GHC.Internal.Data.Either")
gHC_INTERNAL_DATA_STRING            = mkGhcInternalModule (fsLit "GHC.Internal.Data.String")
gHC_INTERNAL_DATA_FOLDABLE          = mkGhcInternalModule (fsLit "GHC.Internal.Data.Foldable")
gHC_INTERNAL_DATA_TRAVERSABLE       = mkGhcInternalModule (fsLit "GHC.Internal.Data.Traversable")
gHC_INTERNAL_CONC                   = mkGhcInternalModule (fsLit "GHC.Internal.GHC.Conc")
gHC_INTERNAL_IO                     = mkGhcInternalModule (fsLit "GHC.Internal.IO")
gHC_INTERNAL_IO_Exception           = mkGhcInternalModule (fsLit "GHC.Internal.IO.Exception")
gHC_INTERNAL_ST                     = mkGhcInternalModule (fsLit "GHC.Internal.ST")
gHC_INTERNAL_IX                     = mkGhcInternalModule (fsLit "GHC.Internal.Ix")
gHC_INTERNAL_STABLE                 = mkGhcInternalModule (fsLit "GHC.Internal.Stable")
gHC_INTERNAL_PTR                    = mkGhcInternalModule (fsLit "GHC.Internal.Ptr")
gHC_INTERNAL_ERR                    = mkGhcInternalModule (fsLit "GHC.Internal.Err")
gHC_INTERNAL_REAL                   = mkGhcInternalModule (fsLit "GHC.Internal.Real")
gHC_INTERNAL_FLOAT                  = mkGhcInternalModule (fsLit "GHC.Internal.Float")
gHC_INTERNAL_TOP_HANDLER            = mkGhcInternalModule (fsLit "GHC.Internal.TopHandler")
gHC_INTERNAL_SYSTEM_IO              = mkGhcInternalModule (fsLit "GHC.Internal.System.IO")
gHC_INTERNAL_DYNAMIC                = mkGhcInternalModule (fsLit "GHC.Internal.Data.Dynamic")
gHC_INTERNAL_TYPEABLE               = mkGhcInternalModule (fsLit "GHC.Internal.Data.Typeable")
gHC_INTERNAL_TYPEABLE_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.Data.Typeable.Internal")
gHC_INTERNAL_DATA_DATA              = mkGhcInternalModule (fsLit "GHC.Internal.Data.Data")
gHC_INTERNAL_READ_PREC              = mkGhcInternalModule (fsLit "GHC.Internal.Text.ParserCombinators.ReadPrec")
gHC_INTERNAL_LEX                    = mkGhcInternalModule (fsLit "GHC.Internal.Text.Read.Lex")
gHC_INTERNAL_INT                    = mkGhcInternalModule (fsLit "GHC.Internal.Int")
gHC_INTERNAL_WORD                   = mkGhcInternalModule (fsLit "GHC.Internal.Word")
gHC_INTERNAL_MONAD                  = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad")
gHC_INTERNAL_MONAD_FIX              = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad.Fix")
gHC_INTERNAL_MONAD_FAIL             = mkGhcInternalModule (fsLit "GHC.Internal.Control.Monad.Fail")
gHC_INTERNAL_ARROW                  = mkGhcInternalModule (fsLit "GHC.Internal.Control.Arrow")
gHC_INTERNAL_DESUGAR                = mkGhcInternalModule (fsLit "GHC.Internal.Desugar")
gHC_INTERNAL_RANDOM                 = mkGhcInternalModule (fsLit "GHC.Internal.System.Random")
gHC_INTERNAL_EXTS                   = mkGhcInternalModule (fsLit "GHC.Internal.Exts")
gHC_INTERNAL_IS_LIST                = mkGhcInternalModule (fsLit "GHC.Internal.IsList")
gHC_INTERNAL_CONTROL_EXCEPTION_BASE = mkGhcInternalModule (fsLit "GHC.Internal.Control.Exception.Base")
gHC_INTERNAL_EXCEPTION_CONTEXT      = mkGhcInternalModule (fsLit "GHC.Internal.Exception.Context")
gHC_INTERNAL_GENERICS               = mkGhcInternalModule (fsLit "GHC.Internal.Generics")
gHC_INTERNAL_TYPEERROR              = mkGhcInternalModule (fsLit "GHC.Internal.TypeError")
gHC_INTERNAL_TYPELITS               = mkGhcInternalModule (fsLit "GHC.Internal.TypeLits")
gHC_INTERNAL_TYPELITS_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.TypeLits.Internal")
gHC_INTERNAL_TYPENATS               = mkGhcInternalModule (fsLit "GHC.Internal.TypeNats")
gHC_INTERNAL_TYPENATS_INTERNAL      = mkGhcInternalModule (fsLit "GHC.Internal.TypeNats.Internal")
gHC_INTERNAL_DATA_COERCE            = mkGhcInternalModule (fsLit "GHC.Internal.Data.Coerce")
gHC_INTERNAL_DEBUG_TRACE            = mkGhcInternalModule (fsLit "GHC.Internal.Debug.Trace")
gHC_INTERNAL_UNSAFE_COERCE          = mkGhcInternalModule (fsLit "GHC.Internal.Unsafe.Coerce")
gHC_INTERNAL_FOREIGN_C_CONSTPTR     = mkGhcInternalModule (fsLit "GHC.Internal.Foreign.C.ConstPtr")
gHC_INTERNAL_JS_PRIM                = mkGhcInternalModule (fsLit "GHC.Internal.JS.Prim")
gHC_INTERNAL_WASM_PRIM_TYPES        = mkGhcInternalModule (fsLit "GHC.Internal.Wasm.Prim.Types")

gHC_INTERNAL_SRCLOC :: Module
gHC_INTERNAL_SRCLOC = mkGhcInternalModule (fsLit "GHC.Internal.SrcLoc")

gHC_INTERNAL_STACK, gHC_INTERNAL_STACK_TYPES :: Module
gHC_INTERNAL_STACK = mkGhcInternalModule (fsLit "GHC.Internal.Stack")
gHC_INTERNAL_STACK_TYPES = mkGhcInternalModule (fsLit "GHC.Internal.Stack.Types")

gHC_INTERNAL_STATICPTR :: Module
gHC_INTERNAL_STATICPTR = mkGhcInternalModule (fsLit "GHC.Internal.StaticPtr")

gHC_INTERNAL_STATICPTR_INTERNAL :: Module
gHC_INTERNAL_STATICPTR_INTERNAL = mkGhcInternalModule (fsLit "GHC.Internal.StaticPtr.Internal")

gHC_INTERNAL_FINGERPRINT_TYPE :: Module
gHC_INTERNAL_FINGERPRINT_TYPE = mkGhcInternalModule (fsLit "GHC.Internal.Fingerprint.Type")

gHC_INTERNAL_OVER_LABELS :: Module
gHC_INTERNAL_OVER_LABELS = mkGhcInternalModule (fsLit "GHC.Internal.OverloadedLabels")

gHC_INTERNAL_RECORDS :: Module
gHC_INTERNAL_RECORDS = mkGhcInternalModule (fsLit "GHC.Internal.Records")

rOOT_MAIN :: Module
rOOT_MAIN       = mkMainModule (fsLit ":Main") -- Root module for initialisation

mkInteractiveModule :: String -> Module
-- (mkInteractiveMoudule "9") makes module 'interactive:Ghci9'
mkInteractiveModule n = mkModule interactiveUnit (mkModuleName ("Ghci" ++ n))

pRELUDE_NAME, mAIN_NAME :: ModuleName
pRELUDE_NAME   = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME      = mkModuleNameFS (fsLit "Main")

mkGhcInternalModule :: FastString -> Module
mkGhcInternalModule m = mkGhcInternalModule_ (mkModuleNameFS m)

mkGhcInternalModule_ :: ModuleName -> Module
mkGhcInternalModule_ m = mkModule ghcInternalUnit m

mkThisGhcModule :: FastString -> Module
mkThisGhcModule m = mkThisGhcModule_ (mkModuleNameFS m)

mkThisGhcModule_ :: ModuleName -> Module
mkThisGhcModule_ m = mkModule thisGhcUnit m

mkMainModule :: FastString -> Module
mkMainModule m = mkModule mainUnit (mkModuleNameFS m)

mkMainModule_ :: ModuleName -> Module
mkMainModule_ m = mkModule mainUnit m

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

map_RDR, append_RDR :: RdrName
map_RDR                 = nameRdrName mapName
append_RDR              = nameRdrName appendName

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

fromEnum_RDR, toEnum_RDR, toEnumError_RDR, succError_RDR, predError_RDR, enumIntToWord_RDR :: RdrName
fromEnum_RDR            = varQual_RDR gHC_INTERNAL_ENUM (fsLit "fromEnum")
toEnum_RDR              = varQual_RDR gHC_INTERNAL_ENUM (fsLit "toEnum")
toEnumError_RDR         = varQual_RDR gHC_INTERNAL_ENUM (fsLit "toEnumError")
succError_RDR           = varQual_RDR gHC_INTERNAL_ENUM (fsLit "succError")
predError_RDR           = varQual_RDR gHC_INTERNAL_ENUM (fsLit "predError")
enumIntToWord_RDR       = varQual_RDR gHC_INTERNAL_ENUM (fsLit "enumIntToWord")

enumFrom_RDR, enumFromTo_RDR, enumFromThen_RDR, enumFromThenTo_RDR :: RdrName
enumFrom_RDR            = nameRdrName enumFromName
enumFromTo_RDR          = nameRdrName enumFromToName
enumFromThen_RDR        = nameRdrName enumFromThenName
enumFromThenTo_RDR      = nameRdrName enumFromThenToName

times_RDR, plus_RDR :: RdrName
times_RDR               = varQual_RDR  gHC_INTERNAL_NUM (fsLit "*")
plus_RDR                = varQual_RDR gHC_INTERNAL_NUM (fsLit "+")

compose_RDR :: RdrName
compose_RDR             = varQual_RDR gHC_INTERNAL_BASE (fsLit ".")

not_RDR, tagToEnum_RDR, dataToTag_RDR,
    succ_RDR, pred_RDR, minBound_RDR, maxBound_RDR,
    and_RDR, range_RDR, inRange_RDR, index_RDR,
    unsafeIndex_RDR, unsafeRangeSize_RDR :: RdrName
and_RDR                 = varQual_RDR gHC_CLASSES (fsLit "&&")
not_RDR                 = varQual_RDR gHC_CLASSES (fsLit "not")
tagToEnum_RDR           = varQual_RDR gHC_MAGIC_TAGTOENUM (fsLit "tagToEnum#")
dataToTag_RDR           = varQual_RDR gHC_MAGIC (fsLit "dataToTag#")
succ_RDR                = varQual_RDR gHC_INTERNAL_ENUM (fsLit "succ")
pred_RDR                = varQual_RDR gHC_INTERNAL_ENUM (fsLit "pred")
minBound_RDR            = varQual_RDR gHC_INTERNAL_ENUM (fsLit "minBound")
maxBound_RDR            = varQual_RDR gHC_INTERNAL_ENUM (fsLit "maxBound")
range_RDR               = varQual_RDR gHC_INTERNAL_IX (fsLit "range")
inRange_RDR             = varQual_RDR gHC_INTERNAL_IX (fsLit "inRange")
index_RDR               = varQual_RDR gHC_INTERNAL_IX (fsLit "index")
unsafeIndex_RDR         = varQual_RDR gHC_INTERNAL_IX (fsLit "unsafeIndex")
unsafeRangeSize_RDR     = varQual_RDR gHC_INTERNAL_IX (fsLit "unsafeRangeSize")

readList_RDR, readListDefault_RDR, readListPrec_RDR, readListPrecDefault_RDR,
    readPrec_RDR, parens_RDR, choose_RDR, lexP_RDR, expectP_RDR :: RdrName
readList_RDR            = varQual_RDR gHC_INTERNAL_READ (fsLit "readList")
readListDefault_RDR     = varQual_RDR gHC_INTERNAL_READ (fsLit "readListDefault")
readListPrec_RDR        = varQual_RDR gHC_INTERNAL_READ (fsLit "readListPrec")
readListPrecDefault_RDR = varQual_RDR gHC_INTERNAL_READ (fsLit "readListPrecDefault")
readPrec_RDR            = varQual_RDR gHC_INTERNAL_READ (fsLit "readPrec")
parens_RDR              = varQual_RDR gHC_INTERNAL_READ (fsLit "parens")
choose_RDR              = varQual_RDR gHC_INTERNAL_READ (fsLit "choose")
lexP_RDR                = varQual_RDR gHC_INTERNAL_READ (fsLit "lexP")
expectP_RDR             = varQual_RDR gHC_INTERNAL_READ (fsLit "expectP")

readField_RDR, readFieldHash_RDR, readSymField_RDR :: RdrName
readField_RDR           = varQual_RDR gHC_INTERNAL_READ (fsLit "readField")
readFieldHash_RDR       = varQual_RDR gHC_INTERNAL_READ (fsLit "readFieldHash")
readSymField_RDR        = varQual_RDR gHC_INTERNAL_READ (fsLit "readSymField")

punc_RDR, ident_RDR, symbol_RDR :: RdrName
punc_RDR                = dataQual_RDR gHC_INTERNAL_LEX (fsLit "Punc")
ident_RDR               = dataQual_RDR gHC_INTERNAL_LEX (fsLit "Ident")
symbol_RDR              = dataQual_RDR gHC_INTERNAL_LEX (fsLit "Symbol")

step_RDR, alt_RDR, reset_RDR, prec_RDR, pfail_RDR :: RdrName
step_RDR                = varQual_RDR  gHC_INTERNAL_READ_PREC (fsLit "step")
alt_RDR                 = varQual_RDR  gHC_INTERNAL_READ_PREC (fsLit "+++")
reset_RDR               = varQual_RDR  gHC_INTERNAL_READ_PREC (fsLit "reset")
prec_RDR                = varQual_RDR  gHC_INTERNAL_READ_PREC (fsLit "prec")
pfail_RDR               = varQual_RDR  gHC_INTERNAL_READ_PREC (fsLit "pfail")

showsPrec_RDR, shows_RDR, showString_RDR,
    showSpace_RDR, showCommaSpace_RDR, showParen_RDR :: RdrName
showsPrec_RDR           = varQual_RDR gHC_INTERNAL_SHOW (fsLit "showsPrec")
shows_RDR               = varQual_RDR gHC_INTERNAL_SHOW (fsLit "shows")
showString_RDR          = varQual_RDR gHC_INTERNAL_SHOW (fsLit "showString")
showSpace_RDR           = varQual_RDR gHC_INTERNAL_SHOW (fsLit "showSpace")
showCommaSpace_RDR      = varQual_RDR gHC_INTERNAL_SHOW (fsLit "showCommaSpace")
showParen_RDR           = varQual_RDR gHC_INTERNAL_SHOW (fsLit "showParen")

error_RDR :: RdrName
error_RDR = varQual_RDR gHC_INTERNAL_ERR (fsLit "error")

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

u1DataCon_RDR    = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "U1")
par1DataCon_RDR  = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "Par1")
rec1DataCon_RDR  = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "Rec1")
k1DataCon_RDR    = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "K1")
m1DataCon_RDR    = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "M1")

l1DataCon_RDR     = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "L1")
r1DataCon_RDR     = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "R1")

prodDataCon_RDR   = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit ":*:")
comp1DataCon_RDR  = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "Comp1")

unPar1_RDR  = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "Par1")  (fsLit "unPar1")
unRec1_RDR  = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "Rec1")  (fsLit "unRec1")
unK1_RDR    = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "K1")    (fsLit "unK1")
unComp1_RDR = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "Comp1") (fsLit "unComp1")

from_RDR  = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "from")
from1_RDR = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "from1")
to_RDR    = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "to")
to1_RDR   = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "to1")

datatypeName_RDR  = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "datatypeName")
moduleName_RDR    = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "moduleName")
packageName_RDR   = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "packageName")
isNewtypeName_RDR = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "isNewtype")
selName_RDR       = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "selName")
conName_RDR       = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "conName")
conFixity_RDR     = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "conFixity")
conIsRecord_RDR   = varQual_RDR gHC_INTERNAL_GENERICS (fsLit "conIsRecord")

prefixDataCon_RDR     = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "Prefix")
infixDataCon_RDR      = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "Infix")
leftAssocDataCon_RDR  = nameRdrName leftAssociativeDataConName
rightAssocDataCon_RDR = nameRdrName rightAssociativeDataConName
notAssocDataCon_RDR   = nameRdrName notAssociativeDataConName

uAddrDataCon_RDR   = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "UAddr")
uCharDataCon_RDR   = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "UChar")
uDoubleDataCon_RDR = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "UDouble")
uFloatDataCon_RDR  = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "UFloat")
uIntDataCon_RDR    = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "UInt")
uWordDataCon_RDR   = dataQual_RDR gHC_INTERNAL_GENERICS (fsLit "UWord")

uAddrHash_RDR   = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "UAddr")   (fsLit "uAddr#")
uCharHash_RDR   = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "UChar")   (fsLit "uChar#")
uDoubleHash_RDR = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "UDouble") (fsLit "uDouble#")
uFloatHash_RDR  = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "UFloat")  (fsLit "uFloat#")
uIntHash_RDR    = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "UInt")    (fsLit "uInt#")
uWordHash_RDR   = fieldQual_RDR gHC_INTERNAL_GENERICS (fsLit "UWord")   (fsLit "uWord#")

fmap_RDR, replace_RDR, pure_RDR, ap_RDR, liftA2_RDR, foldable_foldr_RDR,
    foldMap_RDR, null_RDR, all_RDR, traverse_RDR, mempty_RDR,
    mappend_RDR :: RdrName
fmap_RDR                = nameRdrName fmapName
replace_RDR             = varQual_RDR gHC_INTERNAL_BASE (fsLit "<$")
pure_RDR                = nameRdrName pureAName
ap_RDR                  = nameRdrName apAName
liftA2_RDR              = varQual_RDR gHC_INTERNAL_BASE (fsLit "liftA2")
foldable_foldr_RDR      = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "foldr")
foldMap_RDR             = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "foldMap")
null_RDR                = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "null")
all_RDR                 = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "all")
traverse_RDR            = varQual_RDR gHC_INTERNAL_DATA_TRAVERSABLE    (fsLit "traverse")
mempty_RDR              = nameRdrName memptyName
mappend_RDR             = nameRdrName mappendName

----------------------
varQual_RDR, tcQual_RDR, clsQual_RDR, dataQual_RDR
    :: Module -> FastString -> RdrName
varQual_RDR  mod str = mkOrig mod (mkOccNameFS varName str)
tcQual_RDR   mod str = mkOrig mod (mkOccNameFS tcName str)
clsQual_RDR  mod str = mkOrig mod (mkOccNameFS clsName str)
dataQual_RDR mod str = mkOrig mod (mkOccNameFS dataName str)

fieldQual_RDR :: Module -> FastString -> FastString -> RdrName
fieldQual_RDR mod con str = mkOrig mod (mkOccNameFS (fieldName con) str)

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
runMainIOName = varQual gHC_INTERNAL_TOP_HANDLER (fsLit "runMainIO") runMainKey
runRWName     = varQual gHC_MAGIC       (fsLit "runRW#")    runRWKey

orderingTyConName, ordLTDataConName, ordEQDataConName, ordGTDataConName :: Name
orderingTyConName = tcQual  gHC_TYPES (fsLit "Ordering") orderingTyConKey
ordLTDataConName     = dcQual gHC_TYPES (fsLit "LT") ordLTDataConKey
ordEQDataConName     = dcQual gHC_TYPES (fsLit "EQ") ordEQDataConKey
ordGTDataConName     = dcQual gHC_TYPES (fsLit "GT") ordGTDataConKey

specTyConName :: Name
specTyConName     = tcQual gHC_TYPES (fsLit "SPEC") specTyConKey

eitherTyConName, leftDataConName, rightDataConName :: Name
eitherTyConName   = tcQual  gHC_INTERNAL_DATA_EITHER (fsLit "Either") eitherTyConKey
leftDataConName   = dcQual gHC_INTERNAL_DATA_EITHER (fsLit "Left")   leftDataConKey
rightDataConName  = dcQual gHC_INTERNAL_DATA_EITHER (fsLit "Right")  rightDataConKey

voidTyConName :: Name
voidTyConName = tcQual gHC_INTERNAL_BASE (fsLit "Void") voidTyConKey

-- Generics (types)
v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
  k1TyConName, m1TyConName, sumTyConName, prodTyConName,
  compTyConName, rTyConName, dTyConName,
  cTyConName, sTyConName, rec0TyConName,
  d1TyConName, c1TyConName, s1TyConName,
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

v1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "V1") v1TyConKey
u1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "U1") u1TyConKey
par1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "Par1") par1TyConKey
rec1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "Rec1") rec1TyConKey
k1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "K1") k1TyConKey
m1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "M1") m1TyConKey

sumTyConName    = tcQual gHC_INTERNAL_GENERICS (fsLit ":+:") sumTyConKey
prodTyConName   = tcQual gHC_INTERNAL_GENERICS (fsLit ":*:") prodTyConKey
compTyConName   = tcQual gHC_INTERNAL_GENERICS (fsLit ":.:") compTyConKey

rTyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "R") rTyConKey
dTyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "D") dTyConKey
cTyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "C") cTyConKey
sTyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "S") sTyConKey

rec0TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "Rec0") rec0TyConKey
d1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "D1") d1TyConKey
c1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "C1") c1TyConKey
s1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "S1") s1TyConKey

repTyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "Rep")  repTyConKey
rep1TyConName = tcQual gHC_INTERNAL_GENERICS (fsLit "Rep1") rep1TyConKey

uRecTyConName      = tcQual gHC_INTERNAL_GENERICS (fsLit "URec") uRecTyConKey
uAddrTyConName     = tcQual gHC_INTERNAL_GENERICS (fsLit "UAddr") uAddrTyConKey
uCharTyConName     = tcQual gHC_INTERNAL_GENERICS (fsLit "UChar") uCharTyConKey
uDoubleTyConName   = tcQual gHC_INTERNAL_GENERICS (fsLit "UDouble") uDoubleTyConKey
uFloatTyConName    = tcQual gHC_INTERNAL_GENERICS (fsLit "UFloat") uFloatTyConKey
uIntTyConName      = tcQual gHC_INTERNAL_GENERICS (fsLit "UInt") uIntTyConKey
uWordTyConName     = tcQual gHC_INTERNAL_GENERICS (fsLit "UWord") uWordTyConKey

prefixIDataConName = dcQual gHC_INTERNAL_GENERICS (fsLit "PrefixI")  prefixIDataConKey
infixIDataConName  = dcQual gHC_INTERNAL_GENERICS (fsLit "InfixI")   infixIDataConKey
leftAssociativeDataConName  = dcQual gHC_INTERNAL_GENERICS (fsLit "LeftAssociative")   leftAssociativeDataConKey
rightAssociativeDataConName = dcQual gHC_INTERNAL_GENERICS (fsLit "RightAssociative")  rightAssociativeDataConKey
notAssociativeDataConName   = dcQual gHC_INTERNAL_GENERICS (fsLit "NotAssociative")    notAssociativeDataConKey

sourceUnpackDataConName         = dcQual gHC_INTERNAL_GENERICS (fsLit "SourceUnpack")         sourceUnpackDataConKey
sourceNoUnpackDataConName       = dcQual gHC_INTERNAL_GENERICS (fsLit "SourceNoUnpack")       sourceNoUnpackDataConKey
noSourceUnpackednessDataConName = dcQual gHC_INTERNAL_GENERICS (fsLit "NoSourceUnpackedness") noSourceUnpackednessDataConKey
sourceLazyDataConName           = dcQual gHC_INTERNAL_GENERICS (fsLit "SourceLazy")           sourceLazyDataConKey
sourceStrictDataConName         = dcQual gHC_INTERNAL_GENERICS (fsLit "SourceStrict")         sourceStrictDataConKey
noSourceStrictnessDataConName   = dcQual gHC_INTERNAL_GENERICS (fsLit "NoSourceStrictness")   noSourceStrictnessDataConKey
decidedLazyDataConName          = dcQual gHC_INTERNAL_GENERICS (fsLit "DecidedLazy")          decidedLazyDataConKey
decidedStrictDataConName        = dcQual gHC_INTERNAL_GENERICS (fsLit "DecidedStrict")        decidedStrictDataConKey
decidedUnpackDataConName        = dcQual gHC_INTERNAL_GENERICS (fsLit "DecidedUnpack")        decidedUnpackDataConKey

metaDataDataConName  = dcQual gHC_INTERNAL_GENERICS (fsLit "MetaData")  metaDataDataConKey
metaConsDataConName  = dcQual gHC_INTERNAL_GENERICS (fsLit "MetaCons")  metaConsDataConKey
metaSelDataConName   = dcQual gHC_INTERNAL_GENERICS (fsLit "MetaSel")   metaSelDataConKey

-- Primitive Int
divIntName, modIntName :: Name
divIntName = varQual gHC_CLASSES (fsLit "divInt#") divIntIdKey
modIntName = varQual gHC_CLASSES (fsLit "modInt#") modIntIdKey

-- Base strings Strings
unpackCStringName, unpackCStringFoldrName,
    unpackCStringUtf8Name, unpackCStringFoldrUtf8Name,
    unpackCStringAppendName, unpackCStringAppendUtf8Name,
    eqStringName, cstringLengthName :: Name
cstringLengthName       = varQual gHC_CSTRING (fsLit "cstringLength#") cstringLengthIdKey
eqStringName            = varQual gHC_INTERNAL_BASE (fsLit "eqString")  eqStringIdKey

unpackCStringName       = varQual gHC_CSTRING (fsLit "unpackCString#") unpackCStringIdKey
unpackCStringAppendName = varQual gHC_CSTRING (fsLit "unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual gHC_CSTRING (fsLit "unpackFoldrCString#") unpackCStringFoldrIdKey

unpackCStringUtf8Name       = varQual gHC_CSTRING (fsLit "unpackCStringUtf8#") unpackCStringUtf8IdKey
unpackCStringAppendUtf8Name = varQual gHC_CSTRING (fsLit "unpackAppendCStringUtf8#") unpackCStringAppendUtf8IdKey
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
functorClassName  = clsQual gHC_INTERNAL_BASE    (fsLit "Functor") functorClassKey
fmapName          = varQual gHC_INTERNAL_BASE    (fsLit "fmap")    fmapClassOpKey

-- Class Monad
monadClassName, thenMName, bindMName, returnMName :: Name
monadClassName     = clsQual gHC_INTERNAL_BASE (fsLit "Monad")  monadClassKey
thenMName          = varQual gHC_INTERNAL_BASE (fsLit ">>")     thenMClassOpKey
bindMName          = varQual gHC_INTERNAL_BASE (fsLit ">>=")    bindMClassOpKey
returnMName        = varQual gHC_INTERNAL_BASE (fsLit "return") returnMClassOpKey

-- Class MonadFail
monadFailClassName, failMName :: Name
monadFailClassName = clsQual gHC_INTERNAL_MONAD_FAIL (fsLit "MonadFail") monadFailClassKey
failMName          = varQual gHC_INTERNAL_MONAD_FAIL (fsLit "fail")      failMClassOpKey

-- Class Applicative
applicativeClassName, pureAName, apAName, thenAName :: Name
applicativeClassName = clsQual gHC_INTERNAL_BASE (fsLit "Applicative") applicativeClassKey
apAName              = varQual gHC_INTERNAL_BASE (fsLit "<*>")         apAClassOpKey
pureAName            = varQual gHC_INTERNAL_BASE (fsLit "pure")        pureAClassOpKey
thenAName            = varQual gHC_INTERNAL_BASE (fsLit "*>")          thenAClassOpKey

-- Classes (Foldable, Traversable)
foldableClassName, traversableClassName :: Name
foldableClassName     = clsQual  gHC_INTERNAL_DATA_FOLDABLE       (fsLit "Foldable")    foldableClassKey
traversableClassName  = clsQual  gHC_INTERNAL_DATA_TRAVERSABLE    (fsLit "Traversable") traversableClassKey

-- Classes (Semigroup, Monoid)
semigroupClassName, sappendName :: Name
semigroupClassName = clsQual gHC_INTERNAL_BASE       (fsLit "Semigroup") semigroupClassKey
sappendName        = varQual gHC_INTERNAL_BASE       (fsLit "<>")        sappendClassOpKey
monoidClassName, memptyName, mappendName, mconcatName :: Name
monoidClassName    = clsQual gHC_INTERNAL_BASE       (fsLit "Monoid")    monoidClassKey
memptyName         = varQual gHC_INTERNAL_BASE       (fsLit "mempty")    memptyClassOpKey
mappendName        = varQual gHC_INTERNAL_BASE       (fsLit "mappend")   mappendClassOpKey
mconcatName        = varQual gHC_INTERNAL_BASE       (fsLit "mconcat")   mconcatClassOpKey



-- AMP additions

joinMName, alternativeClassName :: Name
joinMName            = varQual gHC_INTERNAL_BASE (fsLit "join")        joinMIdKey
alternativeClassName = clsQual gHC_INTERNAL_MONAD (fsLit "Alternative") alternativeClassKey

--
joinMIdKey, apAClassOpKey, pureAClassOpKey, thenAClassOpKey,
    alternativeClassKey :: Unique
joinMIdKey          = mkPreludeMiscIdUnique 750
apAClassOpKey       = mkPreludeMiscIdUnique 751 -- <*>
pureAClassOpKey     = mkPreludeMiscIdUnique 752
thenAClassOpKey     = mkPreludeMiscIdUnique 753
alternativeClassKey = mkPreludeMiscIdUnique 754


-- Functions for GHC extensions
considerAccessibleName :: Name
considerAccessibleName = varQual gHC_INTERNAL_EXTS (fsLit "considerAccessible") considerAccessibleIdKey

-- Random GHC.Internal.Base functions
fromStringName, otherwiseIdName, foldrName, buildName, augmentName,
    mapName, appendName, assertName,
    dollarName :: Name
dollarName        = varQual gHC_INTERNAL_BASE (fsLit "$")          dollarIdKey
otherwiseIdName   = varQual gHC_INTERNAL_BASE (fsLit "otherwise")  otherwiseIdKey
foldrName         = varQual gHC_INTERNAL_BASE (fsLit "foldr")      foldrIdKey
buildName         = varQual gHC_INTERNAL_BASE (fsLit "build")      buildIdKey
augmentName       = varQual gHC_INTERNAL_BASE (fsLit "augment")    augmentIdKey
mapName           = varQual gHC_INTERNAL_BASE (fsLit "map")        mapIdKey
appendName        = varQual gHC_INTERNAL_BASE (fsLit "++")         appendIdKey
assertName        = varQual gHC_INTERNAL_BASE (fsLit "assert")     assertIdKey
fromStringName    = varQual gHC_INTERNAL_DATA_STRING (fsLit "fromString") fromStringClassOpKey

-- Module GHC.Internal.Num
numClassName, fromIntegerName, minusName, negateName :: Name
numClassName      = clsQual gHC_INTERNAL_NUM (fsLit "Num")         numClassKey
fromIntegerName   = varQual gHC_INTERNAL_NUM (fsLit "fromInteger") fromIntegerClassOpKey
minusName         = varQual gHC_INTERNAL_NUM (fsLit "-")           minusClassOpKey
negateName        = varQual gHC_INTERNAL_NUM (fsLit "negate")      negateClassOpKey

---------------------------------
-- ghc-bignum
---------------------------------
integerFromNaturalName
   , integerToNaturalClampName
   , integerToNaturalThrowName
   , integerToNaturalName
   , integerToWordName
   , integerToIntName
   , integerToWord64Name
   , integerToInt64Name
   , integerFromWordName
   , integerFromWord64Name
   , integerFromInt64Name
   , integerAddName
   , integerMulName
   , integerSubName
   , integerNegateName
   , integerAbsName
   , integerPopCountName
   , integerQuotName
   , integerRemName
   , integerDivName
   , integerModName
   , integerDivModName
   , integerQuotRemName
   , integerEncodeFloatName
   , integerEncodeDoubleName
   , integerGcdName
   , integerLcmName
   , integerAndName
   , integerOrName
   , integerXorName
   , integerComplementName
   , integerBitName
   , integerTestBitName
   , integerShiftLName
   , integerShiftRName
   , naturalToWordName
   , naturalPopCountName
   , naturalShiftRName
   , naturalShiftLName
   , naturalAddName
   , naturalSubName
   , naturalSubThrowName
   , naturalSubUnsafeName
   , naturalMulName
   , naturalQuotRemName
   , naturalQuotName
   , naturalRemName
   , naturalAndName
   , naturalAndNotName
   , naturalOrName
   , naturalXorName
   , naturalTestBitName
   , naturalBitName
   , naturalGcdName
   , naturalLcmName
   , naturalLog2Name
   , naturalLogBaseWordName
   , naturalLogBaseName
   , naturalPowModName
   , naturalSizeInBaseName
   , bignatEqName
   , bignatCompareName
   , bignatCompareWordName
   :: Name

bnbVarQual, bnnVarQual, bniVarQual :: String -> Unique -> Name
bnbVarQual str key = varQual gHC_INTERNAL_NUM_BIGNAT  (fsLit str) key
bnnVarQual str key = varQual gHC_INTERNAL_NUM_NATURAL (fsLit str) key
bniVarQual str key = varQual gHC_INTERNAL_NUM_INTEGER (fsLit str) key

-- Types and DataCons
bignatEqName              = bnbVarQual "bigNatEq#"                 bignatEqIdKey
bignatCompareName         = bnbVarQual "bigNatCompare"             bignatCompareIdKey
bignatCompareWordName     = bnbVarQual "bigNatCompareWord#"        bignatCompareWordIdKey

naturalToWordName         = bnnVarQual "naturalToWord#"            naturalToWordIdKey
naturalPopCountName       = bnnVarQual "naturalPopCount#"          naturalPopCountIdKey
naturalShiftRName         = bnnVarQual "naturalShiftR#"            naturalShiftRIdKey
naturalShiftLName         = bnnVarQual "naturalShiftL#"            naturalShiftLIdKey
naturalAddName            = bnnVarQual "naturalAdd"                naturalAddIdKey
naturalSubName            = bnnVarQual "naturalSub"                naturalSubIdKey
naturalSubThrowName       = bnnVarQual "naturalSubThrow"           naturalSubThrowIdKey
naturalSubUnsafeName      = bnnVarQual "naturalSubUnsafe"          naturalSubUnsafeIdKey
naturalMulName            = bnnVarQual "naturalMul"                naturalMulIdKey
naturalQuotRemName        = bnnVarQual "naturalQuotRem#"           naturalQuotRemIdKey
naturalQuotName           = bnnVarQual "naturalQuot"               naturalQuotIdKey
naturalRemName            = bnnVarQual "naturalRem"                naturalRemIdKey
naturalAndName            = bnnVarQual "naturalAnd"                naturalAndIdKey
naturalAndNotName         = bnnVarQual "naturalAndNot"             naturalAndNotIdKey
naturalOrName             = bnnVarQual "naturalOr"                 naturalOrIdKey
naturalXorName            = bnnVarQual "naturalXor"                naturalXorIdKey
naturalTestBitName        = bnnVarQual "naturalTestBit#"           naturalTestBitIdKey
naturalBitName            = bnnVarQual "naturalBit#"               naturalBitIdKey
naturalGcdName            = bnnVarQual "naturalGcd"                naturalGcdIdKey
naturalLcmName            = bnnVarQual "naturalLcm"                naturalLcmIdKey
naturalLog2Name           = bnnVarQual "naturalLog2#"              naturalLog2IdKey
naturalLogBaseWordName    = bnnVarQual "naturalLogBaseWord#"       naturalLogBaseWordIdKey
naturalLogBaseName        = bnnVarQual "naturalLogBase#"           naturalLogBaseIdKey
naturalPowModName         = bnnVarQual "naturalPowMod"             naturalPowModIdKey
naturalSizeInBaseName     = bnnVarQual "naturalSizeInBase#"        naturalSizeInBaseIdKey

integerFromNaturalName    = bniVarQual "integerFromNatural"        integerFromNaturalIdKey
integerToNaturalClampName = bniVarQual "integerToNaturalClamp"     integerToNaturalClampIdKey
integerToNaturalThrowName = bniVarQual "integerToNaturalThrow"     integerToNaturalThrowIdKey
integerToNaturalName      = bniVarQual "integerToNatural"          integerToNaturalIdKey
integerToWordName         = bniVarQual "integerToWord#"            integerToWordIdKey
integerToIntName          = bniVarQual "integerToInt#"             integerToIntIdKey
integerToWord64Name       = bniVarQual "integerToWord64#"          integerToWord64IdKey
integerToInt64Name        = bniVarQual "integerToInt64#"           integerToInt64IdKey
integerFromWordName       = bniVarQual "integerFromWord#"          integerFromWordIdKey
integerFromWord64Name     = bniVarQual "integerFromWord64#"        integerFromWord64IdKey
integerFromInt64Name      = bniVarQual "integerFromInt64#"         integerFromInt64IdKey
integerAddName            = bniVarQual "integerAdd"                integerAddIdKey
integerMulName            = bniVarQual "integerMul"                integerMulIdKey
integerSubName            = bniVarQual "integerSub"                integerSubIdKey
integerNegateName         = bniVarQual "integerNegate"             integerNegateIdKey
integerAbsName            = bniVarQual "integerAbs"                integerAbsIdKey
integerPopCountName       = bniVarQual "integerPopCount#"          integerPopCountIdKey
integerQuotName           = bniVarQual "integerQuot"               integerQuotIdKey
integerRemName            = bniVarQual "integerRem"                integerRemIdKey
integerDivName            = bniVarQual "integerDiv"                integerDivIdKey
integerModName            = bniVarQual "integerMod"                integerModIdKey
integerDivModName         = bniVarQual "integerDivMod#"            integerDivModIdKey
integerQuotRemName        = bniVarQual "integerQuotRem#"           integerQuotRemIdKey
integerEncodeFloatName    = bniVarQual "integerEncodeFloat#"       integerEncodeFloatIdKey
integerEncodeDoubleName   = bniVarQual "integerEncodeDouble#"      integerEncodeDoubleIdKey
integerGcdName            = bniVarQual "integerGcd"                integerGcdIdKey
integerLcmName            = bniVarQual "integerLcm"                integerLcmIdKey
integerAndName            = bniVarQual "integerAnd"                integerAndIdKey
integerOrName             = bniVarQual "integerOr"                 integerOrIdKey
integerXorName            = bniVarQual "integerXor"                integerXorIdKey
integerComplementName     = bniVarQual "integerComplement"         integerComplementIdKey
integerBitName            = bniVarQual "integerBit#"               integerBitIdKey
integerTestBitName        = bniVarQual "integerTestBit#"           integerTestBitIdKey
integerShiftLName         = bniVarQual "integerShiftL#"            integerShiftLIdKey
integerShiftRName         = bniVarQual "integerShiftR#"            integerShiftRIdKey



---------------------------------
-- End of ghc-bignum
---------------------------------

-- GHC.Internal.Real types and classes
rationalTyConName, ratioTyConName, ratioDataConName, realClassName,
    integralClassName, realFracClassName, fractionalClassName,
    fromRationalName, toIntegerName, toRationalName, fromIntegralName,
    realToFracName, mkRationalBase2Name, mkRationalBase10Name :: Name
rationalTyConName   = tcQual  gHC_INTERNAL_REAL (fsLit "Rational")     rationalTyConKey
ratioTyConName      = tcQual  gHC_INTERNAL_REAL (fsLit "Ratio")        ratioTyConKey
ratioDataConName    = dcQual  gHC_INTERNAL_REAL (fsLit ":%")           ratioDataConKey
realClassName       = clsQual gHC_INTERNAL_REAL (fsLit "Real")         realClassKey
integralClassName   = clsQual gHC_INTERNAL_REAL (fsLit "Integral")     integralClassKey
realFracClassName   = clsQual gHC_INTERNAL_REAL (fsLit "RealFrac")     realFracClassKey
fractionalClassName = clsQual gHC_INTERNAL_REAL (fsLit "Fractional")   fractionalClassKey
fromRationalName    = varQual gHC_INTERNAL_REAL (fsLit "fromRational") fromRationalClassOpKey
toIntegerName       = varQual gHC_INTERNAL_REAL (fsLit "toInteger")    toIntegerClassOpKey
toRationalName      = varQual gHC_INTERNAL_REAL (fsLit "toRational")   toRationalClassOpKey
fromIntegralName    = varQual  gHC_INTERNAL_REAL (fsLit "fromIntegral")fromIntegralIdKey
realToFracName      = varQual  gHC_INTERNAL_REAL (fsLit "realToFrac")  realToFracIdKey
mkRationalBase2Name  = varQual  gHC_INTERNAL_REAL  (fsLit "mkRationalBase2")  mkRationalBase2IdKey
mkRationalBase10Name = varQual  gHC_INTERNAL_REAL  (fsLit "mkRationalBase10") mkRationalBase10IdKey

-- GHC.Internal.Float classes
floatingClassName, realFloatClassName :: Name
floatingClassName  = clsQual gHC_INTERNAL_FLOAT (fsLit "Floating")  floatingClassKey
realFloatClassName = clsQual gHC_INTERNAL_FLOAT (fsLit "RealFloat") realFloatClassKey

-- other GHC.Internal.Float functions
integerToFloatName, integerToDoubleName,
  naturalToFloatName, naturalToDoubleName,
  rationalToFloatName, rationalToDoubleName :: Name
integerToFloatName   = varQual gHC_INTERNAL_FLOAT (fsLit "integerToFloat#") integerToFloatIdKey
integerToDoubleName  = varQual gHC_INTERNAL_FLOAT (fsLit "integerToDouble#") integerToDoubleIdKey
naturalToFloatName   = varQual gHC_INTERNAL_FLOAT (fsLit "naturalToFloat#") naturalToFloatIdKey
naturalToDoubleName  = varQual gHC_INTERNAL_FLOAT (fsLit "naturalToDouble#") naturalToDoubleIdKey
rationalToFloatName  = varQual gHC_INTERNAL_FLOAT (fsLit "rationalToFloat") rationalToFloatIdKey
rationalToDoubleName = varQual gHC_INTERNAL_FLOAT (fsLit "rationalToDouble") rationalToDoubleIdKey

-- Class Ix
ixClassName :: Name
ixClassName = clsQual gHC_INTERNAL_IX (fsLit "Ix") ixClassKey

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
  , typeLitCharDataConName
  :: Name
typeLitSortTyConName     = tcQual gHC_TYPES       (fsLit "TypeLitSort")    typeLitSortTyConKey
typeLitSymbolDataConName = dcQual gHC_TYPES       (fsLit "TypeLitSymbol")  typeLitSymbolDataConKey
typeLitNatDataConName    = dcQual gHC_TYPES       (fsLit "TypeLitNat")     typeLitNatDataConKey
typeLitCharDataConName   = dcQual gHC_TYPES       (fsLit "TypeLitChar")    typeLitCharDataConKey

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
  , typeCharTypeRepName
  , trGhcPrimModuleName
  :: Name
typeableClassName     = clsQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "Typeable")       typeableClassKey
typeRepTyConName      = tcQual  gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "TypeRep")        typeRepTyConKey
someTypeRepTyConName   = tcQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "SomeTypeRep")    someTypeRepTyConKey
someTypeRepDataConName = dcQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "SomeTypeRep")    someTypeRepDataConKey
typeRepIdName         = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "typeRep#")       typeRepIdKey
mkTrTypeName          = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "mkTrType")       mkTrTypeKey
mkTrConName           = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "mkTrCon")        mkTrConKey
mkTrAppName           = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "mkTrApp")        mkTrAppKey
mkTrFunName           = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "mkTrFun")        mkTrFunKey
typeNatTypeRepName    = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "typeNatTypeRep") typeNatTypeRepKey
typeSymbolTypeRepName = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "typeSymbolTypeRep") typeSymbolTypeRepKey
typeCharTypeRepName   = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "typeCharTypeRep") typeCharTypeRepKey
-- this is the Typeable 'Module' for GHC.Prim (which has no code, so we place in GHC.Types)
-- See Note [Grand plan for Typeable] in GHC.Tc.Instance.Typeable.
trGhcPrimModuleName   = varQual gHC_TYPES         (fsLit "tr$ModuleGHCPrim")  trGhcPrimModuleKey

-- Typeable KindReps for some common cases
starKindRepName, starArrStarKindRepName,
  starArrStarArrStarKindRepName, constraintKindRepName :: Name
starKindRepName        = varQual gHC_TYPES         (fsLit "krep$*")          starKindRepKey
starArrStarKindRepName = varQual gHC_TYPES         (fsLit "krep$*Arr*")      starArrStarKindRepKey
starArrStarArrStarKindRepName = varQual gHC_TYPES  (fsLit "krep$*->*->*")    starArrStarArrStarKindRepKey
constraintKindRepName  = varQual gHC_TYPES         (fsLit "krep$Constraint") constraintKindRepKey

-- WithDict
withDictClassName :: Name
withDictClassName = clsQual gHC_MAGIC_DICT (fsLit "WithDict") withDictClassKey

-- TagToEnum
tagToEnumClassName :: Name
tagToEnumClassName = clsQual gHC_MAGIC_TAGTOENUM (fsLit "TagToEnum") tagToEnumClassKey

-- DataToTag
dataToTagClassName :: Name
dataToTagClassName    = clsQual gHC_MAGIC (fsLit "DataToTag") dataToTagClassKey

nonEmptyTyConName :: Name
nonEmptyTyConName = tcQual gHC_INTERNAL_BASE (fsLit "NonEmpty") nonEmptyTyConKey

-- seq#
seqHashName :: Name
seqHashName = varQual gHC_INTERNAL_IO (fsLit "seq#") seqHashKey

-- Custom type errors
errorMessageTypeErrorFamName
  , typeErrorTextDataConName
  , typeErrorAppendDataConName
  , typeErrorVAppendDataConName
  , typeErrorShowTypeDataConName
  :: Name

errorMessageTypeErrorFamName =
  tcQual gHC_INTERNAL_TYPEERROR (fsLit "TypeError") errorMessageTypeErrorFamKey

typeErrorTextDataConName =
  dcQual gHC_INTERNAL_TYPEERROR (fsLit "Text") typeErrorTextDataConKey

typeErrorAppendDataConName =
  dcQual gHC_INTERNAL_TYPEERROR (fsLit ":<>:") typeErrorAppendDataConKey

typeErrorVAppendDataConName =
  dcQual gHC_INTERNAL_TYPEERROR (fsLit ":$$:") typeErrorVAppendDataConKey

typeErrorShowTypeDataConName =
  dcQual gHC_INTERNAL_TYPEERROR (fsLit "ShowType") typeErrorShowTypeDataConKey

-- "Unsatisfiable" constraint
unsatisfiableClassName, unsatisfiableIdName :: Name
unsatisfiableClassName =
  clsQual gHC_INTERNAL_TYPEERROR (fsLit "Unsatisfiable") unsatisfiableClassNameKey
unsatisfiableIdName =
  varQual gHC_INTERNAL_TYPEERROR (fsLit "unsatisfiable") unsatisfiableIdNameKey

-- Unsafe coercion proofs
unsafeEqualityProofName, unsafeEqualityTyConName, unsafeCoercePrimName,
  unsafeReflDataConName :: Name
unsafeEqualityProofName = varQual gHC_INTERNAL_UNSAFE_COERCE (fsLit "unsafeEqualityProof") unsafeEqualityProofIdKey
unsafeEqualityTyConName = tcQual gHC_INTERNAL_UNSAFE_COERCE (fsLit "UnsafeEquality") unsafeEqualityTyConKey
unsafeReflDataConName   = dcQual gHC_INTERNAL_UNSAFE_COERCE (fsLit "UnsafeRefl")     unsafeReflDataConKey
unsafeCoercePrimName    = varQual gHC_INTERNAL_UNSAFE_COERCE (fsLit "unsafeCoerce#") unsafeCoercePrimIdKey

-- Dynamic
toDynName :: Name
toDynName = varQual gHC_INTERNAL_DYNAMIC (fsLit "toDyn") toDynIdKey

-- Class Data
dataClassName :: Name
dataClassName = clsQual gHC_INTERNAL_DATA_DATA (fsLit "Data") dataClassKey

-- Error module
assertErrorName    :: Name
assertErrorName   = varQual gHC_INTERNAL_IO_Exception (fsLit "assertError") assertErrorIdKey

-- GHC.Internal.Debug.Trace
traceName          :: Name
traceName         = varQual gHC_INTERNAL_DEBUG_TRACE (fsLit "trace") traceKey

-- Enum module (Enum, Bounded)
enumClassName, enumFromName, enumFromToName, enumFromThenName,
    enumFromThenToName, boundedClassName :: Name
enumClassName      = clsQual gHC_INTERNAL_ENUM (fsLit "Enum")           enumClassKey
enumFromName       = varQual gHC_INTERNAL_ENUM (fsLit "enumFrom")       enumFromClassOpKey
enumFromToName     = varQual gHC_INTERNAL_ENUM (fsLit "enumFromTo")     enumFromToClassOpKey
enumFromThenName   = varQual gHC_INTERNAL_ENUM (fsLit "enumFromThen")   enumFromThenClassOpKey
enumFromThenToName = varQual gHC_INTERNAL_ENUM (fsLit "enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual gHC_INTERNAL_ENUM (fsLit "Bounded")        boundedClassKey

-- List functions
concatName, filterName, zipName :: Name
concatName        = varQual gHC_INTERNAL_LIST (fsLit "concat") concatIdKey
filterName        = varQual gHC_INTERNAL_LIST (fsLit "filter") filterIdKey
zipName           = varQual gHC_INTERNAL_LIST (fsLit "zip")    zipIdKey

-- Overloaded lists
isListClassName, fromListName, fromListNName, toListName :: Name
isListClassName = clsQual gHC_INTERNAL_IS_LIST (fsLit "IsList")    isListClassKey
fromListName    = varQual gHC_INTERNAL_IS_LIST (fsLit "fromList")  fromListClassOpKey
fromListNName   = varQual gHC_INTERNAL_IS_LIST (fsLit "fromListN") fromListNClassOpKey
toListName      = varQual gHC_INTERNAL_IS_LIST (fsLit "toList")    toListClassOpKey

-- HasField class ops
getFieldName, setFieldName :: Name
getFieldName   = varQual gHC_INTERNAL_RECORDS (fsLit "getField") getFieldClassOpKey
setFieldName   = varQual gHC_INTERNAL_RECORDS (fsLit "setField") setFieldClassOpKey

-- Class Show
showClassName :: Name
showClassName   = clsQual gHC_INTERNAL_SHOW (fsLit "Show")      showClassKey

-- Class Read
readClassName :: Name
readClassName   = clsQual gHC_INTERNAL_READ (fsLit "Read")      readClassKey

-- Classes Generic and Generic1, Datatype, Constructor and Selector
genClassName, gen1ClassName, datatypeClassName, constructorClassName,
  selectorClassName :: Name
genClassName  = clsQual gHC_INTERNAL_GENERICS (fsLit "Generic")  genClassKey
gen1ClassName = clsQual gHC_INTERNAL_GENERICS (fsLit "Generic1") gen1ClassKey

datatypeClassName    = clsQual gHC_INTERNAL_GENERICS (fsLit "Datatype")    datatypeClassKey
constructorClassName = clsQual gHC_INTERNAL_GENERICS (fsLit "Constructor") constructorClassKey
selectorClassName    = clsQual gHC_INTERNAL_GENERICS (fsLit "Selector")    selectorClassKey

genericClassNames :: [Name]
genericClassNames = [genClassName, gen1ClassName]

-- GHCi things
ghciIoClassName, ghciStepIoMName :: Name
ghciIoClassName = clsQual gHC_INTERNAL_GHCI (fsLit "GHCiSandboxIO") ghciIoClassKey
ghciStepIoMName = varQual gHC_INTERNAL_GHCI (fsLit "ghciStepIO") ghciStepIoMClassOpKey

-- IO things
ioTyConName, ioDataConName,
  thenIOName, bindIOName, returnIOName, failIOName :: Name
ioTyConName       = tcQual  gHC_TYPES (fsLit "IO")       ioTyConKey
ioDataConName     = dcQual  gHC_TYPES (fsLit "IO")       ioDataConKey
thenIOName        = varQual gHC_INTERNAL_BASE  (fsLit "thenIO")   thenIOIdKey
bindIOName        = varQual gHC_INTERNAL_BASE  (fsLit "bindIO")   bindIOIdKey
returnIOName      = varQual gHC_INTERNAL_BASE  (fsLit "returnIO") returnIOIdKey
failIOName        = varQual gHC_INTERNAL_IO    (fsLit "failIO")   failIOIdKey

-- IO things
printName :: Name
printName         = varQual gHC_INTERNAL_SYSTEM_IO (fsLit "print") printIdKey

-- Int, Word, and Addr things
int8TyConName, int16TyConName, int32TyConName, int64TyConName :: Name
int8TyConName     = tcQual gHC_INTERNAL_INT  (fsLit "Int8")  int8TyConKey
int16TyConName    = tcQual gHC_INTERNAL_INT  (fsLit "Int16") int16TyConKey
int32TyConName    = tcQual gHC_INTERNAL_INT  (fsLit "Int32") int32TyConKey
int64TyConName    = tcQual gHC_INTERNAL_INT  (fsLit "Int64") int64TyConKey

-- Word module
word8TyConName, word16TyConName, word32TyConName, word64TyConName :: Name
word8TyConName    = tcQual  gHC_INTERNAL_WORD (fsLit "Word8")  word8TyConKey
word16TyConName   = tcQual  gHC_INTERNAL_WORD (fsLit "Word16") word16TyConKey
word32TyConName   = tcQual  gHC_INTERNAL_WORD (fsLit "Word32") word32TyConKey
word64TyConName   = tcQual  gHC_INTERNAL_WORD (fsLit "Word64") word64TyConKey

-- PrelPtr module
ptrTyConName, funPtrTyConName :: Name
ptrTyConName      = tcQual   gHC_INTERNAL_PTR (fsLit "Ptr")    ptrTyConKey
funPtrTyConName   = tcQual   gHC_INTERNAL_PTR (fsLit "FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName, newStablePtrName :: Name
stablePtrTyConName    = tcQual   gHC_INTERNAL_STABLE (fsLit "StablePtr")    stablePtrTyConKey
newStablePtrName      = varQual  gHC_INTERNAL_STABLE (fsLit "newStablePtr") newStablePtrIdKey

-- Recursive-do notation
monadFixClassName, mfixName :: Name
monadFixClassName  = clsQual gHC_INTERNAL_MONAD_FIX (fsLit "MonadFix") monadFixClassKey
mfixName           = varQual gHC_INTERNAL_MONAD_FIX (fsLit "mfix")     mfixIdKey

-- Arrow notation
arrAName, composeAName, firstAName, appAName, choiceAName, loopAName :: Name
arrAName           = varQual gHC_INTERNAL_ARROW (fsLit "arr")       arrAIdKey
composeAName       = varQual gHC_INTERNAL_DESUGAR (fsLit ">>>") composeAIdKey
firstAName         = varQual gHC_INTERNAL_ARROW (fsLit "first")     firstAIdKey
appAName           = varQual gHC_INTERNAL_ARROW (fsLit "app")       appAIdKey
choiceAName        = varQual gHC_INTERNAL_ARROW (fsLit "|||")       choiceAIdKey
loopAName          = varQual gHC_INTERNAL_ARROW (fsLit "loop")      loopAIdKey

-- Monad comprehensions
guardMName, liftMName, mzipName :: Name
guardMName         = varQual gHC_INTERNAL_MONAD (fsLit "guard")    guardMIdKey
liftMName          = varQual gHC_INTERNAL_MONAD (fsLit "liftM")    liftMIdKey
mzipName           = varQual gHC_INTERNAL_CONTROL_MONAD_ZIP (fsLit "mzip") mzipIdKey


-- Annotation type checking
toAnnotationWrapperName :: Name
toAnnotationWrapperName = varQual gHC_INTERNAL_DESUGAR (fsLit "toAnnotationWrapper") toAnnotationWrapperIdKey

-- Other classes, needed for type defaulting
monadPlusClassName, isStringClassName :: Name
monadPlusClassName  = clsQual gHC_INTERNAL_MONAD (fsLit "MonadPlus")      monadPlusClassKey
isStringClassName   = clsQual gHC_INTERNAL_DATA_STRING (fsLit "IsString") isStringClassKey

-- Type-level naturals
knownNatClassName :: Name
knownNatClassName     = clsQual gHC_INTERNAL_TYPENATS (fsLit "KnownNat") knownNatClassNameKey
knownSymbolClassName :: Name
knownSymbolClassName  = clsQual gHC_INTERNAL_TYPELITS (fsLit "KnownSymbol") knownSymbolClassNameKey
knownCharClassName :: Name
knownCharClassName  = clsQual gHC_INTERNAL_TYPELITS (fsLit "KnownChar") knownCharClassNameKey

-- Overloaded labels
fromLabelClassOpName :: Name
fromLabelClassOpName
 = varQual gHC_INTERNAL_OVER_LABELS (fsLit "fromLabel") fromLabelClassOpKey

-- Implicit Parameters
ipClassName :: Name
ipClassName
  = clsQual gHC_CLASSES (fsLit "IP") ipClassKey

-- Overloaded record fields
hasFieldClassName :: Name
hasFieldClassName
 = clsQual gHC_INTERNAL_RECORDS (fsLit "HasField") hasFieldClassNameKey

-- ExceptionContext
exceptionContextTyConName, emptyExceptionContextName :: Name
exceptionContextTyConName =
    tcQual gHC_INTERNAL_EXCEPTION_CONTEXT (fsLit "ExceptionContext") exceptionContextTyConKey
emptyExceptionContextName
  = varQual gHC_INTERNAL_EXCEPTION_CONTEXT (fsLit "emptyExceptionContext") emptyExceptionContextKey

-- Source Locations
callStackTyConName, emptyCallStackName, pushCallStackName,
  srcLocDataConName :: Name
callStackTyConName
  = tcQual gHC_INTERNAL_STACK_TYPES  (fsLit "CallStack") callStackTyConKey
emptyCallStackName
  = varQual gHC_INTERNAL_STACK_TYPES (fsLit "emptyCallStack") emptyCallStackKey
pushCallStackName
  = varQual gHC_INTERNAL_STACK_TYPES (fsLit "pushCallStack") pushCallStackKey
srcLocDataConName
  = dcQual gHC_INTERNAL_STACK_TYPES  (fsLit "SrcLoc")    srcLocDataConKey

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
    varQual gHC_INTERNAL_STATICPTR_INTERNAL (fsLit "makeStatic") makeStaticKey

staticPtrInfoTyConName :: Name
staticPtrInfoTyConName =
    tcQual gHC_INTERNAL_STATICPTR (fsLit "StaticPtrInfo") staticPtrInfoTyConKey

staticPtrInfoDataConName :: Name
staticPtrInfoDataConName =
    dcQual gHC_INTERNAL_STATICPTR (fsLit "StaticPtrInfo") staticPtrInfoDataConKey

staticPtrTyConName :: Name
staticPtrTyConName =
    tcQual gHC_INTERNAL_STATICPTR (fsLit "StaticPtr") staticPtrTyConKey

staticPtrDataConName :: Name
staticPtrDataConName =
    dcQual gHC_INTERNAL_STATICPTR (fsLit "StaticPtr") staticPtrDataConKey

fromStaticPtrName :: Name
fromStaticPtrName =
    varQual gHC_INTERNAL_STATICPTR (fsLit "fromStaticPtr") fromStaticPtrClassOpKey

fingerprintDataConName :: Name
fingerprintDataConName =
    dcQual gHC_INTERNAL_FINGERPRINT_TYPE (fsLit "Fingerprint") fingerprintDataConKey

constPtrConName :: Name
constPtrConName =
    tcQual gHC_INTERNAL_FOREIGN_C_CONSTPTR (fsLit "ConstPtr") constPtrTyConKey

jsvalTyConName :: Name
jsvalTyConName = tcQual gHC_INTERNAL_WASM_PRIM_TYPES (fsLit "JSVal") jsvalTyConKey

unsafeUnpackJSStringUtf8ShShName :: Name
unsafeUnpackJSStringUtf8ShShName = varQual gHC_INTERNAL_JS_PRIM (fsLit "unsafeUnpackJSStringUtf8##") unsafeUnpackJSStringUtf8ShShKey

{-
************************************************************************
*                                                                      *
\subsection{Local helpers}
*                                                                      *
************************************************************************

All these are original names; hence mkOrig
-}

{-# INLINE varQual #-}
{-# INLINE tcQual #-}
{-# INLINE clsQual #-}
{-# INLINE dcQual #-}
varQual, tcQual, clsQual, dcQual :: Module -> FastString -> Unique -> Name
varQual  modu str unique = mk_known_key_name varName modu str unique
tcQual   modu str unique = mk_known_key_name tcName modu str unique
clsQual  modu str unique = mk_known_key_name clsName modu str unique
dcQual   modu str unique = mk_known_key_name dataName modu str unique

mk_known_key_name :: NameSpace -> Module -> FastString -> Unique -> Name
{-# INLINE mk_known_key_name #-}
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

withDictClassKey :: Unique
withDictClassKey        = mkPreludeClassUnique 21

tagToEnumClassKey :: Unique
tagToEnumClassKey       = mkPreludeClassUnique 22

dataToTagClassKey :: Unique
dataToTagClassKey       = mkPreludeClassUnique 23

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

-- KnownNat: see Note [KnownNat & KnownSymbol and EvLit] in GHC.Tc.Instance.Class
knownNatClassNameKey :: Unique
knownNatClassNameKey = mkPreludeClassUnique 42

-- KnownSymbol: see Note [KnownNat & KnownSymbol and EvLit] in GHC.Tc.Instance.Class
knownSymbolClassNameKey :: Unique
knownSymbolClassNameKey = mkPreludeClassUnique 43

knownCharClassNameKey :: Unique
knownCharClassNameKey = mkPreludeClassUnique 44

ghciIoClassKey :: Unique
ghciIoClassKey = mkPreludeClassUnique 45

semigroupClassKey, monoidClassKey :: Unique
semigroupClassKey = mkPreludeClassUnique 47
monoidClassKey    = mkPreludeClassUnique 48

-- Implicit Parameters
ipClassKey :: Unique
ipClassKey = mkPreludeClassUnique 49

-- Overloaded record fields
hasFieldClassNameKey :: Unique
hasFieldClassNameKey = mkPreludeClassUnique 50


---------------- Template Haskell -------------------
--      GHC.Builtin.Names.TH: USES ClassUniques 200-299
-----------------------------------------------------

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
*                                                                      *
************************************************************************
-}

addrPrimTyConKey, arrayPrimTyConKey, boolTyConKey,
    byteArrayPrimTyConKey, charPrimTyConKey, charTyConKey, doublePrimTyConKey,
    doubleTyConKey, floatPrimTyConKey, floatTyConKey, fUNTyConKey,
    intPrimTyConKey, intTyConKey, int8TyConKey, int16TyConKey,
    int8PrimTyConKey, int16PrimTyConKey, int32PrimTyConKey, int32TyConKey,
    int64PrimTyConKey, int64TyConKey,
    integerTyConKey, naturalTyConKey,
    listTyConKey, foreignObjPrimTyConKey, maybeTyConKey,
    weakPrimTyConKey, mutableArrayPrimTyConKey,
    mutableByteArrayPrimTyConKey, orderingTyConKey, mVarPrimTyConKey,
    ratioTyConKey, rationalTyConKey, realWorldTyConKey, stablePtrPrimTyConKey,
    stablePtrTyConKey, eqTyConKey, heqTyConKey, ioPortPrimTyConKey,
    smallArrayPrimTyConKey, smallMutableArrayPrimTyConKey,
    stringTyConKey,
    ccArrowTyConKey, ctArrowTyConKey, tcArrowTyConKey :: Unique
addrPrimTyConKey                        = mkPreludeTyConUnique  1
arrayPrimTyConKey                       = mkPreludeTyConUnique  3
boolTyConKey                            = mkPreludeTyConUnique  4
byteArrayPrimTyConKey                   = mkPreludeTyConUnique  5
stringTyConKey                          = mkPreludeTyConUnique  6
charPrimTyConKey                        = mkPreludeTyConUnique  7
charTyConKey                            = mkPreludeTyConUnique  8
doublePrimTyConKey                      = mkPreludeTyConUnique  9
doubleTyConKey                          = mkPreludeTyConUnique 10
floatPrimTyConKey                       = mkPreludeTyConUnique 11
floatTyConKey                           = mkPreludeTyConUnique 12
fUNTyConKey                             = mkPreludeTyConUnique 13
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
ioPortPrimTyConKey                      = mkPreludeTyConUnique 34
ratioTyConKey                           = mkPreludeTyConUnique 35
rationalTyConKey                        = mkPreludeTyConUnique 36
realWorldTyConKey                       = mkPreludeTyConUnique 37
stablePtrPrimTyConKey                   = mkPreludeTyConUnique 38
stablePtrTyConKey                       = mkPreludeTyConUnique 39
eqTyConKey                              = mkPreludeTyConUnique 40
heqTyConKey                             = mkPreludeTyConUnique 41

ctArrowTyConKey                       = mkPreludeTyConUnique 42
ccArrowTyConKey                       = mkPreludeTyConUnique 43
tcArrowTyConKey                       = mkPreludeTyConUnique 44

statePrimTyConKey, stableNamePrimTyConKey, stableNameTyConKey,
    mutVarPrimTyConKey, ioTyConKey,
    wordPrimTyConKey, wordTyConKey, word8PrimTyConKey, word8TyConKey,
    word16PrimTyConKey, word16TyConKey, word32PrimTyConKey, word32TyConKey,
    word64PrimTyConKey, word64TyConKey,
    kindConKey, boxityConKey,
    typeConKey, threadIdPrimTyConKey, bcoPrimTyConKey, ptrTyConKey,
    funPtrTyConKey, tVarPrimTyConKey, eqPrimTyConKey,
    eqReprPrimTyConKey, eqPhantPrimTyConKey,
    compactPrimTyConKey, stackSnapshotPrimTyConKey,
    promptTagPrimTyConKey, constPtrTyConKey, jsvalTyConKey :: Unique
statePrimTyConKey                       = mkPreludeTyConUnique 50
stableNamePrimTyConKey                  = mkPreludeTyConUnique 51
stableNameTyConKey                      = mkPreludeTyConUnique 52
eqPrimTyConKey                          = mkPreludeTyConUnique 53
eqReprPrimTyConKey                      = mkPreludeTyConUnique 54
eqPhantPrimTyConKey                     = mkPreludeTyConUnique 55
mutVarPrimTyConKey                      = mkPreludeTyConUnique 56
ioTyConKey                              = mkPreludeTyConUnique 57
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
kindConKey                              = mkPreludeTyConUnique 72
boxityConKey                            = mkPreludeTyConUnique 73
typeConKey                              = mkPreludeTyConUnique 74
threadIdPrimTyConKey                    = mkPreludeTyConUnique 75
bcoPrimTyConKey                         = mkPreludeTyConUnique 76
ptrTyConKey                             = mkPreludeTyConUnique 77
funPtrTyConKey                          = mkPreludeTyConUnique 78
tVarPrimTyConKey                        = mkPreludeTyConUnique 79
compactPrimTyConKey                     = mkPreludeTyConUnique 80
stackSnapshotPrimTyConKey               = mkPreludeTyConUnique 81
promptTagPrimTyConKey                   = mkPreludeTyConUnique 82

eitherTyConKey :: Unique
eitherTyConKey                          = mkPreludeTyConUnique 84

voidTyConKey :: Unique
voidTyConKey                            = mkPreludeTyConUnique 85

nonEmptyTyConKey :: Unique
nonEmptyTyConKey                        = mkPreludeTyConUnique 86

dictTyConKey :: Unique
dictTyConKey                            = mkPreludeTyConUnique 87

-- Kind constructors
liftedTypeKindTyConKey, unliftedTypeKindTyConKey,
  tYPETyConKey, cONSTRAINTTyConKey,
  liftedRepTyConKey, unliftedRepTyConKey,
  constraintKindTyConKey, levityTyConKey, runtimeRepTyConKey,
  vecCountTyConKey, vecElemTyConKey,
  zeroBitRepTyConKey, zeroBitTypeTyConKey :: Unique
liftedTypeKindTyConKey                  = mkPreludeTyConUnique 88
unliftedTypeKindTyConKey                = mkPreludeTyConUnique 89
tYPETyConKey                            = mkPreludeTyConUnique 91
cONSTRAINTTyConKey                      = mkPreludeTyConUnique 92
constraintKindTyConKey                  = mkPreludeTyConUnique 93
levityTyConKey                          = mkPreludeTyConUnique 94
runtimeRepTyConKey                      = mkPreludeTyConUnique 95
vecCountTyConKey                        = mkPreludeTyConUnique 96
vecElemTyConKey                         = mkPreludeTyConUnique 97
liftedRepTyConKey                       = mkPreludeTyConUnique 98
unliftedRepTyConKey                     = mkPreludeTyConUnique 99
zeroBitRepTyConKey                         = mkPreludeTyConUnique 100
zeroBitTypeTyConKey                        = mkPreludeTyConUnique 101

pluginTyConKey, frontendPluginTyConKey :: Unique
pluginTyConKey                          = mkPreludeTyConUnique 102
frontendPluginTyConKey                  = mkPreludeTyConUnique 103

trTyConTyConKey, trModuleTyConKey, trNameTyConKey,
  kindRepTyConKey, typeLitSortTyConKey :: Unique
trTyConTyConKey                         = mkPreludeTyConUnique 104
trModuleTyConKey                        = mkPreludeTyConUnique 105
trNameTyConKey                          = mkPreludeTyConUnique 106
kindRepTyConKey                         = mkPreludeTyConUnique 107
typeLitSortTyConKey                     = mkPreludeTyConUnique 108

-- Generics (Unique keys)
v1TyConKey, u1TyConKey, par1TyConKey, rec1TyConKey,
  k1TyConKey, m1TyConKey, sumTyConKey, prodTyConKey,
  compTyConKey, rTyConKey, dTyConKey,
  cTyConKey, sTyConKey, rec0TyConKey,
  d1TyConKey, c1TyConKey, s1TyConKey,
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

repTyConKey  = mkPreludeTyConUnique 155
rep1TyConKey = mkPreludeTyConUnique 156

uRecTyConKey    = mkPreludeTyConUnique 157
uAddrTyConKey   = mkPreludeTyConUnique 158
uCharTyConKey   = mkPreludeTyConUnique 159
uDoubleTyConKey = mkPreludeTyConUnique 160
uFloatTyConKey  = mkPreludeTyConUnique 161
uIntTyConKey    = mkPreludeTyConUnique 162
uWordTyConKey   = mkPreludeTyConUnique 163

-- "Unsatisfiable" constraint
unsatisfiableClassNameKey :: Unique
unsatisfiableClassNameKey = mkPreludeTyConUnique 170

anyTyConKey :: Unique
anyTyConKey = mkPreludeTyConUnique 171

zonkAnyTyConKey :: Unique
zonkAnyTyConKey = mkPreludeTyConUnique 172

-- Custom user type-errors
errorMessageTypeErrorFamKey :: Unique
errorMessageTypeErrorFamKey = mkPreludeTyConUnique 181

coercibleTyConKey :: Unique
coercibleTyConKey = mkPreludeTyConUnique 183

proxyPrimTyConKey :: Unique
proxyPrimTyConKey = mkPreludeTyConUnique 184

specTyConKey :: Unique
specTyConKey = mkPreludeTyConUnique 185

smallArrayPrimTyConKey        = mkPreludeTyConUnique  187
smallMutableArrayPrimTyConKey = mkPreludeTyConUnique  188

staticPtrTyConKey  :: Unique
staticPtrTyConKey  = mkPreludeTyConUnique 189

staticPtrInfoTyConKey :: Unique
staticPtrInfoTyConKey = mkPreludeTyConUnique 190

callStackTyConKey :: Unique
callStackTyConKey = mkPreludeTyConUnique 191

-- Typeables
typeRepTyConKey, someTypeRepTyConKey, someTypeRepDataConKey :: Unique
typeRepTyConKey       = mkPreludeTyConUnique 192
someTypeRepTyConKey   = mkPreludeTyConUnique 193
someTypeRepDataConKey = mkPreludeTyConUnique 194


typeSymbolAppendFamNameKey :: Unique
typeSymbolAppendFamNameKey = mkPreludeTyConUnique 195

-- Unsafe equality
unsafeEqualityTyConKey :: Unique
unsafeEqualityTyConKey = mkPreludeTyConUnique 196

-- Linear types
multiplicityTyConKey :: Unique
multiplicityTyConKey = mkPreludeTyConUnique 197

unrestrictedFunTyConKey :: Unique
unrestrictedFunTyConKey = mkPreludeTyConUnique 198

multMulTyConKey :: Unique
multMulTyConKey = mkPreludeTyConUnique 199

---------------- Template Haskell -------------------
--      GHC.Builtin.Names.TH: USES TyConUniques 200-299
-----------------------------------------------------

----------------------- SIMD ------------------------
--      USES TyConUniques 300-399
-----------------------------------------------------

#include "primop-vector-uniques.hs-incl"

------------- Type-level Symbol, Nat, Char ----------
--      USES TyConUniques 400-499
-----------------------------------------------------
typeSymbolKindConNameKey, typeCharKindConNameKey,
  typeNatAddTyFamNameKey, typeNatMulTyFamNameKey, typeNatExpTyFamNameKey,
  typeNatSubTyFamNameKey
  , typeSymbolCmpTyFamNameKey, typeNatCmpTyFamNameKey, typeCharCmpTyFamNameKey
  , typeLeqCharTyFamNameKey
  , typeNatDivTyFamNameKey
  , typeNatModTyFamNameKey
  , typeNatLogTyFamNameKey
  , typeConsSymbolTyFamNameKey, typeUnconsSymbolTyFamNameKey
  , typeCharToNatTyFamNameKey, typeNatToCharTyFamNameKey
  , exceptionContextTyConKey, unsafeUnpackJSStringUtf8ShShKey
  :: Unique
typeSymbolKindConNameKey  = mkPreludeTyConUnique 400
typeCharKindConNameKey    = mkPreludeTyConUnique 401
typeNatAddTyFamNameKey    = mkPreludeTyConUnique 402
typeNatMulTyFamNameKey    = mkPreludeTyConUnique 403
typeNatExpTyFamNameKey    = mkPreludeTyConUnique 404
typeNatSubTyFamNameKey    = mkPreludeTyConUnique 405
typeSymbolCmpTyFamNameKey = mkPreludeTyConUnique 406
typeNatCmpTyFamNameKey    = mkPreludeTyConUnique 407
typeCharCmpTyFamNameKey   = mkPreludeTyConUnique 408
typeLeqCharTyFamNameKey   = mkPreludeTyConUnique 409
typeNatDivTyFamNameKey  = mkPreludeTyConUnique 410
typeNatModTyFamNameKey  = mkPreludeTyConUnique 411
typeNatLogTyFamNameKey  = mkPreludeTyConUnique 412
typeConsSymbolTyFamNameKey = mkPreludeTyConUnique 413
typeUnconsSymbolTyFamNameKey = mkPreludeTyConUnique 414
typeCharToNatTyFamNameKey = mkPreludeTyConUnique 415
typeNatToCharTyFamNameKey = mkPreludeTyConUnique 416
constPtrTyConKey = mkPreludeTyConUnique 417

jsvalTyConKey = mkPreludeTyConUnique 418

exceptionContextTyConKey = mkPreludeTyConUnique 420

unsafeUnpackJSStringUtf8ShShKey  = mkPreludeMiscIdUnique 805

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
*                                                                      *
************************************************************************
-}

charDataConKey, consDataConKey, doubleDataConKey, falseDataConKey,
    floatDataConKey, intDataConKey, nilDataConKey,
    ratioDataConKey, stableNameDataConKey, trueDataConKey, wordDataConKey,
    word8DataConKey, ioDataConKey, heqDataConKey,
    eqDataConKey, nothingDataConKey, justDataConKey :: Unique

charDataConKey                          = mkPreludeDataConUnique  1
consDataConKey                          = mkPreludeDataConUnique  2
doubleDataConKey                        = mkPreludeDataConUnique  3
falseDataConKey                         = mkPreludeDataConUnique  4
floatDataConKey                         = mkPreludeDataConUnique  5
intDataConKey                           = mkPreludeDataConUnique  6
nothingDataConKey                       = mkPreludeDataConUnique  7
justDataConKey                          = mkPreludeDataConUnique  8
eqDataConKey                            = mkPreludeDataConUnique  9
nilDataConKey                           = mkPreludeDataConUnique 10
ratioDataConKey                         = mkPreludeDataConUnique 11
word8DataConKey                         = mkPreludeDataConUnique 12
stableNameDataConKey                    = mkPreludeDataConUnique 13
trueDataConKey                          = mkPreludeDataConUnique 14
wordDataConKey                          = mkPreludeDataConUnique 15
ioDataConKey                            = mkPreludeDataConUnique 16
heqDataConKey                           = mkPreludeDataConUnique 18

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

mkDictDataConKey :: Unique
mkDictDataConKey                        = mkPreludeDataConUnique 30

coercibleDataConKey :: Unique
coercibleDataConKey                     = mkPreludeDataConUnique 32

staticPtrDataConKey :: Unique
staticPtrDataConKey                     = mkPreludeDataConUnique 33

staticPtrInfoDataConKey :: Unique
staticPtrInfoDataConKey                 = mkPreludeDataConUnique 34

fingerprintDataConKey :: Unique
fingerprintDataConKey                   = mkPreludeDataConUnique 35

srcLocDataConKey :: Unique
srcLocDataConKey                        = mkPreludeDataConUnique 37

trTyConDataConKey, trModuleDataConKey,
  trNameSDataConKey, trNameDDataConKey,
  trGhcPrimModuleKey :: Unique
trTyConDataConKey                       = mkPreludeDataConUnique 41
trModuleDataConKey                      = mkPreludeDataConUnique 43
trNameSDataConKey                       = mkPreludeDataConUnique 45
trNameDDataConKey                       = mkPreludeDataConUnique 46
trGhcPrimModuleKey                      = mkPreludeDataConUnique 47

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

vecRepDataConKey, sumRepDataConKey,
  tupleRepDataConKey, boxedRepDataConKey :: Unique
vecRepDataConKey                        = mkPreludeDataConUnique 71
tupleRepDataConKey                      = mkPreludeDataConUnique 72
sumRepDataConKey                        = mkPreludeDataConUnique 73
boxedRepDataConKey                      = mkPreludeDataConUnique 74

boxedRepDataConTyConKey, tupleRepDataConTyConKey :: Unique
-- A promoted data constructors (i.e. a TyCon) has
-- the same key as the data constructor itself
boxedRepDataConTyConKey = boxedRepDataConKey
tupleRepDataConTyConKey = tupleRepDataConKey

-- See Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- Includes all nullary-data-constructor reps. Does not
-- include BoxedRep, VecRep, SumRep, TupleRep.
runtimeRepSimpleDataConKeys :: [Unique]
runtimeRepSimpleDataConKeys
  = map mkPreludeDataConUnique [75..87]

liftedDataConKey,unliftedDataConKey :: Unique
liftedDataConKey = mkPreludeDataConUnique 88
unliftedDataConKey = mkPreludeDataConUnique 89

-- See Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- VecCount
vecCountDataConKeys :: [Unique]
vecCountDataConKeys = map mkPreludeDataConUnique [90..95]

-- See Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- VecElem
vecElemDataConKeys :: [Unique]
vecElemDataConKeys = map mkPreludeDataConUnique [96..105]

-- Typeable things
kindRepTyConAppDataConKey, kindRepVarDataConKey, kindRepAppDataConKey,
    kindRepFunDataConKey, kindRepTYPEDataConKey,
    kindRepTypeLitSDataConKey, kindRepTypeLitDDataConKey
    :: Unique
kindRepTyConAppDataConKey = mkPreludeDataConUnique 106
kindRepVarDataConKey      = mkPreludeDataConUnique 107
kindRepAppDataConKey      = mkPreludeDataConUnique 108
kindRepFunDataConKey      = mkPreludeDataConUnique 109
kindRepTYPEDataConKey     = mkPreludeDataConUnique 110
kindRepTypeLitSDataConKey = mkPreludeDataConUnique 111
kindRepTypeLitDDataConKey = mkPreludeDataConUnique 112

typeLitSymbolDataConKey, typeLitNatDataConKey, typeLitCharDataConKey :: Unique
typeLitSymbolDataConKey   = mkPreludeDataConUnique 113
typeLitNatDataConKey      = mkPreludeDataConUnique 114
typeLitCharDataConKey     = mkPreludeDataConUnique 115

-- Unsafe equality
unsafeReflDataConKey :: Unique
unsafeReflDataConKey      = mkPreludeDataConUnique 116

-- Multiplicity

oneDataConKey, manyDataConKey :: Unique
oneDataConKey = mkPreludeDataConUnique 117
manyDataConKey = mkPreludeDataConUnique 118

-- ghc-bignum
integerISDataConKey, integerINDataConKey, integerIPDataConKey,
   naturalNSDataConKey, naturalNBDataConKey :: Unique
integerISDataConKey       = mkPreludeDataConUnique 120
integerINDataConKey       = mkPreludeDataConUnique 121
integerIPDataConKey       = mkPreludeDataConUnique 122
naturalNSDataConKey       = mkPreludeDataConUnique 123
naturalNBDataConKey       = mkPreludeDataConUnique 124


---------------- Template Haskell -------------------
--      GHC.Builtin.Names.TH: USES DataUniques 200-250
-----------------------------------------------------


{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
*                                                                      *
************************************************************************
-}

wildCardKey, absentErrorIdKey, absentConstraintErrorIdKey, augmentIdKey, appendIdKey,
    buildIdKey, foldrIdKey, recSelErrorIdKey,
    seqIdKey, eqStringIdKey,
    noMethodBindingErrorIdKey, nonExhaustiveGuardsErrorIdKey,
    impossibleErrorIdKey, impossibleConstraintErrorIdKey,
    patErrorIdKey, voidPrimIdKey,
    realWorldPrimIdKey, recConErrorIdKey,
    unpackCStringUtf8IdKey, unpackCStringAppendUtf8IdKey, unpackCStringFoldrUtf8IdKey,
    unpackCStringIdKey, unpackCStringAppendIdKey, unpackCStringFoldrIdKey,
    typeErrorIdKey, divIntIdKey, modIntIdKey,
    absentSumFieldErrorIdKey, cstringLengthIdKey
    :: Unique

wildCardKey                    = mkPreludeMiscIdUnique  0  -- See Note [WildCard binders]
absentErrorIdKey               = mkPreludeMiscIdUnique  1
absentConstraintErrorIdKey     = mkPreludeMiscIdUnique  2
augmentIdKey                   = mkPreludeMiscIdUnique  3
appendIdKey                    = mkPreludeMiscIdUnique  4
buildIdKey                     = mkPreludeMiscIdUnique  5
foldrIdKey                     = mkPreludeMiscIdUnique  6
recSelErrorIdKey               = mkPreludeMiscIdUnique  7
seqIdKey                       = mkPreludeMiscIdUnique  8
absentSumFieldErrorIdKey       = mkPreludeMiscIdUnique  9
eqStringIdKey                  = mkPreludeMiscIdUnique 10
noMethodBindingErrorIdKey      = mkPreludeMiscIdUnique 11
nonExhaustiveGuardsErrorIdKey  = mkPreludeMiscIdUnique 12
impossibleErrorIdKey           = mkPreludeMiscIdUnique 13
impossibleConstraintErrorIdKey = mkPreludeMiscIdUnique 14
patErrorIdKey                  = mkPreludeMiscIdUnique 15
realWorldPrimIdKey             = mkPreludeMiscIdUnique 16
recConErrorIdKey               = mkPreludeMiscIdUnique 17

unpackCStringUtf8IdKey        = mkPreludeMiscIdUnique 18
unpackCStringAppendUtf8IdKey  = mkPreludeMiscIdUnique 19
unpackCStringFoldrUtf8IdKey   = mkPreludeMiscIdUnique 20

unpackCStringIdKey            = mkPreludeMiscIdUnique 21
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 22
unpackCStringFoldrIdKey       = mkPreludeMiscIdUnique 23

voidPrimIdKey                 = mkPreludeMiscIdUnique 24
typeErrorIdKey                = mkPreludeMiscIdUnique 25
divIntIdKey                   = mkPreludeMiscIdUnique 26
modIntIdKey                   = mkPreludeMiscIdUnique 27
cstringLengthIdKey            = mkPreludeMiscIdUnique 28

concatIdKey, filterIdKey, zipIdKey,
    bindIOIdKey, returnIOIdKey, newStablePtrIdKey,
    printIdKey, failIOIdKey, nullAddrIdKey, voidArgIdKey,
    otherwiseIdKey, assertIdKey :: Unique
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
otherwiseIdKey                = mkPreludeMiscIdUnique 43
assertIdKey                   = mkPreludeMiscIdUnique 44

leftSectionKey, rightSectionKey :: Unique
leftSectionKey                = mkPreludeMiscIdUnique 45
rightSectionKey               = mkPreludeMiscIdUnique 46

rootMainKey, runMainKey :: Unique
rootMainKey                   = mkPreludeMiscIdUnique 101
runMainKey                    = mkPreludeMiscIdUnique 102

thenIOIdKey, lazyIdKey, assertErrorIdKey, oneShotKey, runRWKey, seqHashKey :: Unique
thenIOIdKey                   = mkPreludeMiscIdUnique 103
lazyIdKey                     = mkPreludeMiscIdUnique 104
assertErrorIdKey              = mkPreludeMiscIdUnique 105
oneShotKey                    = mkPreludeMiscIdUnique 106
runRWKey                      = mkPreludeMiscIdUnique 107

traceKey :: Unique
traceKey                      = mkPreludeMiscIdUnique 108

nospecIdKey :: Unique
nospecIdKey                   = mkPreludeMiscIdUnique 109

inlineIdKey, noinlineIdKey, noinlineConstraintIdKey :: Unique
inlineIdKey                   = mkPreludeMiscIdUnique 120
-- see below

mapIdKey, dollarIdKey, coercionTokenIdKey, considerAccessibleIdKey :: Unique
mapIdKey                = mkPreludeMiscIdUnique 121
dollarIdKey             = mkPreludeMiscIdUnique 123
coercionTokenIdKey      = mkPreludeMiscIdUnique 124
considerAccessibleIdKey = mkPreludeMiscIdUnique 125
noinlineIdKey           = mkPreludeMiscIdUnique 126
noinlineConstraintIdKey = mkPreludeMiscIdUnique 127

integerToFloatIdKey, integerToDoubleIdKey, naturalToFloatIdKey, naturalToDoubleIdKey :: Unique
integerToFloatIdKey    = mkPreludeMiscIdUnique 128
integerToDoubleIdKey   = mkPreludeMiscIdUnique 129
naturalToFloatIdKey    = mkPreludeMiscIdUnique 130
naturalToDoubleIdKey   = mkPreludeMiscIdUnique 131

rationalToFloatIdKey, rationalToDoubleIdKey :: Unique
rationalToFloatIdKey   = mkPreludeMiscIdUnique 132
rationalToDoubleIdKey  = mkPreludeMiscIdUnique 133

seqHashKey             = mkPreludeMiscIdUnique 134

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

-- fromLabel
fromLabelClassOpKey :: Unique
fromLabelClassOpKey = mkPreludeMiscIdUnique 177

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
--      GHC.Builtin.Names.TH: USES IdUniques 200-499
-----------------------------------------------------

-- Used to make `Typeable` dictionaries
mkTyConKey
  , mkTrTypeKey
  , mkTrConKey
  , mkTrAppKey
  , mkTrFunKey
  , typeNatTypeRepKey
  , typeSymbolTypeRepKey
  , typeCharTypeRepKey
  , typeRepIdKey
  :: Unique
mkTyConKey            = mkPreludeMiscIdUnique 503
mkTrTypeKey           = mkPreludeMiscIdUnique 504
mkTrConKey            = mkPreludeMiscIdUnique 505
mkTrAppKey            = mkPreludeMiscIdUnique 506
typeNatTypeRepKey     = mkPreludeMiscIdUnique 507
typeSymbolTypeRepKey  = mkPreludeMiscIdUnique 508
typeCharTypeRepKey    = mkPreludeMiscIdUnique 509
typeRepIdKey          = mkPreludeMiscIdUnique 510
mkTrFunKey            = mkPreludeMiscIdUnique 511

-- KindReps for common cases
starKindRepKey, starArrStarKindRepKey, starArrStarArrStarKindRepKey, constraintKindRepKey :: Unique
starKindRepKey               = mkPreludeMiscIdUnique 520
starArrStarKindRepKey        = mkPreludeMiscIdUnique 521
starArrStarArrStarKindRepKey = mkPreludeMiscIdUnique 522
constraintKindRepKey         = mkPreludeMiscIdUnique 523

-- Dynamic
toDynIdKey :: Unique
toDynIdKey            = mkPreludeMiscIdUnique 530


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

emptyExceptionContextKey :: Unique
emptyExceptionContextKey = mkPreludeMiscIdUnique 562

-- Unsafe coercion proofs
unsafeEqualityProofIdKey, unsafeCoercePrimIdKey :: Unique
unsafeEqualityProofIdKey = mkPreludeMiscIdUnique 570
unsafeCoercePrimIdKey    = mkPreludeMiscIdUnique 571

-- HasField class ops
getFieldClassOpKey, setFieldClassOpKey :: Unique
getFieldClassOpKey = mkPreludeMiscIdUnique 572
setFieldClassOpKey = mkPreludeMiscIdUnique 573

-- "Unsatisfiable" constraints
unsatisfiableIdNameKey :: Unique
unsatisfiableIdNameKey = mkPreludeMiscIdUnique 580

------------------------------------------------------
-- ghc-bignum uses 600-699 uniques
------------------------------------------------------

integerFromNaturalIdKey
   , integerToNaturalClampIdKey
   , integerToNaturalThrowIdKey
   , integerToNaturalIdKey
   , integerToWordIdKey
   , integerToIntIdKey
   , integerToWord64IdKey
   , integerToInt64IdKey
   , integerAddIdKey
   , integerMulIdKey
   , integerSubIdKey
   , integerNegateIdKey
   , integerAbsIdKey
   , integerPopCountIdKey
   , integerQuotIdKey
   , integerRemIdKey
   , integerDivIdKey
   , integerModIdKey
   , integerDivModIdKey
   , integerQuotRemIdKey
   , integerEncodeFloatIdKey
   , integerEncodeDoubleIdKey
   , integerGcdIdKey
   , integerLcmIdKey
   , integerAndIdKey
   , integerOrIdKey
   , integerXorIdKey
   , integerComplementIdKey
   , integerBitIdKey
   , integerTestBitIdKey
   , integerShiftLIdKey
   , integerShiftRIdKey
   , integerFromWordIdKey
   , integerFromWord64IdKey
   , integerFromInt64IdKey
   , naturalToWordIdKey
   , naturalPopCountIdKey
   , naturalShiftRIdKey
   , naturalShiftLIdKey
   , naturalAddIdKey
   , naturalSubIdKey
   , naturalSubThrowIdKey
   , naturalSubUnsafeIdKey
   , naturalMulIdKey
   , naturalQuotRemIdKey
   , naturalQuotIdKey
   , naturalRemIdKey
   , naturalAndIdKey
   , naturalAndNotIdKey
   , naturalOrIdKey
   , naturalXorIdKey
   , naturalTestBitIdKey
   , naturalBitIdKey
   , naturalGcdIdKey
   , naturalLcmIdKey
   , naturalLog2IdKey
   , naturalLogBaseWordIdKey
   , naturalLogBaseIdKey
   , naturalPowModIdKey
   , naturalSizeInBaseIdKey
   , bignatEqIdKey
   , bignatCompareIdKey
   , bignatCompareWordIdKey
   :: Unique

integerFromNaturalIdKey    = mkPreludeMiscIdUnique 600
integerToNaturalClampIdKey = mkPreludeMiscIdUnique 601
integerToNaturalThrowIdKey = mkPreludeMiscIdUnique 602
integerToNaturalIdKey      = mkPreludeMiscIdUnique 603
integerToWordIdKey         = mkPreludeMiscIdUnique 604
integerToIntIdKey          = mkPreludeMiscIdUnique 605
integerToWord64IdKey       = mkPreludeMiscIdUnique 606
integerToInt64IdKey        = mkPreludeMiscIdUnique 607
integerAddIdKey            = mkPreludeMiscIdUnique 608
integerMulIdKey            = mkPreludeMiscIdUnique 609
integerSubIdKey            = mkPreludeMiscIdUnique 610
integerNegateIdKey         = mkPreludeMiscIdUnique 611
integerAbsIdKey            = mkPreludeMiscIdUnique 618
integerPopCountIdKey       = mkPreludeMiscIdUnique 621
integerQuotIdKey           = mkPreludeMiscIdUnique 622
integerRemIdKey            = mkPreludeMiscIdUnique 623
integerDivIdKey            = mkPreludeMiscIdUnique 624
integerModIdKey            = mkPreludeMiscIdUnique 625
integerDivModIdKey         = mkPreludeMiscIdUnique 626
integerQuotRemIdKey        = mkPreludeMiscIdUnique 627
integerEncodeFloatIdKey    = mkPreludeMiscIdUnique 630
integerEncodeDoubleIdKey   = mkPreludeMiscIdUnique 631
integerGcdIdKey            = mkPreludeMiscIdUnique 632
integerLcmIdKey            = mkPreludeMiscIdUnique 633
integerAndIdKey            = mkPreludeMiscIdUnique 634
integerOrIdKey             = mkPreludeMiscIdUnique 635
integerXorIdKey            = mkPreludeMiscIdUnique 636
integerComplementIdKey     = mkPreludeMiscIdUnique 637
integerBitIdKey            = mkPreludeMiscIdUnique 638
integerTestBitIdKey        = mkPreludeMiscIdUnique 639
integerShiftLIdKey         = mkPreludeMiscIdUnique 640
integerShiftRIdKey         = mkPreludeMiscIdUnique 641
integerFromWordIdKey       = mkPreludeMiscIdUnique 642
integerFromWord64IdKey     = mkPreludeMiscIdUnique 643
integerFromInt64IdKey      = mkPreludeMiscIdUnique 644

naturalToWordIdKey         = mkPreludeMiscIdUnique 650
naturalPopCountIdKey       = mkPreludeMiscIdUnique 659
naturalShiftRIdKey         = mkPreludeMiscIdUnique 660
naturalShiftLIdKey         = mkPreludeMiscIdUnique 661
naturalAddIdKey            = mkPreludeMiscIdUnique 662
naturalSubIdKey            = mkPreludeMiscIdUnique 663
naturalSubThrowIdKey       = mkPreludeMiscIdUnique 664
naturalSubUnsafeIdKey      = mkPreludeMiscIdUnique 665
naturalMulIdKey            = mkPreludeMiscIdUnique 666
naturalQuotRemIdKey        = mkPreludeMiscIdUnique 669
naturalQuotIdKey           = mkPreludeMiscIdUnique 670
naturalRemIdKey            = mkPreludeMiscIdUnique 671
naturalAndIdKey            = mkPreludeMiscIdUnique 672
naturalAndNotIdKey         = mkPreludeMiscIdUnique 673
naturalOrIdKey             = mkPreludeMiscIdUnique 674
naturalXorIdKey            = mkPreludeMiscIdUnique 675
naturalTestBitIdKey        = mkPreludeMiscIdUnique 676
naturalBitIdKey            = mkPreludeMiscIdUnique 677
naturalGcdIdKey            = mkPreludeMiscIdUnique 678
naturalLcmIdKey            = mkPreludeMiscIdUnique 679
naturalLog2IdKey           = mkPreludeMiscIdUnique 680
naturalLogBaseWordIdKey    = mkPreludeMiscIdUnique 681
naturalLogBaseIdKey        = mkPreludeMiscIdUnique 682
naturalPowModIdKey         = mkPreludeMiscIdUnique 683
naturalSizeInBaseIdKey     = mkPreludeMiscIdUnique 684

bignatEqIdKey              = mkPreludeMiscIdUnique 691
bignatCompareIdKey         = mkPreludeMiscIdUnique 692
bignatCompareWordIdKey     = mkPreludeMiscIdUnique 693


------------------------------------------------------
-- ghci optimization for big rationals 700-749 uniques
------------------------------------------------------

-- Creating rationals at runtime.
mkRationalBase2IdKey, mkRationalBase10IdKey :: Unique
mkRationalBase2IdKey  = mkPreludeMiscIdUnique 700
mkRationalBase10IdKey = mkPreludeMiscIdUnique 701 :: Unique

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
(@GHC.Tc.Deriv@).
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

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

 - Knowledge about known-key names.
     knownKeyUniqMap, knownkeyOccMap, basicKnownKeyTable, etc
   See Note [Overview of known-key entities] in GHC.Builtin

 - RdrNames for Ids, DataCons etc that the compiler may emit into
   generated code (e.g. for deriving).
        e.g.    and_RDR :: RdrName
   It's not necessary to know the uniques for these guys, only their names


Note [Known-key names]   <---- OLD VERSION
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
problem: we can't add them to the wiredInNames finite map which we use to
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
import GHC.Types.Name
import GHC.Types.SrcLoc

import GHC.Builtin.Uniques

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
o*                                                                      *
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

basicKnownKeyTable :: [(OccName, KnownKey)]
basicKnownKeyTable
  = [ (rationalTyConOcc,     rationalTyConKey)
    , (mkTcOcc "Show",         showClassKey)
    , (mkTcOcc "Foldable",     foldableClassKey)
    , (mkTcOcc "Traversable",  traversableClassKey)
    , (mkTcOcc "Bounded",      boundedClassKey)
    , (mkTcOcc "Integral",     integralClassKey)
    , (mkTcOcc "Real",         realClassKey)
    , (mkTcOcc "Data",         dataClassKey)
    , (mkTcOcc "Ix",           ixClassKey)
    , (mkTcOcc "Alternative",  alternativeClassKey)
    , (mkTcOcc "Typeable",     typeableClassKey)

    -- Misc
    , (mkVarOcc ".",           composeIdKey)

    -- Class Eq and Ord
    , (mkTcOcc "Eq",           eqClassKey)
    , (mkTcOcc "Ord",          ordClassKey)
    , (mkVarOcc "==",          eqClassOpKey)
    , (mkVarOcc ">=",          geClassOpKey)
    , (mkVarOcc "<=",          leClassOpKey)
    , (mkVarOcc "<",           ltClassOpKey)
    , (mkVarOcc ">",           gtClassOpKey)
    , (mkVarOcc "compare",     compareClassOpKey)

    -- Enum
    , (mkTcOcc "Enum",            enumClassKey)
    , (mkVarOcc "enumFrom",       enumFromClassOpKey)
    , (mkVarOcc "enumFromThen",   enumFromThenClassOpKey)
    , (mkVarOcc "enumFromTo",     enumFromToClassOpKey)
    , (mkVarOcc "enumFromThenTo", enumFromThenToClassOpKey)

    -- Numeric operations
    , (mkTcOcc "Num",               numClassKey)
    , (mkVarOcc "-",                minusClassOpKey)
    , (mkVarOcc "negate",           negateClassOpKey)
    , (mkVarOcc "fromInteger",      fromIntegerClassOpKey)
    , (mkVarOcc "fromRational",     fromRationalClassOpKey)
    , (mkVarOcc "mkRationalBase2",  mkRationalBase2IdKey)
    , (mkVarOcc "mkRationalBase10", mkRationalBase10IdKey)
    , (mkVarOcc "divInt#",          divIntIdKey)
    , (mkVarOcc "modInt#",          modIntIdKey)

    -- Class Functor
    , (mkTcOcc "Functor",     functorClassKey)
    , (mkVarOcc "fmap",       fmapClassOpKey)
    , (mkVarOcc "map",        mapIdKey)

    -- Class Monad, MonadFix, MonadZip
    , (mkTcOcc "Monad",        monadClassKey)
    , (thenMClassOpOcc,        thenMClassOpKey)
    , (mkVarOcc ">>=",         bindMClassOpKey)
    , (returnMClassOpOcc,      returnMClassOpKey)
    , (mkVarOcc "fail",        failMClassOpKey)
    , (mkVarOcc "guard",       guardMIdKey)
    , (mkVarOcc "mfix",        mfixIdKey)
    , (mkVarOcc "join",        joinMIdKey)

    -- Class Applicative
    , (mkTcOcc "Applicative",  applicativeClassKey)
    , (mkVarOcc "mzip",        mzipIdKey)
    , (mkVarOcc "<*>",         apAClassOpKey)
    , (pureAClassOpOcc,        pureAClassOpKey)
    , (thenAClassOpOcc,        thenAClassOpKey)

    -- Class Semigroup, Monoid
    , (mkTcOcc "Semigroup",    semigroupClassKey)
    , (mkTcOcc "Monoid",       monoidClassKey)
    , (sappendClassOpOcc,      sappendClassOpKey)
    , (mappendClassOpOcc,      mappendClassOpKey)
    , (mkVarOcc "mempty",      memptyClassOpKey)

    -- Class IsString
    , (mkTcOcc "IsString",    isStringClassKey)
    , (mkVarOcc "fromString", fromStringClassOpKey)

    -- DataToTag
    , (mkTcOcc "DataToTag",   dataToTagClassKey)
    , (mkVarOcc "dataToTag#", dataToTagClassOpKey)

    -- Lists
    , (mkVarOcc "foldr", foldrIdKey)
    , (mkVarOcc "build",  buildIdKey)

    -- Records
    , (mkTcOcc "HasField",   hasFieldClassKey)
    , (mkVarOcc "fromLabel", fromLabelClassOpKey)
    , (mkVarOcc "getField",  getFieldClassOpKey)
    -- setField is not yet defined in ghc-internal
    -- , (mkVarOcc "setField",  setFieldClassOpKey)

    -- FromList
    , (mkVarOcc "fromList",   fromListClassOpKey)
    , (mkVarOcc "fromListN",  fromListNClassOpKey)
    , (mkVarOcc "toList",     toListClassOpKey)

    -- Arrows
    , (mkVarOcc "arr",        arrAIdKey)
    , (mkVarOcc ">>>",        composeAIdKey)
    , (mkVarOcc "first",      firstAIdKey)
    , (mkVarOcc "app",        appAIdKey)
    , (mkVarOcc "|||",        choiceAIdKey)
    , (mkVarOcc "loop",       loopAIdKey)

    -- IO monad
    , (mkVarOcc "thenIO",   thenIOIdKey)
    , (mkVarOcc "bindIO",   bindIOIdKey)
    , (mkVarOcc "returnIO", returnIOIdKey)
    , (mkVarOcc "print",    printIdKey)

    -- Generics
    , (mkTcOcc "Generic",   genClassKey)
    , (mkTcOcc "Generic1",  gen1ClassKey)

    -- Static pointers
    , (mkVarOcc "fromStaticPtr", fromStaticPtrClassOpKey)
    , (mkVarOcc "makeStatic",    makeStaticKey)

    -- Unsatisfiable class
    , (mkTcOcc  "Unsatisfiable", unsatisfiableClassKey)
    , (mkVarOcc "unsatisfiable", unsatisfiableIdKey)

    -- Known-key names that have BuiltinRules in ConstantFold
    , (mkVarOcc "unpackFoldrCString#",      unpackCStringFoldrIdKey)
    , (mkVarOcc "unpackFoldrCStringUtf8#",  unpackCStringFoldrUtf8IdKey)
    , (mkVarOcc "unpackAppendCString#",     unpackCStringAppendIdKey)
    , (mkVarOcc "unpackAppendCStringUtf8#", unpackCStringAppendUtf8IdKey)
    , (mkVarOcc "cstringLength#",           cstringLengthIdKey)

    , (mkVarOcc "eqString",                 eqStringIdKey)
    , (mkVarOcc "inline",                   inlineIdKey)

    -- Unsafe equality proofs
    , (mkVarOcc "unsafeEqualityProof",      unsafeEqualityProofIdKey)
    , (mkTcOcc  "UnsafeEquality",           unsafeEqualityTyConKey)
    , (mkDataOcc "UnsafeRefl",              unsafeReflDataConKey)

    -- Bignum operations, have BuiltinRules in ConstantFold
    , (mkVarOcc "bigNatEq#",                 bignatEqIdKey)
    , (mkVarOcc "bigNatCompare",             bignatCompareIdKey)
    , (mkVarOcc "bigNatCompareWord#",        bignatCompareWordIdKey)
    , (mkVarOcc "naturalToWord#",            naturalToWordIdKey)
    , (mkVarOcc "naturalPopCount#",          naturalPopCountIdKey)
    , (mkVarOcc "naturalShiftR#",            naturalShiftRIdKey)
    , (mkVarOcc "naturalShiftL#",            naturalShiftLIdKey)
    , (mkVarOcc "naturalAdd",                naturalAddIdKey)
    , (mkVarOcc "naturalSub",                naturalSubIdKey)
    , (mkVarOcc "naturalSubThrow",           naturalSubThrowIdKey)
    , (mkVarOcc "naturalSubUnsafe",          naturalSubUnsafeIdKey)
    , (mkVarOcc "naturalMul",                naturalMulIdKey)
    , (mkVarOcc "naturalQuotRem#",           naturalQuotRemIdKey)
    , (mkVarOcc "naturalQuot",               naturalQuotIdKey)
    , (mkVarOcc "naturalRem",                naturalRemIdKey)
    , (mkVarOcc "naturalAnd",                naturalAndIdKey)
    , (mkVarOcc "naturalOr",                 naturalOrIdKey)
    , (mkVarOcc "naturalXor",                naturalXorIdKey)
    , (mkVarOcc "naturalTestBit#",           naturalTestBitIdKey)
    , (mkVarOcc "naturalBit#",               naturalBitIdKey)
    , (mkVarOcc "naturalGcd",                naturalGcdIdKey)
    , (mkVarOcc "naturalLcm",                naturalLcmIdKey)
    , (mkVarOcc "integerFromNatural",        integerFromNaturalIdKey)
    , (mkVarOcc "integerToNaturalClamp",     integerToNaturalClampIdKey)
    , (mkVarOcc "integerToNaturalThrow",     integerToNaturalThrowIdKey)
    , (mkVarOcc "integerToNatural",          integerToNaturalIdKey)
    , (mkVarOcc "integerToWord#",            integerToWordIdKey)
    , (mkVarOcc "integerToInt#",             integerToIntIdKey)
    , (mkVarOcc "integerToWord64#",          integerToWord64IdKey)
    , (mkVarOcc "integerToInt64#",           integerToInt64IdKey)
    , (mkVarOcc "integerFromWord#",          integerFromWordIdKey)
    , (mkVarOcc "integerFromWord64#",        integerFromWord64IdKey)
    , (mkVarOcc "integerFromInt64#",         integerFromInt64IdKey)
    , (mkVarOcc "integerAdd",                integerAddIdKey)
    , (mkVarOcc "integerMul",                integerMulIdKey)
    , (mkVarOcc "integerSub",                integerSubIdKey)
    , (mkVarOcc "integerNegate",             integerNegateIdKey)
    , (mkVarOcc "integerAbs",                integerAbsIdKey)
    , (mkVarOcc "integerPopCount#",          integerPopCountIdKey)
    , (mkVarOcc "integerQuot",               integerQuotIdKey)
    , (mkVarOcc "integerRem",                integerRemIdKey)
    , (mkVarOcc "integerDiv",                integerDivIdKey)
    , (mkVarOcc "integerMod",                integerModIdKey)
    , (mkVarOcc "integerDivMod#",            integerDivModIdKey)
    , (mkVarOcc "integerQuotRem#",           integerQuotRemIdKey)
    , (mkVarOcc "integerEncodeFloat#",       integerEncodeFloatIdKey)
    , (mkVarOcc "integerEncodeDouble#",      integerEncodeDoubleIdKey)
    , (mkVarOcc "integerGcd",                integerGcdIdKey)
    , (mkVarOcc "integerLcm",                integerLcmIdKey)
    , (mkVarOcc "integerAnd",                integerAndIdKey)
    , (mkVarOcc "integerOr",                 integerOrIdKey)
    , (mkVarOcc "integerXor",                integerXorIdKey)
    , (mkVarOcc "integerComplement",         integerComplementIdKey)
    , (mkVarOcc "integerBit#",               integerBitIdKey)
    , (mkVarOcc "integerTestBit#",           integerTestBitIdKey)
    , (mkVarOcc "integerShiftL#",            integerShiftLIdKey)
    , (mkVarOcc "integerShiftR#",            integerShiftRIdKey)
    ]

basicKnownKeyNames :: [Name]  -- See Note [Known-key names]
basicKnownKeyNames
 = genericTyConNames
 ++ [   --  Classes.  *Must* include:
        --      classes that are grabbed by key (e.g., eqClassKey)
        --      classes in "Class.standardClassKeys" (quite a few)
        -- The IO type
        ioTyConName, ioDataConName,
        runMainIOName,
        runRWName,

        -- Type representation types
        trModuleTyConName, trModuleDataConName,
        trNameSDataConName,
        trTyConTyConName, trTyConDataConName,

        -- Typeable
        someTypeRepTyConName,          -- known-occ
        someTypeRepDataConName,   -- ditto
        kindRepTyConName,
        kindRepTyConAppDataConName,
        kindRepVarDataConName,
        kindRepAppDataConName,
        kindRepFunDataConName,
        kindRepTYPEDataConName,
        kindRepTypeLitSDataConName,
        typeLitSymbolDataConName,
        typeLitNatDataConName,
        typeLitCharDataConName,
        typeRepIdName,
        mkTrConName,
        mkTrAppCheckedName,
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

        -- seq#
        seqHashName,

        -- Dynamic
        toDynName,

        -- Conversion functions
        ratioTyConName, ratioDataConName,
        toIntegerName, toRationalName,
        fromIntegralName, realToFracName,

        -- String stuff
        fromStringName,

        -- Monad stuff
        bindMName,

        -- Read stuff
        readClassName,

        -- Stable pointers
        newStablePtrName,

        -- GHC Extensions
        considerAccessibleName,

        -- Strings and lists
        unpackCStringName, unpackCStringUtf8Name,

        -- Non-empty lists
        nonEmptyTyConName,

        -- FFI primitive types that are not wired-in.
        stablePtrTyConName, ptrTyConName, funPtrTyConName, constPtrConName,
        int8TyConName, int16TyConName, int32TyConName, int64TyConName,
        word8TyConName, word16TyConName, word32TyConName, word64TyConName,
        jsvalTyConName,

        -- Others
        otherwiseIdName,
        assertName,
        assertErrorName, traceName,
        printName,
        dollarName,

        -- Float/Double
        integerToFloatName,
        integerToDoubleName,
        rationalToFloatName,
        rationalToDoubleName,

        -- Type-level naturals
        knownNatClassName, knownSymbolClassName, knownCharClassName,

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

        -- GHCi Sandbox
        , ghciIoClassName, ghciStepIoMName

        -- StaticPtr
        , staticPtrTyConName
        , staticPtrDataConName, staticPtrInfoDataConName

        -- Custom type errors
        , errorMessageTypeErrorFamName
        , typeErrorTextDataConName
        , typeErrorAppendDataConName
        , typeErrorVAppendDataConName
        , typeErrorShowTypeDataConName

        -- Unsafe coercion proofs
        , unsafeCoercePrimName

        , unsafeUnpackJSStringUtf8ShShName
    ]

genericTyConNames :: [Name]
genericTyConNames = [
    v1TyConName, u1TyConName, par1TyConName, rec1TyConName, sumTyConName,
    prodTyConName, compTyConName, rec0TyConName, d1TyConName, c1TyConName,
    s1TyConName, repTyConName, rep1TyConName,
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
    gHC_TYPES, gHC_INTERNAL_DATA_DATA, gHC_MAGIC, gHC_MAGIC_DICT,
    gHC_CLASSES, gHC_CLASSES_IP, gHC_PRIMOPWRAPPERS :: Module
gHC_PRIM           = mkGhcInternalModule (fsLit "GHC.Internal.Prim")   -- Primitive types and values
gHC_PRIM_PANIC     = mkGhcInternalModule (fsLit "GHC.Internal.Prim.Panic")
gHC_TYPES          = mkGhcInternalModule (fsLit "GHC.Internal.Types")
gHC_MAGIC          = mkGhcInternalModule (fsLit "GHC.Internal.Magic")
gHC_MAGIC_DICT     = mkGhcInternalModule (fsLit "GHC.Internal.Magic.Dict")
gHC_CSTRING        = mkGhcInternalModule (fsLit "GHC.Internal.CString")
gHC_CLASSES        = mkGhcInternalModule (fsLit "GHC.Internal.Classes")
gHC_CLASSES_IP     = mkGhcInternalModule (fsLit "GHC.Internal.Classes.IP")
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
    gHC_INTERNAL_ARROW, gHC_INTERNAL_DESUGAR, gHC_INTERNAL_RANDOM, gHC_INTERNAL_EXTS,
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

pRELUDE_NAME, mAIN_NAME, kNOWN_KEY_NAMES :: ModuleName
pRELUDE_NAME    = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME       = mkModuleNameFS (fsLit "Main")
kNOWN_KEY_NAMES = mkModuleNameFS (fsLit "GHC.KnownKeyNames")


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


error_RDR :: RdrName
error_RDR = varQual_RDR gHC_INTERNAL_ERR (fsLit "error")



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
  sumTyConName, prodTyConName, compTyConName, rec0TyConName, d1TyConName,
  c1TyConName, s1TyConName, repTyConName, rep1TyConName,
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

sumTyConName    = tcQual gHC_INTERNAL_GENERICS (fsLit ":+:") sumTyConKey
prodTyConName   = tcQual gHC_INTERNAL_GENERICS (fsLit ":*:") prodTyConKey
compTyConName   = tcQual gHC_INTERNAL_GENERICS (fsLit ":.:") compTyConKey

rec0TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "Rec0") rec0TyConKey
d1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "D1") d1TyConKey
c1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "C1") c1TyConKey
s1TyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "S1") s1TyConKey

repTyConName  = tcQual gHC_INTERNAL_GENERICS (fsLit "Rep")  repTyConKey
rep1TyConName = tcQual gHC_INTERNAL_GENERICS (fsLit "Rep1") rep1TyConKey

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

-- Base strings Strings
unpackCStringName, unpackCStringUtf8Name :: Name
unpackCStringName       = varQual gHC_CSTRING (fsLit "unpackCString#") unpackCStringIdKey
unpackCStringUtf8Name       = varQual gHC_CSTRING (fsLit "unpackCStringUtf8#") unpackCStringUtf8IdKey

-- Class Monad
bindMName  :: Name
bindMName          = varQual gHC_INTERNAL_BASE (fsLit ">>=")    bindMClassOpKey

-- Class MonadFail
failMName :: Name
failMName          = varQual gHC_INTERNAL_MONAD_FAIL (fsLit "fail")      failMClassOpKey

-- Classes (Foldable, Traversable)
traversableClassName :: Name
traversableClassName  = clsQual  gHC_INTERNAL_DATA_TRAVERSABLE    (fsLit "Traversable") traversableClassKey

-- AMP additions
joinMIdKey, apAClassOpKey, pureAClassOpKey, thenAClassOpKey,
    alternativeClassKey :: KnownKey
joinMIdKey          = mkPreludeMiscIdUnique 750
apAClassOpKey       = mkPreludeMiscIdUnique 751 -- <*>
pureAClassOpKey     = mkPreludeMiscIdUnique 752
thenAClassOpKey     = mkPreludeMiscIdUnique 753
alternativeClassKey = mkPreludeMiscIdUnique 754


-- Functions for GHC extensions
considerAccessibleName :: Name
considerAccessibleName = varQual gHC_MAGIC (fsLit "considerAccessible") considerAccessibleIdKey

-- Random GHC.Internal.Base functions
fromStringName, otherwiseIdName,
    assertName,
    dollarName :: Name
dollarName        = varQual gHC_INTERNAL_BASE (fsLit "$")          dollarIdKey
otherwiseIdName   = varQual gHC_INTERNAL_BASE (fsLit "otherwise")  otherwiseIdKey
assertName        = varQual gHC_INTERNAL_BASE (fsLit "assert")     assertIdKey
fromStringName    = varQual gHC_INTERNAL_DATA_STRING (fsLit "fromString") fromStringClassOpKey

bnbVarQual, bnnVarQual, bniVarQual :: String -> Unique -> Name
bnbVarQual str key = varQual gHC_INTERNAL_NUM_BIGNAT  (fsLit str) key
bnnVarQual str key = varQual gHC_INTERNAL_NUM_NATURAL (fsLit str) key
bniVarQual str key = varQual gHC_INTERNAL_NUM_INTEGER (fsLit str) key



---------------------------------
-- End of ghc-bignum
---------------------------------

-- GHC.Internal.Real types and classes
ratioTyConName, ratioDataConName,
    fromRationalName, toIntegerName, toRationalName, fromIntegralName,
    realToFracName :: Name
ratioTyConName      = tcQual  gHC_INTERNAL_REAL (fsLit "Ratio")        ratioTyConKey
ratioDataConName    = dcQual  gHC_INTERNAL_REAL (fsLit ":%")           ratioDataConKey
fromRationalName    = varQual gHC_INTERNAL_REAL (fsLit "fromRational") fromRationalClassOpKey
toIntegerName       = varQual gHC_INTERNAL_REAL (fsLit "toInteger")    toIntegerClassOpKey
toRationalName      = varQual gHC_INTERNAL_REAL (fsLit "toRational")   toRationalClassOpKey
fromIntegralName    = varQual  gHC_INTERNAL_REAL (fsLit "fromIntegral")fromIntegralIdKey
realToFracName      = varQual  gHC_INTERNAL_REAL (fsLit "realToFrac")  realToFracIdKey

-- other GHC.Internal.Float functions
integerToFloatName, integerToDoubleName,
  rationalToFloatName, rationalToDoubleName :: Name
integerToFloatName   = varQual gHC_INTERNAL_FLOAT (fsLit "integerToFloat#") integerToFloatIdKey
integerToDoubleName  = varQual gHC_INTERNAL_FLOAT (fsLit "integerToDouble#") integerToDoubleIdKey
rationalToFloatName  = varQual gHC_INTERNAL_FLOAT (fsLit "rationalToFloat#") rationalToFloatIdKey
rationalToDoubleName = varQual gHC_INTERNAL_FLOAT (fsLit "rationalToDouble#") rationalToDoubleIdKey

-- Typeable representation types
trModuleTyConName
  , trModuleDataConName
  , trNameSDataConName
  , trTyConTyConName
  , trTyConDataConName
  :: Name
trModuleTyConName     = tcQual gHC_TYPES          (fsLit "Module")         trModuleTyConKey
trModuleDataConName   = dcQual gHC_TYPES          (fsLit "Module")         trModuleDataConKey
trNameSDataConName    = dcQual gHC_TYPES          (fsLit "TrNameS")        trNameSDataConKey
trTyConTyConName      = tcQual gHC_TYPES          (fsLit "TyCon")          trTyConTyConKey
trTyConDataConName    = dcQual gHC_TYPES          (fsLit "TyCon")          trTyConDataConKey

kindRepTyConName
  , kindRepTyConAppDataConName
  , kindRepVarDataConName
  , kindRepAppDataConName
  , kindRepFunDataConName
  , kindRepTYPEDataConName
  , kindRepTypeLitSDataConName
  :: Name
kindRepTyConName      = tcQual gHC_TYPES          (fsLit "KindRep")        kindRepTyConKey
kindRepTyConAppDataConName = dcQual gHC_TYPES     (fsLit "KindRepTyConApp") kindRepTyConAppDataConKey
kindRepVarDataConName = dcQual gHC_TYPES          (fsLit "KindRepVar")     kindRepVarDataConKey
kindRepAppDataConName = dcQual gHC_TYPES          (fsLit "KindRepApp")     kindRepAppDataConKey
kindRepFunDataConName = dcQual gHC_TYPES          (fsLit "KindRepFun")     kindRepFunDataConKey
kindRepTYPEDataConName = dcQual gHC_TYPES         (fsLit "KindRepTYPE")    kindRepTYPEDataConKey
kindRepTypeLitSDataConName = dcQual gHC_TYPES     (fsLit "KindRepTypeLitS") kindRepTypeLitSDataConKey

typeLitSymbolDataConName
  , typeLitNatDataConName
  , typeLitCharDataConName
  :: Name
typeLitSymbolDataConName = dcQual gHC_TYPES       (fsLit "TypeLitSymbol")  typeLitSymbolDataConKey
typeLitNatDataConName    = dcQual gHC_TYPES       (fsLit "TypeLitNat")     typeLitNatDataConKey
typeLitCharDataConName   = dcQual gHC_TYPES       (fsLit "TypeLitChar")    typeLitCharDataConKey

-- Class Typeable, and functions for constructing `Typeable` dictionaries
someTypeRepTyConName
  , someTypeRepDataConName
  , mkTrConName
  , mkTrAppCheckedName
  , mkTrFunName
  , typeRepIdName
  , typeNatTypeRepName
  , typeSymbolTypeRepName
  , typeCharTypeRepName
  , trGhcPrimModuleName
  :: Name
someTypeRepTyConName   = tcQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "SomeTypeRep")    someTypeRepTyConKey
someTypeRepDataConName = dcQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "SomeTypeRep")    someTypeRepDataConKey
typeRepIdName         = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "typeRep#")       typeRepIdKey
mkTrConName           = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "mkTrCon")        mkTrConKey
mkTrAppCheckedName    = varQual gHC_INTERNAL_TYPEABLE_INTERNAL (fsLit "mkTrAppChecked") mkTrAppCheckedKey
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

-- Unsafe coercion proofs
unsafeCoercePrimName:: Name
unsafeCoercePrimName    = varQual gHC_INTERNAL_UNSAFE_COERCE (fsLit "unsafeCoerce#") unsafeCoercePrimIdKey

-- Dynamic
toDynName :: Name
toDynName = varQual gHC_INTERNAL_DYNAMIC (fsLit "toDyn") toDynIdKey

-- Error module
assertErrorName    :: Name
assertErrorName   = varQual gHC_INTERNAL_IO_Exception (fsLit "assertError") assertErrorIdKey

-- GHC.Internal.Debug.Trace
traceName          :: Name
traceName         = varQual gHC_INTERNAL_DEBUG_TRACE (fsLit "trace") traceKey

-- Class Read
readClassName :: Name
readClassName   = clsQual gHC_INTERNAL_READ (fsLit "Read")      readClassKey

genericClassKeys :: [KnownKey]
genericClassKeys = [genClassKey, gen1ClassKey]

-- GHCi things
ghciIoClassName, ghciStepIoMName :: Name
ghciIoClassName = clsQual gHC_INTERNAL_GHCI (fsLit "GHCiSandboxIO") ghciIoClassKey
ghciStepIoMName = varQual gHC_INTERNAL_GHCI (fsLit "ghciStepIO") ghciStepIoMClassOpKey

-- IO things
ioTyConName, ioDataConName :: Name
ioTyConName       = tcQual  gHC_TYPES (fsLit "IO")       ioTyConKey
ioDataConName     = dcQual  gHC_TYPES (fsLit "IO")       ioDataConKey

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

-- Annotation type checking
toAnnotationWrapperName :: Name
toAnnotationWrapperName = varQual gHC_INTERNAL_DESUGAR (fsLit "toAnnotationWrapper") toAnnotationWrapperIdKey

-- Type-level naturals
knownNatClassName :: Name
knownNatClassName     = clsQual gHC_INTERNAL_TYPENATS (fsLit "KnownNat") knownNatClassKey
knownSymbolClassName :: Name
knownSymbolClassName  = clsQual gHC_INTERNAL_TYPELITS (fsLit "KnownSymbol") knownSymbolClassKey
knownCharClassName :: Name
knownCharClassName  = clsQual gHC_INTERNAL_TYPELITS (fsLit "KnownChar") knownCharClassKey

-- Implicit Parameters
ipClassName :: Name
ipClassName
  = clsQual gHC_CLASSES_IP (fsLit "IP") ipClassKey

-- Overloaded record fields
hasFieldClassName :: Name
hasFieldClassName
 = clsQual gHC_INTERNAL_RECORDS (fsLit "HasField") hasFieldClassKey

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


{- *********************************************************************
*                                                                      *
                 Statically-known occurrence names
*                                                                      *
********************************************************************* -}

rationalTyConOcc :: KnownOcc
rationalTyConOcc = mkTcOcc "Rational"

sappendClassOpOcc, pureAClassOpOcc, thenAClassOpOcc,
  returnMClassOpOcc, thenMClassOpOcc, mappendClassOpOcc :: KnownOcc
sappendClassOpOcc = mkVarOcc "<>"
pureAClassOpOcc   = mkVarOcc "pure"
returnMClassOpOcc = mkVarOcc "return"
thenMClassOpOcc   = mkVarOcc ">>"
thenAClassOpOcc   = mkVarOcc "*>"
mappendClassOpOcc = mkVarOcc "mappend"


{- *********************************************************************
*                                                                      *
                 Statically-known keys
*                                                                      *
********************************************************************* -}

boundedClassKey, enumClassKey, eqClassKey, floatingClassKey,
    fractionalClassKey, integralClassKey, monadClassKey, dataClassKey,
    functorClassKey, numClassKey, ordClassKey, readClassKey, realClassKey,
    realFloatClassKey, realFracClassKey, showClassKey, ixClassKey :: KnownKey
boundedClassKey         = mkPreludeClassUnique 1
enumClassKey            = mkPreludeClassUnique 2
eqClassKey              = mkPreludeClassUnique 3
floatingClassKey        = mkPreludeClassUnique 5
fractionalClassKey      = mkPreludeClassUnique 6
integralClassKey        = mkPreludeClassUnique 7
monadClassKey           = mkPreludeClassUnique 8
dataClassKey            = mkPreludeClassUnique 9
functorClassKey         = mkPreludeClassUnique 10
numClassKey             = mkPreludeClassUnique 11    -- 2b
ordClassKey             = mkPreludeClassUnique 12    -- 2c
readClassKey            = mkPreludeClassUnique 13
realClassKey            = mkPreludeClassUnique 14
realFloatClassKey       = mkPreludeClassUnique 15
realFracClassKey        = mkPreludeClassUnique 16
showClassKey            = mkPreludeClassUnique 17
ixClassKey              = mkPreludeClassUnique 18

typeableClassKey :: KnownKey
typeableClassKey        = mkPreludeClassUnique 20

withDictClassKey :: KnownKey
withDictClassKey        = mkPreludeClassUnique 21

dataToTagClassKey :: KnownKey
dataToTagClassKey       = mkPreludeClassUnique 23

monadFixClassKey :: KnownKey
monadFixClassKey        = mkPreludeClassUnique 28

monadFailClassKey :: KnownKey
monadFailClassKey       = mkPreludeClassUnique 29

monadPlusClassKey, randomClassKey, randomGenClassKey :: KnownKey
monadPlusClassKey       = mkPreludeClassUnique 30
randomClassKey          = mkPreludeClassUnique 31
randomGenClassKey       = mkPreludeClassUnique 32

isStringClassKey :: KnownKey
isStringClassKey        = mkPreludeClassUnique 33

applicativeClassKey, foldableClassKey, traversableClassKey :: KnownKey
applicativeClassKey     = mkPreludeClassUnique 34
foldableClassKey        = mkPreludeClassUnique 35
traversableClassKey     = mkPreludeClassUnique 36

genClassKey, gen1ClassKey :: KnownKey
genClassKey   = mkPreludeClassUnique 37
gen1ClassKey  = mkPreludeClassUnique 38

-- KnownNat: see Note [KnownNat & KnownSymbol and EvLit] in GHC.Tc.Instance.Class
knownNatClassKey :: KnownKey
knownNatClassKey = mkPreludeClassUnique 42

-- KnownSymbol: see Note [KnownNat & KnownSymbol and EvLit] in GHC.Tc.Instance.Class
knownSymbolClassKey :: KnownKey
knownSymbolClassKey = mkPreludeClassUnique 43

knownCharClassKey :: KnownKey
knownCharClassKey = mkPreludeClassUnique 44

ghciIoClassKey :: KnownKey
ghciIoClassKey = mkPreludeClassUnique 45

semigroupClassKey, monoidClassKey :: KnownKey
semigroupClassKey = mkPreludeClassUnique 47
monoidClassKey    = mkPreludeClassUnique 48

-- Implicit Parameters
ipClassKey :: KnownKey
ipClassKey = mkPreludeClassUnique 49

-- Overloaded record fields
hasFieldClassKey :: KnownKey
hasFieldClassKey = mkPreludeClassUnique 50


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
    stablePtrTyConKey, eqTyConKey, heqTyConKey,
    smallArrayPrimTyConKey, smallMutableArrayPrimTyConKey,
    stringTyConKey,
    ccArrowTyConKey, ctArrowTyConKey, tcArrowTyConKey :: KnownKey
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
-- ioPortPrimTyConKey (34) was killed
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
    promptTagPrimTyConKey, constPtrTyConKey, jsvalTyConKey :: KnownKey
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

eitherTyConKey :: KnownKey
eitherTyConKey                          = mkPreludeTyConUnique 84

voidTyConKey :: KnownKey
voidTyConKey                            = mkPreludeTyConUnique 85

nonEmptyTyConKey :: KnownKey
nonEmptyTyConKey                        = mkPreludeTyConUnique 86

dictTyConKey :: KnownKey
dictTyConKey                            = mkPreludeTyConUnique 87

-- Kind constructors
liftedTypeKindTyConKey, unliftedTypeKindTyConKey,
  tYPETyConKey, cONSTRAINTTyConKey,
  liftedRepTyConKey, unliftedRepTyConKey,
  constraintKindTyConKey, levityTyConKey, runtimeRepTyConKey,
  vecCountTyConKey, vecElemTyConKey,
  zeroBitRepTyConKey, zeroBitTypeTyConKey :: KnownKey
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

pluginTyConKey, frontendPluginTyConKey :: KnownKey
pluginTyConKey                          = mkPreludeTyConUnique 102
frontendPluginTyConKey                  = mkPreludeTyConUnique 103

trTyConTyConKey, trModuleTyConKey,
  kindRepTyConKey :: KnownKey
trTyConTyConKey                         = mkPreludeTyConUnique 104
trModuleTyConKey                        = mkPreludeTyConUnique 105
kindRepTyConKey                         = mkPreludeTyConUnique 107

-- Generics (Unique keys)
v1TyConKey, u1TyConKey, par1TyConKey, rec1TyConKey,
  sumTyConKey, prodTyConKey, compTyConKey, rec0TyConKey,
  d1TyConKey, c1TyConKey, s1TyConKey, repTyConKey, rep1TyConKey,
  uAddrTyConKey, uCharTyConKey, uDoubleTyConKey,
  uFloatTyConKey, uIntTyConKey, uWordTyConKey :: KnownKey

v1TyConKey    = mkPreludeTyConUnique 135
u1TyConKey    = mkPreludeTyConUnique 136
par1TyConKey  = mkPreludeTyConUnique 137
rec1TyConKey  = mkPreludeTyConUnique 138

sumTyConKey   = mkPreludeTyConUnique 141
prodTyConKey  = mkPreludeTyConUnique 142
compTyConKey  = mkPreludeTyConUnique 143

rec0TyConKey  = mkPreludeTyConUnique 149
d1TyConKey    = mkPreludeTyConUnique 151
c1TyConKey    = mkPreludeTyConUnique 152
s1TyConKey    = mkPreludeTyConUnique 153

repTyConKey  = mkPreludeTyConUnique 155
rep1TyConKey = mkPreludeTyConUnique 156

uAddrTyConKey   = mkPreludeTyConUnique 158
uCharTyConKey   = mkPreludeTyConUnique 159
uDoubleTyConKey = mkPreludeTyConUnique 160
uFloatTyConKey  = mkPreludeTyConUnique 161
uIntTyConKey    = mkPreludeTyConUnique 162
uWordTyConKey   = mkPreludeTyConUnique 163

-- "Unsatisfiable" constraint
unsatisfiableClassKey :: KnownKey
unsatisfiableClassKey = mkPreludeTyConUnique 170

anyTyConKey :: KnownKey
anyTyConKey = mkPreludeTyConUnique 171

zonkAnyTyConKey :: KnownKey
zonkAnyTyConKey = mkPreludeTyConUnique 172

-- Custom user type-errors
errorMessageTypeErrorFamKey :: KnownKey
errorMessageTypeErrorFamKey = mkPreludeTyConUnique 181

coercibleTyConKey :: KnownKey
coercibleTyConKey = mkPreludeTyConUnique 183

proxyPrimTyConKey :: KnownKey
proxyPrimTyConKey = mkPreludeTyConUnique 184

specTyConKey :: KnownKey
specTyConKey = mkPreludeTyConUnique 185

smallArrayPrimTyConKey        = mkPreludeTyConUnique  187
smallMutableArrayPrimTyConKey = mkPreludeTyConUnique  188

staticPtrTyConKey  :: KnownKey
staticPtrTyConKey  = mkPreludeTyConUnique 189

staticPtrInfoTyConKey :: KnownKey
staticPtrInfoTyConKey = mkPreludeTyConUnique 190

callStackTyConKey :: KnownKey
callStackTyConKey = mkPreludeTyConUnique 191

-- Typeables
someTypeRepTyConKey, someTypeRepDataConKey :: KnownKey
someTypeRepTyConKey   = mkPreludeTyConUnique 193
someTypeRepDataConKey = mkPreludeTyConUnique 194


typeSymbolAppendFamNameKey :: KnownKey
typeSymbolAppendFamNameKey = mkPreludeTyConUnique 195

-- Unsafe equality
unsafeEqualityTyConKey :: KnownKey
unsafeEqualityTyConKey = mkPreludeTyConUnique 196

-- Linear types
multiplicityTyConKey :: KnownKey
multiplicityTyConKey = mkPreludeTyConUnique 197

unrestrictedFunTyConKey :: KnownKey
unrestrictedFunTyConKey = mkPreludeTyConUnique 198

multMulTyConKey :: KnownKey
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
  :: KnownKey
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
    eqDataConKey, nothingDataConKey, justDataConKey :: KnownKey

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
crossDataConKey, inlDataConKey, inrDataConKey, genUnitDataConKey :: KnownKey
crossDataConKey                         = mkPreludeDataConUnique 20
inlDataConKey                           = mkPreludeDataConUnique 21
inrDataConKey                           = mkPreludeDataConUnique 22
genUnitDataConKey                       = mkPreludeDataConUnique 23

leftDataConKey, rightDataConKey :: KnownKey
leftDataConKey                          = mkPreludeDataConUnique 25
rightDataConKey                         = mkPreludeDataConUnique 26

ordLTDataConKey, ordEQDataConKey, ordGTDataConKey :: KnownKey
ordLTDataConKey                         = mkPreludeDataConUnique 27
ordEQDataConKey                         = mkPreludeDataConUnique 28
ordGTDataConKey                         = mkPreludeDataConUnique 29

mkDictDataConKey :: KnownKey
mkDictDataConKey                        = mkPreludeDataConUnique 30

coercibleDataConKey :: KnownKey
coercibleDataConKey                     = mkPreludeDataConUnique 32

staticPtrDataConKey :: KnownKey
staticPtrDataConKey                     = mkPreludeDataConUnique 33

staticPtrInfoDataConKey :: KnownKey
staticPtrInfoDataConKey                 = mkPreludeDataConUnique 34

srcLocDataConKey :: KnownKey
srcLocDataConKey                        = mkPreludeDataConUnique 37

trTyConDataConKey, trModuleDataConKey,
  trNameSDataConKey,
  trGhcPrimModuleKey :: KnownKey
trTyConDataConKey                       = mkPreludeDataConUnique 41
trModuleDataConKey                      = mkPreludeDataConUnique 43
trNameSDataConKey                       = mkPreludeDataConUnique 45
trGhcPrimModuleKey                      = mkPreludeDataConUnique 47

typeErrorTextDataConKey,
  typeErrorAppendDataConKey,
  typeErrorVAppendDataConKey,
  typeErrorShowTypeDataConKey
  :: KnownKey
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
    metaDataDataConKey, metaConsDataConKey, metaSelDataConKey :: KnownKey
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
  tupleRepDataConKey, boxedRepDataConKey :: KnownKey
vecRepDataConKey                        = mkPreludeDataConUnique 71
tupleRepDataConKey                      = mkPreludeDataConUnique 72
sumRepDataConKey                        = mkPreludeDataConUnique 73
boxedRepDataConKey                      = mkPreludeDataConUnique 74

boxedRepDataConTyConKey, tupleRepDataConTyConKey :: KnownKey
-- A promoted data constructors (i.e. a TyCon) has
-- the same key as the data constructor itself
boxedRepDataConTyConKey = boxedRepDataConKey
tupleRepDataConTyConKey = tupleRepDataConKey

-- See Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- Includes all nullary-data-constructor reps. Does not
-- include BoxedRep, VecRep, SumRep, TupleRep.
runtimeRepSimpleDataConKeys :: [KnownKey]
runtimeRepSimpleDataConKeys
  = map mkPreludeDataConUnique [75..87]

liftedDataConKey,unliftedDataConKey :: KnownKey
liftedDataConKey = mkPreludeDataConUnique 88
unliftedDataConKey = mkPreludeDataConUnique 89

-- See Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- VecCount
vecCountDataConKeys :: [KnownKey]
vecCountDataConKeys = map mkPreludeDataConUnique [90..95]

-- See Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- VecElem
vecElemDataConKeys :: [KnownKey]
vecElemDataConKeys = map mkPreludeDataConUnique [96..105]

-- Typeable things
kindRepTyConAppDataConKey, kindRepVarDataConKey, kindRepAppDataConKey,
    kindRepFunDataConKey, kindRepTYPEDataConKey,
    kindRepTypeLitSDataConKey
    :: KnownKey
kindRepTyConAppDataConKey = mkPreludeDataConUnique 106
kindRepVarDataConKey      = mkPreludeDataConUnique 107
kindRepAppDataConKey      = mkPreludeDataConUnique 108
kindRepFunDataConKey      = mkPreludeDataConUnique 109
kindRepTYPEDataConKey     = mkPreludeDataConUnique 110
kindRepTypeLitSDataConKey = mkPreludeDataConUnique 111

typeLitSymbolDataConKey, typeLitNatDataConKey, typeLitCharDataConKey :: KnownKey
typeLitSymbolDataConKey   = mkPreludeDataConUnique 113
typeLitNatDataConKey      = mkPreludeDataConUnique 114
typeLitCharDataConKey     = mkPreludeDataConUnique 115

-- Unsafe equality
unsafeReflDataConKey :: KnownKey
unsafeReflDataConKey      = mkPreludeDataConUnique 116

-- Multiplicity

oneDataConKey, manyDataConKey :: KnownKey
oneDataConKey = mkPreludeDataConUnique 117
manyDataConKey = mkPreludeDataConUnique 118

-- ghc-bignum
integerISDataConKey, integerINDataConKey, integerIPDataConKey,
   naturalNSDataConKey, naturalNBDataConKey :: KnownKey
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

wildCardKey, absentErrorIdKey, absentConstraintErrorIdKey, augmentIdKey,
    buildIdKey, foldrIdKey, recSelErrorIdKey,
    seqIdKey, eqStringIdKey,
    noMethodBindingErrorIdKey, nonExhaustiveGuardsErrorIdKey,
    impossibleErrorIdKey, impossibleConstraintErrorIdKey,
    patErrorIdKey, voidPrimIdKey,
    realWorldPrimIdKey, recConErrorIdKey,
    unpackCStringUtf8IdKey, unpackCStringAppendUtf8IdKey, unpackCStringFoldrUtf8IdKey,
    unpackCStringIdKey, unpackCStringAppendIdKey, unpackCStringFoldrIdKey,
    typeErrorIdKey, divIntIdKey, modIntIdKey,
    absentSumFieldErrorIdKey, cstringLengthIdKey, composeIdKey
    :: KnownKey

wildCardKey                    = mkPreludeMiscIdUnique  0  -- See Note [WildCard binders]
absentErrorIdKey               = mkPreludeMiscIdUnique  1
absentConstraintErrorIdKey     = mkPreludeMiscIdUnique  2
augmentIdKey                   = mkPreludeMiscIdUnique  3
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
composeIdKey                  = mkPreludeMiscIdUnique 29

bindIOIdKey, returnIOIdKey, newStablePtrIdKey,
    printIdKey, nullAddrIdKey, voidArgIdKey,
    otherwiseIdKey, assertIdKey :: KnownKey
bindIOIdKey                   = mkPreludeMiscIdUnique 34
returnIOIdKey                 = mkPreludeMiscIdUnique 35
newStablePtrIdKey             = mkPreludeMiscIdUnique 36
printIdKey                    = mkPreludeMiscIdUnique 37
nullAddrIdKey                 = mkPreludeMiscIdUnique 39
voidArgIdKey                  = mkPreludeMiscIdUnique 40
otherwiseIdKey                = mkPreludeMiscIdUnique 43
assertIdKey                   = mkPreludeMiscIdUnique 44

leftSectionKey, rightSectionKey :: KnownKey
leftSectionKey                = mkPreludeMiscIdUnique 45
rightSectionKey               = mkPreludeMiscIdUnique 46

rootMainKey, runMainKey :: KnownKey
rootMainKey                   = mkPreludeMiscIdUnique 101
runMainKey                    = mkPreludeMiscIdUnique 102

thenIOIdKey, lazyIdKey, assertErrorIdKey, oneShotKey, runRWKey :: KnownKey
thenIOIdKey                   = mkPreludeMiscIdUnique 103
lazyIdKey                     = mkPreludeMiscIdUnique 104
assertErrorIdKey              = mkPreludeMiscIdUnique 105
oneShotKey                    = mkPreludeMiscIdUnique 106
runRWKey                      = mkPreludeMiscIdUnique 107

traceKey :: KnownKey
traceKey                      = mkPreludeMiscIdUnique 108

nospecIdKey :: KnownKey
nospecIdKey                   = mkPreludeMiscIdUnique 109

inlineIdKey, noinlineIdKey, noinlineConstraintIdKey :: KnownKey
inlineIdKey                   = mkPreludeMiscIdUnique 120
-- see below

mapIdKey, dollarIdKey, coercionTokenIdKey, considerAccessibleIdKey :: KnownKey
mapIdKey                = mkPreludeMiscIdUnique 121
dollarIdKey             = mkPreludeMiscIdUnique 123
coercionTokenIdKey      = mkPreludeMiscIdUnique 124
considerAccessibleIdKey = mkPreludeMiscIdUnique 125
noinlineIdKey           = mkPreludeMiscIdUnique 126
noinlineConstraintIdKey = mkPreludeMiscIdUnique 127

integerToFloatIdKey, integerToDoubleIdKey :: KnownKey
integerToFloatIdKey    = mkPreludeMiscIdUnique 128
integerToDoubleIdKey   = mkPreludeMiscIdUnique 129

rationalToFloatIdKey, rationalToDoubleIdKey :: KnownKey
rationalToFloatIdKey   = mkPreludeMiscIdUnique 132
rationalToDoubleIdKey  = mkPreludeMiscIdUnique 133


seqHashKey, coerceKey :: KnownKey
seqHashKey             = mkPreludeMiscIdUnique 134
coerceKey              = mkPreludeMiscIdUnique 135

-- Just a placeholder for unbound variables produced by the renamer:
unboundKey :: KnownKey
unboundKey             = mkPreludeMiscIdUnique 136

fromIntegerClassOpKey, minusClassOpKey, fromRationalClassOpKey,
    enumFromClassOpKey, enumFromThenClassOpKey, enumFromToClassOpKey,
    enumFromThenToClassOpKey, negateClassOpKey,
    bindMClassOpKey, thenMClassOpKey, returnMClassOpKey, fmapClassOpKey
    :: KnownKey
fromIntegerClassOpKey         = mkPreludeMiscIdUnique 140
minusClassOpKey               = mkPreludeMiscIdUnique 141
fromRationalClassOpKey        = mkPreludeMiscIdUnique 142
enumFromClassOpKey            = mkPreludeMiscIdUnique 143
enumFromThenClassOpKey        = mkPreludeMiscIdUnique 144
enumFromToClassOpKey          = mkPreludeMiscIdUnique 145
enumFromThenToClassOpKey      = mkPreludeMiscIdUnique 146

eqClassOpKey, geClassOpKey, leClassOpKey,
   ltClassOpKey, gtClassOpKey, compareClassOpKey :: KnownKey
eqClassOpKey                  = mkPreludeMiscIdUnique 147
geClassOpKey                  = mkPreludeMiscIdUnique 148
leClassOpKey                  = mkPreludeMiscIdUnique 149
ltClassOpKey                  = mkPreludeMiscIdUnique 150
gtClassOpKey                  = mkPreludeMiscIdUnique 151
compareClassOpKey             = mkPreludeMiscIdUnique 152


negateClassOpKey              = mkPreludeMiscIdUnique 153
bindMClassOpKey               = mkPreludeMiscIdUnique 154
thenMClassOpKey               = mkPreludeMiscIdUnique 155 -- (>>)
fmapClassOpKey                = mkPreludeMiscIdUnique 156
returnMClassOpKey             = mkPreludeMiscIdUnique 157

-- Recursive do notation
mfixIdKey :: KnownKey
mfixIdKey       = mkPreludeMiscIdUnique 158

-- MonadFail operations
failMClassOpKey :: KnownKey
failMClassOpKey = mkPreludeMiscIdUnique 159

-- fromLabel
fromLabelClassOpKey :: KnownKey
fromLabelClassOpKey = mkPreludeMiscIdUnique 160

-- DataToTag
dataToTagClassOpKey :: KnownKey
dataToTagClassOpKey = mkPreludeMiscIdUnique 161

-- Arrow notation
arrAIdKey, composeAIdKey, firstAIdKey, appAIdKey, choiceAIdKey,
    loopAIdKey :: KnownKey
arrAIdKey       = mkPreludeMiscIdUnique 180
composeAIdKey   = mkPreludeMiscIdUnique 181 -- >>>
firstAIdKey     = mkPreludeMiscIdUnique 182
appAIdKey       = mkPreludeMiscIdUnique 183
choiceAIdKey    = mkPreludeMiscIdUnique 184 --  |||
loopAIdKey      = mkPreludeMiscIdUnique 185

fromStringClassOpKey :: KnownKey
fromStringClassOpKey = mkPreludeMiscIdUnique 186

-- Annotation type checking
toAnnotationWrapperIdKey :: KnownKey
toAnnotationWrapperIdKey = mkPreludeMiscIdUnique 187

-- Conversion functions
fromIntegralIdKey, realToFracIdKey, toIntegerClassOpKey, toRationalClassOpKey :: KnownKey
fromIntegralIdKey    = mkPreludeMiscIdUnique 190
realToFracIdKey      = mkPreludeMiscIdUnique 191
toIntegerClassOpKey  = mkPreludeMiscIdUnique 192
toRationalClassOpKey = mkPreludeMiscIdUnique 193

-- Monad comprehensions
guardMIdKey, mzipIdKey :: KnownKey
guardMIdKey     = mkPreludeMiscIdUnique 194
mzipIdKey       = mkPreludeMiscIdUnique 196

-- GHCi
ghciStepIoMClassOpKey :: KnownKey
ghciStepIoMClassOpKey = mkPreludeMiscIdUnique 197

-- Overloaded lists
isListClassKey, fromListClassOpKey, fromListNClassOpKey, toListClassOpKey :: KnownKey
isListClassKey      = mkPreludeMiscIdUnique 198
fromListClassOpKey  = mkPreludeMiscIdUnique 199
fromListNClassOpKey = mkPreludeMiscIdUnique 500
toListClassOpKey    = mkPreludeMiscIdUnique 501

proxyHashKey :: KnownKey
proxyHashKey = mkPreludeMiscIdUnique 502

---------------- Template Haskell -------------------
--      GHC.Builtin.Names.TH: USES IdUniques 200-499
-----------------------------------------------------

-- Used to make `Typeable` dictionaries
mkTyConKey
  , mkTrConKey
  , mkTrAppCheckedKey
  , mkTrFunKey
  , typeNatTypeRepKey
  , typeSymbolTypeRepKey
  , typeCharTypeRepKey
  , typeRepIdKey
  :: KnownKey
mkTyConKey            = mkPreludeMiscIdUnique 503
mkTrConKey            = mkPreludeMiscIdUnique 505
mkTrAppCheckedKey     = mkPreludeMiscIdUnique 506
typeNatTypeRepKey     = mkPreludeMiscIdUnique 507
typeSymbolTypeRepKey  = mkPreludeMiscIdUnique 508
typeCharTypeRepKey    = mkPreludeMiscIdUnique 509
typeRepIdKey          = mkPreludeMiscIdUnique 510
mkTrFunKey            = mkPreludeMiscIdUnique 511

-- KindReps for common cases
starKindRepKey, starArrStarKindRepKey, starArrStarArrStarKindRepKey, constraintKindRepKey :: KnownKey
starKindRepKey               = mkPreludeMiscIdUnique 520
starArrStarKindRepKey        = mkPreludeMiscIdUnique 521
starArrStarArrStarKindRepKey = mkPreludeMiscIdUnique 522
constraintKindRepKey         = mkPreludeMiscIdUnique 523

-- Dynamic
toDynIdKey :: KnownKey
toDynIdKey            = mkPreludeMiscIdUnique 530


heqSCSelIdKey, eqSCSelIdKey, coercibleSCSelIdKey :: KnownKey
eqSCSelIdKey        = mkPreludeMiscIdUnique 551
heqSCSelIdKey       = mkPreludeMiscIdUnique 552
coercibleSCSelIdKey = mkPreludeMiscIdUnique 553

sappendClassOpKey :: KnownKey
sappendClassOpKey = mkPreludeMiscIdUnique 554

memptyClassOpKey, mappendClassOpKey, mconcatClassOpKey :: KnownKey
memptyClassOpKey  = mkPreludeMiscIdUnique 555
mappendClassOpKey = mkPreludeMiscIdUnique 556
mconcatClassOpKey = mkPreludeMiscIdUnique 557

emptyCallStackKey, pushCallStackKey :: KnownKey
emptyCallStackKey = mkPreludeMiscIdUnique 558
pushCallStackKey  = mkPreludeMiscIdUnique 559

fromStaticPtrClassOpKey :: KnownKey
fromStaticPtrClassOpKey = mkPreludeMiscIdUnique 560

makeStaticKey :: KnownKey
makeStaticKey = mkPreludeMiscIdUnique 561

emptyExceptionContextKey :: KnownKey
emptyExceptionContextKey = mkPreludeMiscIdUnique 562

-- Unsafe coercion proofs
unsafeEqualityProofIdKey, unsafeCoercePrimIdKey :: KnownKey
unsafeEqualityProofIdKey = mkPreludeMiscIdUnique 570
unsafeCoercePrimIdKey    = mkPreludeMiscIdUnique 571

-- HasField class ops
getFieldClassOpKey, setFieldClassOpKey :: KnownKey
getFieldClassOpKey = mkPreludeMiscIdUnique 572
setFieldClassOpKey = mkPreludeMiscIdUnique 573

-- "Unsatisfiable" constraints
unsatisfiableIdKey :: KnownKey
unsatisfiableIdKey = mkPreludeMiscIdUnique 580

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
   , naturalOrIdKey
   , naturalXorIdKey
   , naturalTestBitIdKey
   , naturalBitIdKey
   , naturalGcdIdKey
   , naturalLcmIdKey
   , bignatEqIdKey
   , bignatCompareIdKey
   , bignatCompareWordIdKey
   :: KnownKey

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
naturalOrIdKey             = mkPreludeMiscIdUnique 674
naturalXorIdKey            = mkPreludeMiscIdUnique 675
naturalTestBitIdKey        = mkPreludeMiscIdUnique 676
naturalBitIdKey            = mkPreludeMiscIdUnique 677
naturalGcdIdKey            = mkPreludeMiscIdUnique 678
naturalLcmIdKey            = mkPreludeMiscIdUnique 679

bignatEqIdKey              = mkPreludeMiscIdUnique 691
bignatCompareIdKey         = mkPreludeMiscIdUnique 692
bignatCompareWordIdKey     = mkPreludeMiscIdUnique 693


------------------------------------------------------
-- ghci optimization for big rationals 700-749 uniques
------------------------------------------------------

-- Creating rationals at runtime.
mkRationalBase2IdKey, mkRationalBase10IdKey :: KnownKey
mkRationalBase2IdKey  = mkPreludeMiscIdUnique 700
mkRationalBase10IdKey = mkPreludeMiscIdUnique 701 :: KnownKey

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

numericClassKeys :: [KnownKey]
numericClassKeys =
        [ numClassKey
        , realClassKey
        , integralClassKey
        ]
        ++ fractionalClassKeys

fractionalClassKeys :: [KnownKey]
fractionalClassKeys =
        [ fractionalClassKey
        , floatingClassKey
        , realFracClassKey
        , realFloatClassKey
        ]

-- The "standard classes" are used in defaulting (Haskell 98 report 4.3.4),
-- and are: "classes defined in the Prelude or a standard library"
standardClassKeys :: [KnownKey]
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

derivableClassKeys :: [KnownKey]
derivableClassKeys
  = [ eqClassKey, ordClassKey, enumClassKey, ixClassKey,
      boundedClassKey, showClassKey, readClassKey ]

interactiveClassKeys :: [KnownKey]
-- These are the "interactive classes" that are consulted when doing
-- defaulting. Does not include Num or IsString, which have special
-- handling.
interactiveClassKeys = [ showClassKey, eqClassKey, ordClassKey
                       , foldableClassKey, traversableClassKey ]

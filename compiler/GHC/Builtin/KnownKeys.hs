{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Nota Bene: all Names defined in here should come from the base package,
the big-num package or (for plugins) the ghc package.

 - ModuleNames for prelude modules,
        e.g.    pRELUDE_NAME :: ModuleName

 - Modules for prelude modules
        e.g.    pRELUDE :: Module

 - Uniques for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    orderingTyConKey :: Unique

 - Knowledge about known-key names.
     knownKeyUniqMap, knownkeyOccMap, basicKnownKeyTable, etc
   See Note [Overview of known entities] in GHC.Builtin

 - RdrNames for Ids, DataCons etc that the compiler may emit into
   generated code (e.g. for deriving).
        e.g.    and_RDR :: RdrName
   It's not necessary to know the uniques for these guys, only their names


Note [Infinite families of known-key names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Infinite families of known-key things (e.g. tuples and sums) pose a tricky
problem: we can't add them to the wiredInNames finite map which we use to
ensure that, e.g., a reference to (,) gets assigned the right unique (if this
doesn't sound familiar see Note [Overview of known entities]).

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

module GHC.Builtin.KnownKeys
   ( Unique, Uniquable(..), hasKey  -- Re-exported for convenience

   , module GHC.Builtin.KnownKeys
   )
where

import GHC.Prelude

import GHC.Builtin.Uniques

import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Name

import GHC.Utils.Misc( HasDebugCallStack )
import GHC.Utils.Panic
import GHC.Data.Maybe


{- *********************************************************************
*                                                                      *
                  Infrastructure
*                                                                      *
********************************************************************* -}

-- | `knownKeyOccMap` maps the OccName of a known-key to its Unique
knownKeyOccMap :: OccEnv KnownKey
knownKeyOccMap = mkOccEnv knownKeyTable

knownKeyUniqMap :: UniqFM KnownKey OccName
knownKeyUniqMap = listToUFM [ (uniq, occ) | (occ, uniq) <- knownKeyTable ]

knownKeyOccName :: HasDebugCallStack => KnownKey -> OccName
-- Find the OccName from the KnownKey,
-- by looking in the knownKeyUniqMap
knownKeyOccName key
  = knownKeyOccName_maybe key `orElse`
    pprPanic "knownKeyOccName" (pprKnownKey key)

knownKeyOccName_maybe :: HasDebugCallStack
                      => KnownKey -> Maybe OccName
knownKeyOccName_maybe key
  = lookupUFM knownKeyUniqMap key

knownKeyRdrName :: KnownKey -> RdrName
knownKeyRdrName key = knownOccRdrName (knownKeyOccName key)

{- *********************************************************************
*                                                                      *
              The master list of known-key names
*                                                                      *
************************************************************************

This section tells what the compiler knows about the association of
names with uniques.  These ones are the *non* wired-in ones.  The
wired in ones are defined in GHC.Builtin.Types etc.

See Note [Overview of known entities] in GHC.Builtin
-}

knownKeyTable :: [(OccName, KnownKey)]
knownKeyTable
  = [ (ioTyConOcc, ioTyConKey)
    , (mkVarOcc "$",                  dollarIdKey)
    , (mkVarOcc "assert",             assertIdKey)
    , (mkVarOcc "considerAccessible", considerAccessibleIdKey)
    , (mkVarOcc "augment",            augmentIdKey)
    , (mkVarOcc "otherwise",          otherwiseIdKey)

     -- Classes
    , (mkTcOcc "Eq",           eqClassKey)
    , (mkTcOcc "Ord",          ordClassKey)
    , (mkTcOcc "Enum",         enumClassKey)
    , (mkTcOcc "Bounded",      boundedClassKey)
    , (mkTcOcc "Read",         readClassKey)
    , (mkTcOcc "Show",         showClassKey)
    , (mkTcOcc "Foldable",     foldableClassKey)
    , (mkTcOcc "Traversable",  traversableClassKey)
    , (mkTcOcc "Data",         dataClassKey)
    , (mkTcOcc "Ix",           ixClassKey)
    , (mkTcOcc "Alternative",  alternativeClassKey)
    , (mkTcOcc "Typeable",     typeableClassKey)
    , (mkTcOcc "Functor",      functorClassKey)
    , (mkTcOcc "Lift",         liftClassKey)
    , (mkTcOcc "MonadPlus",    monadPlusClassKey)
    , (mkTcOcc "MonadFail",    monadFailClassKey)

    -- Numeric classes
    , (mkTcOcc "Num",               numClassKey)
    , (mkTcOcc "Integral",          integralClassKey)
    , (mkTcOcc "Real",              realClassKey)
    , (mkTcOcc "Floating",          floatingClassKey)
    , (mkTcOcc "Fractional",        fractionalClassKey)
    , (mkTcOcc "RealFloat",         realFloatClassKey)
    , (mkTcOcc "RealFrac",          realFracClassKey)

    , (mkTcOcc "Ratio",             ratioTyConKey)
    , (mkDataOcc ":%",              ratioDataConKey)
    , (mkVarOcc "fromIntegral",     fromIntegralIdKey)
    , (mkVarOcc "toInteger",        toIntegerClassOpKey)
    , (mkVarOcc "toRational",       toRationalClassOpKey)
    , (mkVarOcc "realToFrac",       realToFracIdKey)

    -- Int and Word
    , (mkTcOcc "Int8",   int8TyConKey)
    , (mkTcOcc "Int16",  int16TyConKey)
    , (mkTcOcc "Int32",  int32TyConKey)
    , (mkTcOcc "Int64",  int64TyConKey)
    , (mkTcOcc "Word8",  word8TyConKey)
    , (mkTcOcc "Word16", word16TyConKey)
    , (mkTcOcc "Word32", word32TyConKey)
    , (mkTcOcc "Word64", word64TyConKey)

    -- FFI things
    , (mkTcOcc "ConstPtr", constPtrTyConKey)
    , (mkTcOcc "Ptr", ptrTyConKey)
    , (mkTcOcc "FunPtr", funPtrTyConKey)

    -- Class Monad
    , (mkTcOcc "Monad",        monadClassKey)
    , (thenMClassOpOcc,        thenMClassOpKey)
    , (returnMClassOpOcc,      returnMClassOpKey)

    -- Class Applicative
    , (mkTcOcc "Applicative",  applicativeClassKey)
    , (pureAClassOpOcc,        pureAClassOpKey)
    , (thenAClassOpOcc,        thenAClassOpKey)

    -- Class Semigroup, Monoid
    , (mkTcOcc "Semigroup",    semigroupClassKey)
    , (mkTcOcc "Monoid",       monoidClassKey)
    , (sappendClassOpOcc,      sappendClassOpKey)
    , (mappendClassOpOcc,      mappendClassOpKey)

    -- Class IsString
    , (mkTcOcc "IsString",    isStringClassKey)

    -- DataToTag
    , (mkTcOcc "DataToTag",   dataToTagClassKey)

    -- Lists
    , (mkVarOcc "build",  buildIdKey)

    -- Records
    , (mkTcOcc "HasField",   hasFieldClassKey)
    , (getFieldClassOpOcc,   getFieldClassOpKey)

    -- FromList
    , (mkVarOcc "toList",     toListClassOpKey)

    -- Generics
    , (mkTcOcc "Generic",   genClassKey)
    , (mkTcOcc "Generic1",  gen1ClassKey)

    -- Static pointers
    , (mkVarOcc "makeStatic",     makeStaticKey)
    , (mkDataOcc "StaticPtr",     staticPtrDataConKey)
    , (mkDataOcc "StaticPtrInfo", staticPtrInfoDataConKey)

    -- Stable pointers
    , (mkTcOcc "StablePtr", stablePtrTyConKey)

    -- WithDict
    , (mkTcOcc "WithDict", withDictClassKey)

    -- Unsatisfiable class
    , (mkTcOcc  "Unsatisfiable", unsatisfiableClassKey)
    , (mkVarOcc "unsatisfiable", unsatisfiableIdKey)

    -- Implicit Params
    , (mkTcOcc "IP", ipClassKey)

    -- Callstacks
    , (mkTcOcc "CallStack", callStackTyConKey)

    -- Exception context
    , (mkTcOcc "ExceptionContext", exceptionContextTyConKey)

    -- Custom type errors
    , (mkTcOcc   "TypeError", errorMessageTypeErrorFamKey)
    , (mkDataOcc "Text",      typeErrorTextDataConKey)
    , (mkDataOcc ":<>:",      typeErrorAppendDataConKey)
    , (mkDataOcc ":$$:",      typeErrorVAppendDataConKey)
    , (mkDataOcc "ShowType",  typeErrorShowTypeDataConKey)

    -- Known lits
    , (mkTcOcc "KnownNat", knownNatClassKey)
    , (mkTcOcc "KnownSymbol", knownSymbolClassKey)
    , (mkTcOcc "KnownChar", knownCharClassKey)

    -- Unsafe coercion proofs
    , (mkVarOcc "unsafeCoerce#", unsafeCoercePrimIdKey)

    -- Base strings Strings
    , (mkVarOcc "unpackCString#",     unpackCStringIdKey)
    , (mkVarOcc "unpackCStringUtf8#", unpackCStringUtf8IdKey)

    -- JS primitives
#if defined(javascript_HOST_ARCH)
    , (mkVarOcc "unsafeUnpackJSStringUtf8##", unsafeUnpackJSStringUtf8ShShKey)
    , (mkTcOcc "JSVal",                       jsvalTyConKey)
#endif

    -- SpecConstr's SPEC type
    , (mkTcOcc "SPEC", specTyConKey)

    -- Type rep
    , (mkTcOcc "TyCon", trTyConTyConKey)

    -- Known-key names that have BuiltinRules in ConstantFold
    , (mkVarOcc "unpackFoldrCString#",      unpackCStringFoldrIdKey)
    , (mkVarOcc "unpackFoldrCStringUtf8#",  unpackCStringFoldrUtf8IdKey)
    , (mkVarOcc "unpackAppendCString#",     unpackCStringAppendIdKey)
    , (mkVarOcc "unpackAppendCStringUtf8#", unpackCStringAppendUtf8IdKey)
    , (mkVarOcc "cstringLength#",           cstringLengthIdKey)
    , (mkVarOcc "eqString",                 eqStringIdKey)
    , (mkVarOcc "inline",                   inlineIdKey)
    , (mkVarOcc "runRW#",                   runRWKey)
    , (mkVarOcc "seq#",                     seqHashKey)
    , (mkVarOcc "divInt#",                  divIntIdKey)
    , (mkVarOcc "modInt#",                  modIntIdKey)

    -- Unsafe equality proofs
    , (mkVarOcc "unsafeEqualityProof",      unsafeEqualityProofIdKey)
    , (mkTcOcc  "UnsafeEquality",           unsafeEqualityTyConKey)
    , (mkDataOcc "UnsafeRefl",              unsafeReflDataConKey)

    -- BuiltinRules in ConstantFold
    , (mkVarOcc "integerToFloat#",    integerToFloatIdKey)
    , (mkVarOcc "integerToDouble#",   integerToDoubleIdKey)
    , (mkVarOcc "rationalToFloat#",   rationalToFloatIdKey)
    , (mkVarOcc "rationalToDouble#",  rationalToDoubleIdKey)

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


{-
************************************************************************
*                                                                      *
\subsection{Known-key names}
*                                                                      *
************************************************************************

See Note [Overview of known entities]
-}

-- AMP additions
pureAClassOpKey, thenAClassOpKey, alternativeClassKey :: KnownKey
pureAClassOpKey     = mkPreludeMiscIdUnique 752
thenAClassOpKey     = mkPreludeMiscIdUnique 753
alternativeClassKey = mkPreludeMiscIdUnique 754

genericClassKeys :: [KnownKey]
genericClassKeys = [genClassKey, gen1ClassKey]

{- *********************************************************************
*                                                                      *
                 Statically-known occurrence names
*                                                                      *
********************************************************************* -}

sappendClassOpOcc, pureAClassOpOcc, thenAClassOpOcc, bindMClassOpOcc,
  returnMClassOpOcc, thenMClassOpOcc, mappendClassOpOcc, getFieldClassOpOcc,
  ioTyConOcc :: KnownOcc
sappendClassOpOcc  = mkVarOcc "<>"
pureAClassOpOcc    = mkVarOcc "pure"
returnMClassOpOcc  = mkVarOcc "return"
thenMClassOpOcc    = mkVarOcc ">>"
bindMClassOpOcc    = mkVarOcc ">>="
thenAClassOpOcc    = mkVarOcc "*>"
mappendClassOpOcc  = mkVarOcc "mappend"
getFieldClassOpOcc = mkVarOcc "getField"
ioTyConOcc         = mkTcOcc "IO"


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

monadFailClassKey :: KnownKey
monadFailClassKey       = mkPreludeClassUnique 29

monadPlusClassKey :: KnownKey
monadPlusClassKey       = mkPreludeClassUnique 30

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
--      GHC.Builtin.TH: USES ClassUniques 200-299
-----------------------------------------------------

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
*                                                                      *
************************************************************************
-}

addrPrimTyConKey, arrayPrimTyConKey, boolTyConKey, byteArrayPrimTyConKey,
  charPrimTyConKey, charTyConKey, doublePrimTyConKey, doubleTyConKey,
  floatPrimTyConKey, floatTyConKey, fUNTyConKey, intPrimTyConKey, intTyConKey,
  int8TyConKey, int16TyConKey, int8PrimTyConKey, int16PrimTyConKey,
  int32PrimTyConKey, int32TyConKey, int64PrimTyConKey, int64TyConKey,
  integerTyConKey, naturalTyConKey, listTyConKey, maybeTyConKey,
  weakPrimTyConKey, mutableArrayPrimTyConKey, mutableByteArrayPrimTyConKey,
  orderingTyConKey, mVarPrimTyConKey, ratioTyConKey,
  realWorldTyConKey, stablePtrPrimTyConKey, stablePtrTyConKey, eqTyConKey,
  heqTyConKey, smallArrayPrimTyConKey, smallMutableArrayPrimTyConKey,
  stringTyConKey, ccArrowTyConKey, ctArrowTyConKey, tcArrowTyConKey :: KnownKey
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
maybeTyConKey                           = mkPreludeTyConUnique 28
weakPrimTyConKey                        = mkPreludeTyConUnique 29
mutableArrayPrimTyConKey                = mkPreludeTyConUnique 30
mutableByteArrayPrimTyConKey            = mkPreludeTyConUnique 31
orderingTyConKey                        = mkPreludeTyConUnique 32
mVarPrimTyConKey                        = mkPreludeTyConUnique 33
ratioTyConKey                           = mkPreludeTyConUnique 35
realWorldTyConKey                       = mkPreludeTyConUnique 37
stablePtrPrimTyConKey                   = mkPreludeTyConUnique 38
stablePtrTyConKey                       = mkPreludeTyConUnique 39
eqTyConKey                              = mkPreludeTyConUnique 40
heqTyConKey                             = mkPreludeTyConUnique 41

ctArrowTyConKey                       = mkPreludeTyConUnique 42
ccArrowTyConKey                       = mkPreludeTyConUnique 43
tcArrowTyConKey                       = mkPreludeTyConUnique 44

statePrimTyConKey, stableNamePrimTyConKey, mutVarPrimTyConKey, ioTyConKey,
  wordPrimTyConKey, wordTyConKey, word8PrimTyConKey, word8TyConKey,
  word16PrimTyConKey, word16TyConKey, word32PrimTyConKey, word32TyConKey,
  word64PrimTyConKey, word64TyConKey, threadIdPrimTyConKey, bcoPrimTyConKey,
  ptrTyConKey, funPtrTyConKey, tVarPrimTyConKey, eqPrimTyConKey,
  eqReprPrimTyConKey, eqPhantPrimTyConKey, compactPrimTyConKey,
  stackSnapshotPrimTyConKey, promptTagPrimTyConKey, constPtrTyConKey,
  jsvalTyConKey :: KnownKey
statePrimTyConKey                       = mkPreludeTyConUnique 50
stableNamePrimTyConKey                  = mkPreludeTyConUnique 51
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
threadIdPrimTyConKey                    = mkPreludeTyConUnique 75
bcoPrimTyConKey                         = mkPreludeTyConUnique 76
ptrTyConKey                             = mkPreludeTyConUnique 77
funPtrTyConKey                          = mkPreludeTyConUnique 78
tVarPrimTyConKey                        = mkPreludeTyConUnique 79
compactPrimTyConKey                     = mkPreludeTyConUnique 80
stackSnapshotPrimTyConKey               = mkPreludeTyConUnique 81
promptTagPrimTyConKey                   = mkPreludeTyConUnique 82

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

trTyConTyConKey :: KnownKey
trTyConTyConKey                         = mkPreludeTyConUnique 104

-- "Unsatisfiable" constraint
unsatisfiableClassKey :: KnownKey
unsatisfiableClassKey = mkPreludeTyConUnique 170

anyTyConKey :: KnownKey
anyTyConKey = mkPreludeTyConUnique 171

unusedTypeTyConKey :: Unique
unusedTypeTyConKey = mkPreludeTyConUnique 172

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

callStackTyConKey :: KnownKey
callStackTyConKey = mkPreludeTyConUnique 191

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
--      GHC.Builtin.TH: USES TyConUniques 200-299
-----------------------------------------------------

liftClassKey :: Unique
liftClassKey = mkPreludeClassUnique 200

----------------------- SIMD ------------------------
--      USES TyConUniques 300-399
-----------------------------------------------------

#include "primop-vector-uniques.hs-incl"

------------- Type-level Symbol, Nat, Char ----------
--      USES TyConUniques 400-499
-----------------------------------------------------
typeSymbolKindConNameKey,
  typeNatAddTyFamNameKey, typeNatMulTyFamNameKey, typeNatExpTyFamNameKey,
  typeNatSubTyFamNameKey
  , typeSymbolCmpTyFamNameKey, typeNatCmpTyFamNameKey, typeCharCmpTyFamNameKey
  , typeNatDivTyFamNameKey
  , typeNatModTyFamNameKey
  , typeNatLogTyFamNameKey
  , typeConsSymbolTyFamNameKey, typeUnconsSymbolTyFamNameKey
  , typeCharToNatTyFamNameKey, typeNatToCharTyFamNameKey
  , exceptionContextTyConKey, unsafeUnpackJSStringUtf8ShShKey
  :: KnownKey
typeSymbolKindConNameKey  = mkPreludeTyConUnique 400
typeNatAddTyFamNameKey    = mkPreludeTyConUnique 402
typeNatMulTyFamNameKey    = mkPreludeTyConUnique 403
typeNatExpTyFamNameKey    = mkPreludeTyConUnique 404
typeNatSubTyFamNameKey    = mkPreludeTyConUnique 405
typeSymbolCmpTyFamNameKey = mkPreludeTyConUnique 406
typeNatCmpTyFamNameKey    = mkPreludeTyConUnique 407
typeCharCmpTyFamNameKey   = mkPreludeTyConUnique 408
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
  floatDataConKey, intDataConKey, nilDataConKey, ratioDataConKey,
  trueDataConKey, wordDataConKey, word8DataConKey, heqDataConKey, eqDataConKey,
  nothingDataConKey, justDataConKey :: KnownKey

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
trueDataConKey                          = mkPreludeDataConUnique 14
wordDataConKey                          = mkPreludeDataConUnique 15
heqDataConKey                           = mkPreludeDataConUnique 18

ordLTDataConKey, ordEQDataConKey, ordGTDataConKey :: KnownKey
ordLTDataConKey                         = mkPreludeDataConUnique 27
ordEQDataConKey                         = mkPreludeDataConUnique 28
ordGTDataConKey                         = mkPreludeDataConUnique 29

coercibleDataConKey :: KnownKey
coercibleDataConKey                     = mkPreludeDataConUnique 32

staticPtrDataConKey :: KnownKey
staticPtrDataConKey                     = mkPreludeDataConUnique 33

staticPtrInfoDataConKey :: KnownKey
staticPtrInfoDataConKey                 = mkPreludeDataConUnique 34

typeErrorTextDataConKey,
  typeErrorAppendDataConKey,
  typeErrorVAppendDataConKey,
  typeErrorShowTypeDataConKey
  :: KnownKey
typeErrorTextDataConKey                 = mkPreludeDataConUnique 50
typeErrorAppendDataConKey               = mkPreludeDataConUnique 51
typeErrorVAppendDataConKey              = mkPreludeDataConUnique 52
typeErrorShowTypeDataConKey             = mkPreludeDataConUnique 53


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
--      GHC.Builtin.TH: USES DataUniques 200-250
-----------------------------------------------------


{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
*                                                                      *
************************************************************************
-}

wildCardKey, absentErrorIdKey, absentConstraintErrorIdKey, augmentIdKey,
    buildIdKey, recSelErrorIdKey,
    seqIdKey, eqStringIdKey,
    noMethodBindingErrorIdKey, nonExhaustiveGuardsErrorIdKey,
    impossibleErrorIdKey, impossibleConstraintErrorIdKey,
    patErrorIdKey, voidPrimIdKey,
    realWorldPrimIdKey, recConErrorIdKey,
    unpackCStringUtf8IdKey, unpackCStringAppendUtf8IdKey, unpackCStringFoldrUtf8IdKey,
    unpackCStringIdKey, unpackCStringAppendIdKey, unpackCStringFoldrIdKey,
    typeErrorIdKey, divIntIdKey, modIntIdKey,
    absentSumFieldErrorIdKey, cstringLengthIdKey :: KnownKey

wildCardKey                    = mkPreludeMiscIdUnique  0  -- See Note [WildCard binders]
absentErrorIdKey               = mkPreludeMiscIdUnique  1
absentConstraintErrorIdKey     = mkPreludeMiscIdUnique  2
augmentIdKey                   = mkPreludeMiscIdUnique  3
buildIdKey                     = mkPreludeMiscIdUnique  5
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

nullAddrIdKey, voidArgIdKey, otherwiseIdKey, assertIdKey :: KnownKey
nullAddrIdKey                 = mkPreludeMiscIdUnique 39
voidArgIdKey                  = mkPreludeMiscIdUnique 40
otherwiseIdKey                = mkPreludeMiscIdUnique 43
assertIdKey                   = mkPreludeMiscIdUnique 44

leftSectionKey, rightSectionKey :: KnownKey
leftSectionKey                = mkPreludeMiscIdUnique 45
rightSectionKey               = mkPreludeMiscIdUnique 46

rootMainKey :: KnownKey
rootMainKey                   = mkPreludeMiscIdUnique 101

lazyIdKey, oneShotKey, runRWKey :: KnownKey
lazyIdKey                     = mkPreludeMiscIdUnique 104
oneShotKey                    = mkPreludeMiscIdUnique 106
runRWKey                      = mkPreludeMiscIdUnique 107

nospecIdKey :: KnownKey
nospecIdKey                   = mkPreludeMiscIdUnique 109

inlineIdKey, noinlineIdKey, noinlineConstraintIdKey :: KnownKey
inlineIdKey                   = mkPreludeMiscIdUnique 120

dollarIdKey, coercionTokenIdKey, considerAccessibleIdKey :: KnownKey
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
unboundKey = mkPreludeMiscIdUnique 136

thenMClassOpKey, returnMClassOpKey :: KnownKey
thenMClassOpKey               = mkPreludeMiscIdUnique 155 -- (>>)
returnMClassOpKey             = mkPreludeMiscIdUnique 157

-- Conversion functions
fromIntegralIdKey, realToFracIdKey, toIntegerClassOpKey, toRationalClassOpKey :: KnownKey
fromIntegralIdKey    = mkPreludeMiscIdUnique 190
realToFracIdKey      = mkPreludeMiscIdUnique 191
toIntegerClassOpKey  = mkPreludeMiscIdUnique 192
toRationalClassOpKey = mkPreludeMiscIdUnique 193

-- Overloaded lists
toListClassOpKey :: KnownKey
toListClassOpKey    = mkPreludeMiscIdUnique 501

proxyHashKey :: KnownKey
proxyHashKey = mkPreludeMiscIdUnique 502

---------------- Template Haskell -------------------
--      GHC.Builtin.TH: USES IdUniques 200-499
-----------------------------------------------------

heqSCSelIdKey, eqSCSelIdKey, coercibleSCSelIdKey :: KnownKey
eqSCSelIdKey        = mkPreludeMiscIdUnique 551
heqSCSelIdKey       = mkPreludeMiscIdUnique 552
coercibleSCSelIdKey = mkPreludeMiscIdUnique 553

sappendClassOpKey :: KnownKey
sappendClassOpKey = mkPreludeMiscIdUnique 554

mappendClassOpKey :: KnownKey
mappendClassOpKey = mkPreludeMiscIdUnique 556

makeStaticKey :: KnownKey
makeStaticKey = mkPreludeMiscIdUnique 561

-- Unsafe coercion proofs
unsafeEqualityProofIdKey, unsafeCoercePrimIdKey :: KnownKey
unsafeEqualityProofIdKey = mkPreludeMiscIdUnique 570
unsafeCoercePrimIdKey    = mkPreludeMiscIdUnique 571

-- HasField class ops
getFieldClassOpKey :: KnownKey
getFieldClassOpKey = mkPreludeMiscIdUnique 572

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

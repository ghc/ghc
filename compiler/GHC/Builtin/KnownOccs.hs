{-# LANGUAGE MonadComprehensions #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | RdrNames used in 'deriving'

module GHC.Builtin.KnownOccs where

import GHC.Prelude

import GHC.Hs


import GHC.Builtin.PrimOps
import GHC.Builtin.Types  -- A bunch of wired-in TyCons and DataCons
import GHC.Builtin.PrimOps.Ids (primOpId)
import GHC.Builtin.KnownKeys

import GHC.Types.Name( KnownOcc )
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader( RdrName, mkVarUnqual, getRdrName
                            , nameRdrName, knownOccRdrName, mkUnqual )

import GHC.Data.List.Infinite (Infinite (..))
import qualified GHC.Data.List.Infinite as Inf

import GHC.Data.FastString

{- Note [RdrNames in derived code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `deriving` mechanism generates (HsExpr GhcPs) with lots and lots of
references to fixed functions: `enumFrom`, `fmap` etc etc.  We use multiple
mechanisms:

* For wired-in things we use (nameRdrName thing), to get
  an ExactName RdrName for the thing.

* For known-occurrence things we use either
     knownOccRdrName occ  or   knownKeyRdrName key
  to make an ExactOcc RdrName for the thing.  We use the latter for
  known-key things, merely to avoid duplicating knowledge of the KnownOcc

-}


{- *********************************************************************
*                                                                      *
        Infrastructure
*                                                                      *
********************************************************************* -}

knownVarOccRdrName :: String -> RdrName
knownVarOccRdrName s = knownOccRdrName (mkVarOcc s)

knownTcOccRdrName :: String -> RdrName
knownTcOccRdrName s = knownOccRdrName (mkTcOcc s)

knownDataOccRdrName :: String -> RdrName
knownDataOccRdrName s = knownOccRdrName (mkDataOcc s)

knownFieldOccRdrName :: String -> String -> RdrName
knownFieldOccRdrName con s = knownOccRdrName (mkOccNameFS (fieldName (fsLit con)) (fsLit s))

primOpRdrName :: PrimOp -> RdrName
primOpRdrName op = getRdrName (primOpId op)


{- *********************************************************************
*                                                                      *
        Known-occ OccNames
*                                                                      *
********************************************************************* -}

knownOccs :: [KnownOcc]
-- Used only for sanity-checks
-- Sadly incomplete .. is it worth it?  See fromEnum_REDR etc....
knownOccs
  = [ composeIdOcc
    , rationalTyConOcc

    -- Enum class ops
    , enumFromClassOpOcc, enumFromThenClassOpOcc
    , enumFromToClassOpOcc, enumFromThenToClassOpOcc

    -- Static
    , fromStaticPtrClassOpOcc

    -- Typeable stuff
    , someTypeRepTyConOcc, someTypeRepDataConOcc, mkTrConOcc, mkTrAppCheckedOcc
    , mkTrFunOcc, typeRepIdOcc, typeNatTypeRepOcc, typeSymbolTypeRepOcc
    , typeCharTypeRepOcc, typeLitSymbolDataConOcc, typeLitNatDataConOcc
    , typeLitCharDataConOcc
    , trModuleTyConOcc, trModuleDataConOcc, trNameSDataConOcc, trTyConTyConOcc
    , trTyConDataConOcc, kindRepTyConOcc, kindRepTyConAppDataConOcc, kindRepVarDataConOcc
    , kindRepAppDataConOcc, kindRepFunDataConOcc, kindRepTYPEDataConOcc
    , kindRepTypeLitSDataConOcc
    ]

-- Some known-occ data types and their constructors
eitherTyConOcc, leftDataConOcc, rightDataConOcc, voidTyConOcc, rationalTyConOcc :: KnownOcc
eitherTyConOcc   = mkTcOcc "Either"
leftDataConOcc   = mkDataOcc "Left"
rightDataConOcc  = mkDataOcc "Right"
voidTyConOcc     = mkTcOcc "Void"
rationalTyConOcc = mkTcOcc "Rational"

composeIdOcc :: KnownOcc
composeIdOcc = mkVarOcc "."

fromStaticPtrClassOpOcc :: KnownOcc
fromStaticPtrClassOpOcc = mkVarOcc "fromStaticPtr"

returnIOIdOcc, bindIOIdOcc, thenIOIdOcc, printIdOcc :: KnownOcc
returnIOIdOcc = mkVarOcc "returnIO"
bindIOIdOcc   = mkVarOcc "bindIO"
thenIOIdOcc   = mkVarOcc "thenIO"
printIdOcc    = mkVarOcc "print"

-- Enumerations
enumFromClassOpOcc, enumFromThenClassOpOcc,
  enumFromToClassOpOcc, enumFromThenToClassOpOcc :: KnownOcc
enumFromClassOpOcc       = mkVarOcc "enumFrom"
enumFromThenClassOpOcc   = mkVarOcc "enumFromThen"
enumFromToClassOpOcc     = mkVarOcc "enumFromTo"
enumFromThenToClassOpOcc = mkVarOcc "enumFromThenTo"

-- Class Typeable, and functions for constructing `Typeable` dictionaries
someTypeRepTyConOcc
  , someTypeRepDataConOcc
  , mkTrConOcc
  , mkTrAppCheckedOcc
  , mkTrFunOcc
  , typeRepIdOcc
  , typeNatTypeRepOcc
  , typeSymbolTypeRepOcc
  , typeCharTypeRepOcc
  :: KnownOcc
someTypeRepTyConOcc   = mkTcOcc   "SomeTypeRep"
someTypeRepDataConOcc = mkDataOcc "SomeTypeRep"
typeRepIdOcc          = mkVarOcc  "typeRep#"
mkTrConOcc            = mkVarOcc  "mkTrCon"
mkTrAppCheckedOcc     = mkVarOcc  "mkTrAppChecked"
mkTrFunOcc            = mkVarOcc  "mkTrFun"
typeNatTypeRepOcc     = mkVarOcc  "typeNatTypeRep"
typeSymbolTypeRepOcc  = mkVarOcc  "typeSymbolTypeRep"
typeCharTypeRepOcc    = mkVarOcc  "typeCharTypeRep"

typeLitSymbolDataConOcc
  , typeLitNatDataConOcc
  , typeLitCharDataConOcc
  :: KnownOcc
typeLitSymbolDataConOcc = mkDataOcc "TypeLitSymbol"
typeLitNatDataConOcc    = mkDataOcc "TypeLitNat"
typeLitCharDataConOcc   = mkDataOcc "TypeLitChar"


trModuleTyConOcc
  , trModuleDataConOcc
  , trNameSDataConOcc
  , trTyConTyConOcc
  , trTyConDataConOcc
  :: KnownOcc
trModuleTyConOcc     = mkTcOcc "Module"
trModuleDataConOcc   = mkDataOcc "Module"
trNameSDataConOcc    = mkDataOcc "TrNameS"
trTyConTyConOcc      = mkTcOcc   "TyCon"
trTyConDataConOcc    = mkDataOcc "TyCon"

-- Typeable representation types
kindRepTyConOcc
  , kindRepTyConAppDataConOcc
  , kindRepVarDataConOcc
  , kindRepAppDataConOcc
  , kindRepFunDataConOcc
  , kindRepTYPEDataConOcc
  , kindRepTypeLitSDataConOcc
  :: KnownOcc
kindRepTyConOcc           = mkTcOcc "KindRep"
kindRepTyConAppDataConOcc = mkDataOcc "KindRepTyConApp"
kindRepVarDataConOcc      = mkDataOcc "KindRepVar"
kindRepAppDataConOcc      = mkDataOcc "KindRepApp"
kindRepFunDataConOcc      = mkDataOcc "KindRepFun"
kindRepTYPEDataConOcc     = mkDataOcc "KindRepTYPE"
kindRepTypeLitSDataConOcc = mkDataOcc "KindRepTypeLitS"


{- *********************************************************************
*                                                                      *
        Known-occ RdrNames
*                                                                      *
********************************************************************* -}

knownOccRdrNames :: [RdrName]
knownOccRdrNames
  = [ toDyn_RDR, compose_RDR
    , appE_RDR, lift_RDR, liftTyped_RDR
    , enumFrom_RDR, enumFromTo_RDR, enumFromThen_RDR, enumFromThenTo_RDR
    , fromEnum_RDR, toEnum_RDR, toEnumError_RDR, succError_RDR
    , predError_RDR, enumIntToWord_RDR, succ_RDR, pred_RDR
    , minBound_RDR, maxBound_RDR
    , times_RDR, plus_RDR, and_RDR, not_RDR, range_RDR, inRange_RDR, index_RDR
    , unsafeIndex_RDR, unsafeRangeSize_RDR
    ]

main_RDR_Unqual    :: RdrName
main_RDR_Unqual = mkUnqual varName (fsLit "main")
        -- We definitely don't want an Orig RdrName, because
        -- main might, in principle, be imported into module Main


error_RDR :: RdrName
error_RDR = knownVarOccRdrName "error"

toDyn_RDR :: RdrName
toDyn_RDR = knownVarOccRdrName "toDyn"

compose_RDR :: RdrName
compose_RDR = knownOccRdrName composeIdOcc

appE_RDR, lift_RDR, liftTyped_RDR :: RdrName
appE_RDR             = knownVarOccRdrName "appE"
lift_RDR             = knownVarOccRdrName "lift"
liftTyped_RDR        = knownVarOccRdrName "liftTyped"

enumFrom_RDR, enumFromTo_RDR, enumFromThen_RDR, enumFromThenTo_RDR :: RdrName
enumFrom_RDR       = knownOccRdrName enumFromClassOpOcc
enumFromTo_RDR     = knownOccRdrName enumFromToClassOpOcc
enumFromThen_RDR   = knownOccRdrName enumFromThenClassOpOcc
enumFromThenTo_RDR = knownOccRdrName enumFromThenToClassOpOcc

fromEnum_RDR, toEnum_RDR, toEnumError_RDR, succError_RDR,
  predError_RDR, enumIntToWord_RDR,
  succ_RDR, pred_RDR :: RdrName
fromEnum_RDR      = knownVarOccRdrName "fromEnum"
toEnum_RDR        = knownVarOccRdrName "toEnum"
toEnumError_RDR   = knownVarOccRdrName "toEnumError"
succError_RDR     = knownVarOccRdrName "succError"
predError_RDR     = knownVarOccRdrName "predError"
enumIntToWord_RDR = knownVarOccRdrName "enumIntToWord"
succ_RDR          = knownVarOccRdrName "succ"
pred_RDR          = knownVarOccRdrName "pred"

minBound_RDR, maxBound_RDR :: RdrName
minBound_RDR  = knownVarOccRdrName "minBound"
maxBound_RDR  = knownVarOccRdrName "maxBound"

times_RDR, plus_RDR, and_RDR, not_RDR,
  range_RDR, inRange_RDR, index_RDR,
  unsafeIndex_RDR, unsafeRangeSize_RDR :: RdrName
times_RDR           = knownVarOccRdrName "*"
plus_RDR            = knownVarOccRdrName "+"
and_RDR             = knownVarOccRdrName "&&"
not_RDR             = knownVarOccRdrName "not"
range_RDR           = knownVarOccRdrName "range"
inRange_RDR         = knownVarOccRdrName "inRange"
index_RDR           = knownVarOccRdrName "index"
unsafeIndex_RDR     = knownVarOccRdrName "unsafeIndex"
unsafeRangeSize_RDR = knownVarOccRdrName "unsafeRangeSize"

gfoldl_RDR, gunfold_RDR, toConstr_RDR, dataTypeOf_RDR, mkConstrTag_RDR,
    mkDataType_RDR, conIndex_RDR, prefix_RDR, infix_RDR,
    dataCast1_RDR, dataCast2_RDR, gcast1_RDR, gcast2_RDR,
    constr_RDR, dataType_RDR,
    eqChar_RDR  , ltChar_RDR  , geChar_RDR  , gtChar_RDR  , leChar_RDR  ,
    eqInt_RDR   , ltInt_RDR   , geInt_RDR   , gtInt_RDR   , leInt_RDR   , neInt_RDR ,
    eqInt8_RDR  , ltInt8_RDR  , geInt8_RDR  , gtInt8_RDR  , leInt8_RDR  ,
    eqInt16_RDR , ltInt16_RDR , geInt16_RDR , gtInt16_RDR , leInt16_RDR ,
    eqInt32_RDR , ltInt32_RDR , geInt32_RDR , gtInt32_RDR , leInt32_RDR ,
    eqInt64_RDR , ltInt64_RDR , geInt64_RDR , gtInt64_RDR , leInt64_RDR ,
    eqWord_RDR  , ltWord_RDR  , geWord_RDR  , gtWord_RDR  , leWord_RDR  ,
    eqWord8_RDR , ltWord8_RDR , geWord8_RDR , gtWord8_RDR , leWord8_RDR ,
    eqWord16_RDR, ltWord16_RDR, geWord16_RDR, gtWord16_RDR, leWord16_RDR,
    eqWord32_RDR, ltWord32_RDR, geWord32_RDR, gtWord32_RDR, leWord32_RDR,
    eqWord64_RDR, ltWord64_RDR, geWord64_RDR, gtWord64_RDR, leWord64_RDR,
    eqAddr_RDR  , ltAddr_RDR  , geAddr_RDR  , gtAddr_RDR  , leAddr_RDR  ,
    eqFloat_RDR , ltFloat_RDR , geFloat_RDR , gtFloat_RDR , leFloat_RDR ,
    eqDouble_RDR, ltDouble_RDR, geDouble_RDR, gtDouble_RDR, leDouble_RDR,
    int8DataCon_RDR, int16DataCon_RDR, int32DataCon_RDR, int64DataCon_RDR,
    word8DataCon_RDR, word16DataCon_RDR, word32DataCon_RDR, word64DataCon_RDR
    :: RdrName
gfoldl_RDR      = knownVarOccRdrName  "gfoldl"
gunfold_RDR     = knownVarOccRdrName  "gunfold"
toConstr_RDR    = knownVarOccRdrName  "toConstr"
dataTypeOf_RDR  = knownVarOccRdrName  "dataTypeOf"
dataCast1_RDR   = knownVarOccRdrName  "dataCast1"
dataCast2_RDR   = knownVarOccRdrName  "dataCast2"
mkConstrTag_RDR = knownVarOccRdrName  "mkConstrTag"
constr_RDR      = knownTcOccRdrName   "Constr"
mkDataType_RDR  = knownVarOccRdrName  "mkDataType"
dataType_RDR    = knownTcOccRdrName   "DataType"
conIndex_RDR    = knownVarOccRdrName  "constrIndex"
prefix_RDR      = knownDataOccRdrName "Prefix"
infix_RDR       = knownDataOccRdrName "Infix"

gcast1_RDR = knownVarOccRdrName "gcast1"
gcast2_RDR = knownVarOccRdrName "gcast2"

eqChar_RDR     = primOpRdrName CharEqOp
ltChar_RDR     = primOpRdrName CharLtOp
leChar_RDR     = primOpRdrName CharLeOp
gtChar_RDR     = primOpRdrName CharGtOp
geChar_RDR     = primOpRdrName CharGeOp

eqInt_RDR      = primOpRdrName IntEqOp
neInt_RDR      = primOpRdrName IntNeOp
ltInt_RDR      = primOpRdrName IntLtOp
leInt_RDR      = primOpRdrName IntLeOp
gtInt_RDR      = primOpRdrName IntGtOp
geInt_RDR      = primOpRdrName IntGeOp

eqInt8_RDR     = primOpRdrName Int8EqOp
ltInt8_RDR     = primOpRdrName Int8LtOp
leInt8_RDR     = primOpRdrName Int8LeOp
gtInt8_RDR     = primOpRdrName Int8GtOp
geInt8_RDR     = primOpRdrName Int8GeOp

eqInt16_RDR    = primOpRdrName Int16EqOp
ltInt16_RDR    = primOpRdrName Int16LtOp
leInt16_RDR    = primOpRdrName Int16LeOp
gtInt16_RDR    = primOpRdrName Int16GtOp
geInt16_RDR    = primOpRdrName Int16GeOp

eqInt32_RDR    = primOpRdrName Int32EqOp
ltInt32_RDR    = primOpRdrName Int32LtOp
leInt32_RDR    = primOpRdrName Int32LeOp
gtInt32_RDR    = primOpRdrName Int32GtOp
geInt32_RDR    = primOpRdrName Int32GeOp

eqInt64_RDR    = primOpRdrName Int64EqOp
ltInt64_RDR    = primOpRdrName Int64LtOp
leInt64_RDR    = primOpRdrName Int64LeOp
gtInt64_RDR    = primOpRdrName Int64GtOp
geInt64_RDR    = primOpRdrName Int64GeOp

eqWord_RDR     = primOpRdrName WordEqOp
ltWord_RDR     = primOpRdrName WordLtOp
leWord_RDR     = primOpRdrName WordLeOp
gtWord_RDR     = primOpRdrName WordGtOp
geWord_RDR     = primOpRdrName WordGeOp

eqWord8_RDR    = primOpRdrName Word8EqOp
ltWord8_RDR    = primOpRdrName Word8LtOp
leWord8_RDR    = primOpRdrName Word8LeOp
gtWord8_RDR    = primOpRdrName Word8GtOp
geWord8_RDR    = primOpRdrName Word8GeOp

eqWord16_RDR   = primOpRdrName Word16EqOp
ltWord16_RDR   = primOpRdrName Word16LtOp
leWord16_RDR   = primOpRdrName Word16LeOp
gtWord16_RDR   = primOpRdrName Word16GtOp
geWord16_RDR   = primOpRdrName Word16GeOp

eqWord32_RDR   = primOpRdrName Word32EqOp
ltWord32_RDR   = primOpRdrName Word32LtOp
leWord32_RDR   = primOpRdrName Word32LeOp
gtWord32_RDR   = primOpRdrName Word32GtOp
geWord32_RDR   = primOpRdrName Word32GeOp

eqWord64_RDR   = primOpRdrName Word64EqOp
ltWord64_RDR   = primOpRdrName Word64LtOp
leWord64_RDR   = primOpRdrName Word64LeOp
gtWord64_RDR   = primOpRdrName Word64GtOp
geWord64_RDR   = primOpRdrName Word64GeOp

eqAddr_RDR     = primOpRdrName AddrEqOp
ltAddr_RDR     = primOpRdrName AddrLtOp
leAddr_RDR     = primOpRdrName AddrLeOp
gtAddr_RDR     = primOpRdrName AddrGtOp
geAddr_RDR     = primOpRdrName AddrGeOp

eqFloat_RDR    = primOpRdrName FloatEqOp
ltFloat_RDR    = primOpRdrName FloatLtOp
leFloat_RDR    = primOpRdrName FloatLeOp
gtFloat_RDR    = primOpRdrName FloatGtOp
geFloat_RDR    = primOpRdrName FloatGeOp

eqDouble_RDR   = primOpRdrName DoubleEqOp
ltDouble_RDR   = primOpRdrName DoubleLtOp
leDouble_RDR   = primOpRdrName DoubleLeOp
gtDouble_RDR   = primOpRdrName DoubleGtOp
geDouble_RDR   = primOpRdrName DoubleGeOp

int8DataCon_RDR   = knownDataOccRdrName "I8#"
int16DataCon_RDR  = knownDataOccRdrName "I16#"
int32DataCon_RDR  = knownDataOccRdrName "I32#"
int64DataCon_RDR  = knownDataOccRdrName "I64#"
word8DataCon_RDR  = knownDataOccRdrName "W8#"
word16DataCon_RDR = knownDataOccRdrName "W16#"
word32DataCon_RDR = knownDataOccRdrName "W32#"
word64DataCon_RDR = knownDataOccRdrName "W64#"

eq_RDR, ge_RDR, le_RDR, lt_RDR, gt_RDR, compare_RDR :: RdrName
eq_RDR      = knownKeyRdrName eqClassOpKey
ge_RDR      = knownKeyRdrName geClassOpKey
le_RDR      = knownKeyRdrName leClassOpKey
lt_RDR      = knownKeyRdrName ltClassOpKey
gt_RDR      = knownKeyRdrName gtClassOpKey
compare_RDR = knownKeyRdrName compareClassOpKey

returnM_RDR :: RdrName
returnM_RDR = knownKeyRdrName returnMClassOpKey

boolTyCon_RDR, false_RDR, true_RDR, intTyCon_RDR, charTyCon_RDR, stringTyCon_RDR,
    intDataCon_RDR, listTyCon_RDR, consDataCon_RDR,
    ltTag_RDR, eqTag_RDR, gtTag_RDR :: RdrName
-- These are all wired-in
boolTyCon_RDR   = nameRdrName boolTyConName
false_RDR       = nameRdrName falseDataConName
true_RDR        = nameRdrName trueDataConName
intTyCon_RDR    = nameRdrName intTyConName
charTyCon_RDR   = nameRdrName charTyConName
stringTyCon_RDR = nameRdrName stringTyConName
intDataCon_RDR  = nameRdrName intDataConName
listTyCon_RDR   = nameRdrName listTyConName
consDataCon_RDR = nameRdrName consDataConName
ltTag_RDR       = nameRdrName ordLTDataConName
eqTag_RDR       = nameRdrName ordEQDataConName
gtTag_RDR       = nameRdrName ordGTDataConName

map_RDR, fmap_RDR, replace_RDR, pure_RDR, ap_RDR, liftA2_RDR, foldable_foldr_RDR,
    foldMap_RDR, null_RDR, all_RDR, traverse_RDR, mempty_RDR,
    mappend_RDR :: RdrName
map_RDR            = knownKeyRdrName mapIdKey
fmap_RDR           = knownKeyRdrName fmapClassOpKey
pure_RDR           = knownKeyRdrName pureAClassOpKey
ap_RDR             = knownKeyRdrName apAClassOpKey
mempty_RDR         = knownKeyRdrName memptyClassOpKey
mappend_RDR        = knownKeyRdrName mappendClassOpKey
replace_RDR        = knownVarOccRdrName "<$"
liftA2_RDR         = knownVarOccRdrName "liftA2"
foldable_foldr_RDR = knownVarOccRdrName "foldr"
foldMap_RDR        = knownVarOccRdrName "foldMap"
null_RDR           = knownVarOccRdrName "null"
all_RDR            = knownVarOccRdrName "all"
traverse_RDR       = knownVarOccRdrName "traverse"

ltTag_Expr, eqTag_Expr, gtTag_Expr, false_Expr,
  true_Expr, pure_Expr,
  mempty_Expr, foldMap_Expr,
  traverse_Expr, all_Expr, null_Expr :: LHsExpr GhcPs
ltTag_Expr            = nlHsVar ltTag_RDR
eqTag_Expr            = nlHsVar eqTag_RDR
gtTag_Expr            = nlHsVar gtTag_RDR
false_Expr            = nlHsVar false_RDR
true_Expr             = nlHsVar true_RDR
pure_Expr             = nlHsVar pure_RDR
mempty_Expr           = nlHsVar mempty_RDR
foldMap_Expr          = nlHsVar foldMap_RDR
traverse_Expr         = nlHsVar traverse_RDR
all_Expr              = nlHsVar all_RDR
null_Expr             = nlHsVar null_RDR

minusInt_RDR, tagToEnum_RDR, dataToTag_RDR :: RdrName
minusInt_RDR  = primOpRdrName IntSubOp
tagToEnum_RDR = primOpRdrName TagToEnumOp
dataToTag_RDR = knownKeyRdrName dataToTagClassOpKey

-- Generics (constructors and functions)
u1DataCon_RDR, par1DataCon_RDR, rec1DataCon_RDR,
  k1DataCon_RDR, m1DataCon_RDR, l1DataCon_RDR, r1DataCon_RDR,
  prodDataCon_RDR, comp1DataCon_RDR,
  unPar1_RDR, unRec1_RDR, unK1_RDR, unComp1_RDR,
  from_RDR, from1_RDR, to_RDR, to1_RDR,
  datatypeName_RDR, moduleName_RDR, packageName_RDR, isNewtypeName_RDR,
  conName_RDR, conFixity_RDR, conIsRecord_RDR, selName_RDR,
  leftAssocDataCon_RDR, rightAssocDataCon_RDR, notAssocDataCon_RDR,
  uAddrDataCon_RDR, uCharDataCon_RDR, uDoubleDataCon_RDR,
  uFloatDataCon_RDR, uIntDataCon_RDR, uWordDataCon_RDR,
  uAddrHash_RDR, uCharHash_RDR, uDoubleHash_RDR,
  uFloatHash_RDR, uIntHash_RDR, uWordHash_RDR :: RdrName

u1DataCon_RDR    = knownDataOccRdrName "U1"
par1DataCon_RDR  = knownDataOccRdrName "Par1"
rec1DataCon_RDR  = knownDataOccRdrName "Rec1"
k1DataCon_RDR    = knownDataOccRdrName "K1"
m1DataCon_RDR    = knownDataOccRdrName "M1"

l1DataCon_RDR    = knownDataOccRdrName "L1"
r1DataCon_RDR    = knownDataOccRdrName "R1"

prodDataCon_RDR  = knownDataOccRdrName ":*:"
comp1DataCon_RDR = knownDataOccRdrName "Comp1"

unPar1_RDR  = knownFieldOccRdrName "Par1"  "unPar1"
unRec1_RDR  = knownFieldOccRdrName "Rec1"  "unRec1"
unK1_RDR    = knownFieldOccRdrName "K1"    "unK1"
unComp1_RDR = knownFieldOccRdrName "Comp1" "unComp1"

from_RDR  = knownVarOccRdrName "from"
from1_RDR = knownVarOccRdrName "from1"
to_RDR    = knownVarOccRdrName "to"
to1_RDR   = knownVarOccRdrName "to1"

datatypeName_RDR  = knownVarOccRdrName "datatypeName"
moduleName_RDR    = knownVarOccRdrName "moduleName"
packageName_RDR   = knownVarOccRdrName "packageName"
isNewtypeName_RDR = knownVarOccRdrName "isNewtype"
selName_RDR       = knownVarOccRdrName "selName"
conName_RDR       = knownVarOccRdrName "conName"
conFixity_RDR     = knownVarOccRdrName "conFixity"
conIsRecord_RDR   = knownVarOccRdrName "conIsRecord"

leftAssocDataCon_RDR  = nameRdrName leftAssociativeDataConName
rightAssocDataCon_RDR = nameRdrName rightAssociativeDataConName
notAssocDataCon_RDR   = nameRdrName notAssociativeDataConName

uAddrDataCon_RDR   = knownDataOccRdrName "UAddr"
uCharDataCon_RDR   = knownDataOccRdrName "UChar"
uDoubleDataCon_RDR = knownDataOccRdrName "UDouble"
uFloatDataCon_RDR  = knownDataOccRdrName "UFloat"
uIntDataCon_RDR    = knownDataOccRdrName "UInt"
uWordDataCon_RDR   = knownDataOccRdrName "UWord"

uAddrHash_RDR   = knownFieldOccRdrName "UAddr"   "uAddr#"
uCharHash_RDR   = knownFieldOccRdrName "UChar"   "uChar#"
uDoubleHash_RDR = knownFieldOccRdrName "UDouble" "uDouble#"
uFloatHash_RDR  = knownFieldOccRdrName "UFloat"  "uFloat#"
uIntHash_RDR    = knownFieldOccRdrName "UInt"    "uInt#"
uWordHash_RDR   = knownFieldOccRdrName "UWord"   "uWord#"

readList_RDR, readListDefault_RDR, readListPrec_RDR, readListPrecDefault_RDR,
    readPrec_RDR, parens_RDR, choose_RDR, lexP_RDR, expectP_RDR :: RdrName
readList_RDR            = knownVarOccRdrName "readList"
readListDefault_RDR     = knownVarOccRdrName "readListDefault"
readListPrec_RDR        = knownVarOccRdrName "readListPrec"
readListPrecDefault_RDR = knownVarOccRdrName "readListPrecDefault"
readPrec_RDR            = knownVarOccRdrName "readPrec"
parens_RDR              = knownVarOccRdrName "parens"
choose_RDR              = knownVarOccRdrName "choose"
lexP_RDR                = knownVarOccRdrName "lexP"
expectP_RDR             = knownVarOccRdrName "expectP"

readField_RDR, readFieldHash_RDR, readSymField_RDR :: RdrName
readField_RDR           = knownVarOccRdrName "readField"
readFieldHash_RDR       = knownVarOccRdrName "readFieldHash"
readSymField_RDR        = knownVarOccRdrName "readSymField"

punc_RDR, ident_RDR, symbol_RDR :: RdrName
punc_RDR                = knownDataOccRdrName "Punc"
ident_RDR               = knownDataOccRdrName "Ident"
symbol_RDR              = knownDataOccRdrName "Symbol"

step_RDR, alt_RDR, reset_RDR, prec_RDR, pfail_RDR :: RdrName
step_RDR                = knownVarOccRdrName "step"
alt_RDR                 = knownVarOccRdrName "+++"
reset_RDR               = knownVarOccRdrName "reset"
prec_RDR                = knownVarOccRdrName "prec"
pfail_RDR               = knownVarOccRdrName "pfail"

showsPrec_RDR, shows_RDR, showString_RDR,
    showSpace_RDR, showCommaSpace_RDR, showParen_RDR :: RdrName
showsPrec_RDR      = knownVarOccRdrName "showsPrec"
shows_RDR          = knownVarOccRdrName "shows"
showString_RDR     = knownVarOccRdrName "showString"
showSpace_RDR      = knownVarOccRdrName "showSpace"
showCommaSpace_RDR = knownVarOccRdrName "showCommaSpace"
showParen_RDR      = knownVarOccRdrName "showParen"

{- *********************************************************************
*                                                                      *
        Local (unqualified) RdrNames used by derived instances
*                                                                      *
********************************************************************* -}

a_RDR, b_RDR, c_RDR, d_RDR, f_RDR, k_RDR, z_RDR, ah_RDR, bh_RDR, ch_RDR, dh_RDR
    :: RdrName
a_RDR  = mkVarUnqual (fsLit "a")
b_RDR  = mkVarUnqual (fsLit "b")
c_RDR  = mkVarUnqual (fsLit "c")
d_RDR  = mkVarUnqual (fsLit "d")
f_RDR  = mkVarUnqual (fsLit "f")
k_RDR  = mkVarUnqual (fsLit "k")
z_RDR  = mkVarUnqual (fsLit "z")
ah_RDR = mkVarUnqual (fsLit "a#")
bh_RDR = mkVarUnqual (fsLit "b#")
ch_RDR = mkVarUnqual (fsLit "c#")
dh_RDR = mkVarUnqual (fsLit "d#")

as_RDRsInf, bs_RDRsInf, cs_RDRsInf :: Infinite RdrName
as_RDRsInf = [ mkVarUnqual (mkFastString ("a"++show i)) | i <- Inf.enumFrom (1::Int) ]
bs_RDRsInf = [ mkVarUnqual (mkFastString ("b"++show i)) | i <- Inf.enumFrom (1::Int) ]
cs_RDRsInf = [ mkVarUnqual (mkFastString ("c"++show i)) | i <- Inf.enumFrom (1::Int) ]

as_VarsInf, bs_VarsInf :: Infinite (LHsExpr GhcPs)
as_VarsInf = fmap nlHsVar as_RDRsInf
bs_VarsInf = fmap nlHsVar bs_RDRsInf

as_RDRs, bs_RDRs, cs_RDRs :: [RdrName]
as_RDRs = Inf.toList as_RDRsInf
bs_RDRs = Inf.toList bs_RDRsInf
cs_RDRs = Inf.toList cs_RDRsInf

as_Vars, bs_Vars :: [LHsExpr GhcPs]
as_Vars = fmap nlHsVar as_RDRs
bs_Vars = fmap nlHsVar bs_RDRs

a_Expr, b_Expr, c_Expr, f_Expr, z_Expr  :: LHsExpr GhcPs
a_Expr = nlHsVar a_RDR
b_Expr = nlHsVar b_RDR
c_Expr = nlHsVar c_RDR
f_Expr = nlHsVar f_RDR
z_Expr = nlHsVar z_RDR

a_Pat, b_Pat, c_Pat, d_Pat, f_Pat, k_Pat, z_Pat :: LPat GhcPs
a_Pat = nlVarPat a_RDR
b_Pat = nlVarPat b_RDR
c_Pat = nlVarPat c_RDR
d_Pat = nlVarPat d_RDR
f_Pat = nlVarPat f_RDR
k_Pat = nlVarPat k_RDR
z_Pat = nlVarPat z_RDR

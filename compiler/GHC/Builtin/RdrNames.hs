{-# LANGUAGE MonadComprehensions #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | RdrNames used in 'deriving'

module GHC.Builtin.RdrNames where

import GHC.Prelude

import GHC.Hs


import GHC.Builtin
import GHC.Builtin.PrimOps
import GHC.Builtin.Types  -- A bunch of wired-in TyCons and DataCons
import GHC.Builtin.PrimOps.Ids (primOpId)
import GHC.Builtin.TH( unsafeCodeCoerceName, liftTypedName )
import GHC.Builtin.Names

import GHC.Types.Name.Reader( RdrName, mkVarUnqual, getRdrName
                            , nameRdrName )
import GHC.Types.Id.Make( coerceName )  -- `coerce` is wired-in


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
        Global RdrNames used by derived instances
*                                                                      *
********************************************************************* -}

appE_RDR, lift_RDR, liftTyped_RDR, unsafeCodeCoerce_RDR :: RdrName
appE_RDR             = knownVarOccRdrName "appE"
lift_RDR             = knownVarOccRdrName "lift"
unsafeCodeCoerce_RDR = nameRdrName unsafeCodeCoerceName
liftTyped_RDR        = nameRdrName liftTypedName

compose_RDR :: RdrName
compose_RDR = knownKeyRdrName composeIdKey

enumFrom_RDR, enumFromTo_RDR, enumFromThen_RDR, enumFromThenTo_RDR :: RdrName
enumFrom_RDR       = knownKeyRdrName enumFromClassOpKey
enumFromTo_RDR     = knownKeyRdrName enumFromToClassOpKey
enumFromThen_RDR   = knownKeyRdrName enumFromThenClassOpKey
enumFromThenTo_RDR = knownKeyRdrName enumFromThenToClassOpKey

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
gfoldl_RDR     = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "gfoldl")
gunfold_RDR    = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "gunfold")
toConstr_RDR   = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "toConstr")
dataTypeOf_RDR = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "dataTypeOf")
dataCast1_RDR  = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "dataCast1")
dataCast2_RDR  = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "dataCast2")
mkConstrTag_RDR = varQual_RDR gHC_INTERNAL_DATA_DATA (fsLit "mkConstrTag")
constr_RDR     = tcQual_RDR   gHC_INTERNAL_DATA_DATA (fsLit "Constr")
mkDataType_RDR = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "mkDataType")
dataType_RDR   = tcQual_RDR   gHC_INTERNAL_DATA_DATA (fsLit "DataType")
conIndex_RDR   = varQual_RDR  gHC_INTERNAL_DATA_DATA (fsLit "constrIndex")
prefix_RDR     = dataQual_RDR gHC_INTERNAL_DATA_DATA (fsLit "Prefix")
infix_RDR      = dataQual_RDR gHC_INTERNAL_DATA_DATA (fsLit "Infix")

gcast1_RDR     = varQual_RDR  gHC_INTERNAL_TYPEABLE (fsLit "gcast1")
gcast2_RDR     = varQual_RDR  gHC_INTERNAL_TYPEABLE (fsLit "gcast2")

eqChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqChar#")
ltChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltChar#")
leChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "leChar#")
gtChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtChar#")
geChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "geChar#")

eqInt_RDR      = varQual_RDR  gHC_PRIM (fsLit "==#")
neInt_RDR      = varQual_RDR  gHC_PRIM (fsLit "/=#")
ltInt_RDR      = varQual_RDR  gHC_PRIM (fsLit "<#" )
leInt_RDR      = varQual_RDR  gHC_PRIM (fsLit "<=#")
gtInt_RDR      = varQual_RDR  gHC_PRIM (fsLit ">#" )
geInt_RDR      = varQual_RDR  gHC_PRIM (fsLit ">=#")

eqInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqInt8#")
ltInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltInt8#" )
leInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "leInt8#")
gtInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtInt8#" )
geInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "geInt8#")

eqInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqInt16#")
ltInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltInt16#" )
leInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "leInt16#")
gtInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtInt16#" )
geInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "geInt16#")

eqInt32_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqInt32#")
ltInt32_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltInt32#" )
leInt32_RDR    = varQual_RDR  gHC_PRIM (fsLit "leInt32#")
gtInt32_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtInt32#" )
geInt32_RDR    = varQual_RDR  gHC_PRIM (fsLit "geInt32#")

eqInt64_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqInt64#")
ltInt64_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltInt64#" )
leInt64_RDR    = varQual_RDR  gHC_PRIM (fsLit "leInt64#")
gtInt64_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtInt64#" )
geInt64_RDR    = varQual_RDR  gHC_PRIM (fsLit "geInt64#")

eqWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqWord#")
ltWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltWord#")
leWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "leWord#")
gtWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtWord#")
geWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "geWord#")

eqWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqWord8#")
ltWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltWord8#" )
leWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "leWord8#")
gtWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtWord8#" )
geWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "geWord8#")

eqWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "eqWord16#")
ltWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "ltWord16#" )
leWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "leWord16#")
gtWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "gtWord16#" )
geWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "geWord16#")

eqWord32_RDR   = varQual_RDR  gHC_PRIM (fsLit "eqWord32#")
ltWord32_RDR   = varQual_RDR  gHC_PRIM (fsLit "ltWord32#" )
leWord32_RDR   = varQual_RDR  gHC_PRIM (fsLit "leWord32#")
gtWord32_RDR   = varQual_RDR  gHC_PRIM (fsLit "gtWord32#" )
geWord32_RDR   = varQual_RDR  gHC_PRIM (fsLit "geWord32#")

eqWord64_RDR   = varQual_RDR  gHC_PRIM (fsLit "eqWord64#")
ltWord64_RDR   = varQual_RDR  gHC_PRIM (fsLit "ltWord64#" )
leWord64_RDR   = varQual_RDR  gHC_PRIM (fsLit "leWord64#")
gtWord64_RDR   = varQual_RDR  gHC_PRIM (fsLit "gtWord64#" )
geWord64_RDR   = varQual_RDR  gHC_PRIM (fsLit "geWord64#")

eqAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqAddr#")
ltAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltAddr#")
leAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "leAddr#")
gtAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtAddr#")
geAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "geAddr#")

eqFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqFloat#")
ltFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltFloat#")
leFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "leFloat#")
gtFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtFloat#")
geFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "geFloat#")

eqDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit "==##")
ltDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit "<##" )
leDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit "<=##")
gtDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit ">##" )
geDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit ">=##")

int8DataCon_RDR   = dataQual_RDR gHC_INTERNAL_INT (fsLit "I8#")
int16DataCon_RDR  = dataQual_RDR gHC_INTERNAL_INT (fsLit "I16#")
int32DataCon_RDR  = dataQual_RDR gHC_INTERNAL_INT (fsLit "I32#")
int64DataCon_RDR  = dataQual_RDR gHC_INTERNAL_INT (fsLit "I64#")
word8DataCon_RDR  = dataQual_RDR gHC_INTERNAL_WORD (fsLit "W8#")
word16DataCon_RDR = dataQual_RDR gHC_INTERNAL_WORD (fsLit "W16#")
word32DataCon_RDR = dataQual_RDR gHC_INTERNAL_WORD (fsLit "W32#")
word64DataCon_RDR = dataQual_RDR gHC_INTERNAL_WORD (fsLit "W64#")

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
map_RDR                 = knownKeyRdrName mapIdKey
fmap_RDR                = knownKeyRdrName fmapClassOpKey
pure_RDR                = knownKeyRdrName pureAClassOpKey
ap_RDR                  = knownKeyRdrName apAClassOpKey
mempty_RDR              = knownKeyRdrName memptyClassOpKey
mappend_RDR             = knownKeyRdrName mappendClassOpKey
replace_RDR             = varQual_RDR gHC_INTERNAL_BASE (fsLit "<$")
liftA2_RDR              = varQual_RDR gHC_INTERNAL_BASE (fsLit "liftA2")
foldable_foldr_RDR      = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "foldr")
foldMap_RDR             = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "foldMap")
null_RDR                = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "null")
all_RDR                 = varQual_RDR gHC_INTERNAL_DATA_FOLDABLE       (fsLit "all")
traverse_RDR            = varQual_RDR gHC_INTERNAL_DATA_TRAVERSABLE    (fsLit "traverse")

ltTag_Expr, eqTag_Expr, gtTag_Expr, false_Expr,
  true_Expr, pure_Expr, unsafeCodeCoerce_Expr,
  mempty_Expr, foldMap_Expr,
  traverse_Expr, coerce_Expr, all_Expr, null_Expr :: LHsExpr GhcPs
ltTag_Expr            = nlHsVar ltTag_RDR
eqTag_Expr            = nlHsVar eqTag_RDR
gtTag_Expr            = nlHsVar gtTag_RDR
false_Expr            = nlHsVar false_RDR
true_Expr             = nlHsVar true_RDR
pure_Expr             = nlHsVar pure_RDR
unsafeCodeCoerce_Expr = nlHsVar unsafeCodeCoerce_RDR
mempty_Expr           = nlHsVar mempty_RDR
foldMap_Expr          = nlHsVar foldMap_RDR
traverse_Expr         = nlHsVar traverse_RDR
coerce_Expr           = nlHsVar (nameRdrName coerceName)
all_Expr              = nlHsVar all_RDR
null_Expr             = nlHsVar null_RDR

minusInt_RDR, tagToEnum_RDR, dataToTag_RDR :: RdrName
minusInt_RDR  = getRdrName (primOpId IntSubOp   )
tagToEnum_RDR = getRdrName (primOpId TagToEnumOp)
dataToTag_RDR = knownKeyRdrName dataToTagClassOpKey

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

{- *********************************************************************
*                                                                      *
        Local (unqualified) RdrNames used by derived instances
*                                                                      *
********************************************************************* -}

a_RDR, b_RDR, c_RDR, d_RDR, f_RDR, k_RDR, z_RDR, ah_RDR, bh_RDR, ch_RDR, dh_RDR
    :: RdrName
a_RDR           = mkVarUnqual (fsLit "a")
b_RDR           = mkVarUnqual (fsLit "b")
c_RDR           = mkVarUnqual (fsLit "c")
d_RDR           = mkVarUnqual (fsLit "d")
f_RDR           = mkVarUnqual (fsLit "f")
k_RDR           = mkVarUnqual (fsLit "k")
z_RDR           = mkVarUnqual (fsLit "z")
ah_RDR          = mkVarUnqual (fsLit "a#")
bh_RDR          = mkVarUnqual (fsLit "b#")
ch_RDR          = mkVarUnqual (fsLit "c#")
dh_RDR          = mkVarUnqual (fsLit "d#")

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
a_Expr                = nlHsVar a_RDR
b_Expr                = nlHsVar b_RDR
c_Expr                = nlHsVar c_RDR
f_Expr                = nlHsVar f_RDR
z_Expr                = nlHsVar z_RDR

a_Pat, b_Pat, c_Pat, d_Pat, f_Pat, k_Pat, z_Pat :: LPat GhcPs
a_Pat           = nlVarPat a_RDR
b_Pat           = nlVarPat b_RDR
c_Pat           = nlVarPat c_RDR
d_Pat           = nlVarPat d_RDR
f_Pat           = nlVarPat f_RDR
k_Pat           = nlVarPat k_RDR
z_Pat           = nlVarPat z_RDR

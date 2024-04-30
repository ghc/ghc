{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.Heap
  ( closureType
  , infoClosureType
  , infoFunArity
  , isObject
  , isThunk
  , isThunk'
  , isBlackhole
  , isFun
  , isFun'
  , isPap
  , isPap'
  , isCon
  , isCon'
  , conTag
  , conTag'
  , closureInfo
  , closureMeta
  , closureField1
  , closureField2
  , closureCC
  , funArity
  , papArity
  , funOrPapArity
  -- * Field names
  , closureInfo_
  , closureMeta_
  , closureCC_
  , closureField1_
  , closureField2_
  -- * Javascript Type literals
  , jTyObject
  )
where

import GHC.Prelude

import GHC.JS.JStg.Syntax
import GHC.JS.Make
import GHC.StgToJS.Types
import GHC.Data.FastString

-- | Closure infotable field name
closureInfo_ :: FastString
closureInfo_ = "f"

-- | Closure first payload field name
closureField1_ :: FastString
closureField1_ = "d1"

-- | Closure second payload field name
closureField2_ :: FastString
closureField2_ = "d2"

-- | Closure meta field name
closureMeta_ :: FastString
closureMeta_ = "m"

-- | Closure cost-center field name
closureCC_ :: FastString
closureCC_ = "cc"

-- | Infotable type field name
infoClosureType_ :: FastString
infoClosureType_ = "t"

-- | Infotable tag field name
infoConTag_ :: FastString
infoConTag_ = "a"

-- | Infotable arity field name
infoFunArity_ :: FastString
infoFunArity_ = "a"

jTyObject :: JStgExpr
jTyObject = jString "object"

-- | Closure type from infotable
infoClosureType :: JStgExpr -> JStgExpr
infoClosureType f = f .^ infoClosureType_

-- | Function arity from infotable
infoFunArity :: JStgExpr -> JStgExpr
infoFunArity f = f .^ infoFunArity_

closureType :: JStgExpr -> JStgExpr
closureType = infoClosureType . closureInfo

isObject :: JStgExpr -> JStgExpr
isObject c = typeof c .===. String "object"

isThunk :: JStgExpr -> JStgExpr
isThunk c = closureType c .===. toJExpr Thunk

isThunk' :: JStgExpr -> JStgExpr
isThunk' f = infoClosureType f .===. toJExpr Thunk

isBlackhole :: JStgExpr -> JStgExpr
isBlackhole c = closureType c .===. toJExpr Blackhole

isFun :: JStgExpr -> JStgExpr
isFun c = closureType c .===. toJExpr Fun

isFun' :: JStgExpr -> JStgExpr
isFun' f = infoClosureType f .===. toJExpr Fun

isPap :: JStgExpr -> JStgExpr
isPap c = closureType c .===. toJExpr Pap

isPap' :: JStgExpr -> JStgExpr
isPap' f = infoClosureType f .===. toJExpr Pap

isCon :: JStgExpr -> JStgExpr
isCon c = closureType c .===. toJExpr Con

isCon' :: JStgExpr -> JStgExpr
isCon' f = infoClosureType f .===. toJExpr Con

conTag :: JStgExpr -> JStgExpr
conTag = conTag' . closureInfo

conTag' :: JStgExpr -> JStgExpr
conTag' f = f .^ infoConTag_

-- | Get closure infotable
closureInfo :: JStgExpr -> JStgExpr
closureInfo p = p .^ closureInfo_

-- | Get closure metadata
closureMeta :: JStgExpr -> JStgExpr
closureMeta p = p .^ closureMeta_

-- | Get closure cost-center
closureCC :: JStgExpr -> JStgExpr
closureCC p = p .^ closureCC_

-- | Get closure extra field 1
closureField1 :: JStgExpr -> JStgExpr
closureField1 p = p .^ closureField1_

-- | Get closure extra field 2
closureField2 :: JStgExpr -> JStgExpr
closureField2 p = p .^ closureField2_

-- | Number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JStgExpr -> JStgExpr
funArity = infoFunArity . closureInfo

-- arity of a partial application
papArity :: JStgExpr -> JStgExpr
papArity cp = closureField1 (closureField2 cp)

funOrPapArity
  :: JStgExpr       -- ^ heap object
  -> Maybe JStgExpr -- ^ reference to infotable, if you have one already (saves a c.f lookup twice)
  -> JStgExpr       -- ^ arity tag (tag >> 8 = registers, tag & 0xff = arguments)
funOrPapArity c = \case
  Nothing -> ((IfExpr (toJExpr (isFun c))) (toJExpr (funArity c)))
             (toJExpr (papArity c))
  Just f  -> ((IfExpr (toJExpr (isFun' f))) (toJExpr (infoFunArity f)))
             (toJExpr (papArity c))

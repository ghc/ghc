{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.Heap
  ( closureType
  , entryClosureType
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
  , closureEntry
  , closureMeta
  , closureField1
  , closureField2
  , closureCC
  , funArity
  , funArity'
  , papArity
  , funOrPapArity
  -- * Field names
  , closureEntry_
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

closureEntry_ :: FastString
closureEntry_ = "f"

closureField1_ :: FastString
closureField1_ = "d1"

closureField2_ :: FastString
closureField2_ = "d2"

closureMeta_ :: FastString
closureMeta_ = "m"

closureCC_ :: FastString
closureCC_ = "cc"

entryClosureType_ :: FastString
entryClosureType_ = "t"

entryConTag_ :: FastString
entryConTag_ = "a"

entryFunArity_ :: FastString
entryFunArity_ = "a"

jTyObject :: JStgExpr
jTyObject = jString "object"

closureType :: JStgExpr -> JStgExpr
closureType = entryClosureType . closureEntry

entryClosureType :: JStgExpr -> JStgExpr
entryClosureType f = f .^ entryClosureType_

isObject :: JStgExpr -> JStgExpr
isObject c = typeof c .===. String "object"

isThunk :: JStgExpr -> JStgExpr
isThunk c = closureType c .===. toJExpr Thunk

isThunk' :: JStgExpr -> JStgExpr
isThunk' f = entryClosureType f .===. toJExpr Thunk

isBlackhole :: JStgExpr -> JStgExpr
isBlackhole c = closureType c .===. toJExpr Blackhole

isFun :: JStgExpr -> JStgExpr
isFun c = closureType c .===. toJExpr Fun

isFun' :: JStgExpr -> JStgExpr
isFun' f = entryClosureType f .===. toJExpr Fun

isPap :: JStgExpr -> JStgExpr
isPap c = closureType c .===. toJExpr Pap

isPap' :: JStgExpr -> JStgExpr
isPap' f = entryClosureType f .===. toJExpr Pap

isCon :: JStgExpr -> JStgExpr
isCon c = closureType c .===. toJExpr Con

isCon' :: JStgExpr -> JStgExpr
isCon' f = entryClosureType f .===. toJExpr Con

conTag :: JStgExpr -> JStgExpr
conTag = conTag' . closureEntry

conTag' :: JStgExpr -> JStgExpr
conTag' f = f .^ entryConTag_

-- | Get closure entry function
closureEntry :: JStgExpr -> JStgExpr
closureEntry p = p .^ closureEntry_

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

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JStgExpr -> JStgExpr
funArity = funArity' . closureEntry

-- function arity with raw reference to the entry
funArity' :: JStgExpr -> JStgExpr
funArity' f = f .^ entryFunArity_

-- arity of a partial application
papArity :: JStgExpr -> JStgExpr
papArity cp = closureField1 (closureField2 cp)

funOrPapArity
  :: JStgExpr       -- ^ heap object
  -> Maybe JStgExpr -- ^ reference to entry, if you have one already (saves a c.f lookup twice)
  -> JStgExpr       -- ^ arity tag (tag >> 8 = registers, tag & 0xff = arguments)
funOrPapArity c = \case
  Nothing -> ((IfExpr (toJExpr (isFun c))) (toJExpr (funArity c)))
             (toJExpr (papArity c))
  Just f  -> ((IfExpr (toJExpr (isFun' f))) (toJExpr (funArity' f)))
             (toJExpr (papArity c))

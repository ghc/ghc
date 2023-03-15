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

import GHC.JS.Unsat.Syntax
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

jTyObject :: JExpr
jTyObject = jString "object"

closureType :: JExpr -> JExpr
closureType = entryClosureType . closureEntry

entryClosureType :: JExpr -> JExpr
entryClosureType f = f .^ entryClosureType_

isObject :: JExpr -> JExpr
isObject c = typeof c .===. String "object"

isThunk :: JExpr -> JExpr
isThunk c = closureType c .===. toJExpr Thunk

isThunk' :: JExpr -> JExpr
isThunk' f = entryClosureType f .===. toJExpr Thunk

isBlackhole :: JExpr -> JExpr
isBlackhole c = closureType c .===. toJExpr Blackhole

isFun :: JExpr -> JExpr
isFun c = closureType c .===. toJExpr Fun

isFun' :: JExpr -> JExpr
isFun' f = entryClosureType f .===. toJExpr Fun

isPap :: JExpr -> JExpr
isPap c = closureType c .===. toJExpr Pap

isPap' :: JExpr -> JExpr
isPap' f = entryClosureType f .===. toJExpr Pap

isCon :: JExpr -> JExpr
isCon c = closureType c .===. toJExpr Con

isCon' :: JExpr -> JExpr
isCon' f = entryClosureType f .===. toJExpr Con

conTag :: JExpr -> JExpr
conTag = conTag' . closureEntry

conTag' :: JExpr -> JExpr
conTag' f = f .^ entryConTag_

-- | Get closure entry function
closureEntry :: JExpr -> JExpr
closureEntry p = p .^ closureEntry_

-- | Get closure metadata
closureMeta :: JExpr -> JExpr
closureMeta p = p .^ closureMeta_

-- | Get closure cost-center
closureCC :: JExpr -> JExpr
closureCC p = p .^ closureCC_

-- | Get closure extra field 1
closureField1 :: JExpr -> JExpr
closureField1 p = p .^ closureField1_

-- | Get closure extra field 2
closureField2 :: JExpr -> JExpr
closureField2 p = p .^ closureField2_

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JExpr -> JExpr
funArity = funArity' . closureEntry

-- function arity with raw reference to the entry
funArity' :: JExpr -> JExpr
funArity' f = f .^ entryFunArity_

-- arity of a partial application
papArity :: JExpr -> JExpr
papArity cp = closureField1 (closureField2 cp)

funOrPapArity
  :: JExpr       -- ^ heap object
  -> Maybe JExpr -- ^ reference to entry, if you have one already (saves a c.f lookup twice)
  -> JExpr       -- ^ arity tag (tag >> 8 = registers, tag & 0xff = arguments)
funOrPapArity c = \case
  Nothing -> ((IfExpr (toJExpr (isFun c))) (toJExpr (funArity c)))
             (toJExpr (papArity c))
  Just f  -> ((IfExpr (toJExpr (isFun' f))) (toJExpr (funArity' f)))
             (toJExpr (papArity c))

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
  , entry
  , funArity
  , funArity'
  , papArity
  , funOrPapArity
  -- * Field names
  , closureEntry_
  , closureMeta_
  , closureExtra1_
  , closureExtra2_
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.StgToJS.Types
import GHC.Data.ShortText (ShortText)

closureEntry_ :: ShortText
closureEntry_ = "f"

closureExtra1_ :: ShortText
closureExtra1_ = "d1"

closureExtra2_ :: ShortText
closureExtra2_ = "d2"

closureMeta_ :: ShortText
closureMeta_ = "m"

entryClosureType_ :: ShortText
entryClosureType_ = "t"

entryConTag_ :: ShortText
entryConTag_ = "a"

entryFunArity_ :: ShortText
entryFunArity_ = "a"



closureType :: JExpr -> JExpr
closureType = entryClosureType . entry

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
conTag = conTag' . entry

conTag' :: JExpr -> JExpr
conTag' f = f .^ entryConTag_

entry :: JExpr -> JExpr
entry p = p .^ closureEntry_

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JExpr -> JExpr
funArity = funArity' . entry

-- function arity with raw reference to the entry
funArity' :: JExpr -> JExpr
funArity' f = f .^ entryFunArity_

-- arity of a partial application
papArity :: JExpr -> JExpr
papArity cp = cp .^ closureExtra2_ .^ closureExtra1_

funOrPapArity
  :: JExpr       -- ^ heap object
  -> Maybe JExpr -- ^ reference to entry, if you have one already (saves a c.f lookup twice)
  -> JExpr       -- ^ arity tag (tag >> 8 = registers, tag & 0xff = arguments)
funOrPapArity c = \case
  Nothing -> ((IfExpr (toJExpr (isFun c))) (toJExpr (funArity c)))
             (toJExpr (papArity c))
  Just f  -> ((IfExpr (toJExpr (isFun' f))) (toJExpr (funArity' f)))
             (toJExpr (papArity c))

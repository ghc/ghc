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

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.StgToJS.Types
import GHC.Data.ShortText (ShortText)

-- FIXME: Jeff (2022,03): These helpers are a classic case of using a newtype
-- over a type synonym to leverage GHC's type checker. Basically we never want
-- to mix these up, and so we should have:
--------------------------------------
-- newtype ClosureEntry  = ClosureEntry  { unClosureEntry  :: ShortText }
-- newtype ClosureExtra1 = ClosureExtra1 { unClosureExtra1 :: ShortText }
-- newtype ClosureExtra2 = ClosureExtra2 { unClosureExtra2 :: ShortText }
-- newtype ClosureMeta   = ClosureMeta   { unClosureMeta   :: ShortText }
--------------------------------------
-- especially since any bugs which result from confusing these will be catastrophic and hard to debug
-- also NOTE: if ClosureExtra<N> is truly unbounded then we should have:
-- newtype ClosureExtras = ClosureExtras { unClosureExtras :: [ShortText] }
-- or use an Array and amortize increasing the arrays size when needed; depending
-- on its use case in the RTS of course

closureEntry_ :: ShortText
closureEntry_ = "f"

closureField1_ :: ShortText
closureField1_ = "d1"

closureField2_ :: ShortText
closureField2_ = "d2"

closureMeta_ :: ShortText
closureMeta_ = "m"

closureCC_ :: ShortText
closureCC_ = "cc"

entryClosureType_ :: ShortText
entryClosureType_ = "t"

entryConTag_ :: ShortText
entryConTag_ = "a"

entryFunArity_ :: ShortText
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

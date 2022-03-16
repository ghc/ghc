{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
  , closureExtra1
  , closureExtra2
  , closureCC
  , funArity
  , funArity'
  , papArity
  , funOrPapArity
  , Closure (..)
  , newClosure
  , assignClosure
  , CopyCC (..)
  , copyClosure
  -- * Field names
  , closureEntry_
  , closureMeta_
  , closureCC_
  , closureExtra1_
  , closureExtra2_
  -- * Javascript Type literals
  , jTyObject
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.StgToJS.Types
import GHC.Data.ShortText (ShortText)

import Data.Monoid

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

closureExtra1_ :: ShortText
closureExtra1_ = "d1"

closureExtra2_ :: ShortText
closureExtra2_ = "d2"

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
closureExtra1 :: JExpr -> JExpr
closureExtra1 p = p .^ closureExtra1_

-- | Get closure extra field 2
closureExtra2 :: JExpr -> JExpr
closureExtra2 p = p .^ closureExtra2_

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JExpr -> JExpr
funArity = funArity' . closureEntry

-- function arity with raw reference to the entry
funArity' :: JExpr -> JExpr
funArity' f = f .^ entryFunArity_

-- arity of a partial application
papArity :: JExpr -> JExpr
papArity cp = closureExtra1 (closureExtra2 cp)

funOrPapArity
  :: JExpr       -- ^ heap object
  -> Maybe JExpr -- ^ reference to entry, if you have one already (saves a c.f lookup twice)
  -> JExpr       -- ^ arity tag (tag >> 8 = registers, tag & 0xff = arguments)
funOrPapArity c = \case
  Nothing -> ((IfExpr (toJExpr (isFun c))) (toJExpr (funArity c)))
             (toJExpr (papArity c))
  Just f  -> ((IfExpr (toJExpr (isFun' f))) (toJExpr (funArity' f)))
             (toJExpr (papArity c))

-- | Used to pass arguments to newClosure with some safety
data Closure = Closure
  { clEntry  :: JExpr
  , clExtra1 :: JExpr
  , clExtra2 :: JExpr
  , clMeta   :: JExpr
  , clCC     :: Maybe JExpr
  }

newClosure :: Closure -> JExpr
newClosure Closure{..} =
  let xs = [ (closureEntry_ , clEntry)
           , (closureExtra1_, clExtra1)
           , (closureExtra2_, clExtra2)
           , (closureMeta_  , clMeta)
           ]
  in case clCC of
    -- CC field is optional (probably to minimize code size as we could assign
    -- null_, but we get the same effect implicitly)
    Nothing -> ValExpr (jhFromList xs)
    Just cc -> ValExpr (jhFromList $ (closureCC_,cc) : xs)

assignClosure :: JExpr -> Closure -> JStat
assignClosure t Closure{..} = BlockStat
  [ closureEntry  t |= clEntry
  , closureExtra1 t |= clExtra1
  , closureExtra2 t |= clExtra2
  , closureMeta   t |= clMeta
  ] <> case clCC of
      Nothing -> mempty
      Just cc -> closureCC t |= cc

data CopyCC = CopyCC | DontCopyCC

copyClosure :: CopyCC -> JExpr -> JExpr -> JStat
copyClosure copy_cc t s = BlockStat
  [ closureEntry  t |= closureEntry  s
  , closureExtra1 t |= closureExtra1 s
  , closureExtra2 t |= closureExtra2 s
  , closureMeta   t |= closureMeta   s
  ] <> case copy_cc of
      DontCopyCC -> mempty
      CopyCC     -> closureCC t |= closureCC s

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
  -- * Javascript Type literals
  , jTyObject
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.StgToJS.Types
import GHC.Data.ShortText (ShortText)

-- Note [JS heap objects]
-- ~~~~~~~~~~~~~~~~~~~~~~
--
-- TODO: add more details from https://www.haskell.org/haskell-symposium/2013/ghcjs.pdf
--
-- Objects on the heap ("closures") are represented as JavaScript objects with
-- the following fields:
--
--  { f: function -- entry function
--  , m: meta     -- meta data
--  , d1: x       -- closure specific fields
--  , d2: y
--  }
--
-- The object returned when entering heap objects (closure.f) has the following
-- fields:
--
--  { t: closure type
--  , a: constructor tag / fun arity
--  }
--
-- THUNK =
--  { f  = returns the object reduced to WHNF
--  , m  = ?
--  , d1 = ?
--  , d2 = ?
--  }
--
-- FUN =
--  { f  = function itself
--  , m  = ?
--  , d1 = free variable 1
--  , d2 = free variable 2
--  }
--
-- PAP =
--  { f  = ?
--  , m  = ?
--  , d1 = ?
--  , d2 =
--    { d1 = PAP arity
--    }
--  }
--
-- CON =
--  { f  = entry function of the datacon worker
--  , m  = 0
--  , d1 = first arg
--  , d2 = arity = 2: second arg
--         arity > 2: { d1, d2, ...} object with remaining args (starts with "d1 = x2"!)
--  }
--
-- BLACKHOLE =
--  { f  = h$blackhole
--  , m  = ?
--  , d1 = owning TSO
--  , d2 = waiters array
--  }
--
-- STACKFRAME =
--  { f  = ?
--  , m  = ?
--  , d1 = ?
--  , d2 = ?
--  }

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

entryClosureType_ :: ShortText
entryClosureType_ = "t"

entryConTag_ :: ShortText
entryConTag_ = "a"

entryFunArity_ :: ShortText
entryFunArity_ = "a"

jTyObject :: JExpr
jTyObject = jString "object"

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

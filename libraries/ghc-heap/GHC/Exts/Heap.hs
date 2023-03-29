{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
#if MIN_TOOL_VERSION_ghc(9,7,0)
{-# LANGUAGE RecordWildCards #-}
#endif
{-# LANGUAGE UnliftedFFITypes #-}

{-|
Module      :  GHC.Exts.Heap
Copyright   :  (c) 2012 Joachim Breitner
License     :  BSD3
Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>

With this module, you can investigate the heap representation of Haskell
values, i.e. to investigate sharing and lazy evaluation.
-}

module GHC.Exts.Heap (
    -- * Closure types
      Closure
    , GenClosure(..)
    , StackFrame(..)
    , ClosureType(..)
    , PrimType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , RetFunType(..)
    , TsoFlags(..)
    , HasHeapRep(getClosureData)
    , getClosureDataFromHeapRep
    , getClosureDataFromHeapRepPrim

    -- * Info Table types
    , StgInfoTable(..)
    , EntryFunPtr
    , HalfWord
    , ItblCodes
    , itblSize
    , peekItbl
    , pokeItbl

    -- * Cost Centre (profiling) types
    , StgTSOProfInfo(..)
    , IndexTable(..)
    , CostCentre(..)
    , CostCentreStack(..)

     -- * Closure inspection
    , getBoxedClosureData
    , allClosures
    , closureSize

    -- * Boxes
    , Box(..)
    , asBox
    , areBoxesEqual
    ) where

import Prelude
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.ProfInfo.Types
#if defined(PROFILING)
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable
#endif
import GHC.Exts.Heap.Decode

import GHC.Exts
import GHC.Int
import GHC.Word
#if MIN_TOOL_VERSION_ghc(9,7,0)
import GHC.Stack.CloneStack
import GHC.Exts.Stack.Decode
import GHC.Exts.Stack.Constants
import Data.Functor
import Debug.Trace
#endif


#include "ghcconfig.h"

class HasHeapRep (a :: TYPE rep) where

    -- | Decode a closure to it's heap representation ('GenClosure').
    getClosureData
        :: a
        -- ^ Closure to decode.
        -> IO Closure
        -- ^ Heap representation of the closure.

#if __GLASGOW_HASKELL__ >= 901
instance HasHeapRep (a :: TYPE ('BoxedRep 'Lifted)) where
#else
instance HasHeapRep (a :: TYPE 'LiftedRep) where
#endif
    getClosureData = getClosureDataFromHeapObject

#if __GLASGOW_HASKELL__ >= 901
instance HasHeapRep (a :: TYPE ('BoxedRep 'Unlifted)) where
#else
instance HasHeapRep (a :: TYPE 'UnliftedRep) where
#endif
    getClosureData x = getClosureDataFromHeapObject (unsafeCoerce# x)

instance Int# ~ a => HasHeapRep (a :: TYPE 'IntRep) where
    getClosureData x = return $
        IntClosure { ptipe = PInt, intVal = I# x }

instance Word# ~ a => HasHeapRep (a :: TYPE 'WordRep) where
    getClosureData x = return $
        WordClosure { ptipe = PWord, wordVal = W# x }

instance Int64# ~ a => HasHeapRep (a :: TYPE 'Int64Rep) where
    getClosureData x = return $
        Int64Closure { ptipe = PInt64, int64Val = I64# (unsafeCoerce# x) }

instance Word64# ~ a => HasHeapRep (a :: TYPE 'Word64Rep) where
    getClosureData x = return $
        Word64Closure { ptipe = PWord64, word64Val = W64# (unsafeCoerce# x) }

instance Addr# ~ a => HasHeapRep (a :: TYPE 'AddrRep) where
    getClosureData x = return $
        AddrClosure { ptipe = PAddr, addrVal = Ptr x }

instance Float# ~ a => HasHeapRep (a :: TYPE 'FloatRep) where
    getClosureData x = return $
        FloatClosure { ptipe = PFloat, floatVal = F# x }

instance Double# ~ a => HasHeapRep (a :: TYPE 'DoubleRep) where
    getClosureData x = return $
        DoubleClosure { ptipe = PDouble, doubleVal = D# x }

-- | Get the heap representation of a closure _at this moment_, even if it is
-- unevaluated or an indirection or other exotic stuff. Beware when passing
-- something to this function, the same caveats as for
-- 'GHC.Exts.Heap.Closures.asBox' apply.
--
-- For most use cases 'getClosureData' is an easier to use alternative.
--
-- Currently TSO and STACK objects will return `UnsupportedClosure`. This is
-- because it is not memory safe to extract TSO and STACK objects (done via
-- `unpackClosure#`). Other threads may be mutating those objects and interleave
-- with reads in `unpackClosure#`. This is particularly problematic with STACKs
-- where pointer values may be overwritten by non-pointer values as the
-- corresponding haskell thread runs.
getClosureDataFromHeapObject
    :: a
    -- ^ Heap object to decode.
    -> IO Closure
    -- ^ Heap representation of the closure.
getClosureDataFromHeapObject x = do
    case unpackClosure# x of
        (# infoTableAddr, heapRep, pointersArray #) -> do
            let infoTablePtr = Ptr infoTableAddr
                ptrList = [case indexArray# pointersArray i of
                                (# ptr #) -> Box ptr
                            | I# i <- [0..I# (sizeofArray# pointersArray) - 1]
                            ]

            infoTable <- peekItbl infoTablePtr
            case tipe infoTable of
                TSO   -> pure $ UnsupportedClosure infoTable
                STACK -> pure $ UnsupportedClosure infoTable
                _ -> getClosureDataFromHeapRep heapRep infoTablePtr ptrList

-- | Like 'getClosureData', but taking a 'Box', so it is easier to work with.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureData a

-- | Get the size of the top-level closure in words.
-- Includes header and payload. Does not follow pointers.
--
-- @since 8.10.1
closureSize :: Box -> IO Int
closureSize (Box x) = pure $ I# (closureSize# x)

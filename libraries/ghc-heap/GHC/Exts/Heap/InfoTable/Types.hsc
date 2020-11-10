{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module GHC.Exts.Heap.InfoTable.Types
    ( StgInfoTable_(..)
    , StgInfoTable
    , StgStackInfoTable
    , PointerOrData(..)
    , toPointerOrData
    , Layout(..)
    , EntryFunPtr
    , HalfWord
    , ItblCodes
    ) where

#include "Rts.h"

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Generics
import GHC.Exts.Heap.ClosureTypes
import Foreign

type ItblCodes = Either [Word8] [Word32]

#include "ghcautoconf.h"
-- Ultra-minimalist version specially for constructors
#if SIZEOF_VOID_P == 8
type HalfWord = Word32
#elif SIZEOF_VOID_P == 4
type HalfWord = Word16
#else
#error Unknown SIZEOF_VOID_P
#endif

type EntryFunPtr = FunPtr (Ptr () -> IO (Ptr ()))

data LayoutType = Pointers | Bitmap | LargeBitmap

data Layout (a :: LayoutType) where
  Payload :: { ptrs :: HalfWord, nptrs :: HalfWord } -> Layout Pointers
  BM :: [PointerOrData ()] -> Layout Bitmap

data PointerOrData p = Pointer p | Data deriving (Show, Traversable, Foldable, Functor)

toPointerOrData :: Bool -> PointerOrData ()
toPointerOrData True = Pointer ()
toPointerOrData False = Data

deriving instance Show (Layout a)

-- | This is a somewhat faithful representation of an info table. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/includes/rts/storage/InfoTables.h>
-- for more details on this data structure.
data StgInfoTable_ a = StgInfoTable {
   entry  :: Maybe EntryFunPtr, -- Just <=> not TABLES_NEXT_TO_CODE
   layout :: Layout a,
   tipe   :: ClosureType,
   srtlen :: HalfWord,
   code   :: Maybe ItblCodes -- Just <=> TABLES_NEXT_TO_CODE
  } deriving (Show, Generic)

type StgInfoTable = StgInfoTable_ Pointers
type StgStackInfoTable = StgInfoTable_ Bitmap

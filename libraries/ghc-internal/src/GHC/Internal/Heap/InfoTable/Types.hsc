{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Internal.Heap.InfoTable.Types
    ( StgInfoTable(..)
    , EntryFunPtr
    , HalfWord(..)
    , ItblCodes
    ) where

#include "Rts.h"

import GHC.Internal.Base
import GHC.Internal.Generics
import GHC.Internal.ClosureTypes
import GHC.Internal.Foreign.Ptr
import GHC.Internal.Foreign.Storable
import GHC.Internal.Enum
import GHC.Internal.Num
import GHC.Internal.Word
import GHC.Internal.Show
import GHC.Internal.Real
import GHC.Internal.Data.Either

type ItblCodes = Either [Word8] [Word32]

#include "ghcautoconf.h"
-- Ultra-minimalist version specially for constructors
#if SIZEOF_VOID_P == 8
type HalfWord' = Word32
#elif SIZEOF_VOID_P == 4
type HalfWord' = Word16
#else
#error Unknown SIZEOF_VOID_P
#endif

newtype HalfWord = HalfWord HalfWord'
    deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show, Storable)

type EntryFunPtr = FunPtr (Ptr () -> IO (Ptr ()))

-- | This is a somewhat faithful representation of an info table. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/InfoTables.h>
-- for more details on this data structure.
data StgInfoTable = StgInfoTable {
   entry  :: Maybe EntryFunPtr, -- Just <=> not TABLES_NEXT_TO_CODE
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: ClosureType,
   srtlen :: HalfWord,
   code   :: Maybe ItblCodes -- Just <=> TABLES_NEXT_TO_CODE
  } deriving (Eq, Show, Generic)

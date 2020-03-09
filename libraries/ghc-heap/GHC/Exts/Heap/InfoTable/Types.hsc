{-# LANGUAGE DeriveGeneric #-}
module GHC.Exts.Heap.InfoTable.Types
    ( StgInfoTable(..)
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

-- | This is a somewhat faithful representation of an info table. See
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/includes/rts/storage/InfoTables.h>
-- for more details on this data structure.
data StgInfoTable = StgInfoTable {
   entry  :: Maybe EntryFunPtr, -- Just <=> not TABLES_NEXT_TO_CODE
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: ClosureType,
   srtlen :: HalfWord,
   code   :: Maybe ItblCodes -- Just <=> TABLES_NEXT_TO_CODE
  } deriving (Show, Generic)

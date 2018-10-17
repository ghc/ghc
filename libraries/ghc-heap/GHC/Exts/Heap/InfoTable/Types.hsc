module GHC.Exts.Heap.InfoTable.Types
    ( StgInfoTable(..)
    , EntryFunPtr
    , HalfWord
    , ItblCodes
    ) where

#include "Rts.h"

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
-- <http://hackage.haskell.org/trac/ghc/browser/includes/rts/storage/InfoTables.h>
-- for more details on this data structure.
data StgInfoTable = StgInfoTable {
   entry  :: Maybe EntryFunPtr, -- Just <=> not ghciTablesNextToCode
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: ClosureType,
   srtlen :: HalfWord,
   code   :: Maybe ItblCodes -- Just <=> ghciTablesNextToCode
  } deriving (Show)

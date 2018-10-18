module GHC.Exts.Heap.InfoTableProf
    ( module GHC.Exts.Heap.InfoTable.Types
    , itblSize
    , peekItbl
    , pokeItbl
    ) where

-- This file overrides InfoTable.hsc's implementation of peekItbl and pokeItbl.
-- Manually defining PROFILING gives the #peek and #poke macros an accurate
-- representation of StgInfoTable_ when hsc2hs runs.
#define PROFILING
#include "Rts.h"

import GHC.Exts.Heap.InfoTable.Types
#if !defined(TABLES_NEXT_TO_CODE)
import GHC.Exts.Heap.Constants
import Data.Maybe
#endif
import Foreign

-- | Read an InfoTable from the heap into a haskell type.
-- WARNING: This code assumes it is passed a pointer to a "standard" info
-- table. If tables_next_to_code is enabled, it will look 1 byte before the
-- start for the entry field.
peekItbl :: Ptr StgInfoTable -> IO StgInfoTable
peekItbl a0 = do
#if !defined(TABLES_NEXT_TO_CODE)
  let ptr = a0 `plusPtr` (negate wORD_SIZE)
  entry' <- Just <$> (#peek struct StgInfoTable_, entry) ptr
#else
  let ptr = a0
      entry' = Nothing
#endif
  ptrs'   <- (#peek struct StgInfoTable_, layout.payload.ptrs) ptr
  nptrs'  <- (#peek struct StgInfoTable_, layout.payload.nptrs) ptr
  tipe'   <- (#peek struct StgInfoTable_, type) ptr
#if __GLASGOW_HASKELL__ > 804
  srtlen' <- (#peek struct StgInfoTable_, srt) a0
#else
  srtlen' <- (#peek struct StgInfoTable_, srt_bitmap) ptr
#endif
  return StgInfoTable
    { entry  = entry'
    , ptrs   = ptrs'
    , nptrs  = nptrs'
    , tipe   = toEnum (fromIntegral (tipe' :: HalfWord))
    , srtlen = srtlen'
    , code   = Nothing
    }

pokeItbl :: Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl a0 itbl = do
#if !defined(TABLES_NEXT_TO_CODE)
  (#poke StgInfoTable, entry) a0 (fromJust (entry itbl))
#endif
  (#poke StgInfoTable, layout.payload.ptrs) a0 (ptrs itbl)
  (#poke StgInfoTable, layout.payload.nptrs) a0 (nptrs itbl)
  (#poke StgInfoTable, type) a0 (fromEnum (tipe itbl))
#if __GLASGOW_HASKELL__ > 804
  (#poke StgInfoTable, srt) a0 (srtlen itbl)
#else
  (#poke StgInfoTable, srt_bitmap) a0 (srtlen itbl)
#endif
#if defined(TABLES_NEXT_TO_CODE)
  let code_offset = a0 `plusPtr` (#offset StgInfoTable, code)
  case code itbl of
    Nothing -> return ()
    Just (Left xs) -> pokeArray code_offset xs
    Just (Right xs) -> pokeArray code_offset xs
#endif

itblSize :: Int
itblSize = (#size struct StgInfoTable_)

module GHC.Internal.Heap.InfoTableProf
    ( module GHC.Internal.Heap.InfoTable.Types
    , itblSize
    , peekItbl
    , pokeItbl
    ) where

-- This file overrides InfoTable.hsc's implementation of peekItbl and pokeItbl.
-- Manually defining PROFILING gives the #peek and #poke macros an accurate
-- representation of StgInfoTable_ when hsc2hs runs.
#define PROFILING
#include "Rts.h"

import GHC.Internal.Base
import GHC.Internal.Data.Either
import GHC.Internal.Real
import GHC.Internal.Enum

import GHC.Internal.Heap.InfoTable.Types
#if !defined(TABLES_NEXT_TO_CODE)
import GHC.Internal.Heap.Constants
import GHC.Internal.Data.Maybe
#endif
import GHC.Internal.Foreign.Ptr
import GHC.Internal.Foreign.Storable
import GHC.Internal.Foreign.Marshal.Array

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
  srtlen' <- (#peek struct StgInfoTable_, srt) a0
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
  (#poke StgInfoTable, srt) a0 (srtlen itbl)
#if defined(TABLES_NEXT_TO_CODE)
  let code_offset = a0 `plusPtr` (#offset StgInfoTable, code)
  case code itbl of
    Nothing -> return ()
    Just (Left xs) -> pokeArray code_offset xs
    Just (Right xs) -> pokeArray code_offset xs
#endif

itblSize :: Int
itblSize = (#size struct StgInfoTable_)

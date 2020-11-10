{-# LANGUAGE DuplicateRecordFields #-}
module GHC.Exts.Heap.InfoTable
    ( module GHC.Exts.Heap.InfoTable.Types
    , itblSize
    , peekItbl
    , peekStackItbl
    , pokeItbl
    ) where

#include "Rts.h"

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.InfoTable.Types
#if !defined(TABLES_NEXT_TO_CODE)
import GHC.Exts.Heap.Constants
import Data.Maybe
#endif
import Foreign
import Debug.Trace


-------------------------------------------------------------------------
-- Profiling specific code
--
-- The functions that follow all rely on PROFILING. They are duplicated in
-- ghc-heap/GHC/Exts/Heap/InfoTableProf.hsc where PROFILING is defined. This
-- allows hsc2hs to generate values for both profiling and non-profiling builds.

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
    , layout = Payload ptrs' nptrs'
    , tipe   = toEnum (fromIntegral (tipe' :: HalfWord))
    , srtlen = srtlen'
    , code   = Nothing
    }

pokeItbl :: Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl a0 itbl = do
#if !defined(TABLES_NEXT_TO_CODE)
  (#poke StgInfoTable, entry) a0 (fromJust (entry itbl))
#endif
  (#poke StgInfoTable, layout.payload.ptrs) a0 (ptrs (layout itbl))
  (#poke StgInfoTable, layout.payload.nptrs) a0 (nptrs (layout itbl))
  (#poke StgInfoTable, type) a0 (toHalfWord (fromEnum (tipe itbl)))
  (#poke StgInfoTable, srt) a0 (srtlen itbl)
#if defined(TABLES_NEXT_TO_CODE)
  let code_offset = a0 `plusPtr` (#offset StgInfoTable, code)
  case code itbl of
    Nothing -> return ()
    Just (Left xs) -> pokeArray code_offset xs
    Just (Right xs) -> pokeArray code_offset xs
#endif
  where
    toHalfWord :: Int -> HalfWord
    toHalfWord i = fromIntegral i

-- | Size in bytes of a standard InfoTable
itblSize :: Int
itblSize = (#size struct StgInfoTable_)

peekStackItbl :: Ptr StgInfoTable -> IO StgStackInfoTable
peekStackItbl a0 = do
#if !defined(TABLES_NEXT_TO_CODE)
  let ptr = a0 `plusPtr` (negate wORD_SIZE)
  entry' <- Just <$> (#peek struct StgInfoTable_, entry) ptr
#else
  let ptr = a0
      entry' = Nothing
#endif
  bitmap'   <- (#peek StgRetInfoTable, i.layout.bitmap) ptr
  tipe'   <- (#peek StgRetInfoTable, i.type) ptr
  srtlen' <- (#peek StgRetInfoTable, i.srt) a0
  return StgInfoTable
    { entry  = entry'
    , layout   = BM (bitmapBits bitmap')
    , tipe   = toEnum (fromIntegral (tipe' :: HalfWord))
    , srtlen = srtlen'
    , code   = Nothing
    }


bitmapSize ::  Word -> Int
bitmapSize w = fromIntegral (w .&. bITMAP_SIZE_MASK)

bITMAP_SIZE_MASK :: Word
bITMAP_SIZE_MASK = 0x3f
bITMAP_BITS_SHIFT :: Int
bITMAP_BITS_SHIFT = 6

-- True = Pointer
-- False = Data
bitmapBits :: Word -> [PointerOrData ()]
bitmapBits w = reverse (map (toPointerOrData . testBit bs) [0..k-1])
  where
    k = traceShowId (bitmapSize w)
    bs = w `shiftR` bITMAP_BITS_SHIFT

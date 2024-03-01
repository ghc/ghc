{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

--
--  (c) The University of Glasgow 2002-2006
--

-- | Create real byte-code objects from 'ResolvedBCO's.
module GHCi.CreateBCO (createBCOs) where

import Prelude -- See note [Why do we import Prelude here?]
import GHCi.ResolvedBCO
import GHCi.RemoteTypes
import GHCi.BreakArray
import GHC.Data.SizedSeq

import System.IO (fixIO)
import Control.Monad
import Data.Array.Base
import Foreign hiding (newArray)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Arr          ( Array(..) )
import GHC.Exts
import GHC.IO
import Control.Exception ( ErrorCall(..) )

createBCOs :: [ResolvedBCO] -> IO [HValueRef]
createBCOs bcos = do
  let n_bcos = length bcos
  hvals <- fixIO $ \hvs -> do
     let arr = listArray (0, n_bcos-1) hvs
     mapM (createBCO arr) bcos
  mapM mkRemoteRef hvals

createBCO :: Array Int HValue -> ResolvedBCO -> IO HValue
createBCO _   ResolvedBCO{..} | resolvedBCOIsLE /= isLittleEndian
  = throwIO (ErrorCall $
        unlines [ "The endianness of the ResolvedBCO does not match"
                , "the systems endianness. Using ghc and iserv in a"
                , "mixed endianness setup is not supported!"
                ])
createBCO arr bco
   = do linked_bco <- linkBCO' arr bco
        -- Note [Updatable CAF BCOs]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Why do we need mkApUpd0 here?  Otherwise top-level
        -- interpreted CAFs don't get updated after evaluation.  A
        -- top-level BCO will evaluate itself and return its value
        -- when entered, but it won't update itself.  Wrapping the BCO
        -- in an AP_UPD thunk will take care of the update for us.
        --
        -- Furthermore:
        --   (a) An AP thunk *must* point directly to a BCO
        --   (b) A zero-arity BCO *must* be wrapped in an AP thunk
        --   (c) An AP is always fully saturated, so we *can't* wrap
        --       non-zero arity BCOs in an AP thunk.
        --
        -- See #17424.
        if (resolvedBCOArity bco > 0)
           then return (HValue (unsafeCoerce linked_bco))
           else case mkApUpd0# linked_bco of { (# final_bco #) ->
                  return (HValue final_bco) }


linkBCO' :: Array Int HValue -> ResolvedBCO -> IO BCO
linkBCO' arr ResolvedBCO{..} = do
  let
      ptrs   = ssElts resolvedBCOPtrs
      n_ptrs = sizeSS resolvedBCOPtrs

      !(I# arity#)  = resolvedBCOArity

      !(EmptyArr empty#) = emptyArr -- See Note [BCO empty array]
      barr arr# = if I# (sizeofByteArray# arr#) == 0 then empty# else arr#
      insns_barr = barr (getBCOByteArray resolvedBCOInstrs)
      bitmap_barr = barr (getBCOByteArray resolvedBCOBitmap)
      literals_barr = barr (getBCOByteArray resolvedBCOLits)

  PtrsArr marr <- mkPtrsArray arr n_ptrs ptrs
  IO $ \s ->
    case unsafeFreezeArray# marr s of { (# s, arr #) ->
    case newBCO insns_barr literals_barr arr arity# bitmap_barr of { IO io ->
    io s
    }}


-- we recursively link any sub-BCOs while making the ptrs array
mkPtrsArray :: Array Int HValue -> Word -> [ResolvedBCOPtr] -> IO PtrsArr
mkPtrsArray arr n_ptrs ptrs = do
  marr <- newPtrsArray (fromIntegral n_ptrs)
  let
    fill (ResolvedBCORef n) i =
      writePtrsArrayHValue i (arr ! n) marr  -- must be lazy!
    fill (ResolvedBCOPtr r) i = do
      hv <- localRef r
      writePtrsArrayHValue i hv marr
    fill (ResolvedBCOStaticPtr r) i = do
      writePtrsArrayPtr i (fromRemotePtr r)  marr
    fill (ResolvedBCOPtrBCO bco) i = do
      bco <- linkBCO' arr bco
      writePtrsArrayBCO i bco marr
    fill (ResolvedBCOPtrBreakArray r) i = do
      BA mba <- localRef r
      writePtrsArrayMBA i mba marr
  zipWithM_ fill ptrs [0..]
  return marr

data PtrsArr = PtrsArr (MutableArray# RealWorld HValue)

newPtrsArray :: Int -> IO PtrsArr
newPtrsArray (I# i) = IO $ \s ->
  case newArray# i undefined s of (# s', arr #) -> (# s', PtrsArr arr #)

writePtrsArrayHValue :: Int -> HValue -> PtrsArr -> IO ()
writePtrsArrayHValue (I# i) hv (PtrsArr arr) = IO $ \s ->
  case writeArray# arr i hv s of s' -> (# s', () #)

writePtrsArrayPtr :: Int -> Ptr a -> PtrsArr -> IO ()
writePtrsArrayPtr (I# i) (Ptr a#) (PtrsArr arr) = IO $ \s ->
  case writeArrayAddr# arr i a# s of s' -> (# s', () #)

-- This is rather delicate: convincing GHC to pass an Addr# as an Any but
-- without making a thunk turns out to be surprisingly tricky.
{-# NOINLINE writeArrayAddr# #-}
writeArrayAddr# :: MutableArray# s a -> Int# -> Addr# -> State# s -> State# s
#if defined(javascript_HOST_ARCH)
-- Addr# isn't coercible with Any with the JS backend.
writeArrayAddr# = error "writeArrayAddr#: currently unsupported with the JS backend"
#else
writeArrayAddr# marr i addr s = unsafeCoerce# writeArray# marr i addr s
#endif

writePtrsArrayBCO :: Int -> BCO -> PtrsArr -> IO ()
writePtrsArrayBCO (I# i) bco (PtrsArr arr) = IO $ \s ->
  case (unsafeCoerce# writeArray#) arr i bco s of s' -> (# s', () #)

writePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()
writePtrsArrayMBA (I# i) mba (PtrsArr arr) = IO $ \s ->
  case (unsafeCoerce# writeArray#) arr i mba s of s' -> (# s', () #)

newBCO :: ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> IO BCO
newBCO instrs lits ptrs arity bitmap = IO $ \s ->
  newBCO# instrs lits ptrs arity bitmap s

{- Note [BCO empty array]
   ~~~~~~~~~~~~~~~~~~~~~~
Lots of BCOs have empty ptrs or nptrs, but empty arrays are not free:
they are 2-word heap objects.  So let's make a single empty array and
share it between all BCOs.
-}

data EmptyArr = EmptyArr ByteArray#

{-# NOINLINE emptyArr #-}
emptyArr :: EmptyArr
emptyArr = unsafeDupablePerformIO $ IO $ \s ->
  case newByteArray# 0# s of { (# s, arr #) ->
  case unsafeFreezeByteArray# arr s of { (# s, farr #) ->
  (# s, EmptyArr farr #)
  }}

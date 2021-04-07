#include "HsBaseConfig.h"
#if !defined(HAVE_IO_URING)

module System.Linux.IO.URing.Ring where
-- empty module for Cabal

#else
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Linux.IO.URing.Ring
  ( URing
  , newURing
  , submit
  , submitAndWait
  , getSqe
  , freeSqe
  , pushSqe
  , sqePtr
  , popCq
  ) where

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Show (Show)
import Data.Bits
import Data.Functor
import Data.Foldable
import Data.Maybe
import Data.Word

import Foreign.C.Error
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, FunPtr)
import Foreign.Marshal.Array (advancePtr)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))

import System.Linux.IO.URing.Barrier
import System.Linux.IO.URing.Sqe
import System.Linux.IO.URing.Cqe
import System.Linux.IO.URing.PVar

#include <linux/io_uring.h>
#include "hs_uring.h"

data URing
  = URing { uringFptr    :: !(ForeignPtr HsURing)
          , uringFd      :: !CInt
          , uringSQ      :: !SubmissionQueue
          , uringCQ      :: !CompletionQueue
          , uringFreeSqe :: !(PVar SqeIndex)
            -- ^ this is the head of a linked-listed of available 'SqeIndex's,
            -- linked via the fd field.
          }

data SubmissionQueue
  = SQ { uringSQHead     :: !(Ptr Word32)
       , uringSQTail     :: !(Ptr Word32)
       , uringSQRingMask :: !(Ptr Word32)
       , uringSQArray    :: !(Ptr SqeIndex)
       , uringSQEArray   :: !(Ptr Sqe)
       }

data CompletionQueue
  = CQ { uringCQHead     :: !(Ptr Word32)
       , uringCQTail     :: !(Ptr Word32)
       , uringCQRingMask :: !(Ptr Word32)
       , uringCQArray    :: !(Ptr Cqe)
       }

-- | Create a new io-uring.
newURing :: Int -> IO URing
newURing entries = do
    u <- throwErrnoIfNull "new_uring" $ c_new_uring (fromIntegral entries)
    fptr <- newForeignPtr c_free_uring u

    HsURing {..} <- peekHsURing u
    let uringSQ =
          SQ
            { uringSQHead     =           sqPtr sqroHead
            , uringSQTail     =           sqPtr sqroTail
            , uringSQRingMask =           sqPtr sqroRingMask
            , uringSQArray    = castPtr $ sqPtr sqroArray
            , uringSQEArray   = sqeAperture
            }
          where sqPtr field = sqAperture `plusPtr` fromIntegral (field $ uringpSQRingOffsets params)

        uringCQ =
          CQ
            { uringCQHead     = cqPtr cqroHead
            , uringCQTail     = cqPtr cqroTail
            , uringCQRingMask = cqPtr cqroRingMask
            , uringCQArray    = cqPtr cqroCqes
            }
          where cqPtr field = cqAperture `plusPtr` fromIntegral (field $ uringpCQRingOffsets params)

    uringFreeSqe <- newPVar (SqeIndex 0)
    let uring = URing { uringFptr = fptr
                      , uringFd = hsURingFd
                      , ..
                      }

    -- Link together all SQEs.
    maxSqeIndex <- SqeIndex . pred <$> #{peek struct hs_uring, params.sq_entries} u
    flip mapM_ [SqeIndex 0..maxSqeIndex] $ \sqeIdx ->
      pokeSqeLink (sqePtr uring sqeIdx) (succ sqeIdx)

    pokeSqeLink (sqePtr uring maxSqeIndex) invalidSqeIndex

    return uring

invalidSqeIndex :: SqeIndex
invalidSqeIndex = SqeIndex 0xffffffff

-- | An index in the SQ ring.
newtype SqRingIndex = SqRingIndex Word32
  deriving (Eq, Ord, Show, Storable)

-- | Set the SQE index at the given entry in the SQ array.
setSqIndex :: URing -> SqRingIndex -> SqeIndex -> IO ()
setSqIndex uring (SqRingIndex i) x =
    poke (arr `advancePtr` fromIntegral i) x
  where arr = uringSQArray $ uringSQ uring

-- | A pointer to the 'Sqe' at the given 'SqeIndex'.
sqePtr :: URing -> SqeIndex -> Ptr Sqe
sqePtr uring (SqeIndex i) =
    uringSQEArray (uringSQ uring) `plusPtr` off
  where off = fromIntegral i * sqeSize

-- | A pointer to the 'Cqe' at the given 'CqeIndex'.
cqePtr :: URing -> CqeIndex -> Ptr Cqe
cqePtr uring (CqeIndex i) =
    uringCQArray (uringCQ uring) `plusPtr` off
  where
    off = fromIntegral i * cqeSize
    cqeSize = Foreign.Storable.sizeOf (undefined :: Cqe)

-- | Notify the kernel of new SQ submissions, optionally blocking until some
-- have completed.
submit
  :: URing
  -> Int       -- ^ number to submit
  -> Maybe Int -- ^ minimum to complete
  -> IO Int
submit uring to_submit min_complete = do
    let flags = if isJust min_complete then #{const IORING_ENTER_GETEVENTS} else 0
    fmap fromIntegral $ throwErrnoIfMinus1 "io_uring_enter" $ c_io_uring_enter_unsafe
      (uringFd uring)
      (fromIntegral to_submit)
      (maybe 0 fromIntegral min_complete)
      flags
      nullPtr

-- | Notify the kernel of new SQ submissions blocking until some
-- have completed. Uses a safe foreign call under the
-- expectation that this wants to block.
submitAndWait
  :: URing
  -> Int       -- ^ number to submit
  -> Int       -- ^ minimum to complete
  -> IO Int
submitAndWait uring to_submit min_complete = do
    let flags = #{const IORING_ENTER_GETEVENTS}
    fmap fromIntegral $ throwErrnoIfMinus1 "io_uring_enter" $ c_io_uring_enter_safe
      (uringFd uring)
      (fromIntegral to_submit)
      (fromIntegral min_complete)
      flags
      nullPtr

-- | Grab a fresh SQE from the free list.
getSqe :: URing -> IO (Maybe SqeIndex)
getSqe uring = do
    sqeIdx <- readPVar (uringFreeSqe uring)
    if sqeIdx == invalidSqeIndex
      then return Nothing
      else do
        next <- peekSqeLink (sqePtr uring sqeIdx)
        writePVar (uringFreeSqe uring) next
        return $ Just sqeIdx

-- | Return an SQE to the free list.
freeSqe :: URing -> SqeIndex -> IO ()
freeSqe uring sqeIdx = do
    old <- readPVar (uringFreeSqe uring)
    pokeSqeLink (sqePtr uring sqeIdx) old
    writePVar (uringFreeSqe uring) sqeIdx

-- | Push a SQE on to the submission queue. Returns 'False' if the SQ is full.
pushSqe :: URing -> SqeIndex -> IO Bool
pushSqe uring sqeIdx = do
    tail0 <- peek (uringSQTail $ uringSQ uring)
    head0 <- peek (uringSQHead $ uringSQ uring)
    readBarrier
    mask <- peek (uringSQRingMask $ uringSQ uring)
    let toRingIndex :: Word32 -> SqRingIndex
        toRingIndex x = SqRingIndex (x .&. mask)
    if toRingIndex (succ tail0) == toRingIndex head0
      then return False -- SQ ring full
      else do
        setSqIndex uring (toRingIndex tail0) sqeIdx
        writeBarrier
        poke (uringSQTail $ uringSQ uring) (succ tail0)
        writeBarrier
        return True

-- | Pop a completion off of the completion queue.
popCq :: URing -> IO (Maybe Cqe)
popCq uring = do
    hd <- peek $ uringCQHead $ uringCQ uring
    tl <- peek $ uringCQTail $ uringCQ uring
    if hd == tl
      then return Nothing
      else do
        readBarrier
        mask <- peek (uringCQRingMask $ uringCQ uring)
        let index = CqeIndex $ hd .&. mask
        cqe <- peek (cqePtr uring index)
        poke (uringCQHead $ uringCQ uring) (succ hd)
        return (Just cqe)


-----------------------------------------------------------
-- The C interface
-----------------------------------------------------------

data IOURingParams
  = IOURingParams { uringpSQEntries     :: !Word32
                  , uringpCQEntries     :: !Word32
                  , uringpFlags         :: !Word32
                  , uringpSQThreadCpu   :: !Word32
                  , uringpSQThreadIdle  :: !Word32
                  , uringpSQRingOffsets :: !SQRingOffsets
                  , uringpCQRingOffsets :: !CQRingOffsets
                  }

peekIOURingParams :: Ptr IOURingParams -> IO IOURingParams
peekIOURingParams p =
  IOURingParams
    <$> #{peek struct io_uring_params, sq_entries} p
    <*> #{peek struct io_uring_params, cq_entries} p
    <*> #{peek struct io_uring_params, flags} p
    <*> #{peek struct io_uring_params, sq_thread_cpu} p
    <*> #{peek struct io_uring_params, sq_thread_idle} p
    <*> peekSQRingOffsets (#{ptr struct io_uring_params, sq_off} p)
    <*> peekCQRingOffsets (#{ptr struct io_uring_params, cq_off} p)

-- | @struct io_cqring_offsets@.
data CQRingOffsets
  = CQRingOffsets { cqroHead        :: !Word32
                  , cqroTail        :: !Word32
                  , cqroRingMask    :: !Word32
                  , cqroRingEntries :: !Word32
                  , cqroOverflow    :: !Word32
                  , cqroCqes        :: !Word32
                  }

peekCQRingOffsets :: Ptr CQRingOffsets -> IO CQRingOffsets
peekCQRingOffsets p =
  CQRingOffsets
    <$> #{peek struct io_cqring_offsets, head}         p
    <*> #{peek struct io_cqring_offsets, tail}         p
    <*> #{peek struct io_cqring_offsets, ring_mask}    p
    <*> #{peek struct io_cqring_offsets, ring_entries} p
    <*> #{peek struct io_cqring_offsets, overflow}     p
    <*> #{peek struct io_cqring_offsets, cqes}         p

-- | @struct io_sqring_offsets@.
data SQRingOffsets
  = SQRingOffsets { sqroHead        :: !Word32
                  , sqroTail        :: !Word32
                  , sqroRingMask    :: !Word32
                  , sqroRingEntries :: !Word32
                  , sqroFlags       :: !Word32
                  , sqroDropped     :: !Word32
                  , sqroArray       :: !Word32
                  }

peekSQRingOffsets :: Ptr SQRingOffsets -> IO SQRingOffsets
peekSQRingOffsets p =
  SQRingOffsets
    <$> #{peek struct io_sqring_offsets, head}         p
    <*> #{peek struct io_sqring_offsets, tail}         p
    <*> #{peek struct io_sqring_offsets, ring_mask}    p
    <*> #{peek struct io_sqring_offsets, ring_entries} p
    <*> #{peek struct io_sqring_offsets, flags}        p
    <*> #{peek struct io_sqring_offsets, dropped}      p
    <*> #{peek struct io_sqring_offsets, array}        p

data HsURing
    = HsURing { sqeAperture :: !(Ptr Sqe)
              , sqAperture  :: !(Ptr ())
              , cqAperture  :: !(Ptr ())
              , hsURingFd   :: !CInt
              , params      :: !IOURingParams
              }

peekHsURing :: Ptr HsURing -> IO HsURing
peekHsURing p =
  HsURing
    <$> #{peek struct hs_uring, sqe_aperture} p
    <*> #{peek struct hs_uring, sq_aperture}  p
    <*> #{peek struct hs_uring, cq_aperture}  p
    <*> #{peek struct hs_uring, uring_fd}     p
    <*> peekIOURingParams (#{ptr struct hs_uring, params} p)

foreign import ccall "hs_new_uring"
    c_new_uring :: CInt -> IO (Ptr HsURing)
foreign import ccall "&hs_free_uring"
    c_free_uring :: FunPtr (Ptr HsURing -> IO ())
foreign import ccall safe "io_uring_enter"
    c_io_uring_enter_safe
      :: CInt    -- ^ fd
      -> CUInt   -- ^ to_submit
      -> CUInt   -- ^ min_complete
      -> CUInt   -- ^ flags
      -> Ptr a   -- ^ sig
      -> IO CInt
foreign import ccall unsafe "io_uring_enter"
    c_io_uring_enter_unsafe
      :: CInt    -- ^ fd
      -> CUInt   -- ^ to_submit
      -> CUInt   -- ^ min_complete
      -> CUInt   -- ^ flags
      -> Ptr a   -- ^ sig
      -> IO CInt

#endif /* defined(HAVE_IO_URING) */

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Linux.IO.URing.Sqe
  (
    -- * Operations
    nop
  , fsync
  , fdatasync
  , pollAdd
  , pollRemove
  , readv
  , writev
  , nCompletions
  , timeout
  , timeoutNCompletions
  , Timespec(..)
    -- ** Modifiers
  , chained
  , drain
    -- * Types
  , SqeIndex(..)
  , Sqe
  , UserData
  , SqeBuilder
    -- * Internal
  , pokeSqe
  , sqeSize
  -- , dumpSqe
  , peekSqeLink
  , pokeSqeLink
  ) where

import GHC.Base
import GHC.Enum (Enum)
import GHC.Real
import GHC.Show (Show)
import Data.Functor
import Data.Word
import Data.Int
import Data.Bits
import System.Posix.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils (fillBytes)

import System.Linux.IO.URing.PollEvent
import System.Linux.IO.URing.IoVec
import System.Linux.IO.URing.PVar

#include <linux/io_uring.h>

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | An index of the SQ entries array.
newtype SqeIndex = SqeIndex Word32
  deriving (Eq, Ord, Show, Storable, Enum, Prim)

data Sqe

-- | The user data word attached to each SQE.
type UserData = Word64

newtype SqeBuilder a = SqeBuilder { pokeSqe :: Ptr Sqe -> IO a }
  deriving (Functor)

instance Applicative SqeBuilder where
  pure x = SqeBuilder $ \_ -> return x
  SqeBuilder f <*> SqeBuilder g = SqeBuilder $ \p -> do
    h <- f p
    x <- g p
    return $! h x

instance Monad SqeBuilder where
  return = pure
  SqeBuilder f >>= g = SqeBuilder $ \p -> do
    r <- f p
    case g r of
      SqeBuilder g' -> g' p


-----------------------------------------------------------------------------
-- Builder primitives
-----------------------------------------------------------------------------

zeroIt :: SqeBuilder ()
zeroIt = SqeBuilder $ \ptr -> fillBytes ptr 0 (#size struct io_uring_sqe)

pokeField :: (Ptr Sqe -> a -> IO ()) -> a -> SqeBuilder ()
pokeField f x = SqeBuilder $ \ptr -> f ptr x
{-# INLINE pokeField #-}

setOpCode :: Word8 -> SqeBuilder ()
setOpCode = pokeField #{poke struct io_uring_sqe, opcode}

setOff :: Word64 -> SqeBuilder ()
setOff = pokeField #{poke struct io_uring_sqe, off}

setLen :: Word32 -> SqeBuilder ()
setLen = pokeField #{poke struct io_uring_sqe, len}

setAddr :: Ptr a -> SqeBuilder ()
setAddr = pokeField #{poke struct io_uring_sqe, addr}

setFlags :: Word8 -> SqeBuilder ()
setFlags = pokeField #{poke struct io_uring_sqe, flags}

setFd :: Fd -> SqeBuilder ()
setFd fd = pokeField #{poke struct io_uring_sqe, fd} (fromIntegral fd :: Word32)

setUserData :: UserData -> SqeBuilder ()
setUserData = pokeField #{poke struct io_uring_sqe, user_data}

peekSqeLink :: Ptr Sqe -> IO SqeIndex
peekSqeLink = #{peek struct io_uring_sqe, fd}

pokeSqeLink :: Ptr Sqe -> SqeIndex -> IO ()
pokeSqeLink = #{poke struct io_uring_sqe, fd}

-----------------------------------------------------------------------------
-- Modifiers
-----------------------------------------------------------------------------

modifyFlags :: (Word8 -> Word8) -> SqeBuilder a -> SqeBuilder a
modifyFlags f action = do
    r <- action
    flags <- SqeBuilder $ #{peek struct io_uring_sqe, flags}
    setFlags (f flags)
    return r

-- | Mark an SQE as a full pipeline barrier.
drain :: SqeBuilder () -> SqeBuilder ()
drain = modifyFlags (.|. iO_DRAIN)
  where iO_DRAIN = #{const IOSQE_IO_DRAIN}

-- | Ensure that no subsequent SQEs execute before this one has completed
-- successfully.
chained :: SqeBuilder () -> SqeBuilder ()
chained = modifyFlags (.|. iO_LINK)
  where iO_LINK = #{const IOSQE_IO_LINK}

-----------------------------------------------------------------------------
-- Command types
-----------------------------------------------------------------------------

-- | Poll a file descriptor for the specified events.
pollAdd
  :: Fd
  -> Event
  -> UserData
  -> SqeBuilder ()
pollAdd fd events userd = do
    zeroIt
    setOpCode (#const IORING_OP_POLL_ADD)
    setFd fd
    setUserData userd
    pokeField #{poke struct io_uring_sqe, poll_events} events

-- | Cancel a previously issued poll request.
pollRemove
  :: UserData
  -> UserData
  -> SqeBuilder ()
pollRemove toCancel userd = do
    zeroIt
    setOpCode (#const IORING_OP_POLL_REMOVE)
    setAddr $ wordPtrToPtr $ fromIntegral toCancel
    setUserData userd

-- | No operation.
nop :: UserData -> SqeBuilder ()
nop userd = do
    zeroIt
    setOpCode #{const IORING_OP_NOP}
    setUserData userd

-- | Flush metadata and writes to a file.
fsync :: Fd -> UserData -> SqeBuilder ()
fsync fd userd = do
    zeroIt
    setOpCode #{const IORING_OP_FSYNC}
    setFd fd
    setUserData userd

-- | Flush writes to a file.
fdatasync :: Fd -> UserData -> SqeBuilder ()
fdatasync fd userd = do
    zeroIt
    setOpCode #{const IORING_OP_FSYNC}
    setFd fd
    setFlags #{const IORING_FSYNC_DATASYNC}
    setUserData userd

data Timespec = Timespec { tv_sec :: Int64, tv_nsec :: Int64 }

instance Storable Timespec where
  sizeOf _ = 16
  alignment _ = 8
  poke p (Timespec s ns) = pokeElemOff p' 0 s >> pokeElemOff p' 1 ns
    where p' = castPtr p
  peek p = Timespec <$> peekElemOff p' 0 <*> peekElemOff p' 1
    where p' = castPtr p

-- | Completes after the given amount of time has elapsed.
timeout :: Ptr Timespec -> UserData -> SqeBuilder ()
timeout ts userd = timeoutNCompletions (Just ts) 0 userd

-- | Completes when @n@ commands have completed since this command was started.
nCompletions :: Int -> UserData -> SqeBuilder ()
nCompletions n userd = timeoutNCompletions Nothing n userd

-- | Completes when *either* @n@ commands have completed or the given amount of
-- time has elapsed, whichever happens first.
timeoutNCompletions :: Maybe (Ptr Timespec) -> Int -> UserData -> SqeBuilder ()
timeoutNCompletions ts n userd = do
    zeroIt
    setFd (-1)
    setOpCode #{const IORING_OP_TIMEOUT}
    case ts of
      Just ptr -> setAddr ptr >> setLen 1
      Nothing  -> return ()
    setOff (fromIntegral n)
    setUserData userd

-- | Vectored read.
readv
  :: Fd          -- ^ 'Fd' to read from
  -> Word64      -- ^ offset in bytes
  -> Ptr IoVec   -- ^ IO vectors
  -> Word32      -- ^ number of IO vectors
  -> UserData
  -> SqeBuilder ()
readv fd offset iovs iov_cnt userd = do
    zeroIt
    setOpCode (#const IORING_OP_READV)
    setFd fd
    setOff offset
    setAddr iovs
    setLen iov_cnt
    setFlags 0
    setUserData userd

-- | Vectored write.
writev
  :: Fd          -- ^ 'Fd' to read from
  -> Word64      -- ^ offset in bytes
  -> Ptr IoVec   -- ^ IO vectors
  -> Word32      -- ^ number of IO vectors
  -> UserData
  -> SqeBuilder ()
writev fd offset iovs iov_cnt userd = do
    zeroIt
    setOpCode (#const IORING_OP_WRITEV)
    setFd fd
    setOff offset
    setAddr iovs
    setLen iov_cnt
    setFlags 0
    setUserData userd

sqeSize :: Integral a => a
sqeSize = #{size struct io_uring_sqe}


-----------------------------------------------------------------------------
-- Debugging
-----------------------------------------------------------------------------

-- commented out because `sequenceA` and `unlines` would have to be imported and that's
-- not so easy in GHC
-- dumpSqe :: Ptr Sqe -> IO String
-- dumpSqe ptr =
--     unlines <$> sequenceA fields
--   where
--     fields =
--       [ showIt @Word8  "opcode"  #{peek struct io_uring_sqe, opcode}
--       , showIt @Word8  "flags"   #{peek struct io_uring_sqe, flags}
--       , showIt @Word16 "ioprio"  #{peek struct io_uring_sqe, ioprio}
--       , showIt @Int32  "fd"      #{peek struct io_uring_sqe, fd}
--       , showIt @Word64 "offset"  #{peek struct io_uring_sqe, off}
--       , showIt @Word64 "addr"    #{peek struct io_uring_sqe, addr}
--       , showIt @Word32 "len"     #{peek struct io_uring_sqe, len}
--       , showIt @Word32 "other"   #{peek struct io_uring_sqe, fsync_flags}
--       , showIt @Word64 "userd"   #{peek struct io_uring_sqe, user_data}
--       ]
--
--     showIt :: forall a. (Show a, Storable a)
--            => String -> (Ptr Sqe -> IO a) -> IO String
--     showIt name peekField = do
--       val <- peekField ptr
--       return $ name ++ " = " ++ show val


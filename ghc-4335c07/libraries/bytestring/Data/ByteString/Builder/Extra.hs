{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- | Copyright : (c) 2010      Jasper Van der Jeugt
--               (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC
--
-- Extra functions for creating and executing 'Builder's. They are intended
-- for application-specific fine-tuning the performance of 'Builder's.
--
-----------------------------------------------------------------------------
module Data.ByteString.Builder.Extra
    (
    -- * Execution strategies
      toLazyByteStringWith
    , AllocationStrategy
    , safeStrategy
    , untrimmedStrategy
    , smallChunkSize
    , defaultChunkSize

    -- * Controlling chunk boundaries
    , byteStringCopy
    , byteStringInsert
    , byteStringThreshold

    , lazyByteStringCopy
    , lazyByteStringInsert
    , lazyByteStringThreshold

    , flush

    -- * Low level execution
    , BufferWriter
    , Next(..)
    , runBuilder

    -- * Host-specific binary encodings
    , intHost
    , int16Host
    , int32Host
    , int64Host

    , wordHost
    , word16Host
    , word32Host
    , word64Host

    , floatHost
    , doubleHost

    ) where


import Data.ByteString.Builder.Internal
         ( Builder, toLazyByteStringWith
         , AllocationStrategy, safeStrategy, untrimmedStrategy
         , smallChunkSize, defaultChunkSize, flush
         , byteStringCopy, byteStringInsert, byteStringThreshold
         , lazyByteStringCopy, lazyByteStringInsert, lazyByteStringThreshold )

import qualified Data.ByteString.Builder.Internal as I
import qualified Data.ByteString.Builder.Prim  as P
import qualified Data.ByteString.Internal      as S

import Foreign

------------------------------------------------------------------------------
-- Builder execution public API
------------------------------------------------------------------------------

-- | A 'BufferWriter' represents the result of running a 'Builder'.
-- It unfolds as a sequence of chunks of data. These chunks come in two forms:
--
--  * an IO action for writing the Builder's data into a user-supplied memory
--    buffer.
--
--  * a pre-existing chunks of data represented by a strict 'ByteString'
--
-- While this is rather low level, it provides you with full flexibility in
-- how the data is written out.
--
-- The 'BufferWriter' itself is an IO action: you supply it with a buffer
-- (as a pointer and length) and it will write data into the buffer.
-- It returns a number indicating how many bytes were actually written
-- (which can be @0@). It also returns a 'Next' which describes what
-- comes next.
--
type BufferWriter = Ptr Word8 -> Int -> IO (Int, Next)

-- | After running a 'BufferWriter' action there are three possibilities for
-- what comes next:
--
data Next =
     -- | This means we're all done. All the builder data has now been written.
     Done

     -- | This indicates that there may be more data to write. It
     -- gives you the next 'BufferWriter' action. You should call that action
     -- with an appropriate buffer. The int indicates the /minimum/ buffer size
     -- required by the next 'BufferWriter' action. That is, if you call the next
     -- action you /must/ supply it with a buffer length of at least this size.
   | More   !Int          BufferWriter

     -- | In addition to the data that has just been written into your buffer
     -- by the 'BufferWriter' action, it gives you a pre-existing chunk
     -- of data as a 'S.ByteString'. It also gives you the following 'BufferWriter'
     -- action. It is safe to run this following action using a buffer with as
     -- much free space as was left by the previous run action.
   | Chunk  !S.ByteString BufferWriter

-- | Turn a 'Builder' into its initial 'BufferWriter' action.
--
runBuilder :: Builder -> BufferWriter
runBuilder = run . I.runBuilder
  where
    bytesWritten startPtr endPtr = endPtr `minusPtr` startPtr

    run :: I.BuildStep () -> BufferWriter
    run step = \buf len ->
      let doneH endPtr () =
            let !wc  = bytesWritten buf endPtr
                next = Done
             in return (wc, next)

          bufferFullH endPtr minReq step' =
            let !wc  = bytesWritten buf endPtr
                next = More minReq (run step')
             in return (wc, next)

          insertChunkH endPtr bs step' =
            let !wc  = bytesWritten buf endPtr
                next = Chunk bs (run step')
             in return (wc, next)

          br = I.BufferRange buf (buf `plusPtr` len)

      in I.fillWithBuildStep step doneH bufferFullH insertChunkH br



------------------------------------------------------------------------------
-- Host-specific encodings
------------------------------------------------------------------------------

-- | Encode a single native machine 'Int'. The 'Int' is encoded in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or int sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: Int -> Builder
intHost = P.primFixed P.intHost

-- | Encode a 'Int16' in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: Int16 -> Builder
int16Host = P.primFixed P.int16Host

-- | Encode a 'Int32' in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: Int32 -> Builder
int32Host = P.primFixed P.int32Host

-- | Encode a 'Int64' in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: Int64 -> Builder
int64Host = P.primFixed P.int64Host

-- | Encode a single native machine 'Word'. The 'Word' is encoded in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: Word -> Builder
wordHost = P.primFixed P.wordHost

-- | Encode a 'Word16' in native host order and host endianness.
{-# INLINE word16Host #-}
word16Host :: Word16 -> Builder
word16Host = P.primFixed P.word16Host

-- | Encode a 'Word32' in native host order and host endianness.
{-# INLINE word32Host #-}
word32Host :: Word32 -> Builder
word32Host = P.primFixed P.word32Host

-- | Encode a 'Word64' in native host order and host endianness.
{-# INLINE word64Host #-}
word64Host :: Word64 -> Builder
word64Host = P.primFixed P.word64Host

-- | Encode a 'Float' in native host order. Values encoded this way are not
-- portable to different endian machines, without conversion.
{-# INLINE floatHost #-}
floatHost :: Float -> Builder
floatHost = P.primFixed P.floatHost

-- | Encode a 'Double' in native host order.
{-# INLINE doubleHost #-}
doubleHost :: Double -> Builder
doubleHost = P.primFixed P.doubleHost


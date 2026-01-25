{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.Buffer
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable (GHC Extensions)
--
-- Buffers used in the IO system
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.IO.Buffer should be removed in GHC 10.02."
#endif

module GHC.IO.Buffer
  {-# DEPRECATED ["GHC.IO.Buffer is deprecated and will be removed in GHC 10.02. Please use the ghc-internal package."] #-}
    (-- *  Buffers of any element
     Buffer(..),
     BufferState(..),
     CharBuffer,
     CharBufElem,
     -- **  Creation
     newByteBuffer,
     newCharBuffer,
     newBuffer,
     emptyBuffer,
     -- **  Insertion/removal
     bufferRemove,
     bufferAdd,
     slideContents,
     bufferAdjustL,
     bufferAddOffset,
     bufferAdjustOffset,
     -- **  Inspecting
     isEmptyBuffer,
     isFullBuffer,
     isFullCharBuffer,
     isWriteBuffer,
     bufferElems,
     bufferAvailable,
     bufferOffset,
     summaryBuffer,
     -- **  Operating on the raw buffer as a Ptr
     withBuffer,
     withRawBuffer,
     -- **  Assertions
     checkBuffer,
     -- *  Raw buffers
     RawBuffer,
     readWord8Buf,
     writeWord8Buf,
     RawCharBuffer,
     peekCharBuf,
     readCharBuf,
     writeCharBuf,
     readCharBufPtr,
     writeCharBufPtr,
     charSize
     ) where

import GHC.Internal.IO.Buffer
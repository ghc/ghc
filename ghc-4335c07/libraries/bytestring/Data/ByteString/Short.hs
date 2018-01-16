{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.ByteString.Short
-- Copyright   : (c) Duncan Coutts 2012-2013
-- License     : BSD-style
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : stable
-- Portability : ghc only
-- 
-- A compact representation suitable for storing short byte strings in memory.
--
-- In typical use cases it can be imported alongside "Data.ByteString", e.g.
--
-- > import qualified Data.ByteString       as B
-- > import qualified Data.ByteString.Short as B
-- >          (ShortByteString, toShort, fromShort)
--
-- Other 'ShortByteString' operations clash with "Data.ByteString" or "Prelude"
-- functions however, so they should be imported @qualified@ with a different
-- alias e.g.
--
-- > import qualified Data.ByteString.Short as B.Short
--
module Data.ByteString.Short (

    -- * The @ShortByteString@ type

    ShortByteString,

    -- ** Memory overhead
    -- | With GHC, the memory overheads are as follows, expressed in words and
    -- in bytes (words are 4 and 8 bytes on 32 or 64bit machines respectively).
    --
    -- * 'ByteString' unshared: 9 words; 36 or 72 bytes.
    --
    -- * 'ByteString' shared substring: 5 words; 20 or 40 bytes.
    --
    -- * 'ShortByteString': 4 words; 16 or 32 bytes.
    --
    -- For the string data itself, both 'ShortByteString' and 'ByteString' use
    -- one byte per element, rounded up to the nearest word. For example,
    -- including the overheads, a length 10 'ShortByteString' would take
    -- @16 + 12 = 28@ bytes on a 32bit platform and @32 + 16 = 48@ bytes on a
    -- 64bit platform.
    --
    -- These overheads can all be reduced by 1 word (4 or 8 bytes) when the
    -- 'ShortByteString' or 'ByteString' is unpacked into another constructor.
    --
    -- For example:
    --
    -- > data ThingId = ThingId {-# UNPACK #-} !Int
    -- >                        {-# UNPACK #-} !ShortByteString
    --
    -- This will take @1 + 1 + 3@ words (the @ThingId@ constructor +
    -- unpacked @Int@ + unpacked @ShortByteString@), plus the words for the
    -- string data.
    
    -- ** Heap fragmentation
    -- | With GHC, the 'ByteString' representation uses /pinned/ memory,
    -- meaning it cannot be moved by the GC. This is usually the right thing to
    -- do for larger strings, but for small strings using pinned memory can
    -- lead to heap fragmentation which wastes space. The 'ShortByteString'
    -- type (and the @Text@ type from the @text@ package) use /unpinned/ memory
    -- so they do not contribute to heap fragmentation. In addition, with GHC,
    -- small unpinned strings are allocated in the same way as normal heap
    -- allocations, rather than in a separate pinned area.

    -- * Conversions
    toShort,
    fromShort,
    pack,
    unpack,

    -- * Other operations
    empty, null, length, index,
  ) where

import Data.ByteString.Short.Internal
import Prelude ()


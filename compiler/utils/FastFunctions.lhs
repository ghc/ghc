Z%
% (c) The University of Glasgow, 2000-2006
%
\section{Fast functions}

\begin{code}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

module FastFunctions (
    unsafeChr, inlinePerformIO, unsafeDupableInterleaveIO,
    indexWord8OffFastPtr,
    indexWord8OffFastPtrAsFastChar, indexWord8OffFastPtrAsFastInt,
    global, Global
  ) where

#include "HsVersions.h"

import FastTypes
import Data.IORef
import System.IO.Unsafe

#if defined(__GLASGOW_HASKELL__)

import GHC.Exts
import GHC.Word
import GHC.Base (unsafeChr)

import GHC.IO   (IO(..), unsafeDupableInterleaveIO)

-- Just like unsafePerformIO, but we inline it.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

indexWord8OffFastPtr p i = W8# (indexWord8OffAddr# p i)
indexWord8OffFastPtrAsFastChar p i = indexCharOffAddr# p i
indexWord8OffFastPtrAsFastInt p i = word2Int# (indexWord8OffAddr# p i)
-- or ord# (indexCharOffAddr# p i)

#else /* ! __GLASGOW_HASKELL__ */

import Foreign.Ptr
import Data.Word

-- hey, no harm inlining it, :-P
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO = unsafePerformIO

unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO = unsafeInterleaveIO

-- truly, these functions are unsafe: they assume
-- a certain immutability of the pointer's target area.
indexWord8OffFastPtr p i = inlinePerformIO (peekByteOff p n) :: Word8
indexWord8OffFastPtrAsFastInt p i =
  iUnbox (fromIntegral (inlinePerformIO (peekByteOff p n) :: Word8))
indexWord8OffFastPtrAsFastChar p i =
  fastChr (iUnbox (fromIntegral (inlinePerformIO (peekByteOff p n) :: Word8)))

#endif /* ! __GLASGOW_HASKELL__ */

--just so we can refer to the type clearly in a macro
type Global a = IORef a
global :: a -> Global a
global a = unsafePerformIO (newIORef a)

indexWord8OffFastPtr :: FastPtr Word8 -> FastInt -> Word8
indexWord8OffFastPtrAsFastChar :: FastPtr Word8 -> FastInt -> FastChar
indexWord8OffFastPtrAsFastInt :: FastPtr Word8 -> FastInt -> FastInt

\end{code}

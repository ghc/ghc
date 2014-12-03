{-
Z%
(c) The University of Glasgow, 2000-2006

\section{Fast functions}
-}

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

--just so we can refer to the type clearly in a macro
type Global a = IORef a
global :: a -> Global a
global a = unsafePerformIO (newIORef a)

indexWord8OffFastPtr :: FastPtr Word8 -> FastInt -> Word8
indexWord8OffFastPtrAsFastChar :: FastPtr Word8 -> FastInt -> FastChar
indexWord8OffFastPtrAsFastInt :: FastPtr Word8 -> FastInt -> FastInt

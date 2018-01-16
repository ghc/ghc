{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings,
    RecordWildCards, MagicHash, UnboxedTuples #-}

module Data.Attoparsec.Internal.Fhthagn
    (
      inlinePerformIO
    ) where

import GHC.Base (realWorld#)
import GHC.IO (IO(IO))

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

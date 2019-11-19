{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fbyte-code #-}
module T16876B where

import T16876A
import GHC.Exts
import GHC.IO

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

g :: Int
g = inlinePerformIO $ return 1

-- |
--
-- >>> h == (f + g)
-- True
h :: Int
h = 2

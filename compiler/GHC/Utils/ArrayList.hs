-- Ripped from the GrowableVector impl of https://hackage.haskell.org/package/datafix
-- The author (SG) is OK with that. It's ISC-licensed anyway.
-- | Implements array doubling

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module GHC.Utils.ArrayList
  ( ArrayList
  , new
  , length
  , pushBack
  , write
  , freeze
  ) where

import Prelude hiding (length)
import GHC.Arr
import GHC.ST
import GHC.Exts
import Control.Monad.ST

data ArrayList v
  = ArrayList
  { buffer :: {-# UNPACK #-} !(STArray RealWorld Int v)
  , len    :: !Int
  }

notInitializedError :: a
notInitializedError = error "newArrayList: Accessed uninitialized value"

new :: Int -> IO (ArrayList v)
new c =
  stToIO $ ArrayList <$> newSTArray (0, c-1) notInitializedError <*> pure 0
{-# INLINE new #-}

capacity :: ArrayList v -> Int
capacity = numElementsSTArray . buffer
{-# INLINE capacity #-}

length :: ArrayList v -> Int
length = len
{-# INLINE length #-}

{-# INLINE copySTArray_ #-}
copySTArray_ :: STArray s i e -> Int -> STArray s i e -> Int -> Int -> ST s ()
copySTArray_ (STArray _ _ _ src#) (I# src_offs#) (STArray _ _ _ dst#) (I# dst_offs#) (I# n#) = ST $ \s1# ->
    case copyMutableArray# src# src_offs# dst# dst_offs# n# s1# of
        s2# -> (# s2#, () #)

{-# INLINE shrinkFreezeSTArrayInt_ #-}
shrinkFreezeSTArrayInt_ :: STArray s Int e -> (Int, Int) -> ST s (Array Int e)
shrinkFreezeSTArrayInt_ (STArray l u _ marr#) r@(l', u') = ST $ \s1# ->
    let !(I# offs#)   = index (l, u) l'
        !n'@(I# len#) = rangeSize r in
    case freezeArray# marr# offs# len# s1#   of { (# s2#, arr# #) ->
    (# s2#, Array l' u' n' arr# #) }

reserve :: ArrayList v -> Int -> IO (ArrayList v)
reserve vec n | n <= capacity vec = pure vec
              | otherwise         = stToIO $ do
  arr <- newSTArray (0, n - 1) notInitializedError
  copySTArray_ arr 0 (buffer vec) 0 (len vec)
  return $! ArrayList arr (len vec)
{-# INLINE reserve #-}

pushBack :: ArrayList v -> v -> IO (ArrayList v)
pushBack vec v = do
  vec' <- if length vec == capacity vec
    then reserve vec (max 8 (2*capacity vec))
    else return vec
  stToIO $ writeSTArray (buffer vec') (len vec') v
  return $! vec' { len = len vec' + 1 }
{-# INLINE pushBack #-}

write :: ArrayList v -> Int -> v -> IO ()
write vec i v = stToIO $ writeSTArray (buffer vec) i v
{-# INLINE write #-}

freeze :: ArrayList v -> IO (Array Int v)
freeze vec = stToIO $ shrinkFreezeSTArrayInt_ (buffer vec) (0, len vec - 1)
{-# INLINE freeze #-}


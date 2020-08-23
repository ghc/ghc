{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
module T18397 where

import GHC.Exts
import GHC.ST

data MutableArray s a = MutableArray (MutableArray# s a)

runArray#
  :: (forall s. ST s (MutableArray s a))
  -> Array# a
runArray# m = case runRW# $ \s ->
  case unST m s of { (# s', MutableArray mary# #) ->
  unsafeFreezeArray# mary# s'} of (# _, ary# #) -> ary#

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f


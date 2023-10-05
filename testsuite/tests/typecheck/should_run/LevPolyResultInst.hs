{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}

import GHC.Exts

main :: IO ()
main = do
  print (example (\x -> I# x > 7))
  case indexArray# (example replicateFalse) 0# of
    (# r #) -> print r

-- Combines base:runST, primitive:newArray, and primitive:unsafeFreezeArray
replicateFalse :: Int# -> Array# Bool
replicateFalse n =
  let !(# _, r #) = runRW#
        (\s -> case newArray# n False s of
          (# s', arr #) -> unsafeFreezeArray# arr s'
        )
   in r

example :: forall (v :: Levity) (a :: TYPE ('BoxedRep v)). (Int# -> a) -> a
{-# noinline example #-}
example f = f 8#

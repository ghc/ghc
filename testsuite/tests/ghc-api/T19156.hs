{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.Exts.Heap
import GHC.Types.SrcLoc

rsl :: RealSrcLoc
rsl = mkRealSrcLoc "Foo" 1 1

main = do
  let !s1 = RealSrcLoc rsl (Just (BufPos 999222))
      !s2 = RealSrcLoc rsl (Just (BufPos 999333))
      !s3 = RealSrcLoc rsl (Just (BufPos 999444))

      !res = combineSrcSpans (combineSrcSpans (srcLocSpan s1) (srcLocSpan s2)) (srcLocSpan s3)
  cs <- unbox res

  -- The output must be an empty list because we don't want to retain
  -- intermediate locations in the heap.
  print (filter (hasDataArg 999333) cs)

hasDataArg x (ConstrClosure _ _ dataArgs _ _ _) = any (== x) dataArgs
hasDataArg x _ = False

unbox :: a -> IO [GenClosure Box]
unbox a = loop (asBox a)
  where
    loop :: Box -> IO [GenClosure Box]
    loop (Box b) = do
      c <- getClosureData b
      p <- concat <$> traverse loop (allClosures c)
      return (c : p)

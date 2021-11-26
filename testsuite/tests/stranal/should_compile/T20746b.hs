module T20746b where

mightThrow :: Int -> IO Int
{-# NOINLINE mightThrow #-}
mightThrow n = return n

-- Should not unbox trp
-- Recursive because if it's too small
-- we don't do worker/wrapper at all
f :: Bool -> (Int, Int, Int) -> IO (Int, Int, Int)
f False trp = f True trp
f True trp@(a,b,c) = do
  _ <- mightThrow a -- this potentially throwing IO action should not force unboxing of trp
  return trp

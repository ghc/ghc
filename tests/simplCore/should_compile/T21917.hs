-- With -fkeep-auto-rules we should get an externally-visible
-- specialisation.  See #21917
--
-- "SPEC foo @IO @Int"
--     forall ($dMonad :: Monad IO) ($dIntegral :: Integral Int).
--      foo1 @IO @Int $dMonad $dIntegral = foo_$sfoo

module T21917 where

{-# INLINABLE foo #-}
foo :: (Monad m, Integral a) => a -> a -> a -> IO (a,a,a,a,a, m a)
foo x y z = do
  let !x1 = x+1
  let !x2 = x+2
  let !x3 = x+3
  let !x4 = x+4
  let !x5 = x+5
  pure (x1, x2, x3, x4, x5, pure (x1+x2))

bar :: Int -> IO (Int,Int,Int,Int,Int, IO Int)
bar = foo 1 2

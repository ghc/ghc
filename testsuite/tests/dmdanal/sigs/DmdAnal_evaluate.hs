module DmdAnal_evaluate where

import Control.Exception (evaluate)

funA :: a -> IO a
funA x = evaluate x

funB :: a -> IO a
funB x = evaluate x >>= evaluate

funC :: (a, b, c) -> IO (c, a)
-- We should detect that the second component is un-used.
funC tup = do
  (x, _, z) <- evaluate tup
  evaluate (z, x)

funD :: (Int, Int) -> IO Int
{-# NOINLINE funD #-}
-- We don't want reboxing to happen here.
funD p = do
  (x, y) <- evaluate p
  evaluate (x + y)

funE :: a -> b -> IO a
funE x y = do
  x' <- evaluate x
  _  <- evaluate y
  pure x'

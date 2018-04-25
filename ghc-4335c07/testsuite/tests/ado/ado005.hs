{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -ddump-types #-}
module Test where

-- This should fail to typecheck because it needs Monad
test :: Applicative f => (Int -> f Int) -> f Int
test f = do
  x <- f 3
  y <- f x
  return (x + y)

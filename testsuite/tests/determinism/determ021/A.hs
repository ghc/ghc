{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -ddump-types #-}
module A where

test2 f = do
  x <- f 3
  y <- f 4
  return (x + y)

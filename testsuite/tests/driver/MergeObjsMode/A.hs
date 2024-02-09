module A where

-- Don't inline otherwise A.o may not be needed by Main.o
{-# NOINLINE a #-}
a :: Int
a = 42


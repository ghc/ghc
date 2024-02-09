module B where

-- Don't inline otherwise B.o may not be needed by Main.o
{-# NOINLINE b #-}
b :: String
b = "hello world"

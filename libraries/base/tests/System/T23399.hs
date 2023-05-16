module Main where

import System.Posix.Internals

main = do
  r <- c_getpid
  -- #23399: JS backend wasn't returning a valid JS number as a CPid hence
  -- "read" would fail because the string was "0\0" (not a number, NUL byte)
  print ((read (show r) :: Int) /= -1)

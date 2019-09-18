{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude       (IO, ($), (.), (>>=), putStrLn)
import System.Exit   (exitFailure)
import Unsafe.Coerce (unsafeCoerce)

sequ :: IO () -> IO a -> IO a
sequ c c' = c >>= \ _ -> c'

main :: IO ()
main =
  unsafeCoerce sequ (putStrLn "Hello,") $
  unsafeCoerce sequ (putStrLn "world!") $
  exitFailure

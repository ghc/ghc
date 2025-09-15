{-
  T25083_A and T25083_B contain a long-running (100ms) Template Haskell splice.

  Run this with -fexternal-interpreter -j to check that we properly synchronize
  the communication with the external interpreter.

  This test will fail with a timeout or serialization error if communication
  is not correctly serialized.
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Language.Haskell.TH
import Control.Concurrent

import T25083_A
import T25083_B

main :: IO ()
main = do
  print ta
  print tb

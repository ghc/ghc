-- Test for #28012.
-- Check that every GC entry point exported by System.Mem can be called.
-- The JS RTS used to define only performMajorGC: performMinorGC and
-- performBlockingMajorGC were foreign imports of C symbols that had no
-- JS implementation, so calling them threw a ReferenceError at runtime.
module Main where

import System.Mem

main :: IO ()
main = do
  performGC
  putStrLn "performGC ok"
  performMinorGC
  putStrLn "performMinorGC ok"
  performMajorGC
  putStrLn "performMajorGC ok"
  performBlockingMajorGC
  putStrLn "performBlockingMajorGC ok"

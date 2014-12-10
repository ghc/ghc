-- A test to show that -XStaticPointers keeps generated CAFs alive.
{-# LANGUAGE StaticPointers #-}
module Main where

import GHC.StaticPtr

import Control.Concurrent
import Data.Maybe (fromJust)
import GHC.Fingerprint
import System.Mem
import System.Mem.Weak
import Unsafe.Coerce (unsafeCoerce)

nats :: [Integer]
nats = [0 .. ]

-- Just a StaticPtr to some CAF so that we can deRef it.
nats_fp :: StaticKey
nats_fp = staticKey (static nats :: StaticPtr [Integer])

main = do
  let z = nats !! 400
  print z
  performGC
  addFinalizer z (putStrLn "finalizer z")
  print z
  performGC
  threadDelay 1000000
  let Just p = unsafeLookupStaticPtr nats_fp
  print (deRefStaticPtr (unsafeCoerce p) !! 800 :: Integer)
  -- Uncommenting the next line keeps primes alive and would prevent a segfault
  -- if nats were garbage collected.
  -- print (nats !! 900)

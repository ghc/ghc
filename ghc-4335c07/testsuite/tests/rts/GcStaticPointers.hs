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

-- The key of a 'StaticPtr' to some CAF.
nats_key :: StaticKey
nats_key = staticKey (static nats :: StaticPtr [Integer])

main = do
  let z = nats !! 400
  print z
  performGC
  addFinalizer z (putStrLn "finalizer z")
  print z
  performGC
  threadDelay 1000000
  Just p <- unsafeLookupStaticPtr nats_key
  print (deRefStaticPtr (unsafeCoerce p) !! 800 :: Integer)
  -- Uncommenting the next line keeps 'nats' alive and would prevent a segfault
  -- if 'nats' were garbage collected.
  -- print (nats !! 900)

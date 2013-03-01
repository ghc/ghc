--
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Modified by Ryan Trinkle: 1) change from divInt# to uncheckedIShiftRA#
--                           2) changed -optc-O to -optc-O3
--                           3) added -optc-ffast-math
-- Translation from Clean by Don Stewart
-- Parallelized by Louis Wasserman
--
-- Should be compiled with:
-- 	-threaded -funbox-strict-fields -O2 -fvia-c -optc-O3
-- 	-fexcess-precision -optc-ffast-math
-- Should be run with:
-- 	+RTS -N<number of cores>

import System.Environment
import Foreign.Marshal.Array
import Foreign
import Text.Printf
import LwConc.Substrate
import LwConc.Concurrent
import LwConc.MVar
import Control.Monad
import GHC.Base
-- import GHC.Conc

type Reals = Ptr Double

initSched = do
  newSched
  n <- getNumCapabilities
  spawnScheds $ n-1
  where
    spawnScheds 0 = return ()
    spawnScheds n = do
      newCapability
      spawnScheds (n-1)

main = do
    initSched
    n <- getArgs >>= readIO . head
    allocaArray n $ \ u -> allocaArray n $ \ v -> do
      forM_ [0..n-1] $ \i -> pokeElemOff u i 1 >> pokeElemOff v i 0

      powerMethod 10 n u v
      printf "%.9f\n" =<< eigenvalue n u v 0 0 0

------------------------------------------------------------------------

eigenvalue :: Int -> Reals -> Reals -> Int -> Double -> Double -> IO Double
eigenvalue !n !u !v !i !vBv !vv
    | i < n     = do	ui <- peekElemOff u i
			vi <- peekElemOff v i
			eigenvalue n u v (i+1) (vBv + ui * vi) (vv + vi * vi)
    | otherwise = return $! sqrt $! vBv / vv

------------------------------------------------------------------------

-- Essentially borrowed from the Java implementation.
data CyclicBarrier = Cyclic !Int !(MVar (Int, [MVar ()]))

await :: CyclicBarrier -> IO ()
await (Cyclic k waitsVar) = do
	(x, waits) <- takeMVar waitsVar
	if x <= 1 then do
		mapM_ (`putMVar` ()) waits
		putMVar waitsVar (k, [])
	  else do
	  	var <- newEmptyMVar
	  	putMVar waitsVar (x-1,var:waits)
	  	takeMVar var

newCyclicBarrier :: Int -> IO CyclicBarrier
newCyclicBarrier k = liftM (Cyclic k) (newMVar (k, []))

powerMethod :: Int -> Int -> Reals -> Reals -> IO ()
powerMethod z n u v = allocaArray n $ \ !t -> do
	numCaps <- getNumCapabilities
	let chunk = (n + numCaps - 1) `quotInt` numCaps
	!barrier <- newCyclicBarrier $! (n + chunk - 1) `quotInt` chunk
	let timesAtAv !s !d l r = do
		timesAv n s t l r
		await barrier
		timesAtv n t d l r
		await barrier
	let thread !l !r = foldr (>>) (return ()) $ replicate z $ do
		timesAtAv u v l r
		timesAtAv v u l r
	let go l = case l + chunk of
		r | r < n	-> forkIO (thread l r) >> go r
		  | otherwise	-> thread l n
	go 0

timesAv :: Int -> Reals -> Reals -> Int -> Int -> IO ()
timesAv !n !u !au !l !r = go l where
    go :: Int -> IO ()
    go !i = when (i < r) $ do
	let avsum !j !acc
		| j < n = do
			!uj <- peekElemOff u j
			avsum (j+1) (acc + ((aij i j) * uj))
		| otherwise = pokeElemOff au i acc >> go (i+1)
	avsum 0 0

timesAtv :: Int -> Reals -> Reals -> Int -> Int -> IO ()
timesAtv !n !u !a !l !r = go l
  where
    go :: Int -> IO ()
    go !i = when (i < r) $ do
	let atvsum !j !acc
		| j < n	= do	!uj <- peekElemOff u j
				atvsum (j+1) (acc + ((aij j i) * uj))
		| otherwise = pokeElemOff a i acc >> go (i+1)
	atvsum 0 0

--
-- manually unbox the inner loop:
-- aij i j = 1 / fromIntegral ((i+j) * (i+j+1) `div` 2 + i + 1)
--
aij (I# i) (I# j) = D# (
    case i +# j of
        n -> 1.0## /## int2Double#
        	(((n *# (n+#1#)) `uncheckedIShiftRA#` 1#) +# (i +# 1#)))

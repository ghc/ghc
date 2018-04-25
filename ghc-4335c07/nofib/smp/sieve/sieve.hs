-------------------------------------------------------------------------------
-- $Id: Primes.hs#1 2005/06/13 15:48:09 REDMOND\\satnams $
-------------------------------------------------------------------------------
 
-- Satnam reported that this didn't show any speedup up from -N1 to -N4

module Main where
import System.Time
import Control.Concurrent
import System.Environment
 
-- how many primes to calculate in each thread
n_primes :: Int
n_primes = 500

primes1 n done
  = do --putStrLn (show ((sieve [n..])!!n_primes))
       show ((sieve [n..])!!n_primes) `seq` return ()
       putMVar done ()
 
sieve (p:xs) = p : sieve [x | x <- xs, not (x `mod` p == 0)]
 
main
  = runInUnboundThread $ do 
       [str] <- getArgs
       let instances = read str :: Int
       dones <- sequence (replicate instances newEmptyMVar)
       sequence_ [forkIO (primes1 (i+2) (dones!!i)) | i <- [0..instances-1]]
       sequence_ [takeMVar (dones!!i) | i <- [0..instances-1]]

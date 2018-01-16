From: Koen Claessen [mailto:koen@cs.chalmers.se] 
Sent: Wednesday, January 27, 1999 3:05 PM
To: glasgow-haskell-bugs-outgoing@dcs.gla.ac.uk
Subject: Possible bug


Hi,

I have been doing probability theory tests. One of them involved the so
called "secretary problem". Here is my code:

\begin{code}
module Main where

import System.Random
import Data.List
import System.IO
import Control.Monad

type Process = [Integer] -> Bool

-- Modified for Haskell 98 by SimonM
-- (2017-03): Modified by michalt to fix build and avoid global RNG
simulate :: Int -> Integer -> Process -> Double
simulate n m proc = length (filter id tries) // n
  where
    tries = [ proc (randomRs (1,m) (mkStdGen seed)) | seed <- [1..n] ]
    n // m = fromIntegral n / fromIntegral m

sim :: Int -> Double
sim k = simulate 5000 100 proc
 where
  proc rs = [best] == take 1 afterk
   where
    xs     = take 100 (nub rs)
    best   = 100
    bestk  = maximum (take k xs)
    afterk = dropWhile (< bestk) (drop k xs)

main :: IO ()
main = print [ sim k | k <- [35..39] ]
\end{code}

When I run this module with ghc-4.01, I get _wrong_ results, and a bus
error:

	[koen] -: ghc-4.01 -O secretary.hs -o secretary
	ghc-4.01: module version changed to 2; reason: usages changed
	[koen] -: secretary
	[3.3e-2,0.0,0.0,0.354,0.174]
	Bus error

The right behavior should be like:

	[koen] -: hbc secretary.hs -o secretary
	[koen] -: secretary
	[0.398, 0.386, 0.376, 0.353, 0.363]

Or even:

	Hugs> main
	[0.366, 0.369, 0.383, 0.383, 0.379]

I am trying to find out if it is because of the random numbers, or because
of the use of -O. But these programs take ages to run...

Regards,
Koen.

--
Koen Claessen,
koen@cs.chalmers.se,
http://www.cs.chalmers.se/~koen,
Chalmers University of Technology.


-- T10728 test case for ``-maxN<n>``

module Main where

import GHC.Conc (getNumProcessors, getNumCapabilities)
import GHC.Environment
import Data.List

main :: IO ()
main = do
  -- We're parsing args passed in to make sure things are proper between the
  -- cli and the program.
  n <- getN <$> getFullArgs

  c <- getNumCapabilities
  p <- getNumProcessors

  putStr $ check n c p

-----

check :: Int -> Int -> Int -> String
check n c p
  |  n /= 0 && c /= 0 && p /= 0 -- These should never be 0
  -- Capabilities are equal to n, are they also within processor count?
  && (n == c && c <= p)
  -- Capabilities are equal to processor count, are they also within n?
  || (c == p && c <= n)
  = "maxN Successful"
check _n _c _p = "maxN Error"

-- Parsing ``-maxN<n>`` from Args to be sure of it.
getN :: [String] -> Int
getN args = case filter (isPrefixOf "-maxN") (reverse args) of
    (maxN:_) -> read (drop 5 maxN)
    _        -> error "Please pass `-maxN<n>` on command-line"

{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Criterion.Main (bench, defaultMain, nf)
import Data.List (foldl')
import qualified Data.IntMap as M
import qualified LookupGE_IntMap as M
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main :: IO ()
main = do
    evaluate $ rnf [m_even, m_odd, m_large]
    defaultMain [b f | b <- benches, f <- funs1]
  where
    m_even = M.fromAscList elems_even :: M.IntMap Int
    m_odd  = M.fromAscList elems_odd :: M.IntMap Int
    m_large = M.fromAscList elems_large :: M.IntMap Int
    bound = 2^12
    elems_even  = zip evens evens
    elems_odd   = zip odds odds
    elems_large = zip large large
    evens = [2,4..bound]
    odds  = [1,3..bound]
    large = [1,100..50*bound]
    benches =
          [ \(n,fun) -> bench (n++" present")  $ nf (fge fun evens) m_even
          , \(n,fun) -> bench (n++" absent")   $ nf (fge fun evens) m_odd
          , \(n,fun) -> bench (n++" far")      $ nf (fge fun odds)  m_large
          , \(n,fun) -> bench (n++" !present") $ nf (fge2 fun evens) m_even
          , \(n,fun) -> bench (n++" !absent")  $ nf (fge2 fun evens) m_odd
          , \(n,fun) -> bench (n++" !far")     $ nf (fge2 fun odds)  m_large
          ]
    funs1 = [ ("GE split", M.lookupGE1)
            , ("GE Craig", M.lookupGE2)
            , ("GE Twan", M.lookupGE3)
            , ("GE Milan", M.lookupGE4) ]

fge :: (Int -> M.IntMap Int -> Maybe (Int,Int)) -> [Int] -> M.IntMap Int -> (Int,Int)
fge fun xs m = foldl' (\n k -> fromMaybe n (fun k m)) (0,0) xs

-- forcing values inside tuples!
fge2 :: (Int -> M.IntMap Int -> Maybe (Int,Int)) -> [Int] -> M.IntMap Int -> (Int,Int)
fge2 fun xs m = foldl' (\n@(!_, !_) k -> fromMaybe n (fun k m)) (0,0) xs


{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Criterion.Main (defaultMain, bench, nf)
import Data.List (foldl')
import qualified Data.Map as M
import qualified LookupGE_Map as M
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main :: IO ()
main = do
    evaluate $ rnf [m_even, m_odd, m_large]
    defaultMain [b f | b <- benches, f <- funs1]
  where
    m_even = M.fromAscList elems_even :: M.Map Int Int
    m_odd  = M.fromAscList elems_odd :: M.Map Int Int
    m_large = M.fromAscList elems_large :: M.Map Int Int
    bound = 2^10
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
            , ("GE caseof", M.lookupGE2)
            , ("GE Twan", M.lookupGE3)
            , ("GE Milan", M.lookupGE4) ]

fge :: (Int -> M.Map Int Int -> Maybe (Int,Int)) -> [Int] -> M.Map Int Int -> (Int,Int)
fge fun xs m = foldl' (\n k -> fromMaybe n (fun k m)) (0,0) xs

-- forcing values inside tuples!
fge2 :: (Int -> M.Map Int Int -> Maybe (Int,Int)) -> [Int] -> M.Map Int Int -> (Int,Int)
fge2 fun xs m = foldl' (\n@(!_, !_) k -> fromMaybe n (fun k m)) (0,0) xs

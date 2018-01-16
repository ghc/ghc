module Main where

import qualified Data.List as L

-- USE_REPORT_PRELUDE versions of nub and nubBy, copied from
-- libraries/base/Data/OldList.hs.
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)

nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)

data Asymmetric = A | B deriving Show

instance Eq Asymmetric where
  A == _ = True
  B == _ = False

main :: IO()
main = do
  print $ L.nub [A,B]
  print $ L.nubBy (<) [1,2]
  -- The implementation from Data.List and the one from the Prelude defined in
  -- the Haskell 98 report should have the same behavior.
  print $ L.nub [A,B] == nub [A,B]
  print $ L.nubBy (<) [1,2] == nubBy (<) [1,2]

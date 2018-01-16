{-# LANGUAGE 
        TypeSynonymInstances, 
        GeneralizedNewtypeDeriving #-}

module DPH.Testsuite.Utils (
  Proxy(..), asProxyTypeOf,
      
  limitRange, update, nest
) where
import Test.QuickCheck
import Text.Show.Functions

import Data.Array.Parallel.Unlifted as U hiding ( update )
import Prelude as P
import Data.List ( delete, sort )
import System.Random ( StdGen, mkStdGen)

-- Test data generators -------------------------------------------------------
instance (Elt a, Arbitrary a) => Arbitrary (Array a) where
  arbitrary = fmap fromList arbitrary




-- Random number generator
instance Arbitrary StdGen where
  arbitrary = mkStdGen `fmap` arbitrary


-- A phantom type similar to the one found in lib `tagged'
data Proxy a = Proxy deriving (Show)

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const


-- Helper functins -----------------------------------------------------
-- TODO: Enhance TH testing infrastructure to allow keeping helper fuctions in
--       in the same files as the properties.
  
-- Adjust elements of an array to be within range [0, n).
-- As a special case, returns an empty array if the length is 0.
limitRange :: Int -> Array Int -> Array Int
limitRange n ixs = if n <= 0 then empty -- no permutations for an empty array
                             else U.map (`mod` n) ixs

-- Update specified list elements using index/value pairs
update :: [a] -> [(Int, a)] -> [a]
update xs []              = xs
update xs ((i,x) : pairs) = update (set xs i x) pairs
    where set xs i x = (P.take i xs) ++ [x] ++ (P.drop (i+1) xs)

-- Nest elements of an array according to segments lengths descriptor
nest :: [Int] -> [a] -> [[a]]
nest (n : ns) xs = let (ys, zs) = P.splitAt n xs
                   in ys : nest ns zs
nest _ _ = []


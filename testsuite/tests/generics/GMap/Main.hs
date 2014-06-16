{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic1)
import GMap

-- We should be able to generate a generic representation for these types
data D a = D0 | D1 { d11 :: a, d12 :: (D a) } deriving (Show, Generic1)

-- Example values
d0 :: D Char
d0 = D0
d1 = D1 (Just 'p') D0

d2 :: D (Int,Float)
d2 = D1 (3,0.14) D0

-- Generic instances
instance GMap D

-- Tests
main = do
  print $ gmap id d0
  (let isJust (Just _) = True
       isJust Nothing = False in print $ gmap isJust d1)
  print $ gmap fst d2

module Main where

import Data.Monoid (First(..))
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty(..))

main = do
  print . sconcat $ First Nothing :| First (Just 1) : undefined
  print . sconcat $ First (Just 2) :| undefined
  print . sconcat $ First Nothing :| First Nothing : First (Just 3) : []

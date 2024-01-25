module Main where

import Data.Semigroup (sconcat, First(..))
import Data.List.NonEmpty (NonEmpty(..))

main = do
  print . sconcat $ First 1 :| undefined
  print . sconcat $ First 1 :| First 2 : []

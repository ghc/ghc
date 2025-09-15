module Main (main) where

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = do
  print $ L.compareLength [] 0
  print $ L.compareLength [] 1
  print $ L.compareLength ['a'] 1
  print $ L.compareLength ['a', 'b'] 1
  print $ L.compareLength [0..] 100
  print $ L.compareLength undefined (-1)
  print $ L.compareLength ('a' : undefined) 0

  print $ NE.compareLength ('a' :| []) 1
  print $ NE.compareLength ('a' :| ['b']) 3
  print $ NE.compareLength (0 :| [1..]) 100
  print $ NE.compareLength undefined 0
  print $ NE.compareLength ('a' :| 'b' : undefined) 1

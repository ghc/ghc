module T5342 (increaseAreas) where

import Control.Monad
import Data.List

nubSorted :: Eq a => [a] -> [a]
nubSorted = undefined

cellsAround :: (Num a, Num b, Ord a, Ord b) => [(a, b)] -> [(a, b)]  
cellsAround = undefined

increaseAreas :: (Num a, Num b, Ord a, Ord b) => [[(a, b)]] -> [[(a, b)]]  
increaseAreas areas 
 = nubSorted $ sort $
     do
         area <- areas
         cell2 <- cellsAround area
         return $ sort $ cell2 : area

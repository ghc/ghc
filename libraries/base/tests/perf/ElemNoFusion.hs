-- As of March 2026, we don't expect `elem` to fuse with `sort` or `NonEmpty.toList`.
-- `elem` isn't even specialized, and performs dictionary-passing, but that may
-- change: #27096
module ElemNoFusion where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

noFusionElemNonEmptyToList :: Int -> NonEmpty Int -> Bool
noFusionElemNonEmptyToList x = elem x . NonEmpty.toList

noFusionElemSort :: Int -> [Int] -> Bool
noFusionElemSort x = elem x . sort

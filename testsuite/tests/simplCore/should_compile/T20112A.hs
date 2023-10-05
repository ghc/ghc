module T20112A (
    -- * Data structure
    AdjacencyMap, adjacencyMap, transpose, overlays

    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

newtype AdjacencyMap a = AM {
    adjacencyMap :: Map a (Set.Set a) }

overlays :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
overlays = AM . Map.unionsWith Set.union . map adjacencyMap

transpose :: Ord a => AdjacencyMap a -> AdjacencyMap a
transpose (AM m) = AM $ Map.foldrWithKey combine vs m
  where
    combine v es = Map.unionWith Set.union (Map.fromSet (const $ Set.singleton v) es)
    vs           = Map.fromSet (const Set.empty) (Map.keysSet m)

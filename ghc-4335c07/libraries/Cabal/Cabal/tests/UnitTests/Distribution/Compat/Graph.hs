{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Distribution.Compat.Graph
    ( tests
    , arbitraryGraph
    ) where

import Distribution.Compat.Graph

import qualified Prelude
import Prelude hiding (null)
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Set as Set
import Control.Monad
import qualified Data.Graph as G
import Data.Array ((!))
import Data.Maybe
import Data.List (sort)

tests :: [TestTree]
tests =
    [ testProperty "arbitrary unbroken" (prop_arbitrary_unbroken :: Graph (Node Int ()) -> Bool)
    , testProperty "nodes consistent" (prop_nodes_consistent :: Graph (Node Int ()) -> Bool)
    , testProperty "edges consistent" (prop_edges_consistent :: Graph (Node Int ()) -> Property)
    , testProperty "closure consistent" (prop_closure_consistent :: Graph (Node Int ()) -> Property)
    ]

-- Our arbitrary instance does not generate broken graphs
prop_arbitrary_unbroken :: Graph a -> Bool
prop_arbitrary_unbroken g = Prelude.null (broken g)

-- Every node from 'toList' maps to a vertex which
-- is present in the constructed graph, and maps back
-- to a node correctly.
prop_nodes_consistent :: (Eq a, IsNode a) => Graph a -> Bool
prop_nodes_consistent g = all p (toList g)
  where
    (_, vtn, ktv) = toGraph g
    p n = case ktv (nodeKey n) of
            Just v  -> vtn v == n
            Nothing -> False

-- A non-broken graph has the 'nodeNeighbors' of each node
-- equal the recorded adjacent edges in the node graph.
prop_edges_consistent :: IsNode a => Graph a -> Property
prop_edges_consistent g = Prelude.null (broken g) ==> all p (toList g)
  where
    (gr, vtn, ktv) = toGraph g
    p n = sort (nodeNeighbors n)
       == sort (map (nodeKey . vtn) (gr ! fromJust (ktv (nodeKey n))))

-- Closure is consistent with reachable
prop_closure_consistent :: (Show a, IsNode a) => Graph a -> Property
prop_closure_consistent g =
    not (null g) ==>
    forAll (elements (toList g)) $ \n ->
           Set.fromList (map nodeKey (fromJust (closure g [nodeKey n])))
        == Set.fromList (map (nodeKey . vtn) (G.reachable gr (fromJust (ktv (nodeKey n)))))
  where
    (gr, vtn, ktv) = toGraph g

hasNoDups :: Ord a => [a] -> Bool
hasNoDups = loop Set.empty
  where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s
                    = loop s' xs
                  | otherwise
                    = False

-- | Produces a graph of size @len@.  We sample with 'suchThat'; if we
-- dropped duplicate entries our size could be smaller.
arbitraryGraph :: (Ord k, Show k, Arbitrary k, Arbitrary a)
               => Int -> Gen (Graph (Node k a))
arbitraryGraph len = do
    -- Careful! Assume k is much larger than size.
    ks <- vectorOf len arbitrary `suchThat` hasNoDups
    ns <- forM ks $ \k -> do
        a <- arbitrary
        ns <- listOf (elements ks)
        -- Allow duplicates!
        return (N a k ns)
    return (fromDistinctList ns)

instance (Ord k, Show k, Arbitrary k, Arbitrary a)
      => Arbitrary (Graph (Node k a)) where
    arbitrary = sized $ \n -> do
        len <- choose (0, n)
        arbitraryGraph len

{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module Dataflow (
        fixedpoint
  ) where

-----------------------------------------------------------------------------
-- | Solve the fixed-point of a dataflow problem.
--
-- Complexity: O(N+H*E) calls to the update function where:
--   N = number of nodes,
--   E = number of edges,
--   H = maximum height of the lattice for any particular node.
--
-- Sketch for proof of complexity:
-- Note that the state is threaded through the entire execution.
-- Also note that the height of the latice at any particular node
-- is the number of times 'update' can return non-Nothing for a
-- particular node.  Every call (except for the top level one)
-- must be caused by a non-Nothing result and each non-Nothing
-- result causes as many calls as it has out-going edges.
-- Thus any particular node, n, may cause in total at
-- most H*out(n) further calls.  When summed over all nodes,
-- that is H*E.  The N term of the complexity is from the initial call
-- when 'update' will be passed 'Nothing'.
fixedpoint ::
    (node -> [node])            -- map from nodes to those who's
                                -- value depend on the argument node
    -> (node -> Maybe node -> s -> Maybe s)
                                -- Given the node which needs to be
                                -- updated, and which node caused that node
                                -- to need to be updated, update the state.
                                --
                                -- The causing node will be 'Nothing' if
                                -- this is the initial/bootstrapping update.
                                --
                                -- Must return 'Nothing' if no change,
                                -- otherwise returrn 'Just' of the new state.

    -> [node]                   -- Nodes that should initially be updated

    -> s                        -- Initial state
                                -- (usually a map from node to
                                -- the value for that node)

    -> s                        -- Final state
fixedpoint dependants update nodes state =
    foldr (fixedpoint' Nothing) state nodes where
        -- Use a depth first traversal of nodes based on the update graph.
        -- Terminate the traversal when the update doesn't change anything.
        fixedpoint' cause node state =
            case update node cause state of
              Nothing -> state
              Just state' ->
                  foldr (fixedpoint' (Just node)) state' (dependants node)

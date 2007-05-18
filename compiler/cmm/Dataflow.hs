module Dataflow (
        fixedpoint
  ) where

--------------------------------------------------------------------------------

-- Solve a fixed-point of a dataflow problem.
-- O(N+H*E) calls to update where
--   N = number of nodes,
--   E = number of edges,
--   H = maximum height of the lattice for any particular node.
-- dependants: map from nodes to those who's value depend on the argument node
-- update:
--   Given the node which needs to be updated, and
--   which node caused that node to need to be updated,
--   update the state.
--   (The causing node will be 'Nothing' if this is the initial update.)
--   Must return 'Nothing' if no change,
--   otherwise returrn 'Just' of the new state
-- nodes: a set of nodes that initially need updating
-- state: some sort of state (usually a map)
--        containing the initial value for each node
--
-- Sketch for proof of complexity:
-- Note that the state is threaded through the entire execution.
-- Also note that the height of the latice at any particular node
-- is the number of times 'update' can return non-Nothing for a particular node.
-- Every call (except for the top level one) must be caused by a non-Nothing
-- result and each non-Nothing result causes as many calls as it has
-- out-going edges.  Thus any particular node, n, may cause in total
-- at most H*out(n) further calls.  When summed over all nodes,
-- that is H*E.  The N term of the complexity is from the initial call
-- when 'update' will be passed 'Nothing'.
fixedpoint ::
    (node -> [node])
    -> (node -> Maybe node -> s -> Maybe s)
    -> [node] -> s -> s
fixedpoint dependants update nodes state =
    foldr (fixedpoint' Nothing) state nodes where
        fixedpoint' cause node state =
            case update node cause state of
              Nothing -> state
              Just state' ->
                  foldr (fixedpoint' (Just node)) state' (dependants node)


-- | Types for the general graph colorer.
module GHC.Data.Graph.Base (
        Triv,
        Graph (..),
        initGraph,
        graphMapModify,

        Node  (..),     newNode,
)


where

import GHC.Prelude

import GHC.Types.Unique.Set
import GHC.Types.Unique.FM


-- | A fn to check if a node is trivially colorable
--      For graphs who's color classes are disjoint then a node is 'trivially colorable'
--      when it has less neighbors and exclusions than available colors for that node.
--
--      For graph's who's color classes overlap, ie some colors alias other colors, then
--      this can be a bit more tricky. There is a general way to calculate this, but
--      it's likely be too slow for use in the code. The coloring algorithm takes
--      a canned function which can be optimised by the user to be specific to the
--      specific graph being colored.
--
--      for details, see  "A Generalised Algorithm for Graph-Coloring Register Allocation"
--                              Smith, Ramsey, Holloway - PLDI 2004.
--
type Triv k cls color
        =  cls                  -- the class of the node we're trying to color.
        -> UniqSet k            -- the node's neighbors.
        -> UniqSet color        -- the node's exclusions.
        -> Bool


-- | The Interference graph.
--      There used to be more fields, but they were turfed out in a previous revision.
--      maybe we'll want more later..
--
newtype Graph k cls color
        = Graph {
        -- | All active nodes in the graph.
          graphMap              :: UniqFM k (Node k cls color)  }


-- | An empty graph.
initGraph :: Graph k cls color
initGraph
        = Graph
        { graphMap              = emptyUFM }


-- | Modify the finite map holding the nodes in the graph.
graphMapModify
        :: (UniqFM k (Node k cls color) -> UniqFM k (Node k cls color))
        -> Graph k cls color -> Graph k cls color

graphMapModify f graph
        = graph { graphMap      = f (graphMap graph) }



-- | Graph nodes.
--      Represents a thing that can conflict with another thing.
--      For the register allocater the nodes represent registers.
--
data Node k cls color
        = Node {
        -- | A unique identifier for this node.
          nodeId                :: k

        -- | The class of this node,
        --      determines the set of colors that can be used.
        , nodeClass             :: cls

        -- | The color of this node, if any.
        , nodeColor             :: Maybe color

        -- | Neighbors which must be colored differently to this node.
        , nodeConflicts         :: UniqSet k

        -- | Colors that cannot be used by this node.
        , nodeExclusions        :: UniqSet color

        -- | Colors that this node would prefer to be, in descending order.
        , nodePreference        :: [color]

        -- | Neighbors that this node would like to be colored the same as.
        , nodeCoalesce          :: UniqSet k }


-- | An empty node.
newNode :: k -> cls -> Node k cls color
newNode k cls
        = Node
        { nodeId                = k
        , nodeClass             = cls
        , nodeColor             = Nothing
        , nodeConflicts         = emptyUniqSet
        , nodeExclusions        = emptyUniqSet
        , nodePreference        = []
        , nodeCoalesce          = emptyUniqSet }

-- | Like GHC.Unit.Module.Graph but for the ExternalModuleGraph which
--   is stored in the EPS.
module GHC.Unit.Module.External.Graph where

import GHC.Prelude
import GHC.Unit.Module.Graph
import GHC.Data.Graph.Directed.Reachability
import GHC.Data.Graph.Directed
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

data ExternalKey = ExternalModuleKey ModNodeKeyWithUid deriving (Eq, Ord)

data ExternalGraphNode = NodeHomePackage {
                              externalNodeKey :: ModNodeKeyWithUid
                             , externalNodeDeps :: [ExternalKey] }

externalKey :: ExternalGraphNode -> ExternalKey
externalKey (NodeHomePackage k _) = ExternalModuleKey k

type ExternalNode = Node Int ExternalGraphNode

data ExternalModuleGraph = ExternalModuleGraph
                              { external_nodes :: [ExternalGraphNode]
                              , external_trans :: (ReachabilityIndex ExternalNode, ExternalKey -> Maybe ExternalNode)
                              , external_fully_loaded :: !(S.Set ExternalKey) }

emptyExternalModuleGraph :: ExternalModuleGraph
emptyExternalModuleGraph = ExternalModuleGraph [] (graphReachability emptyGraph, const Nothing) S.empty

extendExternalModuleGraph :: ExternalGraphNode -> ExternalModuleGraph -> ExternalModuleGraph
extendExternalModuleGraph node graph = mkExternalModuleGraph (node : external_nodes graph) (external_fully_loaded graph)

setFullyLoadedModule :: ExternalKey -> ExternalModuleGraph -> ExternalModuleGraph
setFullyLoadedModule key graph = graph { external_fully_loaded = S.insert key (external_fully_loaded graph)}

isFullyLoadedModule :: ExternalKey -> ExternalModuleGraph -> Bool
isFullyLoadedModule key graph = S.member key (external_fully_loaded graph)

mkExternalModuleGraph :: [ExternalGraphNode] -> S.Set ExternalKey -> ExternalModuleGraph
mkExternalModuleGraph nodes loaded =
  ExternalModuleGraph {
      external_nodes = nodes
    , external_trans = let (g, f) = (externalGraphNodes nodes)
                       in (graphReachability g, f)
    , external_fully_loaded = loaded  }

-- | Turn a list of graph nodes into an efficient queriable graph.
-- The first boolean parameter indicates whether nodes corresponding to hs-boot files
-- should be collapsed into their relevant hs nodes.
externalGraphNodes ::
     [ExternalGraphNode]
  -> (Graph ExternalNode, ExternalKey -> Maybe ExternalNode)
externalGraphNodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    -- Map from module to extra boot summary dependencies which need to be merged in
    nodes = map go numbered_summaries

      where
        go (s, key) = DigraphNode s key $ out_edge_keys $
                          (externalNodeDeps s)

    numbered_summaries = zip summaries [1..]

    lookup_node :: ExternalKey -> Maybe ExternalNode
    lookup_node key = M.lookup key node_map

    lookup_key :: ExternalKey -> Maybe Int
    lookup_key = fmap node_key . lookup_node

    node_map :: M.Map ExternalKey ExternalNode
    node_map =
      M.fromList [ (externalKey s, node)
                   | node <- nodes
                   , let s = node_payload node
                   ]

    out_edge_keys :: [ExternalKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key

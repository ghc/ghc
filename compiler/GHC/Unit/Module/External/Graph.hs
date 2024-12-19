{-# LANGUAGE LambdaCase #-}

-- | Like @'GHC.Unit.Module.Graph'@ but for the @'ExternalModuleGraph'@ which is
-- stored in the EPS.
module GHC.Unit.Module.External.Graph
  ( -- * External Module Graph
    --
    -- | A module graph for the EPS.
    ExternalModuleGraph, ExternalGraphNode(..),
    emptyExternalModuleGraph, externalKey

    -- * Extending
    --
    -- | The @'ExternalModuleGraph'@ is a structure which is incrementally
    -- updated as the 'ExternalPackageState' (EPS) is updated (when an iface is
    -- loaded, in 'loadInterface'.
    --
    -- Therefore, there is an operation for extending the 'ExternalModuleGraph',
    -- unlike @'GHC.Unit.Module.Graph.ModuleGraph'@, which is constructed once
    -- during downsweep and never altered (since all of the home units
    -- dependencies are fully known then).
  , extendExternalModuleGraph

    -- * Loading
    --
    -- | As mentioned in the top-level haddocks for the
    -- 'extendExternalModuleGraph', the external module graph is incrementally
    -- updated as interfaces are loaded. This module graph keeps an additional
    -- cache registering which modules have already been fully loaded.
    --
    -- This cache is necessary to quickly check when a full-transitive-closure
    -- reachability query would be valid for some module.
    --
    -- Such a query may be invalid if ran on a module in the
    -- 'ExternalModuleGraph' whose dependencies have /not yet/ been fully loaded
    -- into the EPS.
    -- (Recall that interfaces are lazily loaded, and the 'ExternalModuleGraph'
    -- is only incrementally updated).
    --
    -- To guarantee the full transitive closure of a given module is completely
    -- loaded into the EPS (i.e. all interfaces of the modules below this one
    -- are also loaded), see @'loadHomePackageInterfacesBelow'@ in
    -- 'GHC.Iface.Load'.
  , isFullyLoadedModule
  , setFullyLoadedModule -- arguably, this should happen by construction after the modules are fully loaded, but fine.
  ) where

import GHC.Prelude
import GHC.Unit.Module.Graph
import GHC.Data.Graph.Directed.Reachability
import GHC.Data.Graph.Directed
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import GHC.Utils.Outputable

--------------------------------------------------------------------------------
-- * Main
--------------------------------------------------------------------------------

data ExternalModuleGraph = ExternalModuleGraph
  { external_nodes :: [ExternalGraphNode]
  , external_trans :: (ReachabilityIndex ExternalNode, ExternalKey -> Maybe ExternalNode)
  , external_fully_loaded :: !(S.Set ExternalKey) }

type ExternalNode = Node Int ExternalGraphNode

data ExternalGraphNode
  -- | A node for a home package module that is inserted in the EPS.
  --
  -- INVARIANT: This type of node can only ever exist if compiling in one-shot
  -- mode. In --make mode, it is imperative that the EPS doesn't have any home
  -- package modules ever.
  = NodeHomePackage
      { externalNodeKey :: ModNodeKeyWithUid
      , externalNodeDeps :: [ExternalKey] }
  -- possibly more constructors | ...

newtype ExternalKey = ExternalModuleKey ModNodeKeyWithUid
  deriving (Eq, Ord, Outputable)

externalKey :: ExternalGraphNode -> ExternalKey
externalKey (NodeHomePackage k _) = ExternalModuleKey k

emptyExternalModuleGraph :: ExternalModuleGraph
emptyExternalModuleGraph = ExternalModuleGraph [] (graphReachability emptyGraph, const Nothing) S.empty

mkExternalModuleGraph :: [ExternalGraphNode] -> S.Set ExternalKey -> ExternalModuleGraph
mkExternalModuleGraph nodes loaded =
  ExternalModuleGraph {
      external_nodes = nodes
    , external_trans = let (g, f) = (externalGraphNodes nodes)
                       in (graphReachability g, f)
    , external_fully_loaded = loaded  }

--------------------------------------------------------------------------------
-- * Extending
--------------------------------------------------------------------------------

extendExternalModuleGraph :: ExternalGraphNode -> ExternalModuleGraph -> ExternalModuleGraph
extendExternalModuleGraph node graph = mkExternalModuleGraph (node : external_nodes graph) (external_fully_loaded graph)

--------------------------------------------------------------------------------
-- * Loading
--------------------------------------------------------------------------------

setFullyLoadedModule :: ExternalKey -> ExternalModuleGraph -> ExternalModuleGraph
setFullyLoadedModule key graph = graph { external_fully_loaded = S.insert key (external_fully_loaded graph)}

isFullyLoadedModule :: ExternalKey -> ExternalModuleGraph -> Bool
isFullyLoadedModule key graph = S.member key (external_fully_loaded graph)

--------------------------------------------------------------------------------
-- * Internals
--------------------------------------------------------------------------------

-- | Turn a list of graph nodes into an efficient queriable graph.
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

instance Outputable ExternalGraphNode where
  ppr = \case
    NodeHomePackage mk ds -> text "NodeHomePackage" <+> ppr mk <+> ppr ds

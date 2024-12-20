{-# LANGUAGE LambdaCase #-}

-- | Like @'GHC.Unit.Module.Graph'@ but for the @'ExternalModuleGraph'@ which is
-- stored in the EPS.
module GHC.Unit.Module.External.Graph
  ( -- * External Module Graph
    --
    -- | A module graph for the EPS.
    ExternalModuleGraph, ExternalGraphNode(..)
  , ExternalKey(..), emptyExternalModuleGraph
  , emgNodeKey, emgNodeDeps

    -- * Extending
    --
    -- | The @'ExternalModuleGraph'@ is a structure which is incrementally
    -- updated as the 'ExternalPackageState' (EPS) is updated (when an iface is
    -- loaded, in 'loadInterface').
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
  , setFullyLoadedModule

    -- * Reachability
    --
    -- | Fast reachability queries on the external module graph. Similar to
    -- reachability queries on 'GHC.Unit.Module.Graph'.
  , emgReachable
  , emgReachableMany
  ) where

import GHC.Prelude
import GHC.Unit.Module.Graph
import GHC.Data.Graph.Directed.Reachability
import GHC.Data.Graph.Directed
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import GHC.Utils.Outputable
import GHC.Unit.Types (UnitId)

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
  -- | A node for packages with at least one module loaded in the EPS.
  --
  -- Edge from A to NodeExternalPackage p when A has p as a direct package
  -- dependency.
  | NodeExternalPackage
      { externalPkgKey :: UnitId
      , externalPkgDeps :: [UnitId]
      }

data ExternalKey
  = ExternalModuleKey ModNodeKeyWithUid
  | ExternalPackageKey UnitId
  deriving (Eq, Ord)

emptyExternalModuleGraph :: ExternalModuleGraph
emptyExternalModuleGraph = ExternalModuleGraph [] (graphReachability emptyGraph, const Nothing) S.empty

mkExternalModuleGraph :: [ExternalGraphNode] -> S.Set ExternalKey -> ExternalModuleGraph
-- romes:todo: does this also need to be defined in terms of extend (like for `ModuleGraph`?)
mkExternalModuleGraph nodes loaded =
  ExternalModuleGraph {
      external_nodes = nodes
    , external_trans = let (g, f) = (externalGraphNodes nodes)
                       in (graphReachability g, f)
    , external_fully_loaded = loaded  }

-- | Get the dependencies of an 'ExternalNode'
emgNodeDeps :: ExternalGraphNode -> [ExternalKey]
emgNodeDeps = \case
  NodeHomePackage _ dps -> dps
  NodeExternalPackage _ dps -> map ExternalPackageKey dps

-- | The graph key for a given node
emgNodeKey :: ExternalGraphNode -> ExternalKey
emgNodeKey (NodeHomePackage k _) = ExternalModuleKey k
emgNodeKey (NodeExternalPackage k _) = ExternalPackageKey k

--------------------------------------------------------------------------------
-- * Extending
--------------------------------------------------------------------------------

extendExternalModuleGraph :: ExternalGraphNode -> ExternalModuleGraph -> ExternalModuleGraph
extendExternalModuleGraph node graph = mkExternalModuleGraph (node : external_nodes graph) (external_fully_loaded graph)

--------------------------------------------------------------------------------
-- * Loading
--------------------------------------------------------------------------------

isFullyLoadedModule :: ExternalKey -> ExternalModuleGraph -> Bool
isFullyLoadedModule key graph = S.member key (external_fully_loaded graph)

setFullyLoadedModule :: ExternalKey -> ExternalModuleGraph -> ExternalModuleGraph
setFullyLoadedModule key graph = graph { external_fully_loaded = S.insert key (external_fully_loaded graph)}

--------------------------------------------------------------------------------
-- * Reachability
--------------------------------------------------------------------------------

-- | Return all nodes reachable from the given key, also known as its full
-- transitive closure.
--
-- @Nothing@ if the key couldn't be found in the graph.
emgReachable :: ExternalModuleGraph -> ExternalKey -> Maybe [ExternalGraphNode]
emgReachable mg nk = map node_payload <$> modules_below where
  (td_map, lookup_node) = external_trans mg
  modules_below =
    allReachable td_map <$> lookup_node nk

-- | Return all nodes reachable from all of the given keys.
emgReachableMany :: ExternalModuleGraph -> [ExternalKey] -> [ExternalGraphNode]
emgReachableMany mg nk = map node_payload modules_below where
  (td_map, lookup_node) = external_trans mg
  modules_below =
    allReachableMany td_map (mapMaybe lookup_node nk)

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
                          (emgNodeDeps s)

    numbered_summaries = zip summaries [1..]

    lookup_node :: ExternalKey -> Maybe ExternalNode
    lookup_node key = M.lookup key node_map

    lookup_key :: ExternalKey -> Maybe Int
    lookup_key = fmap node_key . lookup_node

    node_map :: M.Map ExternalKey ExternalNode
    node_map =
      M.fromList [ (emgNodeKey s, node)
                   | node <- nodes
                   , let s = node_payload node
                   ]

    out_edge_keys :: [ExternalKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key

instance Outputable ExternalGraphNode where
  ppr = \case
    NodeHomePackage mk ds -> text "NodeHomePackage" <+> ppr mk <+> ppr ds
    NodeExternalPackage mk ds -> text "NodeExternalPackage" <+> ppr mk <+> ppr ds

instance Outputable ExternalKey where
  ppr = \case
    ExternalModuleKey mk -> text "ExternalModuleKey" <+> ppr mk
    ExternalPackageKey uid -> text "ExternalPackageKey" <+> ppr uid

instance Outputable ExternalModuleGraph where
  ppr ExternalModuleGraph{external_nodes, external_fully_loaded}
    = text "ExternalModuleGraph" <+> ppr external_nodes <+> ppr external_fully_loaded

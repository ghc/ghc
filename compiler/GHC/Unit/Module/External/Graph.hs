{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
    -- are also loaded), see @'loadExternalGraphBelow'@ in
    -- 'GHC.Iface.Load'.
  , isFullyLoadedModule
  , setFullyLoadedModule

    -- * Reachability
    --
    -- | Fast reachability queries on the external module graph. Similar to
    -- reachability queries on 'GHC.Unit.Module.Graph'.
  , emgReachableLoop
  , emgReachableLoopMany
  ) where

import GHC.Prelude
import GHC.Unit.Module.Graph
import GHC.Data.Graph.Directed.Reachability
import GHC.Data.Graph.Directed
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor (first, bimap)
import Data.Maybe
import GHC.Utils.Outputable
import GHC.Unit.Types (UnitId, GenWithIsBoot(..), IsBootInterface(..), mkModule)
import GHC.Utils.Misc


--------------------------------------------------------------------------------
-- * Main
--------------------------------------------------------------------------------

data ExternalModuleGraph = ExternalModuleGraph
  { external_nodes :: [ExternalGraphNode]
  -- This transitive dependency query does not contain hs-boot nodes.
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
      , externalPkgDeps :: S.Set UnitId
      }

data ExternalKey
  = ExternalModuleKey ModNodeKeyWithUid
  | ExternalPackageKey UnitId
  deriving (Eq, Ord)

emptyExternalModuleGraph :: ExternalModuleGraph
emptyExternalModuleGraph = ExternalModuleGraph [] (graphReachability emptyGraph, const Nothing) S.empty

-- | Get the dependencies of an 'ExternalNode'
emgNodeDeps :: Bool -> ExternalGraphNode -> [ExternalKey]
emgNodeDeps drop_hs_boot_nodes = \case
  NodeHomePackage _ dps -> map drop_hs_boot dps
  NodeExternalPackage _ dps -> map ExternalPackageKey $ S.toList dps
  where
    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = NotBoot -- is regular mod or signature
                | otherwise          = IsBoot

    drop_hs_boot (ExternalModuleKey (ModNodeKeyWithUid (GWIB mn IsBoot) uid)) = (ExternalModuleKey (ModNodeKeyWithUid (GWIB mn hs_boot_key) uid))
    drop_hs_boot x = x

-- | The graph key for a given node
emgNodeKey :: ExternalGraphNode -> ExternalKey
emgNodeKey (NodeHomePackage k _) = ExternalModuleKey k
emgNodeKey (NodeExternalPackage k _) = ExternalPackageKey k

--------------------------------------------------------------------------------
-- * Extending
--------------------------------------------------------------------------------

extendExternalModuleGraph :: ExternalGraphNode -> ExternalModuleGraph -> ExternalModuleGraph
extendExternalModuleGraph node ExternalModuleGraph{..} =
  ExternalModuleGraph
    { external_fully_loaded = external_fully_loaded
    , external_nodes = node : external_nodes
    , external_trans = first cyclicGraphReachability $
                       externalGraphNodes True (node : external_nodes)
    }

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
emgReachableLoop :: ExternalModuleGraph -> ExternalKey -> Maybe [ExternalGraphNode]
emgReachableLoop mg nk = map node_payload <$> modules_below where
  (td_map, lookup_node) = external_trans mg
  modules_below =
    allReachable td_map <$> lookup_node nk

-- | Return all nodes reachable from all of the given keys.
emgReachableLoopMany :: ExternalModuleGraph -> [ExternalKey] -> [ExternalGraphNode]
emgReachableLoopMany mg nk = map node_payload modules_below where
  (td_map, lookup_node) = external_trans mg
  modules_below =
    allReachableMany td_map (mapMaybe lookup_node nk)

--------------------------------------------------------------------------------
-- * Internals
--------------------------------------------------------------------------------

-- | Turn a list of graph nodes into an efficient queriable graph.
-- The first boolean parameter indicates whether nodes corresponding to hs-boot files
-- should be collapsed into their relevant hs nodes.
externalGraphNodes :: Bool
  -> [ExternalGraphNode]
  -> (Graph ExternalNode, ExternalKey -> Maybe ExternalNode)
externalGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    -- Map from module to extra boot summary dependencies which need to be merged in
    (boot_summaries, nodes) = bimap M.fromList id $ partitionWith go numbered_summaries

      where
        go (s, key) =
          case s of
                NodeHomePackage (ModNodeKeyWithUid (GWIB mn IsBoot) uid) _deps | drop_hs_boot_nodes
                  -- Using emgNodeDeps here converts dependencies on other
                  -- boot files to dependencies on dependencies on non-boot files.
                  -> Left (mkModule uid mn, emgNodeDeps drop_hs_boot_nodes s)
                _ -> normal_case
          where
           normal_case =
              let lkup_key =
                    case s of
                      NodeHomePackage (ModNodeKeyWithUid (GWIB mn IsBoot) uid) _deps
                        -> Just $ mkModule uid mn
                      _ -> Nothing

                  extra = (lkup_key >>= \key -> M.lookup key boot_summaries)

              in Right $ DigraphNode s key $ out_edge_keys $
                      (fromMaybe [] extra
                        ++ emgNodeDeps drop_hs_boot_nodes s)

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
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else False

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

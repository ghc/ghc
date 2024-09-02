{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}

-- | A module graph should be constructed once and never change from there onwards.
--
-- The only operations should be for building the 'ModuleGraph'
-- (once and for all -- no update-like/insert-like functions)
-- and querying the structure in various ways, e.g. to determine reachability.
--
-- We should avoid exposing fields like 'mg_mss' since it may be a footgun
-- trying to use the nodes directly... We do still expose it, but it feels like
-- all its use cases would be better served by a more proper ModuleGraph
-- abstraction
module GHC.Unit.Module.Graph
   (
    -- * Construct a module graph
    --
    -- | A module graph should be constructed once by downsweep and never modified.
     ModuleGraph(..)
   , emptyMG
   , mkModuleGraph

    -- * Nodes in a module graph
    --
    -- | The user-facing nodes in a module graph are 'ModuleGraphNode's.
    -- There are a few things which we can query out of each 'ModuleGraphNode':
    --
    -- - 'mgNodeDependencies' gets the immediate dependencies of this node
    -- - 'mgNodeUnitId' returns the 'UnitId' of that node
    -- - 'mgNodeModSum' extracts the 'ModSummary' of a node if exists
   , ModuleGraphNode(..)
   , mgNodeDependencies
   , mgNodeModSum
   , mgNodeUnitId

    -- * Module graph operations
   , lengthMG

    -- ** 'ModSummary' operations
    --
    -- | A couple of operations on the module graph allow access to the
    -- 'ModSummary's of the modules in it contained.
    --
    -- In particular, 'mapMG' and 'mapMGM' allow updating these 'ModSummary's
    -- (without changing the 'ModuleGraph' structure itself!).
    -- 'mgModSummaries' lists out all 'ModSummary's, and
    -- 'mgLookupModule' looks up a 'ModSummary' for a given module.
   , mapMG, mgMapM
   , mgModSummaries
   , mgLookupModule
   , mgHasHoles

    -- ** Reachability queries
    --
    -- | A module graph explains the structure and relationship between the
    -- modules being compiled. Often times, this structure is relevant to
    -- answer reachability queries -- is X reachable from Y; or, what is the
    -- transitive closure of Z?
   , mgReachable
   , mgReachableLoop
   , mgQuery
   , mgQueryMany
   , mgMember

    -- ** Other operations
    --
    -- | These operations allow more-internal-than-ideal access to the
    -- ModuleGraph structure. Ideally, we could restructure the code using
    -- these functions to avoid deconstructing/reconstructing the ModuleGraph
    -- and instead extend the "proper interface" of the ModuleGraph to achieve
    -- what is currently done but through a better abstraction.
   , mgModSummaries'
   , moduleGraphNodes
   , moduleGraphModulesBelow -- needed for 'hptSomeThingsBelowUs',
                             -- but I think we could be more clever and cache
                             -- the graph-ixs of boot modules to efficiently
                             -- filter them out of the returned list.
                             -- hptInstancesBelow is re-doing that work every
                             -- time it's called.
   , filterToposortToModules

    -- * Keys into the 'ModuleGraph'
   , NodeKey(..)
   , mkNodeKey
   , nodeKeyUnitId
   , nodeKeyModName
   , ModNodeKey
   , ModNodeKeyWithUid(..)
   , msKey
   , miKey

    -- ** Internal node representation
    --
    -- | 'SummaryNode' is the internal representation for each node stored in
    -- the graph. It's not immediately clear to me why users do depend on them.
   , SummaryNode
   , summaryNodeSummary
   , summaryNodeKey

    -- * Utilities
   , showModMsg
   )
where

import GHC.Prelude
import GHC.Platform

import GHC.Data.Maybe
import GHC.Data.Graph.Directed
import GHC.Data.Graph.Directed.Reachability

import GHC.Driver.Backend
import GHC.Driver.DynFlags

import GHC.Types.SourceFile ( hscSourceString, isHsigFile )

import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Utils.Json
import GHC.Utils.Outputable
import GHC.Unit.Module.ModIface
import GHC.Utils.Misc ( partitionWith )
import GHC.Utils.Panic

import System.FilePath
import qualified Data.Map as Map
import GHC.Types.Unique.DSet
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit.Module
import GHC.Unit.Module.ModNodeKey
import GHC.Linker.Static.Utils

import Data.Bifunctor
import Data.Function
import Data.List (sort)
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Control.Monad

-- | A '@ModuleGraph@' contains all the nodes from the home package (only). See
-- '@ModuleGraphNode@' for information about the nodes.
--
-- Modules need to be compiled. hs-boots need to be typechecked before
-- the associated "real" module so modules with {-# SOURCE #-} imports can be
-- built. Instantiations also need to be typechecked to ensure that the module
-- fits the signature. Substantiation typechecking is roughly comparable to the
-- check that the module and its hs-boot agree.
--
-- The graph is not necessarily stored in topologically-sorted order. Use
-- 'GHC.topSortModuleGraph' and 'GHC.Data.Graph.Directed.flattenSCC' to achieve this.
data ModuleGraph = ModuleGraph
  { mg_mss :: [ModuleGraphNode]
  , mg_graph :: (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
  , mg_loop_graph :: (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)

    -- `mg_graph` and `mg_loop_graph` cached transitive dependency calculations
    -- so that a lot of work is not repeated whenever the transitive
    -- dependencies need to be calculated (for example, hptInstances).
    --
    --- - `mg_graph` is a reachability index constructed from a module
    -- graph /with/ boot nodes (which make the graph acyclic), and
    --
    --- * `mg_loop_graph` is a reachability index for the graph /without/
    -- hs-boot nodes, that may be cyclic.

  , mg_has_holes :: !Bool
  -- Cached computation, whether any of the ModuleGraphNode are isHoleModule,
  -- This is only used for a hack in GHC.Iface.Load to do with backpack, please
  -- remove this at the earliest opportunity.
  }

-- | Why do we ever need to construct empty graphs? Is it because of one shot mode?
emptyMG :: ModuleGraph
emptyMG = ModuleGraph [] (graphReachability emptyGraph, const Nothing) (graphReachability emptyGraph, const Nothing) False

-- | Construct a module graph. This function should be the only entry point for
-- building a 'ModuleGraph', since it is supposed to be built once and never modified.
--
-- If you ever find the need to build a 'ModuleGraph' iteratively, don't
-- add insert and update functions to the API since they become footguns.
-- Instead, design an API that allows iterative construction without posterior
-- modification, perhaps like what is done for building arrays from mutable
-- arrays.
mkModuleGraph :: [ModuleGraphNode] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG) emptyMG

--------------------------------------------------------------------------------
-- * Module Graph Nodes
--------------------------------------------------------------------------------

-- | A '@ModuleGraphNode@' is a node in the '@ModuleGraph@'.
-- Edges between nodes mark dependencies arising from module imports
-- and dependencies arising from backpack instantiations.
data ModuleGraphNode
  -- | Instantiation nodes track the instantiation of other units
  -- (backpack dependencies) with the holes (signatures) of the current package.
  = InstantiationNode UnitId InstantiatedUnit
  -- | There is a module summary node for each module, signature, and boot module being built.
  | ModuleNode [NodeKey] ModSummary
  -- | Link nodes are whether are are creating a linked product (ie executable/shared object etc) for a unit.
  | LinkNode [NodeKey] UnitId
  -- | Package dependency
  | UnitNode [UnitId] UnitId

-- | Collect the immediate dependencies of a ModuleGraphNode,
-- optionally avoiding hs-boot dependencies.
-- If the drop_hs_boot_nodes flag is False, and if this is a .hs and there is
-- an equivalent .hs-boot, add a link from the former to the latter.  This
-- has the effect of detecting bogus cases where the .hs-boot depends on the
-- .hs, by introducing a cycle.  Additionally, it ensures that we will always
-- process the .hs-boot before the .hs, and so the HomePackageTable will always
-- have the most up to date information.
mgNodeDependencies :: Bool -> ModuleGraphNode -> [NodeKey]
mgNodeDependencies drop_hs_boot_nodes = \case
    LinkNode deps _uid -> deps
    InstantiationNode uid iuid ->
      [ NodeKey_Module (ModNodeKeyWithUid (GWIB mod NotBoot) uid) | mod <- uniqDSetToList (instUnitHoles iuid) ]
      ++ [ NodeKey_ExternalUnit (instUnitInstanceOf iuid) ]
    ModuleNode deps _ms ->
      map drop_hs_boot deps
    UnitNode deps _ -> map NodeKey_ExternalUnit deps
  where
    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = NotBoot -- is regular mod or signature
                | otherwise          = IsBoot

    drop_hs_boot (NodeKey_Module (ModNodeKeyWithUid (GWIB mn IsBoot) uid)) = (NodeKey_Module (ModNodeKeyWithUid (GWIB mn hs_boot_key) uid))
    drop_hs_boot x = x

mgNodeModSum :: ModuleGraphNode -> Maybe ModSummary
mgNodeModSum (InstantiationNode {}) = Nothing
mgNodeModSum (LinkNode {})          = Nothing
mgNodeModSum (ModuleNode _ ms)      = Just ms
mgNodeModSum (UnitNode {})       = Nothing

mgNodeUnitId :: ModuleGraphNode -> UnitId
mgNodeUnitId mgn =
  case mgn of
    InstantiationNode uid _iud -> uid
    ModuleNode _ ms           -> toUnitId (moduleUnit (ms_mod ms))
    LinkNode _ uid             -> uid
    UnitNode _ uid          -> uid

instance Outputable ModuleGraphNode where
  ppr = \case
    InstantiationNode _ iuid -> ppr iuid
    ModuleNode nks ms -> ppr (msKey ms) <+> ppr nks
    LinkNode uid _     -> text "LN:" <+> ppr uid
    UnitNode _ uid  -> text "P:" <+> ppr uid

instance ToJson ModuleGraphNode where
  json (InstantiationNode {}) = panic "--buildplan: backpack not supported"
  json (ModuleNode nks ms) = JSObject [
    ("node-kind", json "compile"),
    ("dependencies", JSArray $ map json nks),
    ("unit_id", JSString $ unitIdString $ ms_unitid ms),
    ("module_name", JSString $ moduleNameString $ moduleName $ ms_mod ms),
    ("is_boot", JSBool $ isBootSummary ms == IsBoot),
    ("hs_path", JSString $ normalise $ msHsFilePath ms),
    ("uses_th", JSBool $ isTemplateHaskellOrQQNonBoot ms)
    ]
  json (LinkNode nks uid) = JSObject [
    ("node-kind", json "link"),
    ("dependencies", JSArray $ map json nks),
    ("unit_id", JSString $ unitIdString uid)
    ]
  json (UnitNode nks uid) = JSObject [
    ("node-kind", json "unit"),
    ("dependencies", JSArray $ map (JSString . unitIdString) nks),
    ("unit_id", JSString $ unitIdString uid)
    ]


instance Eq ModuleGraphNode where
  (==) = (==) `on` mkNodeKey

instance Ord ModuleGraphNode where
  compare = compare `on` mkNodeKey

--------------------------------------------------------------------------------
-- * Module Graph operations
--------------------------------------------------------------------------------

-- | Returns the number of nodes in a 'ModuleGraph'
lengthMG :: ModuleGraph -> Int
lengthMG = length . mg_mss

--------------------------------------------------------------------------------
-- ** ModSummaries
--------------------------------------------------------------------------------

-- | Map a function 'f' over all the 'ModSummaries'.
-- To preserve invariants, 'f' can't change the isBoot status.
mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG f mg@ModuleGraph{..} = mg
  { mg_mss = flip fmap mg_mss $ \case
      InstantiationNode uid iuid -> InstantiationNode uid iuid
      LinkNode uid nks -> LinkNode uid nks
      ModuleNode deps ms  -> ModuleNode deps (f ms)
      UnitNode deps uid -> UnitNode deps uid
  }

-- | Map a function 'f' over all the 'ModSummaries', in 'IO'.
-- To preserve invariants, 'f' can't change the isBoot status.
mgMapM :: (ModSummary -> IO ModSummary) -> ModuleGraph -> IO ModuleGraph
mgMapM f mg@ModuleGraph{..} = do
  mss' <- forM mg_mss $ \case
    InstantiationNode uid iuid -> pure $ InstantiationNode uid iuid
    LinkNode uid nks -> pure $ LinkNode uid nks
    ModuleNode deps ms  -> ModuleNode deps <$> (f ms)
    UnitNode deps uid -> pure $ UnitNode deps uid
  return mg
    { mg_mss = mss'
    }

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries mg = [ m | ModuleNode _ m <- mgModSummaries' mg ]

-- | Look up a non-boot ModSummary in the ModuleGraph.
--
-- Careful: Linear in the size of the module graph
mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
mgLookupModule ModuleGraph{..} m = listToMaybe $ mapMaybe go mg_mss
  where
    go (ModuleNode _ ms)
      | NotBoot <- isBootSummary ms
      , ms_mod ms == m
      = Just ms
    go _ = Nothing

mgMember :: ModuleGraph -> NodeKey -> Bool
mgMember graph k = isJust $ snd (mg_graph graph) k

-- | A function you should not need to use, or desire to use. Only used
-- in one place, `GHC.Iface.Load` to facilitate a misimplementation in Backpack.
mgHasHoles :: ModuleGraph -> Bool
mgHasHoles ModuleGraph{..} = mg_has_holes

--------------------------------------------------------------------------------
-- ** Reachability
--------------------------------------------------------------------------------

-- | Return all nodes reachable from the given 'NodeKey'.
--
-- @Nothing@ if the key couldn't be found in the graph.
mgReachable :: ModuleGraph -> NodeKey -> Maybe [ModuleGraphNode]
mgReachable mg nk = map summaryNodeSummary <$> modules_below where
  (td_map, lookup_node) = mg_graph mg
  modules_below =
    allReachable td_map <$> lookup_node nk

-- | Things which are reachable if hs-boot files are ignored. Used by 'getLinkDeps'
mgReachableLoop :: ModuleGraph -> [NodeKey] -> [ModuleGraphNode]
mgReachableLoop mg nk = map summaryNodeSummary modules_below where
  (td_map, lookup_node) = mg_loop_graph mg
  modules_below =
    allReachableMany td_map (mapMaybe lookup_node nk)

-- | Reachability Query.
--
-- @mgQuery(g, a, b)@ asks:
-- Can we reach @b@ from @a@ in graph @g@?
--
-- Both @a@ and @b@ must be in @g@.
mgQuery :: ModuleGraph -- ^ @g@
        -> NodeKey -- ^ @a@
        -> NodeKey -- ^ @b@
        -> Bool -- ^ @b@ is reachable from @a@
mgQuery mg nka nkb = isReachable td_map na nb where
  (td_map, lookup_node) = mg_graph mg
  na = expectJust $ lookup_node nka
  nb = expectJust $ lookup_node nkb

-- | Many roots reachability Query.
--
-- @mgQuery(g, roots, b)@ asks:
-- Can we reach @b@ from any of the @roots@ in graph @g@?
--
-- Node @b@ must be in @g@.
mgQueryMany :: ModuleGraph -- ^ @g@
            -> [NodeKey] -- ^ @roots@
            -> NodeKey -- ^ @b@
            -> Bool -- ^ @b@ is reachable from @roots@
mgQueryMany mg roots nkb = isReachableMany td_map nroots nb where
  (td_map, lookup_node) = mg_graph mg
  nroots = mapMaybe lookup_node roots
  nb = expectJust $ lookup_node nkb

--------------------------------------------------------------------------------
-- ** Other operations (read haddocks on export list)
--------------------------------------------------------------------------------

mgModSummaries' :: ModuleGraph -> [ModuleGraphNode]
mgModSummaries' = mg_mss

-- | Turn a list of graph nodes into an efficient queriable graph.
-- The first boolean parameter indicates whether nodes corresponding to hs-boot files
-- should be collapsed into their relevant hs nodes.
moduleGraphNodes :: Bool
  -> [ModuleGraphNode]
  -> (Graph SummaryNode, NodeKey -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    -- Map from module to extra boot summary dependencies which need to be merged in
    (boot_summaries, nodes) = bimap Map.fromList id $ partitionWith go numbered_summaries

      where
        go (s, key) =
          case s of
                ModuleNode __deps ms | isBootSummary ms == IsBoot, drop_hs_boot_nodes
                  -- Using nodeDependencies here converts dependencies on other
                  -- boot files to dependencies on dependencies on non-boot files.
                  -> Left (ms_mod ms, mgNodeDependencies drop_hs_boot_nodes s)
                _ -> normal_case
          where
           normal_case =
              let lkup_key = ms_mod <$> mgNodeModSum s
                  extra = (lkup_key >>= \key -> Map.lookup key boot_summaries)

              in Right $ DigraphNode s key $ out_edge_keys $
                      (fromMaybe [] extra
                        ++ mgNodeDependencies drop_hs_boot_nodes s)

    numbered_summaries = zip summaries [1..]

    lookup_node :: NodeKey -> Maybe SummaryNode
    lookup_node key = Map.lookup key (unNodeMap node_map)

    lookup_key :: NodeKey -> Maybe Int
    lookup_key = fmap summaryNodeKey . lookup_node

    node_map :: NodeMap SummaryNode
    node_map = NodeMap $
      Map.fromList [ (mkNodeKey s, node)
                   | node <- nodes
                   , let s = summaryNodeSummary node
                   ]

    out_edge_keys :: [NodeKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else False

-- | This function returns all the modules belonging to the home-unit that can
-- be reached by following the given dependencies. Additionally, if both the
-- boot module and the non-boot module can be reached, it only returns the
-- non-boot one.
moduleGraphModulesBelow :: ModuleGraph -> UnitId -> ModuleNameWithIsBoot -> Set ModNodeKeyWithUid
moduleGraphModulesBelow mg uid mn = filtered_mods [ mn | NodeKey_Module mn <- modules_below ]
  where
    modules_below = maybe [] (map mkNodeKey) (mgReachable mg (NodeKey_Module (ModNodeKeyWithUid mn uid)))
    filtered_mods = Set.fromDistinctAscList . filter_mods . sort

    -- IsBoot and NotBoot modules are necessarily consecutive in the sorted list
    -- (cf Ord instance of GenWithIsBoot). Hence we only have to perform a
    -- linear sweep with a window of size 2 to remove boot modules for which we
    -- have the corresponding non-boot.
    filter_mods = \case
      (r1@(ModNodeKeyWithUid (GWIB m1 b1) uid1) : r2@(ModNodeKeyWithUid (GWIB m2 _) uid2): rs)
        | m1 == m2  && uid1 == uid2 ->
                       let !r' = case b1 of
                                  NotBoot -> r1
                                  IsBoot  -> r2
                       in r' : filter_mods rs
        | otherwise -> r1 : filter_mods (r2:rs)
      rs -> rs

-- | This function filters out all the instantiation nodes from each SCC of a
-- topological sort. Use this with care, as the resulting "strongly connected components"
-- may not really be strongly connected in a direct way, as instantiations have been
-- removed. It would probably be best to eliminate uses of this function where possible.
filterToposortToModules
  :: [SCC ModuleGraphNode] -> [SCC ModSummary]
filterToposortToModules = mapMaybe $ mapMaybeSCC $ \case
  ModuleNode _deps node -> Just node
  _ -> Nothing
  where
    -- This higher order function is somewhat bogus,
    -- as the definition of "strongly connected component"
    -- is not necessarily respected.
    mapMaybeSCC :: (a -> Maybe b) -> SCC a -> Maybe (SCC b)
    mapMaybeSCC f = \case
      AcyclicSCC a -> AcyclicSCC <$> f a
      CyclicSCC as -> case mapMaybe f as of
        [] -> Nothing
        [a] -> Just $ AcyclicSCC a
        as -> Just $ CyclicSCC as

--------------------------------------------------------------------------------
-- * Keys into ModuleGraph
--------------------------------------------------------------------------------

data NodeKey = NodeKey_Unit {-# UNPACK #-} !InstantiatedUnit
             | NodeKey_Module {-# UNPACK #-} !ModNodeKeyWithUid
             | NodeKey_Link !UnitId
             | NodeKey_ExternalUnit !UnitId
  deriving (Eq, Ord)

instance Outputable NodeKey where
  ppr (NodeKey_Unit iu)   = ppr iu
  ppr (NodeKey_Module mk) = ppr mk
  ppr (NodeKey_Link uid)  = ppr uid
  ppr (NodeKey_ExternalUnit uid) = ppr uid

instance ToJson NodeKey where
  json (NodeKey_Unit {}) = panic "--buildplan: backpack not supported"
  json (NodeKey_Module (ModNodeKeyWithUid mnwib uid)) = JSObject $ [("unit_id", JSString $ unitIdString uid), ("module_name", JSString $ moduleNameString $ gwib_mod mnwib), ("is_boot", JSBool $ gwib_isBoot mnwib == IsBoot)]
  json (NodeKey_Link uid) = JSObject [("unit_id", JSString $ unitIdString uid)]
  json (NodeKey_ExternalUnit uid) = JSObject [("unit_id", JSString $ unitIdString uid)]

mkNodeKey :: ModuleGraphNode -> NodeKey
mkNodeKey = \case
  InstantiationNode _ iu -> NodeKey_Unit iu
  ModuleNode _ x -> NodeKey_Module $ msKey x
  LinkNode _ uid   -> NodeKey_Link uid
  UnitNode _ uid -> NodeKey_ExternalUnit uid

nodeKeyUnitId :: NodeKey -> UnitId
nodeKeyUnitId (NodeKey_Unit iu)   = instUnitInstanceOf iu
nodeKeyUnitId (NodeKey_Module mk) = mnkUnitId mk
nodeKeyUnitId (NodeKey_Link uid)  = uid
nodeKeyUnitId (NodeKey_ExternalUnit uid) = uid

nodeKeyModName :: NodeKey -> Maybe ModuleName
nodeKeyModName (NodeKey_Module mk) = Just (gwib_mod $ mnkModuleName mk)
nodeKeyModName _ = Nothing

msKey :: ModSummary -> ModNodeKeyWithUid
msKey ms = ModNodeKeyWithUid (ms_mnwib ms) (ms_unitid ms)

miKey :: ModIface -> ModNodeKeyWithUid
miKey hmi = ModNodeKeyWithUid (mi_mnwib hmi) ((toUnitId $ moduleUnit (mi_module hmi)))

type ModNodeKey = ModuleNameWithIsBoot

--------------------------------------------------------------------------------
-- ** Internal node representation (exposed)
--------------------------------------------------------------------------------

type SummaryNode = Node Int ModuleGraphNode

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey = node_key

summaryNodeSummary :: SummaryNode -> ModuleGraphNode
summaryNodeSummary = node_payload

--------------------------------------------------------------------------------
-- * Misc utilities
--------------------------------------------------------------------------------

showModMsg :: DynFlags -> Bool -> ModuleGraphNode -> SDoc
showModMsg dflags _ (LinkNode {}) =
      let staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

          platform  = targetPlatform dflags
          arch_os   = platformArchOS platform
          exe_file  = exeFileName arch_os staticLink (outputFile_ dflags)
      in text exe_file
showModMsg _ _ (UnitNode _deps uid) = ppr uid
showModMsg _ _ (InstantiationNode _uid indef_unit) =
  ppr $ instUnitInstanceOf indef_unit
showModMsg dflags recomp (ModuleNode _ mod_summary) =
  if gopt Opt_HideSourcePaths dflags
      then text mod_str
      else hsep $
         [ text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' ')
         , char '('
         , text (op $ msHsFilePath mod_summary) <> char ','
         , message, char ')' ]

  where
    op       = normalise
    mod_str  = moduleNameString (moduleName (ms_mod mod_summary)) ++
               hscSourceString (ms_hsc_src mod_summary)
    dyn_file = op $ msDynObjFilePath mod_summary
    obj_file = op $ msObjFilePath mod_summary
    files    = obj_file
               :| [ dyn_file | gopt Opt_BuildDynamicToo dflags ]
               ++ [ "interpreted" | gopt Opt_ByteCodeAndObjectCode dflags ]
    message = case backendSpecialModuleSource (backend dflags) recomp of
                Just special -> text special
                Nothing -> foldr1 (\ofile rest -> ofile <> comma <+> rest) (NE.map text files)

--------------------------------------------------------------------------------
-- * Internal methods for module graph
--
-- These are *really* meant to be internal!
-- Don't expose them without careful consideration about the invariants
-- described in the export list haddocks.
--------------------------------------------------------------------------------

newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

mkTransDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransDeps = first graphReachability {- module graph is acyclic -} . moduleGraphNodes False

mkTransLoopDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransLoopDeps = first cyclicGraphReachability . moduleGraphNodes True

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG ModuleGraph{..} node =
  ModuleGraph
    { mg_mss = node : mg_mss
    , mg_graph =  mkTransDeps (node : mg_mss)
    , mg_loop_graph = mkTransLoopDeps (node : mg_mss)
    , mg_has_holes = mg_has_holes || maybe False (isHsigFile . ms_hsc_src) (mgNodeModSum node)
    }

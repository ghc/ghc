{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Unit.Module.Graph
   ( ModuleGraph
   , ModuleGraphNode(..)
   , nodeDependencies
   , emptyMG
   , mkModuleGraph
   , extendMG
   , extendMGInst
   , extendMG'
   , unionMG
   , isTemplateHaskellOrQQNonBoot
   , filterToposortToModules
   , mapMG
   , mgModSummaries
   , mgModSummaries'
   , mgLookupModule
   , showModMsg
   , moduleGraphNodeModule
   , moduleGraphNodeModSum
   , moduleGraphModulesBelow
   , mgReachable
   , mgReachableLoop
   , mgQuery

   , moduleGraphNodes
   , SummaryNode
   , summaryNodeSummary

   , NodeKey(..)
   , nodeKeyUnitId
   , nodeKeyModName
   , ModNodeKey
   , mkNodeKey
   , msKey


   , moduleGraphNodeUnitId

   , ModNodeKeyWithUid(..)
   )
where

import GHC.Prelude
import GHC.Platform

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.Maybe
import GHC.Data.Graph.Directed
import GHC.Data.Graph.Directed.Reachability

import GHC.Driver.Backend
import GHC.Driver.DynFlags

import GHC.Types.SourceFile ( hscSourceString )

import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Utils.Outputable
import GHC.Utils.Misc ( partitionWith )

import System.FilePath
import qualified Data.Map as Map
import GHC.Types.Unique.DSet
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit.Module
import GHC.Linker.Static.Utils

import Data.Bifunctor
import Data.Function
import Data.List (sort)
import GHC.Data.List.SetOps
import GHC.Stack

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

moduleGraphNodeModule :: ModuleGraphNode -> Maybe ModuleName
moduleGraphNodeModule mgn = ms_mod_name <$> (moduleGraphNodeModSum mgn)

moduleGraphNodeModSum :: ModuleGraphNode -> Maybe ModSummary
moduleGraphNodeModSum (InstantiationNode {}) = Nothing
moduleGraphNodeModSum (LinkNode {})          = Nothing
moduleGraphNodeModSum (ModuleNode _ ms)      = Just ms

moduleGraphNodeUnitId :: ModuleGraphNode -> UnitId
moduleGraphNodeUnitId mgn =
  case mgn of
    InstantiationNode uid _iud -> uid
    ModuleNode _ ms           -> toUnitId (moduleUnit (ms_mod ms))
    LinkNode _ uid             -> uid

instance Outputable ModuleGraphNode where
  ppr = \case
    InstantiationNode _ iuid -> ppr iuid
    ModuleNode nks ms -> ppr (msKey ms) <+> ppr nks
    LinkNode uid _     -> text "LN:" <+> ppr uid

instance Eq ModuleGraphNode where
  (==) = (==) `on` mkNodeKey

instance Ord ModuleGraphNode where
  compare = compare `on` mkNodeKey

data NodeKey = NodeKey_Unit {-# UNPACK #-} !InstantiatedUnit
             | NodeKey_Module {-# UNPACK #-} !ModNodeKeyWithUid
             | NodeKey_Link !UnitId
  deriving (Eq, Ord)

instance Outputable NodeKey where
  ppr nk = pprNodeKey nk

pprNodeKey :: NodeKey -> SDoc
pprNodeKey (NodeKey_Unit iu) = ppr iu
pprNodeKey (NodeKey_Module mk) = ppr mk
pprNodeKey (NodeKey_Link uid)  = ppr uid

nodeKeyUnitId :: NodeKey -> UnitId
nodeKeyUnitId (NodeKey_Unit iu)   = instUnitInstanceOf iu
nodeKeyUnitId (NodeKey_Module mk) = mnkUnitId mk
nodeKeyUnitId (NodeKey_Link uid)  = uid

nodeKeyModName :: NodeKey -> Maybe ModuleName
nodeKeyModName (NodeKey_Module mk) = Just (gwib_mod $ mnkModuleName mk)
nodeKeyModName _ = Nothing

data ModNodeKeyWithUid = ModNodeKeyWithUid { mnkModuleName :: !ModuleNameWithIsBoot
                                           , mnkUnitId     :: !UnitId } deriving (Eq, Ord)


instance Outputable ModNodeKeyWithUid where
  ppr (ModNodeKeyWithUid mnwib uid) = ppr uid <> colon <> ppr mnwib

-- | A '@ModuleGraph@' contains all the nodes from the home package (only). See
-- '@ModuleGraphNode@' for information about the nodes.
--
-- Modules need to be compiled. hs-boots need to be typechecked before
-- the associated "real" module so modules with {-# SOURCE #-} imports can be
-- built. Instantiations also need to be typechecked to ensure that the module
-- fits the signature. Substantiation typechecking is roughly comparable to the
-- check that the module and its hs-boot agree.
--
-- The graph is not necessarily stored in topologically-sorted order.  Use
-- 'GHC.topSortModuleGraph' and 'GHC.Data.Graph.Directed.flattenSCC' to achieve this.
data ModuleGraph = ModuleGraph
  { mg_mss :: [ModuleGraphNode]
  , mg_graph :: (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
  , mg_loop_graph :: (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
    -- A cached transitive dependency calculation so that a lot of work is not
    -- repeated whenever the transitive dependencies need to be calculated (for example, hptInstances)
  }

-- | Map a function 'f' over all the 'ModSummaries'.
-- To preserve invariants 'f' can't change the isBoot status.
mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG f mg@ModuleGraph{..} = mg
  { mg_mss = flip fmap mg_mss $ \case
      InstantiationNode uid iuid -> InstantiationNode uid iuid
      LinkNode uid nks -> LinkNode uid nks
      ModuleNode deps ms  -> ModuleNode deps (f ms)
  }

unionMG :: ModuleGraph -> ModuleGraph -> ModuleGraph
unionMG a b =
  let new_mss = nubOrdBy compare $ mg_mss a `mappend` mg_mss b
  in ModuleGraph {
        mg_mss = new_mss
      , mg_graph = mkTransDeps new_mss
      , mg_loop_graph = mkTransLoopDeps new_mss
      }

mkTransDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransDeps = first graphReachability {- module graph is acyclic -} . moduleGraphNodes False

mkTransLoopDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransLoopDeps = first cyclicGraphReachability . moduleGraphNodes True

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries mg = [ m | ModuleNode _ m <- mgModSummaries' mg ]

mgModSummaries' :: ModuleGraph -> [ModuleGraphNode]
mgModSummaries' = mg_mss

-- | Look up a ModSummary in the ModuleGraph
-- Looks up the non-boot ModSummary
-- Linear in the size of the module graph
mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
mgLookupModule ModuleGraph{..} m = listToMaybe $ mapMaybe go mg_mss
  where
    go (ModuleNode _ ms)
      | NotBoot <- isBootSummary ms
      , ms_mod ms == m
      = Just ms
    go _ = Nothing

emptyMG :: ModuleGraph
emptyMG = ModuleGraph [] (graphReachability emptyGraph, const Nothing) (graphReachability emptyGraph, const Nothing)

isTemplateHaskellOrQQNonBoot :: ModSummary -> Bool
isTemplateHaskellOrQQNonBoot ms =
  (xopt LangExt.TemplateHaskell (ms_hspp_opts ms)
    || xopt LangExt.QuasiQuotes (ms_hspp_opts ms)) &&
  (isBootSummary ms == NotBoot)

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> [NodeKey] -> ModSummary -> ModuleGraph
extendMG ModuleGraph{..} deps ms = ModuleGraph
  { mg_mss = ModuleNode deps ms : mg_mss
  , mg_graph = mkTransDeps (ModuleNode deps ms : mg_mss)
  , mg_loop_graph = mkTransLoopDeps (ModuleNode deps ms : mg_mss)
  }

extendMGInst :: ModuleGraph -> UnitId -> InstantiatedUnit -> ModuleGraph
extendMGInst mg uid depUnitId = mg
  { mg_mss = InstantiationNode uid depUnitId : mg_mss mg
  }

extendMGLink :: ModuleGraph -> UnitId -> [NodeKey] -> ModuleGraph
extendMGLink mg uid nks = mg { mg_mss = LinkNode nks uid : mg_mss mg }

extendMG' :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG' mg = \case
  InstantiationNode uid depUnitId -> extendMGInst mg uid depUnitId
  ModuleNode deps ms -> extendMG mg deps ms
  LinkNode deps uid   -> extendMGLink mg uid deps

mkModuleGraph :: [ModuleGraphNode] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG') emptyMG

-- | This function filters out all the instantiation nodes from each SCC of a
-- topological sort. Use this with care, as the resulting "strongly connected components"
-- may not really be strongly connected in a direct way, as instantiations have been
-- removed. It would probably be best to eliminate uses of this function where possible.
filterToposortToModules
  :: [SCC ModuleGraphNode] -> [SCC ModSummary]
filterToposortToModules = mapMaybe $ mapMaybeSCC $ \case
  InstantiationNode _ _ -> Nothing
  LinkNode{} -> Nothing
  ModuleNode _deps node -> Just node
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

showModMsg :: DynFlags -> Bool -> ModuleGraphNode -> SDoc
showModMsg dflags _ (LinkNode {}) =
      let staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

          platform  = targetPlatform dflags
          arch_os   = platformArchOS platform
          exe_file  = exeFileName arch_os staticLink (outputFile_ dflags)
      in text exe_file
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
    files    = [ obj_file ]
               ++ [ dyn_file | gopt Opt_BuildDynamicToo dflags ]
               ++ [ "interpreted" | gopt Opt_ByteCodeAndObjectCode dflags ]
    message = case backendSpecialModuleSource (backend dflags) recomp of
                Just special -> text special
                Nothing -> foldr1 (\ofile rest -> ofile <> comma <+> rest) (map text files)



type SummaryNode = Node Int ModuleGraphNode

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey = node_key

summaryNodeSummary :: SummaryNode -> ModuleGraphNode
summaryNodeSummary = node_payload

-- | Collect the immediate dependencies of a ModuleGraphNode,
-- optionally avoiding hs-boot dependencies.
-- If the drop_hs_boot_nodes flag is False, and if this is a .hs and there is
-- an equivalent .hs-boot, add a link from the former to the latter.  This
-- has the effect of detecting bogus cases where the .hs-boot depends on the
-- .hs, by introducing a cycle.  Additionally, it ensures that we will always
-- process the .hs-boot before the .hs, and so the HomePackageTable will always
-- have the most up to date information.
nodeDependencies :: Bool -> ModuleGraphNode -> [NodeKey]
nodeDependencies drop_hs_boot_nodes = \case
    LinkNode deps _uid -> deps
    InstantiationNode uid iuid ->
      NodeKey_Module . (\mod -> ModNodeKeyWithUid (GWIB mod NotBoot) uid)  <$> uniqDSetToList (instUnitHoles iuid)
    ModuleNode deps _ms ->
      map drop_hs_boot deps
  where
    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = NotBoot -- is regular mod or signature
                | otherwise          = IsBoot

    drop_hs_boot (NodeKey_Module (ModNodeKeyWithUid (GWIB mn IsBoot) uid)) = (NodeKey_Module (ModNodeKeyWithUid (GWIB mn hs_boot_key) uid))
    drop_hs_boot x = x

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
                  -> Left (ms_mod ms, nodeDependencies drop_hs_boot_nodes s)
                _ -> normal_case
          where
           normal_case =
              let lkup_key = ms_mod <$> moduleGraphNodeModSum s
                  extra = (lkup_key >>= \key -> Map.lookup key boot_summaries)

              in Right $ DigraphNode s key $ out_edge_keys $
                      (fromMaybe [] extra
                        ++ nodeDependencies drop_hs_boot_nodes s)

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
newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

mkNodeKey :: ModuleGraphNode -> NodeKey
mkNodeKey = \case
  InstantiationNode _ iu -> NodeKey_Unit iu
  ModuleNode _ x -> NodeKey_Module $ msKey x
  LinkNode _ uid   -> NodeKey_Link uid

msKey :: ModSummary -> ModNodeKeyWithUid
msKey ms = ModNodeKeyWithUid (ms_mnwib ms) (ms_unitid ms)

type ModNodeKey = ModuleNameWithIsBoot


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

mgReachable :: HasCallStack => ModuleGraph -> NodeKey -> Maybe [ModuleGraphNode]
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


-- | Reachability Query. @mgQuery(g, a, b)@ asks: Can we reach @b@ from @a@ in
-- graph @g@?
-- INVARIANT: Both @a@ and @b@ must be in @g@.
mgQuery :: ModuleGraph -- ^ @g@
        -> NodeKey -- ^ @a@
        -> NodeKey -- ^ @b@
        -> Bool -- ^ @b@ is reachable from @a@
mgQuery mg nka nkb = isReachable td_map na nb where
  (td_map, lookup_node) = mg_graph mg
  na = expectJust "mgQuery:a" $ lookup_node nka
  nb = expectJust "mgQuery:b" $ lookup_node nkb


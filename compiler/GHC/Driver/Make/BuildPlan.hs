module GHC.Driver.Make.BuildPlan where
import GHC.Prelude

import GHC.Data.Graph.Directed
import GHC.Data.Maybe      ( expectJust )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Types.SourceFile

import GHC.Unit
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph

import qualified Data.Set as Set

import Data.Maybe

import qualified Data.Map.Strict as M


-- | A ModuleGraphNode which also has a hs-boot file, and the list of nodes on any
-- path from module to its boot file.
data ModuleGraphNodeWithBootFile
  = ModuleGraphNodeWithBootFile
     ModuleGraphNode
       -- ^ The module itself (not the hs-boot module)
     [NodeKey]
       -- ^ The modules in between the module and its hs-boot file,
       -- not including the hs-boot file itself.


instance Outputable ModuleGraphNodeWithBootFile where
  ppr (ModuleGraphNodeWithBootFile mgn deps) = text "ModeGraphNodeWithBootFile: " <+> ppr mgn $$ ppr deps

-- | A 'BuildPlan' is the result of attempting to linearise a single strongly-connected
-- component of the module graph.
data BuildPlan
  -- | A simple, single module all alone (which *might* have an hs-boot file, if it isn't part of a cycle)
  = SingleModule ModuleGraphNode
  -- | A resolved cycle, linearised by hs-boot files
  | ResolvedCycle [Either ModuleGraphNode ModuleGraphNodeWithBootFile]
  -- | An actual cycle, which wasn't resolved by hs-boot files
  | UnresolvedCycle [ModuleGraphNode]

instance Outputable BuildPlan where
  ppr (SingleModule mgn) = text "SingleModule" <> parens (ppr mgn)
  ppr (ResolvedCycle mgn)   = text "ResolvedCycle:" <+> ppr mgn
  ppr (UnresolvedCycle mgn) = text "UnresolvedCycle:" <+> ppr mgn


-- Just used for an assertion
countMods :: BuildPlan -> Int
countMods (SingleModule _) = 1
countMods (ResolvedCycle ns) = length ns
countMods (UnresolvedCycle ns) = length ns

-- See Note [Upsweep] for a high-level description.
createBuildPlan :: ModuleGraph -> Maybe HomeUnitModule -> [BuildPlan]
createBuildPlan mod_graph maybe_top_mod =
    let -- Step 1: Compute SCCs without .hi-boot files, to find the cycles
        cycle_mod_graph = topSortModuleGraph True mod_graph maybe_top_mod

        -- Step 2: Reanalyse loops, with relevant boot modules, to solve the cycles.
        build_plan :: [BuildPlan]
        build_plan
          -- Fast path, if there are no boot modules just do a normal toposort
          | isEmptyModuleEnv boot_modules = collapseAcyclic $ topSortModuleGraph False mod_graph maybe_top_mod
          | otherwise = toBuildPlan cycle_mod_graph []

        toBuildPlan :: [SCC ModuleGraphNode] -> [ModuleGraphNode] -> [BuildPlan]
        toBuildPlan [] mgn = collapseAcyclic (topSortWithBoot mgn)
        toBuildPlan ((AcyclicSCC node):sccs) mgn = toBuildPlan sccs (node:mgn)
        -- Interesting case
        toBuildPlan ((CyclicSCC nodes):sccs) mgn =
          let acyclic = collapseAcyclic (topSortWithBoot mgn)
              -- Now perform another toposort but just with these nodes and relevant hs-boot files.
              -- The result should be acyclic, if it's not, then there's an unresolved cycle in the graph.
              mresolved_cycle = collapseSCC (topSortWithBoot nodes)
          in acyclic ++ [maybe (UnresolvedCycle nodes) ResolvedCycle mresolved_cycle] ++ toBuildPlan sccs []

        (mg, lookup_node) = moduleGraphNodes False (mgModSummaries' mod_graph)
        trans_deps_map = allReachable mg (mkNodeKey . node_payload)
        -- Compute the intermediate modules between a file and its hs-boot file.
        -- See Step 2a in Note [Upsweep]
        boot_path mn uid =
          map (summaryNodeSummary . expectJust "toNode" . lookup_node) $ Set.toList $
          -- Don't include the boot module itself
          Set.delete (NodeKey_Module (key IsBoot))  $
          -- Keep intermediate dependencies: as per Step 2a in Note [Upsweep], these are
          -- the transitive dependencies of the non-boot file which transitively depend
          -- on the boot file.
          Set.filter (\nk -> nodeKeyUnitId nk == uid  -- Cheap test
                              && (NodeKey_Module (key IsBoot)) `Set.member` expectJust "dep_on_boot" (M.lookup nk trans_deps_map)) $
          expectJust "not_boot_dep" (M.lookup (NodeKey_Module (key NotBoot)) trans_deps_map)
          where
            key ib = ModNodeKeyWithUid (GWIB mn ib) uid


        -- An environment mapping a module to its hs-boot file and all nodes on the path between the two, if one exists
        boot_modules = mkModuleEnv
          [ (ms_mod ms, (m, boot_path (ms_mod_name ms) (ms_unitid ms))) | m@(ModuleNode _ ms) <- (mgModSummaries' mod_graph), isBootSummary ms == IsBoot]

        select_boot_modules :: [ModuleGraphNode] -> [ModuleGraphNode]
        select_boot_modules = mapMaybe (fmap fst . get_boot_module)

        get_boot_module :: ModuleGraphNode -> Maybe (ModuleGraphNode, [ModuleGraphNode])
        get_boot_module m = case m of ModuleNode _ ms | HsSrcFile <- ms_hsc_src ms -> lookupModuleEnv boot_modules (ms_mod ms); _ -> Nothing

        -- Any cycles should be resolved now
        collapseSCC :: [SCC ModuleGraphNode] -> Maybe [(Either ModuleGraphNode ModuleGraphNodeWithBootFile)]
        -- Must be at least two nodes, as we were in a cycle
        collapseSCC [AcyclicSCC node1, AcyclicSCC node2] = Just [toNodeWithBoot node1, toNodeWithBoot node2]
        collapseSCC (AcyclicSCC node : nodes) = (toNodeWithBoot node :) <$> collapseSCC nodes
        -- Cyclic
        collapseSCC _ = Nothing

        toNodeWithBoot :: ModuleGraphNode -> Either ModuleGraphNode ModuleGraphNodeWithBootFile
        toNodeWithBoot mn =
          case get_boot_module mn of
            -- The node doesn't have a boot file
            Nothing -> Left mn
            -- The node does have a boot file
            Just path -> Right (ModuleGraphNodeWithBootFile mn (map mkNodeKey (snd path)))

        -- The toposort and accumulation of acyclic modules is solely to pick-up
        -- hs-boot files which are **not** part of cycles.
        collapseAcyclic :: [SCC ModuleGraphNode] -> [BuildPlan]
        collapseAcyclic (AcyclicSCC node : nodes) = SingleModule node : collapseAcyclic nodes
        collapseAcyclic (CyclicSCC cy_nodes : nodes) = (UnresolvedCycle cy_nodes) : collapseAcyclic nodes
        collapseAcyclic [] = []

        topSortWithBoot nodes = topSortModules False (select_boot_modules nodes ++ nodes) Nothing


  in

    assertPpr (sum (map countMods build_plan) == length (mgModSummaries' mod_graph))
              (vcat [text "Build plan missing nodes:", (text "PLAN:" <+> ppr (sum (map countMods build_plan))), (text "GRAPH:" <+> ppr (length (mgModSummaries' mod_graph )))])
              build_plan


-- ---------------------------------------------------------------------------
--
-- | Topological sort of the module graph
topSortModuleGraph
          :: Bool
          -- ^ Drop hi-boot nodes? (see below)
          -> ModuleGraph
          -> Maybe HomeUnitModule
             -- ^ Root module name.  If @Nothing@, use the full graph.
          -> [SCC ModuleGraphNode]
-- ^ Calculate SCCs of the module graph, possibly dropping the hi-boot nodes
-- The resulting list of strongly-connected-components is in topologically
-- sorted order, starting with the module(s) at the bottom of the
-- dependency graph (ie compile them first) and ending with the ones at
-- the top.
--
-- Drop hi-boot nodes (first boolean arg)?
--
-- - @False@:   treat the hi-boot summaries as nodes of the graph,
--              so the graph must be acyclic
--
-- - @True@:    eliminate the hi-boot nodes, and instead pretend
--              the a source-import of Foo is an import of Foo
--              The resulting graph has no hi-boot nodes, but can be cyclic
topSortModuleGraph drop_hs_boot_nodes module_graph mb_root_mod =
    -- stronglyConnCompG flips the original order, so if we reverse
    -- the summaries we get a stable topological sort.
  topSortModules drop_hs_boot_nodes (reverse $ mgModSummaries' module_graph) mb_root_mod

topSortModules :: Bool -> [ModuleGraphNode] -> Maybe HomeUnitModule -> [SCC ModuleGraphNode]
topSortModules drop_hs_boot_nodes summaries mb_root_mod
  = map (fmap summaryNodeSummary) $ stronglyConnCompG initial_graph
  where
    (graph, lookup_node) =
      moduleGraphNodes drop_hs_boot_nodes summaries

    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just (Module uid root_mod) ->
            -- restrict the graph to just those modules reachable from
            -- the specified module.  We do this by building a graph with
            -- the full set of nodes, and determining the reachable set from
            -- the specified node.
            let root | Just node <- lookup_node $ NodeKey_Module $ ModNodeKeyWithUid (GWIB root_mod NotBoot) uid
                     , graph `hasVertexG` node
                     = node
                     | otherwise
                     = throwGhcException (ProgramError "module does not exist")
            in graphFromEdgedVerticesUniq (seq root (reachableG graph root))
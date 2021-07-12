{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

-- Collection of compilation graph creation APIs for use in --make
--
-- A note on the terminology: the ModuleGraph and ModuleGraphNode (defined in
-- GHC.Unit.Module.Graph) are incorrectly named "Graph". The actual graph
-- creation happens here when APIs like buildUpsweepNodes or topSortModuleGraph
-- are used.


module GHC.Driver.Graph (
        UpsweepNode(..), NodeKey(..),
        LoadHowMuch(..), LogQueue(..),

        buildUpsweepNodes,
        topSortModuleGraph,

        mkHomeBuildModule0, mkNodeKey

    ) where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo

import GHC.Data.Maybe
import GHC.Data.Graph.Directed
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

-- | Describes which modules of the module graph need to be loaded.
data LoadHowMuch
   = LoadAllTargets
     -- ^ Load all targets and its dependencies.
   | LoadUpTo ModuleName
     -- ^ Load only the given module and its dependencies.
   | LoadDependenciesOf ModuleName
     -- ^ Load only the dependencies of the given module, but not the module
     -- itself.

-- | Each module is given a unique 'LogQueue' to redirect compilation messages
-- to. A 'Nothing' value contains the result of compilation, and denotes the
-- end of the message queue.
data LogQueue = LogQueue !(IORef [Maybe (MessageClass, SrcSpan, SDoc)])
                         !(MVar ())

data UpsweepNode = UpsweepNode
  { un_node :: ModuleGraphNode
    -- ^ Compile this

  , un_resultMVar :: MVar (SuccessFlag, HomePackageTable)
    -- ^ After compilation this would be filled with the result, and if
    -- successful the HPT will contain this node's data.
    -- Note: The HPT will also contain the transitive deps of this node,
    -- moreover compilation of this node will create new HMIs for all modules
    -- part of the "loop" (see typecheckLoop)

  , un_depsMVars :: [(NodeKey, MVar (SuccessFlag, HomePackageTable))]
    -- ^ In parallel upsweep wait for the compilation of these nodes to
    -- complete. In sequential upsweep this should always be non-empty when
    -- beginning this node's compilation

  , un_logQueue :: LogQueue
    -- ^ Only used in parallel upsweep to maintain deterministic order of messages

  , un_mLoopCloser :: Maybe (NonEmpty NodeKey)
    -- ^ Just value indicates that this node is the loop closer. (see typecheckLoop)
    -- The head of NonEmpty is the current node, the tail contains rest of loop nodes
  }

-- | Builds a topologically sorted list of nodes which could be used to do
-- either single threaded or parallel compilation. Also returning the first, if
-- any, encountered module cycle.
buildUpsweepNodes
  :: LoadHowMuch
  -> ModuleGraph
  -> IO ([UpsweepNode], Maybe [ModuleGraphNode])
buildUpsweepNodes how_much mod_graph = do
  acyclic_with_mvars <- forM acyclic $ \ms -> do
    mvar <- newEmptyMVar
    return (mkNodeKey (summaryNodeSummary ms), (ms,mvar))
  let allMVars = Map.fromList acyclic_with_mvars
  nodes <- mapM (makeUpsweepNode allMVars) acyclic_with_mvars
  return (nodes, map summaryNodeSummary <$> first_cycle)

 where

  -- Topologically sort the module graph, this time including hi-boot
  -- nodes, and possibly just including the portion of the graph
  -- reachable from the module specified in the 2nd argument to load.
  -- This graph should be cycle-free.

  maybe_top_mod = case how_much of
                      LoadUpTo m           -> Just m
                      LoadDependenciesOf m -> Just m
                      _                    -> Nothing

  (graph, _lookup_node, mg0) = topSortModuleGraph' False mod_graph maybe_top_mod

  -- LoadDependenciesOf m: we want the upsweep to stop just
  -- short of the specified module
  all_sccs
      | LoadDependenciesOf _mod <- how_much
      = assert (case summaryNodeSummary <$> (last mg0) of
                  AcyclicSCC (ModuleNode (ExtendedModSummary ms _)) -> ms_mod_name ms == _mod
                  _ -> False) $
        List.init mg0
      | otherwise
      = mg0

  (acyclic, first_cycle) = filterSccs all_sccs
    where
      filterSccs []         = ([], Nothing)
      filterSccs (scc:sccs) = case scc of
        AcyclicSCC ms ->
          let (rest,cycle) = filterSccs sccs
          in (ms:rest, cycle)
        CyclicSCC mss -> ([], Just mss)

  module_loops :: [NonEmpty ModuleGraphNode]
  module_loops = findModuleLoopSets (map summaryNodeSummary $ reverse acyclic) boot_modules

  -- Make a Map and remove the loop_closer from the tail of NonEmpty
  module_loops_map :: Map NodeKey (NonEmpty NodeKey)
  module_loops_map = Map.fromList $ map (\(lc' :| loop) ->
    let lc = mkNodeKey lc'
    in (lc, lc :| (filter (/= lc) (map mkNodeKey loop)))) module_loops

  boot_modules = mkModuleSet
    [ ms_mod ms
    | ModuleNode (ExtendedModSummary ms _) <- map summaryNodeSummary acyclic
    , isBootSummary ms == IsBoot]

  makeUpsweepNode allMVars (mod, (mod_summary_node, mvar)) = do
    log_queue <- do
        ref <- newIORef []
        sem <- newEmptyMVar
        return (LogQueue ref sem)

    let
      direct_deps = Set.fromList $ fmap (mkNodeKey . summaryNodeSummary) $
        adjacentVerticesG graph mod_summary_node

      -- The loop that this module will finish. After this module successfully
      -- compiles, this loop is going to get re-typechecked.
      finish_loop = Map.lookup mod module_loops_map


      -- If this module finishes a loop then it must depend on all the other
      -- modules in that loop because the entire module loop is going to be
      -- re-typechecked once this module gets compiled. These extra dependencies
      -- are this module's "internal" loop dependencies, because this module is
      -- inside the loop in question.
      int_loop_deps :: Set.Set NodeKey
      int_loop_deps = Set.fromList $
        case finish_loop of
          Nothing   -> []
          Just loop -> NonEmpty.tail loop

      -- If this module depends on a module within a loop then it must wait for
      -- that loop to get re-typechecked, i.e. it must wait on the module that
      -- finishes that loop. These extra dependencies are this module's
      -- "external" loop dependencies, because this module is outside of the
      -- loop(s) in question.
      ext_loop_deps :: Set.Set NodeKey
      ext_loop_deps = Set.fromList $
        [ loop_closer
        | (loop_closer :| loop_members) <- Map.elems module_loops_map
        , loop_closer /= mod
        , not (elem mod loop_members)
        , any (`Set.member` (Set.difference direct_deps int_loop_deps)) loop_members]

      all_deps = Set.unions [direct_deps, int_loop_deps, ext_loop_deps]

      -- TODO: Perhaps sort the list of dependencies in reverse-topological
      -- order. This way, by the time we get woken up by the result of an
      -- earlier dependency, subsequent dependencies are more likely to have
      -- finished.
      depsMVars = map (\m -> (m, lookupMVars m)) (Set.toList all_deps)
      lookupMVars m = snd $ (expectJust "makeUpsweepNode") $ Map.lookup m allMVars

    return $ UpsweepNode
      { un_node = summaryNodeSummary mod_summary_node
      , un_resultMVar = mvar
      , un_depsMVars = depsMVars
      , un_logQueue = log_queue
      , un_mLoopCloser = finish_loop
      }


-- | Finds sets of modules which are considered as "part of the loop" for use in
-- re-typecheckLoop. See note [Compilation of module loops]
findModuleLoopSets
  :: [ModuleGraphNode]
  -> ModuleSet
  -> [NonEmpty ModuleGraphNode]
findModuleLoopSets acyclic boot_modules = go acyclic boot_modules
  where
  remove ms bm = case isBootSummary ms of
    IsBoot -> delModuleSet bm (ms_mod ms)
    NotBoot -> bm
  go [] _ = []
  -- TODO: consider InstantiationNode as part of loop as well
  go (InstantiationNode _ : mss) boot_modules
    = go mss boot_modules
  go mg@(mnode@(ModuleNode (ExtendedModSummary ms _)) : mss) boot_modules
    | Just loop <- getModLoop ms mg (`elemModuleSet` boot_modules)
    = (mnode :| loop) : go mss (remove ms boot_modules)
    | otherwise
    = go mss (remove ms boot_modules)


-- | Given a non-boot ModSummary @ms@ of a module, for which there exists a
-- corresponding boot file in @graph@, return the set of modules which
-- transitively depend on this boot file.  This function is slightly misnamed,
-- but its name "getModLoop" alludes to the fact that, when getModLoop is called
-- with a graph that does not contain @ms@ (non-parallel case) or is an
-- SCC with hs-boot nodes dropped (parallel-case), the modules which
-- depend on the hs-boot file are typically (but not always) the
-- modules participating in the recursive module loop.  The returned
-- list includes the hs-boot file.
--
-- Example:
--      let g represent the module graph:
--          C.hs
--          A.hs-boot imports C.hs
--          B.hs imports A.hs-boot
--          A.hs imports B.hs
--      genModLoop A.hs g == Just [A.hs-boot, B.hs, A.hs]
--
--      It would also be permissible to omit A.hs from the graph,
--      in which case the result is [A.hs-boot, B.hs]
--
-- Example:
--      A counter-example to the claim that modules returned
--      by this function participate in the loop occurs here:
--
--      let g represent the module graph:
--          C.hs
--          A.hs-boot imports C.hs
--          B.hs imports A.hs-boot
--          A.hs imports B.hs
--          D.hs imports A.hs-boot
--      genModLoop A.hs g == Just [A.hs-boot, B.hs, A.hs, D.hs]
--
--      Arguably, D.hs should import A.hs, not A.hs-boot, but
--      a dependency on the boot file is not illegal.
--
getModLoop
  :: ModSummary
  -> [ModuleGraphNode]
  -> (Module -> Bool) -- check if a module appears as a boot module in 'graph'
  -> Maybe [ModuleGraphNode]
getModLoop ms graph appearsAsBoot
  | isBootSummary ms == NotBoot
  , appearsAsBoot this_mod
  , let mss = reachableBackwards (ms_mod_name ms) graph
  = Just mss
  | otherwise
  = Nothing
 where
  this_mod = ms_mod ms

  reachableBackwards :: ModuleName -> [ModuleGraphNode] -> [ModuleGraphNode]
  reachableBackwards mod summaries
    = [ node_payload node | node <- reachableG (transposeG graph) root ]
    where -- the rest just sets up the graph:
          (graph, lookup_node) = moduleGraphNodes False summaries
          root  = expectJust "reachableBackwards" (lookup_node $ NodeKey_Module $ GWIB mod IsBoot)

-- ---------------------------------------------------------------------------
--
-- | Topological sort of the module graph
topSortModuleGraph
          :: Bool
          -- ^ Drop hi-boot nodes? (see below)
          -> ModuleGraph
          -> Maybe ModuleName
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
topSortModuleGraph drop_hs_boot_nodes module_graph mb_root_mod
  = map (fmap summaryNodeSummary) $ thdOf3 $
    (topSortModuleGraph' drop_hs_boot_nodes module_graph mb_root_mod)

topSortModuleGraph'
  :: Bool
  -- ^ Drop hi-boot nodes? (see below)
  -> ModuleGraph
  -> Maybe ModuleName
     -- ^ Root module name.  If @Nothing@, use the full graph.
  -> (Graph SummaryNode, NodeKey -> Maybe SummaryNode, [SCC SummaryNode])
topSortModuleGraph' drop_hs_boot_nodes module_graph mb_root_mod
  = (graph, lookup_node, stronglyConnCompG initial_graph)
  where
    summaries = mgModSummaries' module_graph
    -- stronglyConnCompG flips the original order, so if we reverse
    -- the summaries we get a stable topological sort.
    (graph, lookup_node) =
      moduleGraphNodes drop_hs_boot_nodes (reverse summaries)

    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just root_mod ->
            -- restrict the graph to just those modules reachable from
            -- the specified module.  We do this by building a graph with
            -- the full set of nodes, and determining the reachable set from
            -- the specified node.
            let root | Just node <- lookup_node $ NodeKey_Module $ GWIB root_mod NotBoot
                     , graph `hasVertexG` node
                     = node
                     | otherwise
                     = throwGhcException (ProgramError "module does not exist")
            in graphFromEdgedVerticesUniq (seq root (reachableG graph root))

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
unfilteredEdges :: Bool -> ModuleGraphNode -> [NodeKey]
unfilteredEdges drop_hs_boot_nodes = \case
    InstantiationNode iuid ->
      NodeKey_Module . flip GWIB NotBoot <$> uniqDSetToList (instUnitHoles iuid)
    ModuleNode (ExtendedModSummary ms bds) ->
      (NodeKey_Module . flip GWIB hs_boot_key . unLoc <$> ms_home_srcimps ms) ++
      (NodeKey_Module . flip GWIB NotBoot     . unLoc <$> ms_home_imps ms) ++
      [ NodeKey_Module $ GWIB (ms_mod_name ms) IsBoot
      | not $ drop_hs_boot_nodes || ms_hsc_src ms == HsBootFile
      ] ++
      [ NodeKey_Unit inst_unit
      | inst_unit <- bds
      ]
  where
    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = NotBoot -- is regular mod or signature
                | otherwise          = IsBoot

moduleGraphNodes :: Bool -> [ModuleGraphNode]
  -> (Graph SummaryNode
     , NodeKey -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq (map snd nodes), lookup_node)
  where
    numbered_summaries = zip summaries [1..]

    lookup_node :: NodeKey -> Maybe SummaryNode
    lookup_node key = Map.lookup key (unNodeMap node_map)

    lookup_key :: NodeKey -> Maybe Int
    lookup_key = fmap summaryNodeKey . lookup_node

    node_map :: NodeMap SummaryNode
    node_map = NodeMap $
      Map.fromList [ (mkHomeBuildModule s, node)
                   | (_, node) <- nodes
                   , let s = summaryNodeSummary node
                   ]

    -- We use integers as the keys for the SCC algorithm
    nodes :: [(Int, SummaryNode)]
    nodes = [ (key, DigraphNode s key $ out_edge_keys $ unfilteredEdges drop_hs_boot_nodes s)
            | (s, key) <- numbered_summaries
             -- Drop the hi-boot ones if told to do so
            , case s of
                InstantiationNode _ -> True
                ModuleNode ems -> not $ isBootSummary (emsModSummary ems) == IsBoot && drop_hs_boot_nodes
            ]

    out_edge_keys :: [NodeKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else False

-- The nodes of the graph are keyed by (mod, is boot?) pairs for the current
-- modules, and indefinite unit IDs for dependencies which are instantiated with
-- our holes.
--
-- NB: hsig files show up as *normal* nodes (not boot!), since they don't
-- participate in cycles (for now)
data NodeKey = NodeKey_Unit {-# UNPACK #-} !InstantiatedUnit | NodeKey_Module {-# UNPACK #-} !ModuleNameWithIsBoot
  deriving (Eq, Ord)

newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

mkNodeKey :: ModuleGraphNode -> NodeKey
mkNodeKey = \case
  InstantiationNode x -> NodeKey_Unit x
  ModuleNode x -> NodeKey_Module $ mkHomeBuildModule0 (emsModSummary x)

pprNodeKey :: NodeKey -> SDoc
pprNodeKey (NodeKey_Unit iu) = ppr iu
pprNodeKey (NodeKey_Module (GWIB { gwib_mod = mod, gwib_isBoot = boot }))
  = ppr $ (ppr mod) <> (text $ if boot == IsBoot then "[Boot]" else "")

mkHomeBuildModule :: ModuleGraphNode -> NodeKey
mkHomeBuildModule = \case
  InstantiationNode x -> NodeKey_Unit x
  ModuleNode ems -> NodeKey_Module $ mkHomeBuildModule0 (emsModSummary ems)

mkHomeBuildModule0 :: ModSummary -> ModuleNameWithIsBoot
mkHomeBuildModule0 ms = GWIB
  { gwib_mod = moduleName $ ms_mod ms
  , gwib_isBoot = isBootSummary ms
  }

instance Outputable NodeKey where
  ppr = pprNodeKey

instance Outputable UpsweepNode where
  ppr (UpsweepNode { un_node = n
                   , un_depsMVars = deps
                   , un_mLoopCloser = mLoopCloser })
    = vcat $
      [ text "UpsweepNode" <+> parens (pprNodeKey $ mkNodeKey n)
      , nest 2 (text "Depends on:" <+> brackets (interpp'SP $ map (pprNodeKey . fst) deps))
      ]
      ++ case mLoopCloser of
           Nothing -> []
           Just (_:|loop) -> [nest 2 (text "Loop closer of:" <+> brackets (interpp'SP $ map pprNodeKey loop))]

{-
Note [Compilation of module loops]

This note covers the subtle details of how re-typecheckLoop is currently being
used to help with compilation of module loops, what problems it currently has,
and a possibly better solution.

Background:
  The first thing to understand is that type checking of recursive-types require
  knot tying. And the recursive-types which span multiple modules need a HsBoot
  module to break the loop (but it still needs to do the same knot-tying while
  doing sequential compilation, see Note [Knot-tying fallback on boot] )

  In short the typechecker's knot-tying part is taken care of by the use of
  hsc_type_env_var + HsBoot modules.

  But there are still more problems: While compiling the modules in sequence,
  the references of Ids from HsBoot are used in the ModDetails of upstream
  modules. This has an undesirable effect when optimizations are enabled. The
  Ids from the HsBoot dont carry the unfoldings / arity information.

  The modules which import the HsSrc transitively should see the complete IdInfo
  for the optimizations to work. So before we begin the compilation of any of
  these modules, we want to get rid of HsBoot references completely, and for
  doing this re-typecheckLoop is being done two times as explained below.

re-typecheckLoop:
  In short the re-typecheckLoop lazily recreates the ModDetails from the
  ModIface for a set of modules, while ensuring that the knot-tying happens with
  the new ModDetails.

  * The re-typecheckLoop-1 happens before HsSrc compilation begins.
    This ensures that the TypeEnv of HPT no longer contain HsBoot references.

    (If HPT contain them, then the HsBoot's Id may get used during the HsSrc
    compilation.)

  * The re-typecheckLoop-2 happens after HsSrc compilation.

    This is to ensure that the loop-closer's Ids referenced by the loop members
    contain all the IdInfo necessary for optimizations.

So re-typecheckLoop-2 is really the step we need to get optimizations to work,
but the re-typecheckLoop-1 is a necessary cleanup step to make this happen
properly.

There are two interesting aspects to discuss with respect to the above two
re-typecheckLoop(s)
  * When to do it
  * What should be the set of modules that should get recreated

Both of these affect the order of compilation of modules.

Before considering these in detail its good to classify the modules (other than
the HsBoot and HsSrc) with respect to some module loop. (All imports here are
transitive)

  1. Downstream-outside-loop
     - Either imported by HsBoot or by In-loop modules

  2. In-loop
     - Import HsBoot and imported by HsSrc

  3. Dependent-on-loop
     - Import HsBoot, but not imported by HsSrc, nor do these import HsSrc

  4. Upstream-outside-loop
     - Import HsSrc

- re-typecheckLoop-1

  * This needs to be done before beginning HsSrc compilation.

  * The minimal set of modules it needs to recreate is the in-loop.

- re-typecheckLoop-2

  * This needs to be done after HsSrc compilation.

  * The minimal set of modules it needs to recreate is the in-loop + a subset of
    dependent-on-loop (described below).


How the set of modules used in re-typecheckLoop is determined
-------------------------------------------------------------

The re-typecheckLoop-2 is currently being done immediately afterward the
HsSrc compilation, and in fact it is considered a part of HsSrc compilation
as far as the graph dependencies are concerned.

Since this is tied to the compilation step of the HsSrc module, it has some
consequences, it require additional "edges" in the compilation graph to
ensure that the entire set of modules it needs to recreate have finished
compilation.

And it also puts additional constraints on the compilation of
dependent-on-loop modules which are compiled after HsSrc.

(Currently the set of modules that get recreated in both re-typecheckLoop-1
and re-typecheckLoop-2 is the same, though they need not be)

It is a bit tricky to understand what is going on, so lets look at it in detail
now. The following old comment about the findModuleLoopSets algorithm
describes the set of modules being chosen as "part of the loop"

   Picking the modules to re-typecheck is slightly tricky.  Starting from
   the module graph consisting of the modules that have already been
   compiled, we reverse the edges (so they point from the imported module
   to the importing module), and depth-first-search from the .hs-boot
   node.  This gives us all the modules that depend transitively on the
   .hs-boot module, and those are exactly the modules that we need to
   re-typecheck.


This approach is in a way simple as it works on a topologically sorted list of
modules, but it creates yet another issue. Here we have only included the
modules that "have already been compiled" before the HsSrc compilation starts.
(ie the modules in between the HsBoot and HsSrc modules in the topologically
sorted list.). The problem is that it misses the modules which are
"depedent-on-loop", but happen to be compiled after HsSrc.

The way this issue is being fixed right now requires adding more ad-hoc
dependencies called "external loop dependencies". Lets see this in detail

These old comments describe the reason we need int_loop_deps(1) and ext_loop_deps(2)

    Not only do we have to deal with explicit textual dependencies, we also
    have to deal with implicit dependencies introduced by import cycles that
    are broken by an hs-boot file. We have to ensure that:

    1. A module that breaks a loop must depend on all the modules in the
       loop (transitively or otherwise). This is normally always fulfilled
       by the module's textual dependencies except in degenerate loops,
       e.g.:

       A.hs imports B.hs-boot
       B.hs doesn't import A.hs
       C.hs imports A.hs, B.hs

    2. A module that depends on a module in an external loop can't proceed
       until the entire loop is re-typechecked.

    These two invariants have to be maintained to correctly build a
    compilation graph with one or more loops.

Both of these implicit dependencies are necessary because of re-typecheckLoop-2

1. is needed so that re-typecheckLoop-2 happens for B.hs as well.
2. is needed as any module which imports B.hs should wait for re-typecheckLoop-2
   to happen before it can begin compilation.

Closing Remarks
---------------

All of the above works together to ensure that module loops gets compiled
"properly". Though there are a few more minor problems even after doing all this

  * The loop-closer's compilation need to wait for more number of modules than
    necessary.

    The HsSrc does not depend transitively on any of the dependent-on-loop modules.
    So it could start its compilation earlier. The subset these extra
    dependent-on-loop modules is generally quite small, so not a big problem.

  * The compilation of all other dependent-on-loop modules ends up waiting for
    loop-closer to finish.

    This set of modules could be big, and in case of multiple loops the overall
    compilation could become almost linear due to this.

  * The modules which dont include the loop-closer directly, and which happen to
    get compiled after the loop-closer will see the complete unfoldings
    information, which is inconsistent with the one-shot mode compilation.
    (#14092)


Possible fix: A new way to do re-typecheckLoop-2
------------------------------------------------

In order to fix the above mentioned minor issues, the re-typecheckLoop-2 could
be implemented as a virtual node in the compilation graph. This would decouple
the re-typecheckLoop-2 from the compilation of loop-closer, and doing it as a
separate step (via a virtual UpsweepNode) would ensure that it has the correct
set of dependencies, and the upstream-outside-loop modules wait on this new
virtual UpsweepNode.

This would get rid of int_loop_deps completely, and the need of "ext_loop_deps"
could be shifted further upstream.

-}

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Unit.Module.Graph
   ( ModuleGraph(..)
   , ModuleGraphNode(..)
   , nodeDependencies
   , emptyMG
   , mkModuleGraph
   , mkModuleGraphChecked

   -- * Invariant checking
   , checkModuleGraph
   , ModuleGraphInvariantError(..)

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
   , ModuleNameHomeMap
   , mgHomeModuleMap
   , showModMsg
   , moduleGraphNodeModule
   , mgNodeIsModule
   , moduleGraphNodeModSum
   , moduleGraphModulesBelow
   , mgReachable
   , mgQuery
   , mgHasHoles

   , moduleGraphNodes
   , SummaryNode
   , summaryNodeSummary

   , NodeKey(..)
   , nodeKeyUnitId
   , nodeKeyModName
   , ModNodeKey
   , mkNodeKey
   , msKey
   , mnKey

   , moduleGraphNodeUnitId

   , ModNodeKeyWithUid(..)
   , mnkToModule
   , mnkIsBoot

   , ModuleNodeInfo(..)
   , moduleNodeInfoModule
   , moduleNodeInfoModuleName
   , moduleNodeInfoModNodeKeyWithUid
   , moduleNodeInfoHscSource
   , moduleNodeInfoLocation
   , isBootModuleNodeInfo
   )
where

import Data.Bifunctor
import Data.Either
import Data.Function
import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Data.Graph.Directed
import GHC.Data.Graph.Directed.Reachability
import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Driver.Backend
import GHC.Driver.DynFlags
import qualified GHC.LanguageExtensions as LangExt
import GHC.Linker.Static.Utils
import GHC.Platform
import GHC.Prelude
import GHC.Stack
import GHC.Types.SourceFile (HscSource (..), hscSourceString, isHsigFile)
import GHC.Types.Unique.DSet
import GHC.Utils.Misc (partitionWith)
import GHC.Utils.Outputable
import System.FilePath

import GHC.Unit.Module
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types

-- | A '@ModuleGraphNode@' is a node in the '@ModuleGraph@'.
-- Edges between nodes mark dependencies arising from module imports
-- and dependencies arising from backpack instantiations.
data ModuleGraphNode
  -- | Instantiation nodes track the instantiation of other units
  -- (backpack dependencies) with the holes (signatures) of the current package.
  = InstantiationNode UnitId InstantiatedUnit
  -- | There is a module node for each module being built.
  -- A node is either fixed or can be compiled.
  -- - Fixed modules are not compiled, the artifacts are just loaded from disk.
  --   It is up to you to make sure the artifacts are up to date and available.
  -- - Compile modules are compiled from source if needed.
  | ModuleNode [NodeKey] ModuleNodeInfo
  -- | Link nodes are whether are are creating a linked product (ie executable/shared object etc) for a unit.
  | LinkNode [NodeKey] UnitId


data ModuleGraphInvariantError =
        FixedNodeDependsOnCompileNode ModNodeKeyWithUid [NodeKey]
      | DuplicateModuleNodeKey NodeKey
      | DependencyNotInGraph NodeKey [NodeKey]
      deriving (Eq, Ord)

instance Outputable ModuleGraphInvariantError where
  ppr = \case
    FixedNodeDependsOnCompileNode key bad_deps ->
      text "Fixed node" <+> ppr key <+> text "depends on compile nodes" <+> ppr bad_deps
    DuplicateModuleNodeKey k ->
      text "Duplicate module node key" <+> ppr k
    DependencyNotInGraph from to ->
      text "Dependency not in graph" <+> ppr from <+> text "->" <+> ppr to

-- Used for invariant checking. Is a NodeKey fixed or compilable?
data ModuleNodeType = MN_Fixed | MN_Compile

instance Outputable ModuleNodeType where
  ppr = \case
    MN_Fixed -> text "Fixed"
    MN_Compile -> text "Compile"

moduleNodeType :: ModuleGraphNode -> ModuleNodeType
moduleNodeType (ModuleNode _ (ModuleNodeCompile _)) = MN_Compile
moduleNodeType (ModuleNode _ (ModuleNodeFixed _ _)) = MN_Fixed
moduleNodeType _ = MN_Compile

checkModuleGraph :: ModuleGraph -> [ModuleGraphInvariantError]
checkModuleGraph ModuleGraph{..} =
  mapMaybe (checkFixedModuleInvariant node_types) mg_mss
  ++ mapMaybe (checkAllDependenciesInGraph node_types) mg_mss
  ++ duplicate_errs
  where
    duplicate_errs = rights (Map.elems node_types)

    node_types :: Map.Map NodeKey (Either ModuleNodeType ModuleGraphInvariantError)
    node_types = Map.fromListWithKey go [ (mkNodeKey n, Left (moduleNodeType n)) | n <- mg_mss ]
      where
        go :: NodeKey -> Either ModuleNodeType ModuleGraphInvariantError
                      -> Either ModuleNodeType ModuleGraphInvariantError
                      -> Either ModuleNodeType ModuleGraphInvariantError
        go k _ _ = Right (DuplicateModuleNodeKey k)

checkAllDependenciesInGraph :: Map.Map NodeKey (Either ModuleNodeType ModuleGraphInvariantError)
                            -> ModuleGraphNode
                            -> Maybe ModuleGraphInvariantError
checkAllDependenciesInGraph node_types node =
  let nodeKey = mkNodeKey node
      deps = nodeDependencies False node
      missingDeps = filter (\dep -> not (Map.member dep node_types)) deps
  in if null missingDeps
     then Nothing
     else Just (DependencyNotInGraph nodeKey missingDeps)

checkFixedModuleInvariant :: Map.Map NodeKey (Either ModuleNodeType ModuleGraphInvariantError)
                -> ModuleGraphNode
                -> Maybe ModuleGraphInvariantError
checkFixedModuleInvariant node_types node = case node of
  ModuleNode deps (ModuleNodeFixed key _) ->
    let check_node dep = case Map.lookup dep node_types of
                           Just (Left MN_Compile) -> Just dep
                           _ -> Nothing
        bad_deps = mapMaybe check_node deps
    in if null bad_deps
       then Nothing
       else Just (FixedNodeDependsOnCompileNode key bad_deps)
  _ -> Nothing


{- Note [Module Types in the ModuleGraph]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Modules can be one of two different types in the module graph.

1. ModuleNodeCompile, modules with source files we can compile.
2. ModuleNodeFixed, modules which we presume are already compiled and available.

The ModuleGraph can contain a combination of these two types of nodes but must
obey the invariant that Fixed nodes only depend on other Fixed nodes. This invariant
can be checked by the `checkModuleGraph` function, but it's
the responsibility of the code constructing the ModuleGraph to ensure it is upheld.

At the moment, when using --make mode, GHC itself will only use `ModuleNodeCompile` nodes.

In oneshot mode, we don't have access to the source files of dependencies but sometimes need to know
information about the module graph still (for example, getLinkDeps).

In theory, the whole compiler will work if an API program uses ModuleNodeFixed nodes, and
there is a simple test in FixedNodes, which can be extended in future to cover
any missing cases.

-}
data ModuleNodeInfo = ModuleNodeFixed ModNodeKeyWithUid ModLocation
                    | ModuleNodeCompile ModSummary

-- | Extract the Module from a ModuleNodeInfo
moduleNodeInfoModule :: ModuleNodeInfo -> Module
moduleNodeInfoModule (ModuleNodeFixed key _) = mnkToModule key
moduleNodeInfoModule (ModuleNodeCompile ms) = ms_mod ms

-- | Extract the ModNodeKeyWithUid from a ModuleNodeInfo
moduleNodeInfoModNodeKeyWithUid :: ModuleNodeInfo -> ModNodeKeyWithUid
moduleNodeInfoModNodeKeyWithUid (ModuleNodeFixed key _) = key
moduleNodeInfoModNodeKeyWithUid (ModuleNodeCompile ms) = msKey ms

-- | Extract the HscSource from a ModuleNodeInfo, if we can determine it.
moduleNodeInfoHscSource :: ModuleNodeInfo -> Maybe HscSource
moduleNodeInfoHscSource (ModuleNodeFixed _ _) = Nothing
moduleNodeInfoHscSource (ModuleNodeCompile ms) = Just (ms_hsc_src ms)

-- | Extract the ModLocation from a ModuleNodeInfo
moduleNodeInfoLocation :: ModuleNodeInfo -> ModLocation
moduleNodeInfoLocation (ModuleNodeFixed _ loc) = loc
moduleNodeInfoLocation (ModuleNodeCompile ms) = ms_location ms

-- | Extract the IsBootInterface from a ModuleNodeInfo
isBootModuleNodeInfo :: ModuleNodeInfo -> IsBootInterface
isBootModuleNodeInfo (ModuleNodeFixed mnwib _) = mnkIsBoot mnwib
isBootModuleNodeInfo (ModuleNodeCompile ms) = isBootSummary ms

-- | Extract the ModuleName from a ModuleNodeInfo
moduleNodeInfoModuleName :: ModuleNodeInfo -> ModuleName
moduleNodeInfoModuleName m = moduleName (moduleNodeInfoModule m)

moduleGraphNodeModule :: ModuleGraphNode -> Maybe ModuleName
moduleGraphNodeModule mgn = moduleNodeInfoModuleName <$> (mgNodeIsModule mgn)

moduleGraphNodeModSum :: ModuleGraphNode -> Maybe ModSummary
moduleGraphNodeModSum (InstantiationNode {}) = Nothing
moduleGraphNodeModSum (LinkNode {})          = Nothing
moduleGraphNodeModSum (ModuleNode _ (ModuleNodeCompile ms)) = Just ms
moduleGraphNodeModSum (ModuleNode _ (ModuleNodeFixed {}))    = Nothing

mgNodeIsModule :: ModuleGraphNode -> Maybe ModuleNodeInfo
mgNodeIsModule (InstantiationNode {}) = Nothing
mgNodeIsModule (LinkNode {})          = Nothing
mgNodeIsModule (ModuleNode _ ms)      = Just ms

moduleGraphNodeUnitId :: ModuleGraphNode -> UnitId
moduleGraphNodeUnitId mgn =
  case mgn of
    InstantiationNode uid _iud -> uid
    ModuleNode _ ms           -> toUnitId (moduleUnit (moduleNodeInfoModule ms))
    LinkNode _ uid             -> uid

instance Outputable ModuleGraphNode where
  ppr = \case
    InstantiationNode _ iuid -> ppr iuid
    ModuleNode nks ms -> ppr (mnKey ms) <+> ppr nks
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

mnkIsBoot :: ModNodeKeyWithUid -> IsBootInterface
mnkIsBoot (ModNodeKeyWithUid mnwib _) = gwib_isBoot mnwib

mnkToModule :: ModNodeKeyWithUid -> Module
mnkToModule (ModNodeKeyWithUid mnwib uid) = Module (RealUnit (Definite uid)) (gwib_mod mnwib)

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
type ModuleNameHomeMap = (Set UnitId, Map.Map ModuleName (Set UnitId))

-- 'GHC.topSortModuleGraph' and 'GHC.Data.Graph.Directed.flattenSCC' to achieve this.
data ModuleGraph = ModuleGraph
  { mg_mss :: [ModuleGraphNode]
  , mg_graph :: (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
    -- A cached transitive dependency calculation so that a lot of work is not
    -- repeated whenever the transitive dependencies need to be calculated (for example, hptInstances)
  , mg_home_map :: ModuleNameHomeMap
    -- ^ For each module name, which home-unit UnitIds define it together with the set of units for which the listing is complete.
  , mg_has_holes :: Bool
  }

-- | Map a function 'f' over all the 'ModSummaries'.
-- To preserve invariants 'f' can't change the isBoot status.
mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG f mg@ModuleGraph{..} = mg
  { mg_mss = new_mss
  , mg_home_map = mkHomeModuleMap new_mss
  }
  where
    new_mss =
      flip fmap mg_mss $ \case
        InstantiationNode uid iuid -> InstantiationNode uid iuid
        LinkNode uid nks -> LinkNode uid nks
        ModuleNode deps (ModuleNodeFixed key loc) -> ModuleNode deps (ModuleNodeFixed key loc)
        ModuleNode deps (ModuleNodeCompile ms) -> ModuleNode deps (ModuleNodeCompile (f ms))

unionMG :: ModuleGraph -> ModuleGraph -> ModuleGraph
unionMG a b =
  let new_mss = nubOrdBy compare $ mg_mss a `mappend` mg_mss b
  in ModuleGraph {
        mg_mss = new_mss
      , mg_graph = mkTransDeps new_mss
      , mg_home_map = mkHomeModuleMap new_mss
      }

mkTransDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransDeps = first graphReachability {- module graph is acyclic -} . moduleGraphNodes False

mkHomeModuleMap :: [ModuleGraphNode] -> ModuleNameHomeMap
mkHomeModuleMap nodes =
  (complete_units, provider_map)
  where
    provider_map =
      Map.fromListWith Set.union
        [ (moduleNodeInfoModuleName ms, Set.singleton (toUnitId (moduleUnit (moduleNodeInfoModule ms))))
        | ModuleNode _ ms <- nodes
        ]
    complete_units =
      Set.fromList
        [ toUnitId (moduleUnit (moduleNodeInfoModule ms))
        | ModuleNode _ ms <- nodes
        ]

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries mg = [ m | ModuleNode _ (ModuleNodeCompile m) <- mgModSummaries' mg ]

mgModSummaries' :: ModuleGraph -> [ModuleGraphNode]
mgModSummaries' = mg_mss

-- | Look up a ModuleNodeInfo in the ModuleGraph
-- Looks up the non-boot module
-- Linear in the size of the module graph
mgLookupModule :: ModuleGraph -> Module -> Maybe ModuleNodeInfo
mgLookupModule ModuleGraph{..} m = listToMaybe $ mapMaybe go mg_mss
  where
    go (ModuleNode _ ms)
      | NotBoot <- isBootModuleNodeInfo ms
      , moduleNodeInfoModule ms == m
      = Just ms
    go _ = Nothing

mgHomeModuleMap :: ModuleGraph -> ModuleNameHomeMap
mgHomeModuleMap = mg_home_map

emptyMG :: ModuleGraph
emptyMG = ModuleGraph [] (graphReachability emptyGraph, const Nothing) (Set.empty, Map.empty) False

  {-
-- | A function you should not need to use, or desire to use. Only used
-- in one place, `GHC.Iface.Load` to facilitate a misimplementation in Backpack.
mgHasHoles :: ModuleGraph -> Bool
mgHasHoles ModuleGraph{..} = mg_has_holes
-}

--------------------------------------------------------------------------------
-- ** Reachability
--------------------------------------------------------------------------------

isTemplateHaskellOrQQNonBoot :: ModSummary -> Bool
isTemplateHaskellOrQQNonBoot ms =
  (xopt LangExt.TemplateHaskell (ms_hspp_opts ms)
    || xopt LangExt.QuasiQuotes (ms_hspp_opts ms)) &&
  (isBootSummary ms == NotBoot)

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> [NodeKey] -> ModSummary -> ModuleGraph
extendMG ModuleGraph{..} deps ms = ModuleGraph
  { mg_mss = new_mss
  , mg_graph = mkTransDeps new_mss
  , mg_home_map = mkHomeModuleMap new_mss
  , mg_has_holes = False
  }
  where
    new_mss = ModuleNode deps (ModuleNodeCompile ms) : mg_mss

extendMGInst :: ModuleGraph -> UnitId -> InstantiatedUnit -> ModuleGraph
extendMGInst mg uid depUnitId = mg
  { mg_mss = InstantiationNode uid depUnitId : mg_mss mg
  }

extendMGLink :: ModuleGraph -> UnitId -> [NodeKey] -> ModuleGraph
extendMGLink mg uid nks = mg { mg_mss = LinkNode nks uid : mg_mss mg }

extendMG' :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG' mg = \case
  InstantiationNode uid depUnitId -> extendMGInst mg uid depUnitId
  ModuleNode deps (ModuleNodeCompile ms) -> extendMG mg deps ms
  ModuleNode deps mni -> mg
    { mg_mss = ModuleNode deps mni : mg_mss mg
    , mg_graph = mkTransDeps (ModuleNode deps mni : mg_mss mg)
    , mg_home_map = mkHomeModuleMap (ModuleNode deps mni : mg_mss mg)
    , mg_has_holes = mg_has_holes mg || maybe False isHsigFile (moduleNodeInfoHscSource mni)
    }
  LinkNode deps uid   -> extendMGLink mg uid deps

mkModuleGraph :: [ModuleGraphNode] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG') emptyMG

-- | A version of mkModuleGraph that checks the module graph for invariants.
mkModuleGraphChecked :: [ModuleGraphNode] -> Either [ModuleGraphInvariantError] ModuleGraph
mkModuleGraphChecked nodes =
  let mg = mkModuleGraph nodes
  in case checkModuleGraph mg of
       [] -> Right mg
       errors -> Left errors

-- | This function filters out all the instantiation nodes from each SCC of a
-- topological sort. Use this with care, as the resulting "strongly connected components"
-- may not really be strongly connected in a direct way, as instantiations have been
-- removed. It would probably be best to eliminate uses of this function where possible.
filterToposortToModules
  :: [SCC ModuleGraphNode] -> [SCC ModuleNodeInfo]
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
showModMsg dflags recomp (ModuleNode _ mni) =
  if gopt Opt_HideSourcePaths dflags
      then text mod_str
      else hsep $
         [ text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' ')
         , char '('
         , text (moduleNodeInfoSource mni) <> char ','
         , moduleNodeInfoExtraMessage dflags recomp mni, char ')' ]
  where
    mod_str  = moduleNameString (moduleName (moduleNodeInfoModule mni)) ++
               moduleNodeInfoBootString mni

-- | Extra information about a 'ModuleNodeInfo' to display in the progress message.
moduleNodeInfoExtraMessage :: DynFlags -> Bool -> ModuleNodeInfo -> SDoc
moduleNodeInfoExtraMessage dflags recomp (ModuleNodeCompile mod_summary) =
    let dyn_file = normalise $ msDynObjFilePath mod_summary
        obj_file = normalise $ msObjFilePath mod_summary
        files    = [ obj_file ]
                   ++ [ dyn_file | gopt Opt_BuildDynamicToo dflags ]
                   ++ [ "interpreted" | gopt Opt_ByteCodeAndObjectCode dflags ]
    in case backendSpecialModuleSource (backend dflags) recomp of
              Just special -> text special
              Nothing -> foldr1 (\ofile rest -> ofile <> comma <+> rest) (map text files)
moduleNodeInfoExtraMessage _ _ (ModuleNodeFixed {}) = text "fixed"

-- | The source location of the module node to show to the user.
moduleNodeInfoSource :: ModuleNodeInfo -> FilePath
moduleNodeInfoSource (ModuleNodeCompile ms) = normalise $ msHsFilePath ms
moduleNodeInfoSource (ModuleNodeFixed _ loc) = normalise $ ml_hi_file loc

-- | The extra info about a module [boot] or [sig] to display.
moduleNodeInfoBootString :: ModuleNodeInfo -> String
moduleNodeInfoBootString (ModuleNodeCompile ms) = hscSourceString (ms_hsc_src ms)
moduleNodeInfoBootString mn@(ModuleNodeFixed {}) =
  hscSourceString (case isBootModuleNodeInfo mn of
                      IsBoot -> HsBootFile
                      NotBoot -> HsSrcFile)


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
                ModuleNode __deps ms | isBootModuleNodeInfo ms == IsBoot, drop_hs_boot_nodes
                  -- Using nodeDependencies here converts dependencies on other
                  -- boot files to dependencies on dependencies on non-boot files.
                  -> Left (moduleNodeInfoModule ms, nodeDependencies drop_hs_boot_nodes s)
                _ -> normal_case
          where
           normal_case =
              let lkup_key = moduleNodeInfoModule <$> mgNodeIsModule s
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
  ModuleNode _ x -> NodeKey_Module $ mnKey x
  LinkNode _ uid   -> NodeKey_Link uid

msKey :: ModSummary -> ModNodeKeyWithUid
msKey ms = ModNodeKeyWithUid (ms_mnwib ms) (ms_unitid ms)

mnKey :: ModuleNodeInfo -> ModNodeKeyWithUid
mnKey (ModuleNodeFixed key _) = key
mnKey (ModuleNodeCompile ms) = msKey ms

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

mgHasHoles :: ModuleGraph -> Bool
mgHasHoles = mg_has_holes


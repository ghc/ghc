{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Unit.Module.Graph
   ( ModuleGraph
   , ModuleGraphNode(..)
   , emptyMG
   , mkModuleGraph
   , mkModuleGraph'
   , extendMG
   , extendMGInst
   , extendMG'
   , filterToposortToModules
   , mapMG
   , mgModSummaries
   , mgModSummaries'
   , mgExtendedModSummaries
   , mgElemModule
   , mgLookupModule
   , mgBootModules
   , mgTransDeps
   , needsTemplateHaskellOrQQ
   , isTemplateHaskellOrQQNonBoot
   , showModMsg
   , moduleGraphNodeModule
   , moduleGraphNodeModSum

   , moduleGraphNodes
   , SummaryNode
   , summaryNodeSummary

   , NodeKey(..)
   , ModNodeKey
   , mkNodeKey
   , msKey

   )
where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.Maybe
import GHC.Data.Graph.Directed

import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Session

import GHC.Types.SourceFile ( hscSourceString, HscSource (HsBootFile) )

import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Env
import GHC.Unit.Types
import GHC.Utils.Outputable

import System.FilePath
import qualified Data.Map as Map
import GHC.Types.Unique.DSet
import GHC.Types.SrcLoc
import qualified Data.Set as Set
import GHC.Unit.Module

-- | A '@ModuleGraphNode@' is a node in the '@ModuleGraph@'.
-- Edges between nodes mark dependencies arising from module imports
-- and dependencies arising from backpack instantiations.
data ModuleGraphNode
  -- | Instantiation nodes track the instantiation of other units
  -- (backpack dependencies) with the holes (signatures) of the current package.
  = InstantiationNode InstantiatedUnit
  -- | There is a module summary node for each module, signature, and boot module being built.
  | ModuleNode ExtendedModSummary

moduleGraphNodeModSum :: ModuleGraphNode -> Maybe ExtendedModSummary
moduleGraphNodeModSum (InstantiationNode {}) = Nothing
moduleGraphNodeModSum (ModuleNode ems)    = Just ems

moduleGraphNodeModule :: ModuleGraphNode -> Maybe ModuleName
moduleGraphNodeModule = fmap (ms_mod_name . emsModSummary) . moduleGraphNodeModSum

instance Outputable ModuleGraphNode where
  ppr = \case
    InstantiationNode iuid -> ppr iuid
    ModuleNode ems -> ppr ems

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
  , mg_trans_deps :: Map.Map NodeKey (Set.Set NodeKey)
    -- A cached transitive dependency calculation so that a lot of work is not
    -- repeated whenever the transitive dependencies need to be calculated (for example, hptInstances)
  , mg_non_boot :: ModuleEnv ModSummary
    -- a map of all non-boot ModSummaries keyed by Modules
  , mg_boot :: ModuleSet
    -- a set of boot Modules
  , mg_needs_th_or_qq :: !Bool
    -- does any of the modules in mg_mss require TemplateHaskell or
    -- QuasiQuotes?
  }

-- | Determines whether a set of modules requires Template Haskell or
-- Quasi Quotes
--
-- Note that if the session's 'DynFlags' enabled Template Haskell when
-- 'depanal' was called, then each module in the returned module graph will
-- have Template Haskell enabled whether it is actually needed or not.
needsTemplateHaskellOrQQ :: ModuleGraph -> Bool
needsTemplateHaskellOrQQ mg = mg_needs_th_or_qq mg

-- | Map a function 'f' over all the 'ModSummaries'.
-- To preserve invariants 'f' can't change the isBoot status.
mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG f mg@ModuleGraph{..} = mg
  { mg_mss = flip fmap mg_mss $ \case
      InstantiationNode iuid -> InstantiationNode iuid
      ModuleNode (ExtendedModSummary ms bds) -> ModuleNode (ExtendedModSummary (f ms) bds)
  , mg_non_boot = mapModuleEnv f mg_non_boot
  }

mgBootModules :: ModuleGraph -> ModuleSet
mgBootModules ModuleGraph{..} = mg_boot

mgTransDeps :: ModuleGraph -> Map.Map NodeKey (Set.Set NodeKey)
mgTransDeps = mg_trans_deps

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries mg = [ m | ModuleNode (ExtendedModSummary m _) <- mgModSummaries' mg ]

mgExtendedModSummaries :: ModuleGraph -> [ExtendedModSummary]
mgExtendedModSummaries mg = [ ems | ModuleNode ems <- mgModSummaries' mg ]

mgModSummaries' :: ModuleGraph -> [ModuleGraphNode]
mgModSummaries' = mg_mss

mgElemModule :: ModuleGraph -> Module -> Bool
mgElemModule ModuleGraph{..} m = elemModuleEnv m mg_non_boot

-- | Look up a ModSummary in the ModuleGraph
mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
mgLookupModule ModuleGraph{..} m = lookupModuleEnv mg_non_boot m

emptyMG :: ModuleGraph
emptyMG = ModuleGraph [] Map.empty emptyModuleEnv emptyModuleSet False

isTemplateHaskellOrQQNonBoot :: ModSummary -> Bool
isTemplateHaskellOrQQNonBoot ms =
  (xopt LangExt.TemplateHaskell (ms_hspp_opts ms)
    || xopt LangExt.QuasiQuotes (ms_hspp_opts ms)) &&
  (isBootSummary ms == NotBoot)

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> ExtendedModSummary -> ModuleGraph
extendMG ModuleGraph{..} ems@(ExtendedModSummary ms _) = ModuleGraph
  { mg_mss = ModuleNode ems : mg_mss
  , mg_trans_deps = td
  , mg_non_boot = case isBootSummary ms of
      IsBoot -> mg_non_boot
      NotBoot -> extendModuleEnv mg_non_boot (ms_mod ms) ms
  , mg_boot = case isBootSummary ms of
      NotBoot -> mg_boot
      IsBoot -> extendModuleSet mg_boot (ms_mod ms)
  , mg_needs_th_or_qq = mg_needs_th_or_qq || isTemplateHaskellOrQQNonBoot ms
  }
  where
    (gg, _lookup_node) = moduleGraphNodes False (ModuleNode ems : mg_mss)
    td = allReachable gg (mkNodeKey . node_payload)

extendMGInst :: ModuleGraph -> InstantiatedUnit -> ModuleGraph
extendMGInst mg depUnitId = mg
  { mg_mss = InstantiationNode depUnitId : mg_mss mg
  }

extendMG' :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG' mg = \case
  InstantiationNode depUnitId -> extendMGInst mg depUnitId
  ModuleNode ems -> extendMG mg ems

mkModuleGraph :: [ExtendedModSummary] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG) emptyMG

mkModuleGraph' :: [ModuleGraphNode] -> ModuleGraph
mkModuleGraph' = foldr (flip extendMG') emptyMG

-- | This function filters out all the instantiation nodes from each SCC of a
-- topological sort. Use this with care, as the resulting "strongly connected components"
-- may not really be strongly connected in a direct way, as instantiations have been
-- removed. It would probably be best to eliminate uses of this function where possible.
filterToposortToModules
  :: [SCC ModuleGraphNode] -> [SCC ModSummary]
filterToposortToModules = mapMaybe $ mapMaybeSCC $ \case
  InstantiationNode _ -> Nothing
  ModuleNode (ExtendedModSummary node _) -> Just node
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
showModMsg _ _ (InstantiationNode indef_unit) =
  ppr $ instUnitInstanceOf indef_unit
showModMsg dflags recomp (ModuleNode (ExtendedModSummary mod_summary _)) =
  if gopt Opt_HideSourcePaths dflags
      then text mod_str
      else hsep $
         [ text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' ')
         , char '('
         , text (op $ msHsFilePath mod_summary) <> char ','
         , message, char ')' ]

  where
    op       = normalise
    mod      = moduleName (ms_mod mod_summary)
    mod_str  = showPpr dflags mod ++ hscSourceString (ms_hsc_src mod_summary)
    dyn_file = op $ msDynObjFilePath mod_summary
    obj_file = op $ msObjFilePath mod_summary
    message = case backend dflags of
                Interpreter | recomp -> text "interpreted"
                NoBackend            -> text "nothing"
                _                    ->
                  if gopt Opt_BuildDynamicToo  dflags
                    then text obj_file <> comma <+> text dyn_file
                    else text obj_file




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
      [ NodeKey_Unit inst_unit | inst_unit <- bds ] ++
      (NodeKey_Module . flip GWIB hs_boot_key . unLoc <$> ms_home_srcimps ms) ++
      [ NodeKey_Module $ GWIB (ms_mod_name ms) IsBoot
      | not $ drop_hs_boot_nodes || ms_hsc_src ms == HsBootFile
      ] ++
      (NodeKey_Module . flip GWIB NotBoot     . unLoc <$> ms_home_imps ms)
  where
    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = NotBoot -- is regular mod or signature
                | otherwise          = IsBoot

moduleGraphNodes :: Bool -> [ModuleGraphNode]
  -> (Graph SummaryNode, NodeKey -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
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

    -- We use integers as the keys for the SCC algorithm
    nodes :: [SummaryNode]
    nodes = [ DigraphNode s key $ out_edge_keys $ unfilteredEdges drop_hs_boot_nodes s
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

type ModNodeKey = ModuleNameWithIsBoot

data NodeKey = NodeKey_Unit {-# UNPACK #-} !InstantiatedUnit | NodeKey_Module {-# UNPACK #-} !ModNodeKey
  deriving (Eq, Ord)

instance Outputable NodeKey where
  ppr nk = pprNodeKey nk

newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

mkNodeKey :: ModuleGraphNode -> NodeKey
mkNodeKey = \case
  InstantiationNode x -> NodeKey_Unit x
  ModuleNode x -> NodeKey_Module $ ms_mnwib (emsModSummary x)

msKey :: ModSummary -> ModuleNameWithIsBoot
msKey = ms_mnwib

pprNodeKey :: NodeKey -> SDoc
pprNodeKey (NodeKey_Unit iu) = ppr iu
pprNodeKey (NodeKey_Module mk) = ppr mk


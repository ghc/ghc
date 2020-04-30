{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

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
   , needsTemplateHaskellOrQQ
   , isTemplateHaskellOrQQNonBoot
   , showModMsg
   )
where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.Maybe
import GHC.Data.Graph.Directed ( SCC(..) )

import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Session

import GHC.Types.SourceFile ( hscSourceString )

import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Env
import GHC.Unit.Types
import GHC.Utils.Outputable

import System.FilePath

-- | A '@ModuleGraphNode@' is a node in the '@ModuleGraph@'.
-- Edges between nodes mark dependencies arising from module imports
-- and dependencies arising from backpack instantiations.
data ModuleGraphNode
  -- | Instantiation nodes track the instantiation of other units
  -- (backpack dependencies) with the holes (signatures) of the current package.
  = InstantiationNode InstantiatedUnit
  -- | There is a module summary node for each module, signature, and boot module being built.
  | ModuleNode ExtendedModSummary

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
emptyMG = ModuleGraph [] emptyModuleEnv emptyModuleSet False

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
  , mg_non_boot = case isBootSummary ms of
      IsBoot -> mg_non_boot
      NotBoot -> extendModuleEnv mg_non_boot (ms_mod ms) ms
  , mg_boot = case isBootSummary ms of
      NotBoot -> mg_boot
      IsBoot -> extendModuleSet mg_boot (ms_mod ms)
  , mg_needs_th_or_qq = mg_needs_th_or_qq || isTemplateHaskellOrQQNonBoot ms
  }

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
         ] ++
         if gopt Opt_BuildDynamicToo dflags
            then [ text obj_file <> char ','
                 , text dyn_file
                 , char ')'
                 ]
            else [ text obj_file, char ')' ]
  where
    op       = normalise
    mod      = moduleName (ms_mod mod_summary)
    mod_str  = showPpr dflags mod ++ hscSourceString (ms_hsc_src mod_summary)
    dyn_file = op $ msDynObjFilePath mod_summary dflags
    obj_file = case backend dflags of
                Interpreter | recomp -> "interpreted"
                NoBackend            -> "nothing"
                _                    -> (op $ msObjFilePath mod_summary)


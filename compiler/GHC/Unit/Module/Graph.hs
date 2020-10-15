{-# LANGUAGE RecordWildCards #-}

module GHC.Unit.Module.Graph
   ( ModuleGraph
   , emptyMG
   , mkModuleGraph
   , extendMG
   , mapMG
   , mgModSummaries
   , mgElemModule
   , mgLookupModule
   , mgBootModules
   , needsTemplateHaskellOrQQ
   , isTemplateHaskellOrQQNonBoot
   )
where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Driver.Session

import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Env
import GHC.Unit.Types


-- | A ModuleGraph contains all the nodes from the home package (only).
-- There will be a node for each source module, plus a node for each hi-boot
-- module.
--
-- The graph is not necessarily stored in topologically-sorted order.  Use
-- 'GHC.topSortModuleGraph' and 'GHC.Data.Graph.Directed.flattenSCC' to achieve this.
data ModuleGraph = ModuleGraph
  { mg_mss :: [ModSummary]
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
  { mg_mss = map f mg_mss
  , mg_non_boot = mapModuleEnv f mg_non_boot
  }

mgBootModules :: ModuleGraph -> ModuleSet
mgBootModules ModuleGraph{..} = mg_boot

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries = mg_mss

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

-- | Add a ModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> ModSummary -> ModuleGraph
extendMG ModuleGraph{..} ms = ModuleGraph
  { mg_mss = ms:mg_mss
  , mg_non_boot = case isBootSummary ms of
      IsBoot -> mg_non_boot
      NotBoot -> extendModuleEnv mg_non_boot (ms_mod ms) ms
  , mg_boot = case isBootSummary ms of
      NotBoot -> mg_boot
      IsBoot -> extendModuleSet mg_boot (ms_mod ms)
  , mg_needs_th_or_qq = mg_needs_th_or_qq || isTemplateHaskellOrQQNonBoot ms
  }

mkModuleGraph :: [ModSummary] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG) emptyMG


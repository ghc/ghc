{-# LANGUAGE LambdaCase #-}
-- | The 'HomePackageTable' (HPT) contains information about all modules that are part
-- of a home package. At its core, the information for each module is a
-- 'ModInfo'.
--
-- The HPT is a monotonically increasing data structure: it only ever gets
-- extended by inserting modules which are loaded and for which we discover the
-- information required to construct a 'ModInfo'.
--
-- There should only ever exist one single HPT for any given package. It's
-- crucial we don't accidentally leak HPTs (e.g. by filtering it, which used to
-- happen), so the HPT is mutable and only its reference should be shared.
-- This is alright because the modules don't change throughout compilation.
--
-- There are various types of queries the compiler needs to do about the
-- modules of a package, such as getting the available type class instances,
-- type family instances, rules, COMPLETE pragmas, etc...
--
-- Those queries are frequent and should be answered efficiently. That's why
-- the the HPT iteratively constructs a cache of some of these things (another
-- reason why it's useful its interface doesn't allow arbitrary updates).
--
-- Note that to answer some queries, such as which instances are available in the
-- scope of Module X, a 'ModuleGraph' is also needed. The 'ModuleGraph' is
-- constructed once and for all during downsweep. It describes the structure
-- and relationship between different modules in the home units, as opposed to
-- the HPT which stores information about each module, but not about their structure.
--
-- :::WARNING:::
-- If you intend to change this interface, consider carefully whether you are
-- exposing memory-leak footguns which may end up being misused in the compiler
-- eventually. For instance, if you really, really, end up needing a way to take
-- a snapshot of the IORef (think: do you really need to?), at least make
-- obvious in the name like `snapshotCopyHpt`.
--
-- Or, do you really need a function to traverse all modules in the HPT? It is
-- often better to keep the computation internal to this module, such as in
-- 'hptCollectDependencies'...
module GHC.Unit.Home.PackageTable
  (
    HomePackageTable(..)
  , emptyHomePackageTable

    -- * Lookups in the HPT
  , lookupHpt
  , lookupHptByModule

    -- * Extending the HPT
  , addHomeModInfoToHpt
  , addHomeModInfosToHpt

    -- * Queries about home modules
  , hptCompleteSigs
  , hptAllInstances
  , hptAllFamInstances
  , hptAllAnnotations

    -- ** Transitive closure queries
    --
    -- | These are the queries which also require access to the 'ModuleGraph'
    -- which describes the structure of the modules, rather than being "global queries".
    -- Typically about the transitive closure
  , hptRulesBelow
  , hptAnnsBelow
  , hptInstancesBelow

    -- ** More Traversal-based queries
  , hptCollectDependencies
  , hptCollectObjects
  , hptCollectModules

    -- ** Memory dangerous queries
  , concatHpt

    -- * Utilities
  , pprHPT

    -- * Internals
    --
    -- | These provide access to the internals of the HomePackageTable to
    -- facilitate existing workflows that used the previous API. For instance,
    -- if you were listing out all elements or merging, you can keep doing so by reading
    -- the internal IO ref and then using the moduleenv contents directly.
    --
    -- In GHC itself these should be avoided, and other uses should justify why
    -- it is not sufficient to go through the intended insert-only API.
  , hptInternalTableRef
  , hptInternalTableFromRef

    -- * Legacy API
    --
    -- | This API is deprecated and meant to be removed.
  , addToHpt
  , addListToHpt
  ) where

import GHC.Prelude
import GHC.Data.Maybe

import Data.IORef
import Control.Monad ((<$!>))
import qualified Data.Set as Set

import GHC.Core.FamInstEnv
import GHC.Core.InstEnv
import GHC.Core.Rules
import GHC.Linker.Types
import GHC.Types.Annotations
import GHC.Types.CompleteMatch
import GHC.Types.Unique.DFM
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Utils.Outputable
import GHC.Builtin.Names (gHC_PRIM)

-- | Helps us find information about modules in the home package
newtype HomePackageTable = HPT {

    table :: IORef (DModuleNameEnv HomeModInfo)
    -- ^ Domain = modules in the home unit that have been fully compiled
    -- "home" unit id cached (implicit) here for convenience.
    --
    -- This is an IORef because the HPT musn't leak; We want to always augment
    -- it, and handle rehydration such that rehydrated modules point to the
    -- actual modules rather than to hs-boot files... Previously we did this by
    -- tying a knot on the lazy HPT, but this leaked the HPT, ...
    --
    -- The elements of this table may be updated (e.g. on rehydration).
    --
    -- ROMES:TODO: Explain!!!!!
  }

-- | Create a new 'HomePackageTable'.
--
-- Be careful not to share it across e.g. different units, since it uses a
-- mutable variable under the hood to keep the monotonically increasing list of
-- loaded modules.
emptyHomePackageTable :: IO HomePackageTable
-- romes:todo: use a MutableArray directly?
emptyHomePackageTable = do
  table <- newIORef emptyUDFM
  return HPT{table}

--------------------------------------------------------------------------------
-- * Lookups in the HPT
--------------------------------------------------------------------------------

-- | Lookup the 'HomeModInfo' of a module in the HPT, given its name.
lookupHpt :: HomePackageTable -> ModuleName -> IO (Maybe HomeModInfo)
lookupHpt HPT{table=hpt} mn = (`lookupUDFM` mn) <$!> readIORef hpt

-- | Lookup the 'HomeModInfo' of a 'Module' in the HPT.
lookupHptByModule :: HomePackageTable -> Module -> IO (Maybe HomeModInfo)
lookupHptByModule hpt mod
  = -- The HPT is indexed by ModuleName, not Module,
    -- we must check for a hit on the right Module
    lookupHpt hpt (moduleName mod) >>= pure . \case
      Just hm | mi_module (hm_iface hm) == mod -> Just hm
      _otherwise                               -> Nothing

--------------------------------------------------------------------------------
-- * Extending the HPT
--------------------------------------------------------------------------------

-- | Add a new module to the HPT.
--
-- An HPT is a monotonically increasing data structure, holding information about loaded modules in a package.
-- This is the main function by which the HPT is extended or updated.
--
-- When the module of the inserted 'HomeModInfo' does not exist, a new entry in
-- the HPT is created for that module name.
-- When the module already has an entry, inserting a new one entry in the HPT
-- will always overwrite the existing entry for that module.
--
-- $O(1)$
addHomeModInfoToHpt :: HomeModInfo -> HomePackageTable -> IO ()
addHomeModInfoToHpt hmi hpt = addToHpt hpt (moduleName (mi_module (hm_iface hmi))) hmi

{-# DEPRECATED addToHpt "Deprecated in favour of 'addHomeModInfoToHpt', as the module at which a 'HomeModInfo' is inserted should always be derived from the 'HomeModInfo' itself." #-}
-- After deprecation cycle, move `addToHpt` to a `where` clause inside `addHomeModInfoToHpt`.
addToHpt :: HomePackageTable -> ModuleName -> HomeModInfo -> IO ()
addToHpt HPT{table=hptr} mn hmi = do
  atomicModifyIORef' hptr (\hpt -> (addToUDFM hpt mn hmi, ()))
  -- If the key already existed in the map, this insertion is overwriting
  -- the HMI of a previously loaded module (likely in rehydration).

-- | 'addHomeModInfoToHpt' for multiple module infos.
addHomeModInfosToHpt :: HomePackageTable -> [HomeModInfo] -> IO ()
addHomeModInfosToHpt hpt = mapM_ (flip addHomeModInfoToHpt hpt)

{-# DEPRECATED addListToHpt "Deprecated in favour of 'addHomeModInfosToHpt', as the module at which a 'HomeModInfo' is inserted should always be derived from the 'HomeModInfo' itself." #-}
-- After deprecation cycle, remove.
addListToHpt :: HomePackageTable -> [(ModuleName, HomeModInfo)] -> IO ()
addListToHpt hpt = mapM_ (uncurry (addToHpt hpt))

----------------------------------------------------------------------------------
---- * Queries
----------------------------------------------------------------------------------

-- | Get all 'CompleteMatches' (arising from COMPLETE pragmas) present in all
-- modules from this unit's HPT.
--
-- $O(n)$ in the number of modules.
hptCompleteSigs :: HomePackageTable -> IO CompleteMatches
hptCompleteSigs = concatHpt (md_complete_matches . hm_details)

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
-- ROMES:TODO: wait what?
--
-- $O(n)$ in the number of modules.
hptAllInstances :: HomePackageTable -> IO (InstEnv, [FamInst])
hptAllInstances hpt = do
  hits <- flip concatHpt hpt $ \mod_info -> do
     let details = hm_details mod_info
     return (md_insts details, md_fam_insts details)
  let (insts, famInsts) = unzip hits
  return (foldl' unionInstEnv emptyInstEnv insts, concat famInsts)

-- | Find all the family instance declarations from the HPT
--
-- $O(n)$ in the number of modules.
hptAllFamInstances :: HomePackageTable -> IO (ModuleEnv FamInstEnv)
hptAllFamInstances = fmap mkModuleEnv . concatHpt (\hmi -> [(hmiModule hmi, hmiFamInstEnv hmi)])
  where
    hmiModule     = mi_module . hm_iface
    hmiFamInstEnv = extendFamInstEnvList emptyFamInstEnv
                      . md_fam_insts . hm_details

-- | All annotations from the HPT
--
-- $O(n)$ in the number of modules.
hptAllAnnotations :: HomePackageTable -> IO AnnEnv
hptAllAnnotations = fmap mkAnnEnv . concatHpt (md_anns . hm_details)

--------------------------------------------------------------------------------
-- * Queries on Transitive Closure
--------------------------------------------------------------------------------
-- ROMES:TODO: Something else I want to do here is to receive a ModuleGraph and
-- then use the fast reachability queries to determine whether something is
-- reachable or not. That means we can very efficiently filter out things which
-- are not part of the transitive closure...
--
-- So, e.g. it could probably be done faster by filtering out a cached list of
-- rules using a 'ReachabilityIndex' as the filter $O(1)$ fast queries.

-- ROMES:TODO: Do something about the
--
--    | isOneShot (ghcMode (hsc_dflags hsc_env)) = []
--
-- shortcut that we used when this was a function on HscEnv...

-- | Find all rules in modules that are in the transitive closure of the given
-- module.
--
-- $O(n)$ in the number of dependencies?
hptRulesBelow :: HomePackageTable -> ModuleGraph -> UnitId -> ModuleNameWithIsBoot -> IO RuleBase
hptRulesBelow hpt mg uid mn = foldr (flip extendRuleBaseList) emptyRuleBase <$>
  hptSomeThingsBelowUs (md_rules . hm_details) False hpt mg uid mn

-- | Get annotations from modules "below" this one (in the dependency sense)
--
-- $O(n)$ in the number of dependencies?
hptAnnsBelow :: HomePackageTable -> ModuleGraph -> UnitId -> ModuleNameWithIsBoot -> IO AnnEnv
hptAnnsBelow hpt mg uid mn = foldr (flip extendAnnEnvList) emptyAnnEnv <$>
  hptSomeThingsBelowUs (md_anns . hm_details) False hpt mg uid mn

hptInstancesBelow :: HomePackageTable -> ModuleGraph -> UnitId -> ModuleNameWithIsBoot -> IO (InstEnv, [FamInst])
hptInstancesBelow hpt mg uid mnwib = do
-- ouch... improve
 let mn = gwib_mod mnwib
 (insts, famInsts) <-
     unzip . concat <$>
       hptSomeThingsBelowUs (\mod_info ->
                                  let details = hm_details mod_info
                                  -- Don't include instances for the current module
                                  in if moduleName (mi_module (hm_iface mod_info)) == mn
                                       then []
                                       else [(md_insts details, md_fam_insts details)])
                          True -- Include -hi-boot
                          hpt
                          mg
                          uid
                          mnwib
 return (foldl' unionInstEnv emptyInstEnv insts, concat famInsts)

-- | Get things from modules in the transitive closure of the given module.
--
-- Note: Don't expose this function. We can improve the interface further --
-- let's keep the queries on the HPT contained in this module so we can optimise
-- internally without breaking the API to the rest of GHC. This is a footgun if
-- exposed!
--
-- NOTE: We should be able to import this considerably with the reachability
-- index and caching?...
--
-- For example, easiest to go through all modules and filter out the ones in the
-- hpt via the module graph.........
--
-- TODO: This include_hi_boot business is also pretty weird. Do we need it at all?
hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HomePackageTable -> ModuleGraph -> UnitId -> ModuleNameWithIsBoot -> IO [[a]]
hptSomeThingsBelowUs extract include_hi_boot hpt mg uid mn
  = sequence
    [ things
      -- "Finding each non-hi-boot module below me" maybe could be cached (well,
      -- the inverse) in the module graph to avoid filtering the boots out of
      -- the transitive closure out every time this is called
    | (ModNodeKeyWithUid (GWIB { gwib_mod = mod, gwib_isBoot = is_boot }) mod_uid)
          <- Set.toList (moduleGraphModulesBelow mg uid mn)
    , include_hi_boot || (is_boot == NotBoot)

        -- unsavoury: when compiling the base package with --make, we
        -- sometimes try to look up RULES etc for GHC.Prim. GHC.Prim won't
        -- be in the HPT, because we never compile it; it's in the EPT
        -- instead. ToDo: clean up, and remove this slightly bogus filter:
    , mod /= moduleName gHC_PRIM
    , not (mod == gwib_mod mn && uid == mod_uid)

        -- Look it up in the HPT
    , let things = lookupHpt hpt mod >>= \case
                    Just info -> return $ extract info
                    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg mempty
          msg = vcat [text "missing module" <+> ppr mod,
                     text "When starting from"  <+> ppr mn,
                     text "below:" <+> ppr (moduleGraphModulesBelow mg uid mn),
                      text "Probable cause: out-of-date interface files"]
                        -- This really shouldn't happen, but see #962
    ]

--------------------------------------------------------------------------------
-- * Traversal-based queries
--------------------------------------------------------------------------------

-- | Collect the immediate dependencies of all modules in the HPT into a Set.
-- The immediate dependencies are given by the iface as @'dep_direct_pkgs' . 'mi_deps'@.
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectDependencies :: HomePackageTable -> IO (Set.Set UnitId)
hptCollectDependencies HPT{table} = do
  hpt <- readIORef table
  return $
    foldr (Set.union . dep_direct_pkgs . mi_deps . hm_iface) Set.empty hpt

-- | Collect the linkable object of all modules in the HPT.
-- The linkable objects are given by @'homeModInfoObject'@.
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectObjects :: HomePackageTable -> IO [Linkable]
hptCollectObjects HPT{table} = do
  hpt <- readIORef table
  return $
    foldr ((:) . expectJust "collectObjects" . homeModInfoObject) [] hpt

-- | Collect all module ifaces in the HPT
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectModules :: HomePackageTable -> IO [Module]
hptCollectModules HPT{table} = do
  hpt <- readIORef table
  return $
    foldr ((:) . mi_module . hm_iface) [] hpt

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- | Pretty print a 'HomePackageTable'.
--
-- Make sure you really do need to print the whole HPT before infusing too much
-- code with IO.
--
-- For instance, in the HUG, it suffices to print the unit-keys present in the
-- unit map in failed lookups.
pprHPT :: HomePackageTable -> IO SDoc
-- A bit arbitrary for now
pprHPT HPT{table=hptr} = do
  hpt <- readIORef hptr
  return $!
    pprUDFM hpt $ \hms ->
      vcat [ ppr (mi_module (hm_iface hm))
           | hm <- hms ]

----------------------------------------------------------------------------------
-- THE TYPE OF FOOTGUNS WE DON'T WANT TO EXPOSE
----------------------------------------------------------------------------------

-- eltsHpt :: HomePackageTable -> [HomeModInfo]
-- filterHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> HomePackageTable
-- mapHpt :: (HomeModInfo -> HomeModInfo) -> HomePackageTable -> HomePackageTable
-- delFromHpt :: HomePackageTable -> ModuleName -> HomePackageTable
-- listToHpt :: [(ModuleName, HomeModInfo)] -> HomePackageTable
-- listHMIToHpt :: [HomeModInfo] -> HomePackageTable

----------------------------------------------------------------------------------
-- Would be fine, but may lead to linearly traversing the HPT unnecessarily
-- (e.g. `lastLoadedKey` superseded bad usages)
----------------------------------------------------------------------------------

-- allHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
-- allHpt = allUDFM

-- anyHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
-- anyHpt = anyUDFM

----------------------------------------------------------------------------------
-- Would be ok to expose this function very /careful/ with the argument function
----------------------------------------------------------------------------------

-- | Like @concatMap f . 'eltsHpt'@, but filters out all 'HomeModInfo' for which
-- @f@ returns the empty list before doing the sort inherent to 'eltsUDFM'.
--
-- If this function is ever exposed from the HPT module, make sure the
-- argument function doesn't introduce leaks.
concatHpt :: (HomeModInfo -> [a]) -> HomePackageTable -> IO [a]
concatHpt f HPT{table} = do
  hpt <- readIORef table
  return $ concat . eltsUDFM . mapMaybeUDFM g $ hpt
  where
    g hmi = case f hmi of { [] -> Nothing; as -> Just as }

--------------------------------------------------------------------------------
-- * Internals (see haddocks!)
--------------------------------------------------------------------------------

-- | Gets the internal 'IORef' which holds the 'HomeModInfo's of this HPT.
-- Use with care.
hptInternalTableRef :: HomePackageTable -> IORef (DModuleNameEnv HomeModInfo)
hptInternalTableRef = table

-- | Construct a HomePackageTable from the IORef.
-- Use with care, only if you can really justify going around the intended insert-only API.
hptInternalTableFromRef :: IORef (DModuleNameEnv HomeModInfo) -> IO HomePackageTable
hptInternalTableFromRef ref = do
  return HPT {
    table = ref
  }


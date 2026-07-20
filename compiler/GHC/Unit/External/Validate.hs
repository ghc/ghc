module GHC.Unit.External.Validate (
  -- * Validation of unit databases
  validateDatabase,
  reportUnusable,
  UnusableUnits,
  UnusableUnit(..),
  UnusableUnitReason(..),
  pprReason,
  -- * Package resolver
  findPackages,
  selectPackages,
  -- * Unit database closure validation
  UnitErr(..),
  mayThrowUnitErr,
  closeUnitDeps,
  closeUnitDeps',
  -- * Utils
  ignoreUnits,
  pprFlag,
) where

import GHC.Prelude

import Control.Monad
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (partition)
import GHC.Data.Maybe
import GHC.Driver.DynFlags
import GHC.Types.Unique.Map
import GHC.Unit.External.Database
import GHC.Unit.External.Query
import GHC.Unit.External.Substitution
import GHC.Unit.Info
import GHC.Unit.Types
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Outputable qualified as Outputable
import GHC.Utils.Panic

-- -----------------------------------------------------------------------------
-- Database validation

-- | Validates a database, removing unusable units from it
-- (this includes removing units that the user has explicitly
-- ignored.)  Our general strategy:
--
-- 1. Remove all broken units (dangling dependencies)
-- 2. Remove all units that are cyclic
-- 3. Apply ignore flags
-- 4. Remove all units which have deps with mismatching ABIs
--
validateDatabase :: [IgnorePackageFlag] -> UnitInfoMap
                 -> (UnitInfoMap, UnusableUnits, [SCC UnitInfo])
validateDatabase flagsIgnored pkg_map1 =
    (pkg_map5, unusable, sccs)
  where
    ignore_flags = reverse flagsIgnored -- (unitConfigFlagsIgnored cfg)

    -- Compute the reverse dependency index
    index = reverseDeps pkg_map1

    -- Helper function
    mk_unusable mk_err dep_matcher m uids =
      listToUniqMap [ (unitId pkg, (pkg, mk_err (dep_matcher m pkg)))
                    | pkg <- uids
                    ]

    -- Find broken units
    directly_broken = filter (not . null . depsNotAvailable pkg_map1)
                             (nonDetEltsUniqMap pkg_map1)
    (pkg_map2, broken) = removeUnits (map unitId directly_broken) index pkg_map1
    unusable_broken = mk_unusable BrokenDependencies depsNotAvailable pkg_map2 broken

    -- Find recursive units
    sccs = stronglyConnComp [ (pkg, unitId pkg, unitDepends pkg)
                            | pkg <- nonDetEltsUniqMap pkg_map2 ]
    getCyclicSCC (CyclicSCC vs) = map unitId vs
    getCyclicSCC (AcyclicSCC _) = []
    (pkg_map3, cyclic) = removeUnits (concatMap getCyclicSCC sccs) index pkg_map2
    unusable_cyclic = mk_unusable CyclicDependencies depsNotAvailable pkg_map3 cyclic

    -- Apply ignore flags
    directly_ignored = ignoreUnits ignore_flags (nonDetEltsUniqMap pkg_map3)
    (pkg_map4, ignored) = removeUnits (nonDetKeysUniqMap directly_ignored) index pkg_map3
    unusable_ignored = mk_unusable IgnoredDependencies depsNotAvailable pkg_map4 ignored

    -- Knock out units whose dependencies don't agree with ABI
    -- (i.e., got invalidated due to shadowing)
    directly_shadowed = filter (not . null . depsAbiMismatch pkg_map4)
                               (nonDetEltsUniqMap pkg_map4)
    (pkg_map5, shadowed) = removeUnits (map unitId directly_shadowed) index pkg_map4
    unusable_shadowed = mk_unusable ShadowedDependencies depsAbiMismatch pkg_map5 shadowed

    -- combine all unusables. The order is important for shadowing.
    -- plusUniqMapList folds using plusUFM which is right biased (opposite of
    -- Data.Map.union) so the head of the list should be the least preferred
    unusable = plusUniqMapList [ unusable_shadowed
                               , unusable_cyclic
                               , unusable_broken
                               , unusable_ignored
                               , directly_ignored
                               ]


type UnusableUnits = UniqMap UnitId (UnitInfo, UnusableUnitReason)

-- | A unusable unit module origin
data UnusableUnit = UnusableUnit
  { uuUnit        :: !Unit               -- ^ Unusable unit
  , uuReason      :: !UnusableUnitReason -- ^ Reason
  , uuIsReexport  :: !Bool               -- ^ Is the "module" a reexport?
  }

-- | The reason why a unit is unusable.
data UnusableUnitReason
  = -- | We ignored it explicitly using @-ignore-package@.
    IgnoredWithFlag
    -- | This unit transitively depends on a unit that was never present
    -- in any of the provided databases.
  | BrokenDependencies   [UnitId]
    -- | This unit transitively depends on a unit involved in a cycle.
    -- Note that the list of 'UnitId' reports the direct dependencies
    -- of this unit that (transitively) depended on the cycle, and not
    -- the actual cycle itself (which we report separately at high verbosity.)
  | CyclicDependencies   [UnitId]
    -- | This unit transitively depends on a unit which was ignored.
  | IgnoredDependencies  [UnitId]
    -- | This unit transitively depends on a unit which was
    -- shadowed by an ABI-incompatible unit.
  | ShadowedDependencies [UnitId]

instance Outputable UnusableUnitReason where
    ppr IgnoredWithFlag = text "[ignored with flag]"
    ppr (BrokenDependencies uids)   = brackets (text "broken" <+> ppr uids)
    ppr (CyclicDependencies uids)   = brackets (text "cyclic" <+> ppr uids)
    ppr (IgnoredDependencies uids)  = brackets (text "ignored" <+> ppr uids)
    ppr (ShadowedDependencies uids) = brackets (text "shadowed" <+> ppr uids)

pprReason :: SDoc -> UnusableUnitReason -> SDoc
pprReason pref reason = case reason of
  IgnoredWithFlag ->
      pref <+> text "ignored due to an -ignore-package flag"
  BrokenDependencies deps ->
      pref <+> text "unusable due to missing dependencies:" $$
        nest 2 (hsep (map ppr deps))
  CyclicDependencies deps ->
      pref <+> text "unusable due to cyclic dependencies:" $$
        nest 2 (hsep (map ppr deps))
  IgnoredDependencies deps ->
      pref <+> text ("unusable because the -ignore-package flag was used to " ++
                     "ignore at least one of its dependencies:") $$
        nest 2 (hsep (map ppr deps))
  ShadowedDependencies deps ->
      pref <+> text "unusable due to shadowed dependencies:" $$
        nest 2 (hsep (map ppr deps))

reportUnusable :: Logger -> UnusableUnits -> IO ()
reportUnusable logger pkgs = when (logVerbAtLeast logger 2) $ mapM_ report (nonDetUniqMapToList pkgs)
  where
    report (ipid, (_, reason)) =
       debugTraceMsg logger 2 $
         pprReason
           (text "package" <+> ppr ipid <+> text "is") reason

-- -----------------------------------------------------------------------------
-- Package Finding

-- | Like 'selectPackages', but doesn't return a list of unmatched
-- packages.  Furthermore, any packages it returns are *renamed*
-- if the 'UnitArg' has a renaming associated with it.
findPackages :: UnitPrecedenceMap
             -> UnitInfoMap
             -> PackageArg -> [UnitInfo]
             -> UnusableUnits
             -> Either [(UnitInfo, UnusableUnitReason)]
                [UnitInfo]
findPackages prec_map pkg_map arg pkgs unusable
  = let ps = mapMaybe (finder arg) pkgs
    in if null ps
        then Left (mapMaybe (\(x,y) -> finder arg x >>= \x' -> return (x',y))
                            (nonDetEltsUniqMap unusable))
        else Right (sortByPreference prec_map ps)
  where
    finder (PackageArg str) p
      = if matchingStr str p
          then Just p
          else Nothing
    finder (UnitIdArg uid) p
      = case uid of
          RealUnit (Definite iuid)
            | iuid == unitId p
            -> Just p
          VirtUnit inst
            | instUnitInstanceOf inst == unitId p
            -> Just (renameUnitInfo pkg_map (instUnitInsts inst) p)
          _ -> Nothing

selectPackages :: UnitPrecedenceMap -> PackageArg -> [UnitInfo]
               -> UnusableUnits
               -> Either [(UnitInfo, UnusableUnitReason)]
                  ([UnitInfo], [UnitInfo])
selectPackages prec_map arg pkgs unusable
  = let matches = matching arg
        (ps,rest) = partition matches pkgs
    in if null ps
        then Left (filter (matches.fst) (nonDetEltsUniqMap unusable))
        else Right (sortByPreference prec_map ps, rest)

-- -----------------------------------------------------------------------------
-- Ignore units

ignoreUnits :: [IgnorePackageFlag] -> [UnitInfo] -> UnusableUnits
ignoreUnits flags pkgs = listToUniqMap (concatMap doit flags)
  where
  doit (IgnorePackage str) =
     case partition (matchingStr str) pkgs of
         (ps, _) -> [ (unitId p, (p, IgnoredWithFlag))
                    | p <- ps ]
        -- missing unit is not an error for -ignore-package,
        -- because a common usage is to -ignore-package P as
        -- a preventative measure just in case P exists.

-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> UnitInfo -> Bool
matchingStr str p
        =  str == unitPackageIdString p
        || str == unitPackageNameString p

matchingId :: UnitId -> UnitInfo -> Bool
matchingId uid p = uid == unitId p

matching :: PackageArg -> UnitInfo -> Bool
matching (PackageArg str) = matchingStr str
matching (UnitIdArg (RealUnit (Definite uid))) = matchingId uid
matching (UnitIdArg _)  = \_ -> False -- TODO: warn in this case

-- ----------------------------------------------------------------------------
--
-- Closures
--


-- | Takes a list of UnitIds (and their "parent" dependency, used for error
-- messages), and returns the list with dependencies included, in reverse
-- dependency order (a units appears before those it depends on).
closeUnitDeps :: UnitInfoMap -> [(UnitId,Maybe UnitId)] -> MaybeErr UnitErr [UnitId]
closeUnitDeps pkg_map ps = closeUnitDeps' pkg_map [] ps

-- | Similar to closeUnitDeps but takes a list of already loaded units as an
-- additional argument.
closeUnitDeps' :: UnitInfoMap -> [UnitId] -> [(UnitId,Maybe UnitId)] -> MaybeErr UnitErr [UnitId]
closeUnitDeps' pkg_map current_ids ps = foldM (uncurry . add_unit pkg_map) current_ids ps

-- | Add a UnitId and those it depends on (recursively) to the given list of
-- UnitIds if they are not already in it. Return a list in reverse dependency
-- order (a unit appears before those it depends on).
--
-- The UnitId is looked up in the given UnitInfoMap (to find its dependencies).
-- It it's not found, the optional parent unit is used to return a more precise
-- error message ("dependency of <PARENT>").
add_unit :: UnitInfoMap
            -> [UnitId]
            -> UnitId
            -> Maybe UnitId
            -> MaybeErr UnitErr [UnitId]
add_unit pkg_map ps p mb_parent
  | p `elem` ps = return ps     -- Check if we've already added this unit
  | otherwise   = case lookupUnitId' pkg_map p of
      Nothing   -> Failed (CloseUnitErr p mb_parent)
      Just info -> do
         -- Add the unit's dependents also
         ps' <- foldM add_unit_key ps (unitDepends info)
         return (p : ps')
        where
          add_unit_key xs key
            = add_unit pkg_map xs key (Just p)
data UnitErr
  = CloseUnitErr !UnitId !(Maybe UnitId)
  | PackageFlagErr !PackageFlag ![(UnitInfo,UnusableUnitReason)]
  | TrustFlagErr   !TrustFlag   ![(UnitInfo,UnusableUnitReason)]

mayThrowUnitErr :: MaybeErr UnitErr a -> IO a
mayThrowUnitErr = \case
    Failed e    -> throwGhcExceptionIO
                    $ CmdLineError
                    $ renderWithContext defaultSDocContext
                    $ withPprStyle defaultUserStyle
                    $ ppr e
    Succeeded a -> return a

instance Outputable UnitErr where
    ppr = \case
        CloseUnitErr p mb_parent
            -> (text "unknown unit:" <+> ppr p)
               <> case mb_parent of
                     Nothing     -> Outputable.empty
                     Just parent -> space <> parens (text "dependency of"
                                              <+> ftext (unitIdFS parent))
        PackageFlagErr flag reasons
            -> flag_err (pprFlag flag) reasons

        TrustFlagErr flag reasons
            -> flag_err (pprTrustFlag flag) reasons
      where
        flag_err flag_doc reasons =
            text "cannot satisfy "
            <> flag_doc
            <> (if null reasons then Outputable.empty else text ": ")
            $$ nest 4 (vcat (map ppr_reason reasons) $$
                      text "(use -v for more information)")

        ppr_reason (p, reason) =
            pprReason (ppr (unitId p) <+> text "is") reason


pprFlag :: PackageFlag -> SDoc
pprFlag flag = case flag of
    HidePackage p   -> text "-hide-package " <> text p
    ExposePackage doc _ _ -> text doc

pprTrustFlag :: TrustFlag -> SDoc
pprTrustFlag flag = case flag of
    TrustPackage p    -> text "-trust " <> text p
    DistrustPackage p -> text "-distrust " <> text p

-- ----------------------------------------------------------------------------
--
-- Utilities on the database
--

-- | A reverse dependency index, mapping an 'UnitId' to
-- the 'UnitId's which have a dependency on it.
type RevIndex = UniqMap UnitId [UnitId]

-- | Compute the reverse dependency index of a unit database.
reverseDeps :: UnitInfoMap -> RevIndex
reverseDeps db = nonDetFoldUniqMap go emptyUniqMap db
  where
    go :: (UnitId, UnitInfo) -> RevIndex -> RevIndex
    go (_uid, pkg) r = foldl' (go' (unitId pkg)) r (unitDepends pkg)
    go' from r to = addToUniqMap_C (++) r to [from]

-- | Given a list of 'UnitId's to remove, a database,
-- and a reverse dependency index (as computed by 'reverseDeps'),
-- remove those units, plus any units which depend on them.
-- Returns the pruned database, as well as a list of 'UnitInfo's
-- that was removed.
removeUnits :: [UnitId] -> RevIndex
               -> UnitInfoMap
               -> (UnitInfoMap, [UnitInfo])
removeUnits uids index m = go uids (m,[])
  where
    go [] (m,pkgs) = (m,pkgs)
    go (uid:uids) (m,pkgs)
        | Just pkg <- lookupUniqMap m uid
        = case lookupUniqMap index uid of
            Nothing    -> go uids (delFromUniqMap m uid, pkg:pkgs)
            Just rdeps -> go (rdeps ++ uids) (delFromUniqMap m uid, pkg:pkgs)
        | otherwise
        = go uids (m,pkgs)

-- | Given a 'UnitInfo' from some 'UnitInfoMap', return all entries in 'depends'
-- which correspond to units that do not exist in the index.
depsNotAvailable :: UnitInfoMap
                 -> UnitInfo
                 -> [UnitId]
depsNotAvailable pkg_map pkg = filter (not . (`elemUniqMap` pkg_map)) (unitDepends pkg)

-- | Given a 'UnitInfo' from some 'UnitInfoMap' return all entries in
-- 'unitAbiDepends' which correspond to units that do not exist, OR have
-- mismatching ABIs.
depsAbiMismatch :: UnitInfoMap
                -> UnitInfo
                -> [UnitId]
depsAbiMismatch pkg_map pkg = map fst . filter (not . abiMatch) $ unitAbiDepends pkg
  where
    abiMatch (dep_uid, abi)
        | Just dep_pkg <- lookupUniqMap pkg_map dep_uid
        = unitAbiHash dep_pkg == abi
        | otherwise
        = False

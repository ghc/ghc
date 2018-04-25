{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.SolverInstallPlan
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'SolverInstallPlan' is the graph of packages produced by the
-- dependency solver, and specifies at the package-granularity what
-- things are going to be installed.  To put it another way: the
-- dependency solver produces a 'SolverInstallPlan', which is then
-- consumed by various other parts of Cabal.
--
-----------------------------------------------------------------------------
module Distribution.Client.SolverInstallPlan(
  SolverInstallPlan(..),
  SolverPlanPackage,
  ResolverPackage(..),

  -- * Operations on 'SolverInstallPlan's
  new,
  toList,
  toMap,

  remove,

  showPlanIndex,
  showInstallPlan,

  -- * Checking validity of plans
  valid,
  closed,
  consistent,
  acyclic,

  -- ** Details on invalid plans
  SolverPlanProblem(..),
  showPlanProblem,
  problems,

  -- ** Querying the install plan
  dependencyClosure,
  reverseDependencyClosure,
  topologicalOrder,
  reverseTopologicalOrder,
) where

import Distribution.Package
         ( PackageIdentifier(..), Package(..), PackageName
         , HasUnitId(..), PackageId, packageVersion, packageName )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Text
         ( display )

import Distribution.Client.Types
         ( UnresolvedPkgLoc )
import Distribution.Version
         ( Version )

import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId

import Data.List
         ( intercalate )
import Data.Maybe
         ( fromMaybe, mapMaybe )
import Distribution.Compat.Binary (Binary(..))
import Distribution.Compat.Graph (Graph, IsNode(..))
import qualified Data.Graph as OldGraph
import qualified Distribution.Compat.Graph as Graph
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array ((!))
import Data.Typeable

type SolverPlanPackage = ResolverPackage UnresolvedPkgLoc

type SolverPlanIndex = Graph SolverPlanPackage

data SolverInstallPlan = SolverInstallPlan {
    planIndex      :: !SolverPlanIndex,
    planIndepGoals :: !IndependentGoals
  }
  deriving (Typeable)

{-
-- | Much like 'planPkgIdOf', but mapping back to full packages.
planPkgOf :: SolverInstallPlan
          -> Graph.Vertex
          -> SolverPlanPackage
planPkgOf plan v =
    case Graph.lookupKey (planIndex plan)
                         (planPkgIdOf plan v) of
      Just pkg -> pkg
      Nothing  -> error "InstallPlan: internal error: planPkgOf lookup failed"
-}

mkInstallPlan :: SolverPlanIndex
              -> IndependentGoals
              -> SolverInstallPlan
mkInstallPlan index indepGoals =
    SolverInstallPlan {
      planIndex      = index,
      planIndepGoals = indepGoals
    }

instance Binary SolverInstallPlan where
    put SolverInstallPlan {
              planIndex      = index,
              planIndepGoals = indepGoals
        } = put (index, indepGoals)

    get = do
      (index, indepGoals) <- get
      return $! mkInstallPlan index indepGoals

showPlanIndex :: [SolverPlanPackage] -> String
showPlanIndex = intercalate "\n" . map showPlanPackage

showInstallPlan :: SolverInstallPlan -> String
showInstallPlan = showPlanIndex . toList

showPlanPackage :: SolverPlanPackage -> String
showPlanPackage (PreExisting ipkg) = "PreExisting " ++ display (packageId ipkg)
                                            ++ " (" ++ display (installedUnitId ipkg)
                                            ++ ")"
showPlanPackage (Configured  spkg)   = "Configured " ++ display (packageId spkg)

-- | Build an installation plan from a valid set of resolved packages.
--
new :: IndependentGoals
    -> SolverPlanIndex
    -> Either [SolverPlanProblem] SolverInstallPlan
new indepGoals index =
  case problems indepGoals index of
    []    -> Right (mkInstallPlan index indepGoals)
    probs -> Left probs

toList :: SolverInstallPlan -> [SolverPlanPackage]
toList = Graph.toList . planIndex

toMap :: SolverInstallPlan -> Map SolverId SolverPlanPackage
toMap = Graph.toMap . planIndex

-- | Remove packages from the install plan. This will result in an
-- error if there are remaining packages that depend on any matching
-- package. This is primarily useful for obtaining an install plan for
-- the dependencies of a package or set of packages without actually
-- installing the package itself, as when doing development.
--
remove :: (SolverPlanPackage -> Bool)
       -> SolverInstallPlan
       -> Either [SolverPlanProblem]
                 (SolverInstallPlan)
remove shouldRemove plan =
    new (planIndepGoals plan) newIndex
  where
    newIndex = Graph.fromDistinctList $
                 filter (not . shouldRemove) (toList plan)

-- ------------------------------------------------------------
-- * Checking validity of plans
-- ------------------------------------------------------------

-- | A valid installation plan is a set of packages that is 'acyclic',
-- 'closed' and 'consistent'. Also, every 'ConfiguredPackage' in the
-- plan has to have a valid configuration (see 'configuredPackageValid').
--
-- * if the result is @False@ use 'problems' to get a detailed list.
--
valid :: IndependentGoals
      -> SolverPlanIndex
      -> Bool
valid indepGoals index =
    null $ problems indepGoals index

data SolverPlanProblem =
     PackageMissingDeps   SolverPlanPackage
                          [PackageIdentifier]
   | PackageCycle         [SolverPlanPackage]
   | PackageInconsistency PackageName [(PackageIdentifier, Version)]
   | PackageStateInvalid  SolverPlanPackage SolverPlanPackage

showPlanProblem :: SolverPlanProblem -> String
showPlanProblem (PackageMissingDeps pkg missingDeps) =
     "Package " ++ display (packageId pkg)
  ++ " depends on the following packages which are missing from the plan: "
  ++ intercalate ", " (map display missingDeps)

showPlanProblem (PackageCycle cycleGroup) =
     "The following packages are involved in a dependency cycle "
  ++ intercalate ", " (map (display.packageId) cycleGroup)

showPlanProblem (PackageInconsistency name inconsistencies) =
     "Package " ++ display name
  ++ " is required by several packages,"
  ++ " but they require inconsistent versions:\n"
  ++ unlines [ "  package " ++ display pkg ++ " requires "
                            ++ display (PackageIdentifier name ver)
             | (pkg, ver) <- inconsistencies ]

showPlanProblem (PackageStateInvalid pkg pkg') =
     "Package " ++ display (packageId pkg)
  ++ " is in the " ++ showPlanState pkg
  ++ " state but it depends on package " ++ display (packageId pkg')
  ++ " which is in the " ++ showPlanState pkg'
  ++ " state"
  where
    showPlanState (PreExisting _) = "pre-existing"
    showPlanState (Configured  _)   = "configured"

-- | For an invalid plan, produce a detailed list of problems as human readable
-- error messages. This is mainly intended for debugging purposes.
-- Use 'showPlanProblem' for a human readable explanation.
--
problems :: IndependentGoals
         -> SolverPlanIndex
         -> [SolverPlanProblem]
problems indepGoals index =

     [ PackageMissingDeps pkg
       (mapMaybe
         (fmap packageId . flip Graph.lookup index)
         missingDeps)
     | (pkg, missingDeps) <- Graph.broken index ]

  ++ [ PackageCycle cycleGroup
     | cycleGroup <- Graph.cycles index ]

  ++ [ PackageInconsistency name inconsistencies
     | (name, inconsistencies) <-
       dependencyInconsistencies indepGoals index ]

  ++ [ PackageStateInvalid pkg pkg'
     | pkg <- Graph.toList index
     , Just pkg' <- map (flip Graph.lookup index)
                    (nodeNeighbors pkg)
     , not (stateDependencyRelation pkg pkg') ]


-- | Compute all roots of the install plan, and verify that the transitive
-- plans from those roots are all consistent.
--
-- NOTE: This does not check for dependency cycles. Moreover, dependency cycles
-- may be absent from the subplans even if the larger plan contains a dependency
-- cycle. Such cycles may or may not be an issue; either way, we don't check
-- for them here.
dependencyInconsistencies :: IndependentGoals
                          -> SolverPlanIndex
                          -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies indepGoals index  =
    concatMap dependencyInconsistencies' subplans
  where
    subplans :: [SolverPlanIndex]
    subplans = -- Not Graph.closure!!
               map (nonSetupClosure index)
                   (rootSets indepGoals index)

-- NB: When we check for inconsistencies, packages from the setup
-- scripts don't count as part of the closure (this way, we
-- can build, e.g., Cabal-1.24.1 even if its setup script is
-- built with Cabal-1.24.0).
--
-- This is a best effort function that swallows any non-existent
-- SolverIds.
nonSetupClosure :: SolverPlanIndex
                -> [SolverId]
                -> SolverPlanIndex
nonSetupClosure index pkgids0 = closure Graph.empty pkgids0
 where
    closure completed []             = completed
    closure completed (pkgid:pkgids) =
      case Graph.lookup pkgid index of
        Nothing   -> closure completed pkgids
        Just pkg  ->
          case Graph.lookup (nodeKey pkg) completed of
            Just _  -> closure completed  pkgids
            Nothing -> closure completed' pkgids'
              where completed' = Graph.insert pkg completed
                    pkgids'    = CD.nonSetupDeps (resolverPackageLibDeps pkg) ++ pkgids

-- | Compute the root sets of a plan
--
-- A root set is a set of packages whose dependency closure must be consistent.
-- This is the set of all top-level library roots (taken together normally, or
-- as singletons sets if we are considering them as independent goals), along
-- with all setup dependencies of all packages.
rootSets :: IndependentGoals -> SolverPlanIndex -> [[SolverId]]
rootSets (IndependentGoals indepGoals) index =
       if indepGoals then map (:[]) libRoots else [libRoots]
    ++ setupRoots index
  where
    libRoots = libraryRoots index

-- | Compute the library roots of a plan
--
-- The library roots are the set of packages with no reverse dependencies
-- (no reverse library dependencies but also no reverse setup dependencies).
libraryRoots :: SolverPlanIndex -> [SolverId]
libraryRoots index =
    map (nodeKey . toPkgId) roots
  where
    (graph, toPkgId, _) = Graph.toGraph index
    indegree = OldGraph.indegree graph
    roots    = filter isRoot (OldGraph.vertices graph)
    isRoot v = indegree ! v == 0

-- | The setup dependencies of each package in the plan
setupRoots :: SolverPlanIndex -> [[SolverId]]
setupRoots = filter (not . null)
           . map (CD.setupDeps . resolverPackageLibDeps)
           . Graph.toList

-- | Given a package index where we assume we want to use all the packages
-- (use 'dependencyClosure' if you need to get such a index subset) find out
-- if the dependencies within it use consistent versions of each package.
-- Return all cases where multiple packages depend on different versions of
-- some other package.
--
-- Each element in the result is a package name along with the packages that
-- depend on it and the versions they require. These are guaranteed to be
-- distinct.
--
dependencyInconsistencies' :: SolverPlanIndex
                           -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies' index =
    [ (name, [ (pid, packageVersion dep) | (dep,pids) <- uses, pid <- pids])
    | (name, ipid_map) <- Map.toList inverseIndex
    , let uses = Map.elems ipid_map
    , reallyIsInconsistent (map fst uses)
    ]
  where
    -- For each package name (of a dependency, somewhere)
    --   and each installed ID of that that package
    --     the associated package instance
    --     and a list of reverse dependencies (as source IDs)
    inverseIndex :: Map PackageName (Map SolverId (SolverPlanPackage, [PackageId]))
    inverseIndex = Map.fromListWith (Map.unionWith (\(a,b) (_,b') -> (a,b++b')))
      [ (packageName dep, Map.fromList [(sid,(dep,[packageId pkg]))])
      | -- For each package @pkg@
        pkg <- Graph.toList index
        -- Find out which @sid@ @pkg@ depends on
      , sid <- CD.nonSetupDeps (resolverPackageLibDeps pkg)
        -- And look up those @sid@ (i.e., @sid@ is the ID of @dep@)
      , Just dep <- [Graph.lookup sid index]
      ]

    -- If, in a single install plan, we depend on more than one version of a
    -- package, then this is ONLY okay in the (rather special) case that we
    -- depend on precisely two versions of that package, and one of them
    -- depends on the other. This is necessary for example for the base where
    -- we have base-3 depending on base-4.
    reallyIsInconsistent :: [SolverPlanPackage] -> Bool
    reallyIsInconsistent []       = False
    reallyIsInconsistent [_p]     = False
    reallyIsInconsistent [p1, p2] =
      let pid1 = nodeKey p1
          pid2 = nodeKey p2
      in pid1 `notElem` CD.nonSetupDeps (resolverPackageLibDeps p2)
      && pid2 `notElem` CD.nonSetupDeps (resolverPackageLibDeps p1)
    reallyIsInconsistent _ = True


-- | The graph of packages (nodes) and dependencies (edges) must be acyclic.
--
-- * if the result is @False@ use 'PackageIndex.dependencyCycles' to find out
--   which packages are involved in dependency cycles.
--
acyclic :: SolverPlanIndex -> Bool
acyclic = null . Graph.cycles

-- | An installation plan is closed if for every package in the set, all of
-- its dependencies are also in the set. That is, the set is closed under the
-- dependency relation.
--
-- * if the result is @False@ use 'PackageIndex.brokenPackages' to find out
--   which packages depend on packages not in the index.
--
closed :: SolverPlanIndex -> Bool
closed = null . Graph.broken

-- | An installation plan is consistent if all dependencies that target a
-- single package name, target the same version.
--
-- This is slightly subtle. It is not the same as requiring that there be at
-- most one version of any package in the set. It only requires that of
-- packages which have more than one other package depending on them. We could
-- actually make the condition even more precise and say that different
-- versions are OK so long as they are not both in the transitive closure of
-- any other package (or equivalently that their inverse closures do not
-- intersect). The point is we do not want to have any packages depending
-- directly or indirectly on two different versions of the same package. The
-- current definition is just a safe approximation of that.
--
-- * if the result is @False@ use 'PackageIndex.dependencyInconsistencies' to
--   find out which packages are.
--
consistent :: SolverPlanIndex -> Bool
consistent = null . dependencyInconsistencies (IndependentGoals False)

-- | The states of packages have that depend on each other must respect
-- this relation. That is for very case where package @a@ depends on
-- package @b@ we require that @dependencyStatesOk a b = True@.
--
stateDependencyRelation :: SolverPlanPackage
                        -> SolverPlanPackage
                        -> Bool
stateDependencyRelation PreExisting{}   PreExisting{}     = True

stateDependencyRelation (Configured  _) PreExisting{}     = True
stateDependencyRelation (Configured  _) (Configured  _)   = True

stateDependencyRelation _               _                 = False


-- | Compute the dependency closure of a package in a install plan
--
dependencyClosure :: SolverInstallPlan
                  -> [SolverId]
                  -> [SolverPlanPackage]
dependencyClosure plan = fromMaybe [] . Graph.closure (planIndex plan)


reverseDependencyClosure :: SolverInstallPlan
                         -> [SolverId]
                         -> [SolverPlanPackage]
reverseDependencyClosure plan = fromMaybe [] . Graph.revClosure (planIndex plan)


topologicalOrder :: SolverInstallPlan
                 -> [SolverPlanPackage]
topologicalOrder plan = Graph.topSort (planIndex plan)


reverseTopologicalOrder :: SolverInstallPlan
                        -> [SolverPlanPackage]
reverseTopologicalOrder plan = Graph.revTopSort (planIndex plan)

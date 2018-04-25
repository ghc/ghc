{-# LANGUAGE DeriveDataTypeable #-}

-- | Types for the "Distribution.Client.ProjectBuilding"
--
-- Moved out to avoid module cycles.
--
module Distribution.Client.ProjectBuilding.Types (
    -- * Pre-build status
    BuildStatusMap,
    BuildStatus(..),
    buildStatusRequiresBuild,
    buildStatusToString,
    BuildStatusRebuild(..),
    BuildReason(..),
    MonitorChangedReason(..),

    -- * Build outcomes
    BuildOutcomes,
    BuildOutcome,
    BuildResult(..),
    BuildFailure(..),
    BuildFailureReason(..),
  ) where

import Distribution.Client.Types          (DocsResult, TestsResult)
import Distribution.Client.FileMonitor    (MonitorChangedReason(..))

import Distribution.Package               (UnitId, PackageId)
import Distribution.InstalledPackageInfo  (InstalledPackageInfo)
import Distribution.Simple.LocalBuildInfo (ComponentName)

import Data.Map          (Map)
import Data.Set          (Set)
import Data.Typeable     (Typeable)
import Control.Exception (Exception, SomeException)


------------------------------------------------------------------------------
-- Pre-build status: result of the dry run
--

-- | The 'BuildStatus' of every package in the 'ElaboratedInstallPlan'.
--
-- This is used as the result of the dry-run of building an install plan.
--
type BuildStatusMap = Map UnitId BuildStatus

-- | The build status for an individual package is the state that the
-- package is in /prior/ to initiating a (re)build.
--
-- This should not be confused with a 'BuildResult' which is the result
-- /after/ successfully building a package.
--
-- It serves two purposes:
--
--  * For dry-run output, it lets us explain to the user if and why a package
--    is going to be (re)built.
--
--  * It tell us what step to start or resume building from, and carries
--    enough information for us to be able to do so.
--
data BuildStatus =

     -- | The package is in the 'InstallPlan.PreExisting' state, so does not
     --   need building.
     BuildStatusPreExisting

     -- | The package is in the 'InstallPlan.Installed' state, so does not
     --   need building.
   | BuildStatusInstalled

     -- | The package has not been downloaded yet, so it will have to be
     --   downloaded, unpacked and built.
   | BuildStatusDownload

     -- | The package has not been unpacked yet, so it will have to be
     --   unpacked and built.
   | BuildStatusUnpack FilePath

     -- | The package exists in a local dir already, and just needs building
     --   or rebuilding. So this can only happen for 'BuildInplaceOnly' style
     --   packages.
   | BuildStatusRebuild FilePath BuildStatusRebuild

     -- | The package exists in a local dir already, and is fully up to date.
     --   So this package can be put into the 'InstallPlan.Installed' state
     --   and it does not need to be built.
   | BuildStatusUpToDate BuildResult


-- | Which 'BuildStatus' values indicate we'll have to do some build work of
-- some sort. In particular we use this as part of checking if any of a
-- package's deps have changed.
--
buildStatusRequiresBuild :: BuildStatus -> Bool
buildStatusRequiresBuild BuildStatusPreExisting = False
buildStatusRequiresBuild BuildStatusInstalled   = False
buildStatusRequiresBuild BuildStatusUpToDate {} = False
buildStatusRequiresBuild _                      = True

-- | This is primarily here for debugging. It's not actually used anywhere.
--
buildStatusToString :: BuildStatus -> String
buildStatusToString BuildStatusPreExisting    = "BuildStatusPreExisting"
buildStatusToString BuildStatusInstalled      = "BuildStatusInstalled"
buildStatusToString BuildStatusDownload       = "BuildStatusDownload"
buildStatusToString (BuildStatusUnpack fp)    = "BuildStatusUnpack " ++ show fp
buildStatusToString (BuildStatusRebuild fp _) = "BuildStatusRebuild " ++ show fp
buildStatusToString (BuildStatusUpToDate _)   = "BuildStatusUpToDate"


-- | For a package that is going to be built or rebuilt, the state it's in now.
--
-- So again, this tells us why a package needs to be rebuilt and what build
-- phases need to be run. The 'MonitorChangedReason' gives us details like
-- which file changed, which is mainly for high verbosity debug output.
--
data BuildStatusRebuild =

     -- | The package configuration changed, so the configure and build phases
     --   needs to be (re)run.
     BuildStatusConfigure (MonitorChangedReason ())

     -- | The configuration has not changed but the build phase needs to be
     -- rerun. We record the reason the (re)build is needed.
     --
     -- The optional registration info here tells us if we've registered the
     -- package already, or if we still need to do that after building.
     -- @Just Nothing@ indicates that we know that no registration is
     -- necessary (e.g., executable.)
     --
   | BuildStatusBuild (Maybe (Maybe InstalledPackageInfo)) BuildReason

data BuildReason =
     -- | The dependencies of this package have been (re)built so the build
     -- phase needs to be rerun.
     --
     BuildReasonDepsRebuilt

     -- | Changes in files within the package (or first run or corrupt cache)
   | BuildReasonFilesChanged (MonitorChangedReason ())

     -- | An important special case is that no files have changed but the
     -- set of components the /user asked to build/ has changed. We track the
     -- set of components /we have built/, which of course only grows (until
     -- some other change resets it).
     --
     -- The @Set 'ComponentName'@ is the set of components we have built
     -- previously. When we update the monitor we take the union of the ones
     -- we have built previously with the ones the user has asked for this
     -- time and save those. See 'updatePackageBuildFileMonitor'.
     --
   | BuildReasonExtraTargets (Set ComponentName)

     -- | Although we're not going to build any additional targets as a whole,
     -- we're going to build some part of a component or run a repl or any
     -- other action that does not result in additional persistent artifacts.
     --
   | BuildReasonEphemeralTargets


------------------------------------------------------------------------------
-- Build outcomes: result of the build
--

-- | A summary of the outcome for building a whole set of packages.
--
type BuildOutcomes = Map UnitId BuildOutcome

-- | A summary of the outcome for building a single package: either success
-- or failure.
--
type BuildOutcome  = Either BuildFailure BuildResult

-- | Information arising from successfully building a single package.
--
data BuildResult = BuildResult {
       buildResultDocs    :: DocsResult,
       buildResultTests   :: TestsResult,
       buildResultLogFile :: Maybe FilePath
     }
  deriving Show

-- | Information arising from the failure to build a single package.
--
data BuildFailure = BuildFailure {
       buildFailureLogFile :: Maybe FilePath,
       buildFailureReason  :: BuildFailureReason
     }
  deriving (Show, Typeable)

instance Exception BuildFailure

-- | Detail on the reason that a package failed to build.
--
data BuildFailureReason = DependentFailed PackageId
                        | DownloadFailed  SomeException
                        | UnpackFailed    SomeException
                        | ConfigureFailed SomeException
                        | BuildFailed     SomeException
                        | ReplFailed      SomeException
                        | HaddocksFailed  SomeException
                        | TestsFailed     SomeException
                        | BenchFailed     SomeException
                        | InstallFailed   SomeException
  deriving Show


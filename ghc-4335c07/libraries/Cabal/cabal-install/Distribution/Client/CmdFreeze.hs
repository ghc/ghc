{-# LANGUAGE CPP, NamedFieldPuns, RecordWildCards, ViewPatterns #-}

-- | cabal-install CLI command: freeze
--
module Distribution.Client.CmdFreeze (
    freezeCommand,
    freezeAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), ProjectConfigShared(..)
         , writeProjectLocalFreezeConfig )
import Distribution.Client.Targets
         ( UserQualifier(..), UserConstraintScope(..), UserConstraint(..) )
import Distribution.Solver.Types.PackageConstraint
         ( PackageProperty(..) )
import Distribution.Solver.Types.ConstraintSource
         ( ConstraintSource(..) )
import Distribution.Client.DistDirLayout
         ( DistDirLayout(distProjectFile) )
import qualified Distribution.Client.InstallPlan as InstallPlan


import Distribution.Package
         ( PackageName, packageName, packageVersion )
import Distribution.Version
         ( VersionRange, thisVersion
         , unionVersionRanges, simplifyVersionRange )
import Distribution.PackageDescription
         ( FlagAssignment, nullFlagAssignment )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags
         , applyFlagDefaults )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( die', notice, wrapText )
import Distribution.Verbosity
         ( normal )

import Data.Monoid as Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (unless)

import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import qualified Distribution.Client.Setup as Client


freezeCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
freezeCommand = Client.installCommand {
  commandName         = "new-freeze",
  commandSynopsis     = "Freeze dependencies.",
  commandUsage        = usageAlternatives "new-freeze" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "The project configuration is frozen so that it will be reproducible "
     ++ "in future.\n\n"

     ++ "The precise dependency configuration for the project is written to "
     ++ "the 'cabal.project.freeze' file (or '$project_file.freeze' if "
     ++ "'--project-file' is specified). This file extends the configuration "
     ++ "from the 'cabal.project' file and thus is used as the project "
     ++ "configuration for all other commands (such as 'new-build', "
     ++ "'new-repl' etc).\n\n"

     ++ "The freeze file can be kept in source control. To make small "
     ++ "adjustments it may be edited manually, or to make bigger changes "
     ++ "you may wish to delete the file and re-freeze. For more control, "
     ++ "one approach is to try variations using 'new-build --dry-run' with "
     ++ "solver flags such as '--constraint=\"pkg < 1.2\"' and once you have "
     ++ "a satisfactory solution to freeze it using the 'new-freeze' command "
     ++ "with the same set of flags.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-freeze\n"
     ++ "    Freeze the configuration of the current project\n\n"
     ++ "  " ++ pname ++ " new-build --dry-run --constraint=\"aeson < 1\"\n"
     ++ "    Check what a solution with the given constraints would look like\n"
     ++ "  " ++ pname ++ " new-freeze --constraint=\"aeson < 1\"\n"
     ++ "    Freeze a solution using the given constraints\n\n"

     ++ "Note: this command is part of the new project-based system (aka "
     ++ "nix-style\nlocal builds). These features are currently in beta. "
     ++ "Please see\n"
     ++ "http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html "
     ++ "for\ndetails and advice on what you can expect to work. If you "
     ++ "encounter problems\nplease file issues at "
     ++ "https://github.com/haskell/cabal/issues and if you\nhave any time "
     ++ "to get involved and help with testing, fixing bugs etc then\nthat "
     ++ "is very much appreciated.\n"
   }

-- | To a first approximation, the @freeze@ command runs the first phase of
-- the @build@ command where we bring the install plan up to date, and then
-- based on the install plan we write out a @cabal.project.freeze@ config file.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
freezeAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
             -> [String] -> GlobalFlags -> IO ()
freezeAction (applyFlagDefaults -> (configFlags, configExFlags, installFlags, haddockFlags))
             extraArgs globalFlags = do

    unless (null extraArgs) $
      die' verbosity $ "'freeze' doesn't take any extra arguments: "
         ++ unwords extraArgs

    ProjectBaseContext {
      distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages
    } <- establishProjectBaseContext verbosity cliConfig

    (_, elaboratedPlan, _) <-
      rebuildInstallPlan verbosity
                         distDirLayout cabalDirLayout
                         projectConfig
                         localPackages

    let freezeConfig = projectFreezeConfig elaboratedPlan
    writeProjectLocalFreezeConfig distDirLayout freezeConfig
    notice verbosity $
      "Wrote freeze file: " ++ distProjectFile distDirLayout "freeze"

  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags



-- | Given the install plan, produce a config value with constraints that
-- freezes the versions of packages used in the plan.
--
projectFreezeConfig :: ElaboratedInstallPlan -> ProjectConfig
projectFreezeConfig elaboratedPlan =
    Monoid.mempty {
      projectConfigShared = Monoid.mempty {
        projectConfigConstraints =
          concat (Map.elems (projectFreezeConstraints elaboratedPlan))
      }
    }

-- | Given the install plan, produce solver constraints that will ensure the
-- solver picks the same solution again in future in different environments.
--
projectFreezeConstraints :: ElaboratedInstallPlan
                         -> Map PackageName [(UserConstraint, ConstraintSource)]
projectFreezeConstraints plan =
    --
    -- TODO: [required eventually] this is currently an underapproximation
    -- since the constraints language is not expressive enough to specify the
    -- precise solution. See https://github.com/haskell/cabal/issues/3502.
    --
    -- For the moment we deal with multiple versions in the solution by using
    -- constraints that allow either version. Also, we do not include any
    -- /version/ constraints for packages that are local to the project (e.g.
    -- if the solution has two instances of Cabal, one from the local project
    -- and one pulled in as a setup deps then we exclude all constraints on
    -- Cabal, not just the constraint for the local instance since any
    -- constraint would apply to both instances). We do however keep flag
    -- constraints of local packages.
    --
    deleteLocalPackagesVersionConstraints
      (Map.unionWith (++) versionConstraints flagConstraints)
  where
    versionConstraints :: Map PackageName [(UserConstraint, ConstraintSource)]
    versionConstraints =
      Map.mapWithKey
        (\p v -> [(UserConstraint (UserAnyQualifier p) (PackagePropertyVersion v),
                   ConstraintSourceFreeze)])
        versionRanges

    versionRanges :: Map PackageName VersionRange
    versionRanges =
      Map.map simplifyVersionRange $
      Map.fromListWith unionVersionRanges $
          [ (packageName pkg, thisVersion (packageVersion pkg))
          | InstallPlan.PreExisting pkg <- InstallPlan.toList plan
          ]
       ++ [ (packageName pkg, thisVersion (packageVersion pkg))
          | InstallPlan.Configured pkg <- InstallPlan.toList plan
          ]

    flagConstraints :: Map PackageName [(UserConstraint, ConstraintSource)]
    flagConstraints =
      Map.mapWithKey
        (\p f -> [(UserConstraint (UserQualified UserQualToplevel p) (PackagePropertyFlags f),
                   ConstraintSourceFreeze)])
        flagAssignments

    flagAssignments :: Map PackageName FlagAssignment
    flagAssignments =
      Map.fromList
        [ (pkgname, flags)
        | InstallPlan.Configured elab <- InstallPlan.toList plan
        , let flags   = elabFlagAssignment elab
              pkgname = packageName elab
        , not (nullFlagAssignment flags) ]

    -- As described above, remove the version constraints on local packages,
    -- but leave any flag constraints.
    deleteLocalPackagesVersionConstraints
      :: Map PackageName [(UserConstraint, ConstraintSource)]
      -> Map PackageName [(UserConstraint, ConstraintSource)]
    deleteLocalPackagesVersionConstraints =
#if MIN_VERSION_containers(0,5,0)
      Map.mergeWithKey
        (\_pkgname () constraints ->
            case filter (not . isVersionConstraint . fst) constraints of
              []           -> Nothing
              constraints' -> Just constraints')
        (const Map.empty) id
        localPackages
#else
      Map.mapMaybeWithKey
        (\pkgname constraints ->
            if pkgname `Map.member` localPackages
              then case filter (not . isVersionConstraint . fst) constraints of
                     []           -> Nothing
                     constraints' -> Just constraints'
              else Just constraints)
#endif

    isVersionConstraint (UserConstraint _ (PackagePropertyVersion _)) = True
    isVersionConstraint _                                             = False

    localPackages :: Map PackageName ()
    localPackages =
      Map.fromList
        [ (packageName elab, ())
        | InstallPlan.Configured elab <- InstallPlan.toList plan
        , elabLocalToProject elab
        ]


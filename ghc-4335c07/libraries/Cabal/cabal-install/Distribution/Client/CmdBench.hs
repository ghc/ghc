{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}

-- | cabal-install CLI command: bench
--
module Distribution.Client.CmdBench (
    -- * The @bench@ CLI and action
    benchCommand,
    benchAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags
         , applyFlagDefaults )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText, die' )

import Control.Monad (when)


benchCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
benchCommand = Client.installCommand {
  commandName         = "new-bench",
  commandSynopsis     = "Run benchmarks",
  commandUsage        = usageAlternatives "new-bench" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Runs the specified benchmarks, first ensuring they are up to "
     ++ "date.\n\n"

     ++ "Any benchmark in any package in the project can be specified. "
     ++ "A package can be specified in which case all the benchmarks in the "
     ++ "package are run. The default is to run all the benchmarks in the "
     ++ "package in the current directory.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-bench\n"
     ++ "    Run all the benchmarks in the package in the current directory\n"
     ++ "  " ++ pname ++ " new-bench pkgname\n"
     ++ "    Run all the benchmarks in the package named pkgname\n"
     ++ "  " ++ pname ++ " new-bench cname\n"
     ++ "    Run the benchmark named cname\n"
     ++ "  " ++ pname ++ " new-bench cname -O2\n"
     ++ "    Run the benchmark built with '-O2' (including local libs used)\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
benchAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
            -> [String] -> GlobalFlags -> IO ()
benchAction (applyFlagDefaults -> (configFlags, configExFlags, installFlags, haddockFlags))
            targetStrings globalFlags = do

    baseCtx <- establishProjectBaseContext verbosity cliConfig

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                   =<< readTargetSelectors (localPackages baseCtx) targetStrings

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx)) $
              die' verbosity $
                  "The bench command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'bench'."

            -- Interpret the targets on the command line as bench targets
            -- (as opposed to say build or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         targetSelectors

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBench
                                    targets
                                    elaboratedPlan
            return (elaboratedPlan', targets)

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @bench@ command we select all buildable benchmarks,
-- or fail if there are no benchmarks or no buildable benchmarks.
--
selectPackageTargets :: TargetSelector PackageId
                     -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there are any buildable benchmark targets then we select those
  | not (null targetsBenchBuildable)
  = Right targetsBenchBuildable

    -- If there are benchmarks but none are buildable then we report those
  | not (null targetsBench)
  = Left (TargetProblemNoneEnabled targetSelector targetsBench)

    -- If there are no benchmarks but some other targets then we report that
  | not (null targets)
  = Left (TargetProblemNoBenchmarks targetSelector)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targetsBenchBuildable = selectBuildableTargets
                          . filterTargetsKind BenchKind
                          $ targets

    targetsBench          = forgetTargetsDetail
                          . filterTargetsKind BenchKind
                          $ targets


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @bench@ command we just need to check it is a benchmark, in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: PackageId -> ComponentName -> SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget pkgid cname subtarget@WholeComponent t
  | CBenchName _ <- availableTargetComponentName t
  = either (Left . TargetProblemCommon) return $
           selectComponentTargetBasic pkgid cname subtarget t
  | otherwise
  = Left (TargetProblemComponentNotBenchmark pkgid cname)

selectComponentTarget pkgid cname subtarget _
  = Left (TargetProblemIsSubComponent pkgid cname subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @bench@ command.
--
data TargetProblem =
     TargetProblemCommon        TargetProblemCommon

     -- | The 'TargetSelector' matches benchmarks but none are buildable
   | TargetProblemNoneEnabled  (TargetSelector PackageId) [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets    (TargetSelector PackageId)

     -- | The 'TargetSelector' matches targets but no benchmarks
   | TargetProblemNoBenchmarks (TargetSelector PackageId)

     -- | The 'TargetSelector' refers to a component that is not a benchmark
   | TargetProblemComponentNotBenchmark PackageId ComponentName

     -- | Asking to benchmark an individual file or module is not supported
   | TargetProblemIsSubComponent   PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "run" problem

renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "benchmark" targetSelector targets

renderTargetProblem (TargetProblemNoBenchmarks targetSelector) =
    "Cannot run benchmarks for the target '" ++ showTargetSelector targetSelector
 ++ "' which refers to " ++ renderTargetSelector targetSelector
 ++ " because "
 ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
 ++ " not contain any benchmarks."

renderTargetProblem (TargetProblemNoTargets targetSelector) =
    case targetSelectorFilter targetSelector of
      Just kind | kind /= BenchKind
        -> "The bench command is for running benchmarks, but the target '"
           ++ showTargetSelector targetSelector ++ "' refers to "
           ++ renderTargetSelector targetSelector ++ "."

      _ -> renderTargetProblemNoTargets "benchmark" targetSelector

renderTargetProblem (TargetProblemComponentNotBenchmark pkgid cname) =
    "The bench command is for running benchmarks, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " from the package "
 ++ display pkgid ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent

renderTargetProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The bench command can only run benchmarks as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget

module Options (Group(..), groups) where

import Types

import Options.CodeGen
import Options.CompilerDebugging
import Options.Cpp
import Options.FindingImports
import Options.Interactive
import Options.InterfaceFiles
import Options.KeepingIntermediates
import Options.Language
import Options.Linking
import Options.Misc
import Options.Modes
import Options.Optimizations
import Options.OptimizationLevels
import Options.Packages
import Options.Phases
import Options.PhasePrograms
import Options.PhaseSpecific
import Options.PlatformSpecific
import Options.Plugin
import Options.Profiling
import Options.ProgramCoverage
import Options.RecompilationChecking
import Options.RedirectingOutput
import Options.TemporaryFiles
import Options.Verbosity
import Options.Warnings

-- | A group of flags
data Group = Group { grpName  :: String  -- ^ Internal name
                   , grpTitle :: String  -- ^ Human-readable title
                   , grpFlags :: [Flag]  -- ^ Flags in group
                   }

groups :: [Group]
groups =
  [ Group "codegen" "Code generation" codegenOptions
  , Group "compiler-debugging" "Debugging the compiler" compilerDebuggingOptions
  , Group "cpp" "C pre-processor" cppOptions
  , Group "finding-imports" "Finding imports" findingImportsOptions
  , Group "interactive" "Interactive mode" interactiveOptions
  , Group "interface-files" "Interface files" interfaceFilesOptions
  , Group "keeping-intermediates" "Keeping intermediate files" keepingIntermediatesOptions
  , Group "language" "Language options" languageOptions
  , Group "linking" "Linking options" linkingOptions
  , Group "misc" "Miscellaneous options" miscOptions
  , Group "modes" "Modes of operation" modeOptions
  , Group "optimization" "Individual optimizations " optimizationsOptions
  , Group "optimization-levels" "Optimization levels" optimizationLevelsOptions
  , Group "packages" "Package options" packagesOptions
  , Group "phases" "Phases of compilation" phaseOptions
  , Group "phase-programs" "Overriding external programs" phaseProgramsOptions
  , Group "phase-specific" "Phase-specific options" phaseSpecificOptions
  , Group "platform-specific" "Platform-specific options" platformSpecificOptions
  , Group "plugin" "Compiler plugins" pluginOptions
  , Group "profiling" "Profiling" profilingOptions
  , Group "program-coverage" "Program coverage" programCoverageOptions
  , Group "recompilation-checking" "Recompilation checking" recompilationCheckingOptions
  , Group "redirecting-output" "Redirecting output" redirectingOutputOptions
  , Group "temporary-files" "Temporary files" temporaryFilesOptions
  , Group "verbosity" "Verbosity options" verbosityOptions
  , Group "warnings" "Warnings" warningsOptions
  ]

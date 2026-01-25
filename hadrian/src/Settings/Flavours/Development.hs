module Settings.Flavours.Development (developmentFlavour) where

import Expression
import Flavour
import Packages
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
developmentFlavour :: Stage -> Flavour
developmentFlavour ghcStage = disableDynamicLibs $ disableProfiledLibs $ defaultFlavour
    { name = "devel" ++ stageString ghcStage
    , extraArgs = developmentArgs ghcStage <> defaultHaddockExtraArgs
    , ghcDebugAssertions = (== ghcStage) }
    where
      stageString Stage2 = "2"
      stageString Stage1 = "1"
      stageString Stage3 = "3"
      stageString s = error ("developmentFlavour not supported for " ++ show s)

developmentArgs :: Stage -> Args
developmentArgs ghcStage =
    sourceArgs SourceArgs
        { hsDefault  = mconcat [ pure ["-O", "+RTS", "-O64M", "-RTS"],
                                 -- Disable optimization when building Cabal;
                                 -- this saves many minutes of build time.
                                 package cabal ? pure ["-O0"]]
        , hsLibrary  = notStage0 ? arg "-dlint"
        , hsCompiler = mconcat [stage0 ? arg "-O2",
                                buildingCompilerStage ghcStage ? pure ["-O0"]]
        , hsGhc      = buildingCompilerStage ghcStage ? pure ["-O0"] }

module Settings.Flavours.Development (developmentFlavour) where

import qualified Data.Set as Set

import Expression
import Flavour
import Oracles.Flag
import Packages
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
developmentFlavour :: Stage -> Flavour
developmentFlavour ghcStage = defaultFlavour
    { name = "devel" ++ stageString ghcStage
    , extraArgs = developmentArgs ghcStage <> defaultHaddockExtraArgs
    , libraryWays = pure $ Set.fromList [vanilla]
    , rtsWays = Set.fromList <$> mconcat [pure [vanilla, debug], staged targetSupportsThreadedRts ? pure [threaded, threadedDebug]]
    , dynamicGhcPrograms = const (return False)
    , ghcDebugAssertions = (== ghcStage) }
    where
      stageString Stage2 = "2"
      stageString Stage1 = "1"
      stageString Stage3 = "3"
      stageString s = error ("developmentFlavour not supported for " ++ show s)

developmentArgs :: Stage -> Args
developmentArgs ghcStage =
    sourceArgs SourceArgs
        { hsDefault  = mconcat [ pure ["-O", "-H64m"],
                                 -- Disable optimization when building Cabal;
                                 -- this saves many minutes of build time.
                                 package cabal ? pure ["-O0"]]
        , hsLibrary  = notStage0 ? arg "-dlint"
        , hsCompiler = mconcat [stage0 ? arg "-O2",
                                buildingCompilerStage ghcStage ? pure ["-O0"]]
        , hsGhc      = buildingCompilerStage ghcStage ? pure ["-O0"] }

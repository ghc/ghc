module Settings.Flavours.Development (developmentFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default
import Settings.Optimisation

-- TODO: Implement an equivalent of LAX_DEPENDENCIES = YES setting, see #250.
developmentFlavour :: Stage -> Flavour
developmentFlavour ghcStage = defaultFlavour
    { name = "devel" ++ show (fromEnum ghcStage)
    , args = defaultBuilderArgs <> developmentArgs ghcStage <> defaultPackageArgs
    , libraryWays = append [vanilla] }

developmentArgs :: Stage -> Args
developmentArgs ghcStage = do
    stage <- getStage
    optimisationArgs $ Optimisation
        { hsDefault  = append ["-O", "-H64m"]
        , hsLibrary  = notStage0 ? arg "-dcore-lint"
        , hsCompiler = succ stage == ghcStage ? append ["-O0", "-DDEBUG"]
        , hsGhc      = succ stage == ghcStage ? append ["-O0", "-DDEBUG"] }

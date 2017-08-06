module Settings.Flavours.Development (developmentFlavour) where

import Flavour
import Expression
import {-# SOURCE #-} Settings.Default

-- TODO: Implement an equivalent of LAX_DEPENDENCIES = YES setting, see #250.
developmentFlavour :: Stage -> Flavour
developmentFlavour ghcStage = defaultFlavour
    { name = "devel" ++ show (fromEnum ghcStage)
    , args = defaultBuilderArgs <> developmentArgs ghcStage <> defaultPackageArgs }

developmentArgs :: Stage -> Args
developmentArgs ghcStage = do
    stage <- getStage
    sourceArgs $ SourceArgs
        { hsDefault  = append ["-O", "-H32m"]
        , hsLibrary  = notStage0 ? arg "-dcore-lint"
        , hsCompiler = succ stage == ghcStage ? append ["-O0", "-DDEBUG"]
        , hsGhc      = succ stage == ghcStage ? append ["-O0", "-DDEBUG"] }

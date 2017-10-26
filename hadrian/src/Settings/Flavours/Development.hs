module Settings.Flavours.Development (developmentFlavour) where

import Flavour
import Expression
import {-# SOURCE #-} Settings.Default

developmentFlavour :: Stage -> Flavour
developmentFlavour ghcStage = defaultFlavour
    { name = "devel" ++ show (fromEnum ghcStage)
    , args = defaultBuilderArgs <> developmentArgs ghcStage <> defaultPackageArgs }

developmentArgs :: Stage -> Args
developmentArgs ghcStage = do
    stage <- getStage
    sourceArgs $ SourceArgs
        { hsDefault  = pure ["-O", "-H64m"]
        , hsLibrary  = notStage0 ? arg "-dcore-lint"
        , hsCompiler = succ stage == ghcStage ? pure ["-O0", "-DDEBUG"]
        , hsGhc      = succ stage == ghcStage ? pure ["-O0", "-DDEBUG"] }

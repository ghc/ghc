module Settings.Flavours.Development (developmentFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
developmentFlavour :: Stage -> Flavour
developmentFlavour ghcStage = defaultFlavour
    { name = "devel" ++ show (fromEnum ghcStage)
    , args = defaultBuilderArgs <> developmentArgs ghcStage <> defaultPackageArgs }

developmentArgs :: Stage -> Args
developmentArgs ghcStage = do
    stage <- getStage
    sourceArgs SourceArgs
        { hsDefault  = pure ["-O", "-H64m"]
        , hsLibrary  = notStage0 ? arg "-dcore-lint"
        , hsCompiler = succ stage == ghcStage ? pure ["-O0", "-DDEBUG"]
        , hsGhc      = succ stage == ghcStage ? pure ["-O0", "-DDEBUG"] }

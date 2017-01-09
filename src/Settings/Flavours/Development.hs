module Settings.Flavours.Development (developmentFlavour) where

import Flavour
import GHC
import Predicate
import {-# SOURCE #-} Settings.Default

-- TODO: Implement an equivalent of LAX_DEPENDENCIES = YES setting, see #250.
developmentFlavour :: Stage -> Flavour
developmentFlavour ghcStage = defaultFlavour
    { name        = "devel" ++ show (fromEnum ghcStage)
    , args        = developmentArgs ghcStage
    , libraryWays = append [vanilla] }

developmentArgs :: Stage -> Args
developmentArgs ghcStage = do
    stage <- getStage
    pkg   <- getPackage
    let now = succ stage == ghcStage
    mconcat [ defaultBuilderArgs
            , builder Ghc ? mconcat
              [ append ["-O", "-H64m"]
              , now ? pkg == compiler ? append ["-O0", "-DDEBUG", "-dcore-lint"]
              , now ? pkg == ghc      ? append ["-O0", "-DDEBUG"]
              , notStage0 ? isLibrary pkg ? arg "-dcore-lint" ]
            , defaultPackageArgs ]

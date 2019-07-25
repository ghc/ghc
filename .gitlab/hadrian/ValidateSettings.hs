-- Ensure we don't expose any unfoldings to guarantee quick rebuilds
{-# OPTIONS_GHC -O0 #-}

-- | This UserSettings module is used for validating GHC in CI.
module UserSettings (
    userFlavours, userPackages, userDefaultFlavour,
    verboseCommand, buildProgressColour, successColour, finalStage
    ) where

import Flavour
import Expression
import {-# SOURCE #-} Settings.Default

userDefaultFlavour :: String
userDefaultFlavour = "validate"

userFlavours :: [Flavour]
userFlavours = [validateFlavour]

validateFlavour :: Flavour
validateFlavour = werror $ defaultFlavour { name = "validate" }

verboseCommand :: Predicate
verboseCommand = do
    verbosity <- expr getVerbosity
    return $ verbosity >= Loud

buildProgressColour :: BuildProgressColour
buildProgressColour = mkBuildProgressColour (Dull Magenta)

successColour :: SuccessColour
successColour = mkSuccessColour (Dull Green)

finalStage :: Stage
finalStage = Stage2

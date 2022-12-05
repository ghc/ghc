module Settings.Flavours.Release where

import Settings.Flavours.Performance
import Flavour

releaseFlavour :: Flavour
releaseFlavour = splitSections $ enableHaddock performanceFlavour { name = "release" }

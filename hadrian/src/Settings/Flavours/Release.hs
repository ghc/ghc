module Settings.Flavours.Release where

import Settings.Flavours.Performance
import Flavour

releaseFlavour :: Flavour
releaseFlavour = enableLateCCS $ enableHaddock performanceFlavour { name = "release" }

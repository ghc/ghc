module Settings.Flavours.Release where

import Settings.Flavours.Performance
import Flavour

releaseFlavour :: Flavour
releaseFlavour = enableHaddock performanceFlavour { name = "release" }

releaseJsFlavour :: Flavour
releaseJsFlavour = disableDynamicLibs
                   . disableDynamicGhcPrograms
                   . disableProfiledLibs
                   . enableO2Stage0
                   . useNativeBignum
                   $ performanceFlavour { name = "release-js" }

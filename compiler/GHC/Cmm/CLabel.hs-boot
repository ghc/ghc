module GHC.Cmm.CLabel where

import GHC.Utils.Outputable
import GHC.Platform

data CLabel

pprCLabel :: IsLine doc => Platform -> CLabel -> doc

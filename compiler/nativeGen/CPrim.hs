-- | Generating C symbol names emitted by the compiler.
module CPrim (popCntLabel) where

import CmmType
import Outputable

popCntLabel :: Width -> String
popCntLabel w = "hs_popcnt" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "popCntLabel: Unsupported word width " (ppr w)

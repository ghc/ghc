module GHC.StgToCmm.Foreign where

import GHC.Platform.Profile.Class
import GHC.Cmm
import GHC.StgToCmm.Monad

emitPrimCall :: ContainsPlatformProfile c
             => [CmmFormal] -> CallishMachOp -> [CmmActual] -> FCode' c ()

module GHC.StgToCmm.Foreign where

import GHC.Cmm
import GHC.StgToCmm.Monad

emitPrimCall :: [CmmFormal] -> CallishMachOp -> [CmmActual] -> FCode ()

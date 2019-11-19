module GHC.StgToCmm.Bind where

import GHC.StgToCmm.Monad( FCode )
import StgSyn( CgStgBinding )

cgBind :: CgStgBinding -> FCode ()

module GHC.StgToCmm.Bind where

import GHC.StgToCmm.Monad( FCode )
import GHC.Stg.Syntax( CgStgBinding )

cgBind :: CgStgBinding -> FCode ()

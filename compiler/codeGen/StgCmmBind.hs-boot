module StgCmmBind where

import StgCmmMonad( FCode )
import StgSyn( CgStgBinding )

cgBind :: CgStgBinding -> FCode ()

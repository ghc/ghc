module StgCmmBind where

import StgCmmMonad( FCode )
import StgSyn( StgBinding )

cgBind :: StgBinding -> FCode ()

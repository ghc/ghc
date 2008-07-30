
module Exception
    (
#if __GLASGOW_HASKELL__ >= 609
    module Control.OldException
#else
    module Control.Exception
#endif
    )
    where

import Prelude ()

#if __GLASGOW_HASKELL__ >= 609
import Control.OldException
#else
import Control.Exception
#endif


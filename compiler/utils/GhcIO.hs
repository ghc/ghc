
-- | A simple version compatability wrapper around GHC.IO.
--
-- This module exports both the safe and Unsafe version of GHC.IO
-- taking care of the SafeHaskell changeover which spit up the
-- old GHC.IO module.
--
module GhcIO (
#if __GLASGOW_HASKELL__ >= 701
        module GHC.IO.Unsafe,
#endif
        module GHC.IO
    ) where

#if __GLASGOW_HASKELL__ >= 701
import GHC.IO.Unsafe
#endif

import GHC.IO


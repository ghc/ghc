
-- | A simple version compatability wrapper around GHC.IO.
--
-- This module exports both the safe and Unsafe version of GHC.IO
-- taking care of the SafeHaskell changeover which spit up the
-- old GHC.IO module.
--
module GhcIO (
#if __GLASGOW_HASKELL__ >= 701
        module GHC.IO.Safe,
        module GHC.IO.Unsafe
#else
        module GHC.IO
#endif
    ) where

#if __GLASGOW_HASKELL__ >= 701
import GHC.IO.Safe
import GHC.IO.Unsafe
#else
import GHC.IO
#endif



{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module GhcExts (
    build,
) where

import GHC.Exts (build)

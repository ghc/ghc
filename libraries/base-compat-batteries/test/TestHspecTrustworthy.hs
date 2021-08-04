{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Reexports "Test.Hspec" from a @Trustworthy@ module.
module TestHspecTrustworthy (module Test.Hspec) where

import Test.Hspec

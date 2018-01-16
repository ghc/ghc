-- | This module re-exports the non-exposed
-- "Distribution.Compat.Prelude" module for
-- reuse by @cabal-install@'s
-- "Distribution.Client.Compat.Prelude" module.
--
-- It is highly discouraged to rely on this module
-- for @Setup.hs@ scripts since its API is /not/
-- stable.
module Distribution.Compat.Prelude.Internal
    {-# WARNING "This modules' API is not stable. Use at your own risk, or better yet, use @base-compat@!" #-}
    ( module Distribution.Compat.Prelude
    ) where

import Distribution.Compat.Prelude

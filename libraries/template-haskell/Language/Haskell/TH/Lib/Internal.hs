{-# LANGUAGE Safe #-}
-- |
-- Language.Haskell.TH.Lib.Internal exposes some additional functionality that
-- is used internally in GHC's integration with Template Haskell. This is not a
-- part of the public API, and as such, there are no API guarantees for this
-- module from version to version.

-- Why do we have both GHC.Boot.TH.Lib and
-- Language.Haskell.TH.Lib? Ultimately, it's because the functions in the
-- former (which are tailored for GHC's use) need different type signatures
-- than the ones in the latter. Syncing up the Internal type signatures would
-- involve a massive amount of breaking changes, so for the time being, we
-- relegate as many changes as we can to just the Internal module, where it
-- is safe to break things.

module Language.Haskell.TH.Lib.Internal
  ( module GHC.Boot.TH.Lib )
  where

import GHC.Boot.TH.Lib

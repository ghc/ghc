{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.Fingerprint.Type
-- Copyright   :  (c) The University of Glasgow, 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.

module GHC.Fingerprint.Type
    (Fingerprint(..)
     ) where

import GHC.Internal.Fingerprint.Type
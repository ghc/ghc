{-# LANGUAGE Safe #-}

-- |
-- Module      :  GHC.IO.Device
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable
--
-- Type classes for I/O providers.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.IO.Device should be removed in GHC 10.02."
#endif

module GHC.IO.Device (
  {-# DEPRECATED ["GHC.IO.Device is deprecated and will be removed in GHC 10.02. Please use the ghc-internal package."] #-}
        RawIO(..),
        IODevice(..),
        IODeviceType(..),
        SeekMode(..)
    ) where

import GHC.Internal.IO.Device

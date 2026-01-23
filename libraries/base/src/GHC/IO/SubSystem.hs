{-# LANGUAGE CPP #-}

{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.SubSystem
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/392>)
-- Portability :  non-portable
--
-- The 'IoSubSystem' control interface.  These methods can be used to disambiguate
-- between the two operations.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.IO.SubSystem should be removed in GHC 10.02."
#endif

module GHC.IO.SubSystem
  {-# DEPRECATED "GHC.IO.SubSystem is deprecated and will be removed in GHC 10.02. See https://github.com/well-typed/reinstallable-base/tree/main/hackage-uses-of-internals/stability-risk-3 for context." #-}
    (withIoSubSystem,
     withIoSubSystem',
     whenIoSubSystem,
     ioSubSystem,
     IoSubSystem(..),
     conditional,
     (<!>),
     isWindowsNativeIO
     ) where

import GHC.Internal.IO.SubSystem
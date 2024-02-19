{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.SubSystem
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
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

module GHC.IO.SubSystem
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
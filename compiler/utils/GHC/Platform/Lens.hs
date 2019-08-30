module GHC.Platform.Lens
  ( HasPlatform (..)
  , HasPlatformMisc (..)
  ) where

import GhcPrelude

import GHC.Platform

import Lens

class HasPlatform c where
  platform :: Lens' c Platform

instance HasPlatform Platform where
  platform = id

class HasPlatformMisc c where
  platformMisc :: Lens' c PlatformMisc

instance HasPlatformMisc PlatformMisc where
  platformMisc = id

module GHC.Platform.Lens
  ( HasPlatform (..)
  ) where

import GhcPrelude

import GHC.Platform

import Lens

class HasPlatform c where
  platform :: Lens' c Platform

instance HasPlatform Platform where
  platform = id

-- | Linker configuration

module GHC.Linker.Config
  ( FrameworkOpts(..)
  ) where

import GHC.Prelude

-- used on darwin only
data FrameworkOpts = FrameworkOpts
  { foFrameworkPaths    :: [String]
  , foCmdlineFrameworks :: [String]
  }

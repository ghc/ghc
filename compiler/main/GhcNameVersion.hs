module GhcNameVersion
  ( GhcNameVersion (..)
  ) where

import GhcPrelude

-- | Settings for what GHC this is.
data GhcNameVersion = GhcNameVersion
  { ghcNameVersion_programName    :: String
  , ghcNameVersion_projectVersion :: String
  }

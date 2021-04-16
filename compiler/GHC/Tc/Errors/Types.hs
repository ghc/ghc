module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  ) where

import GHC.Prelude
import GHC.Types.Error
import GHC.HsToCore.Errors.Types

-- | An error which might arise during typechecking/renaming.
data TcRnMessage
  = TcRnUnknownMessage !DiagnosticMessage
  -- ^ Simply rewraps a generic 'DiagnosticMessage'. More
  -- instances will be added in the future (#18516).

module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  ) where

import GHC.Types.Error

-- | An error which might arise during typechecking/renaming.
data TcRnMessage
  = TcRnUnknownMessage !DiagnosticMessage
  -- ^ Simply rewraps a generic 'DiagnosticMessage'. More
  -- instances will be added in the future (#18516).

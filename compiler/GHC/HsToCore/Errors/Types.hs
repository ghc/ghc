
module GHC.HsToCore.Errors.Types where

import GHC.Types.Error

-- | Diagnostics messages emitted during desugaring.
data DsMessage =
  DsUnknownMessage !DiagnosticMessage
  -- ^ Simply rewraps a generic 'DiagnosticMessage'. More
  -- constructors will be added in the future (#18516).

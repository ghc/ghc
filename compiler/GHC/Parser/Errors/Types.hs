
module GHC.Parser.Errors.Types where

import GHC.Types.Error

data PsMessage
  = PsUnknownMessage !DiagnosticMessage
  -- ^ Simply rewraps a generic 'DiagnosticMessage'. More
  -- constructors will be added in the future (#18516).

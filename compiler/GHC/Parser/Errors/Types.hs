
module GHC.Parser.Errors.Types where

import GHC.Types.Error

-- NOTE(adn): This is an opaque type where constructors will be added
-- in the context of #18516.
data PsMessage
  = PsUnknownMessage !DiagnosticMessage
  -- ^ Simply rewraps a generic 'DiagnosticMessage'. More
  -- instances will be added in the future (#18516).

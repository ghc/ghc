
module GHC.Parser.Errors.Types where

import GHC.Prelude ()
import GHC.Types.Error

-- NOTE(adn): This is an opaque type where constructors will be added
-- in the context of #18516.
-- FIX(adn) This is temporary. We shouldn't have separate types for
-- parsing warnings and errors.
data PsMessage
  = PsUnknownMessage DiagnosticMessage

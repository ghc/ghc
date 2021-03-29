
module GHC.HsToCore.Errors.Types where

import GHC.Prelude ()
import GHC.Tc.Errors.Types
import GHC.Types.Error (DiagnosticMessage)

-- NOTE(adn): This is an opaque type where constructors will be added
-- in the context of #18516.
data DsMessage =
    DsUnknownMessage !DiagnosticMessage
  | DsLiftedTcRnMessage !TcRnMessage
  -- ^ A diagnostic coming straight from the Typecheck-renamer.

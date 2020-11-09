
module GHC.Driver.Errors.Types where

import GHC.Types.Error (ErrDoc, RenderableDiagnostic)

data GhcError
ghcErrorRawErrDoc :: ErrDoc -> GhcError

instance RenderableDiagnostic GhcError

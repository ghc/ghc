
module GHC.Driver.Errors.Types where

import GHC.Types.Error (ErrDoc, RenderableError)

data GhcError
ghcErrorRawErrDoc :: ErrDoc -> GhcError

instance RenderableError GhcError

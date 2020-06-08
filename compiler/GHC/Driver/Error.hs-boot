module GHC.Driver.Error where

import GHC.Utils.Error

data GhcError
instance RenderableError GhcError
ghcErrorRawErrDoc :: ErrDoc -> GhcError
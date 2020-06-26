module GHC.Driver.Error where

import GHC.Parser.Lexer   (PsError(..))
import GHC.Tc.Utils.Monad (TcRnError(..), DsError)
import GHC.Utils.Error

data GhcError
  = GhcErrorPs PsError
  | GhcErrorTcRn TcRnError
  | GhcErrorDs DsError
  | GhcErrorRaw ErrDoc

ghcErrorRawErrDoc :: ErrDoc -> GhcError
ghcErrorRawErrDoc = GhcErrorRaw

ghcErrorFromWarn :: ErrDoc -> GhcError
ghcErrorFromWarn = GhcErrorRaw

instance RenderableError GhcError where
  renderError (GhcErrorPs e) = renderError e
  renderError (GhcErrorTcRn e) = renderError e
  renderError (GhcErrorDs e) = renderError e
  renderError (GhcErrorRaw d) = d

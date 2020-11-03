
module GHC.Driver.Errors.Types (
    GhcError(..)
  , DriverError(..)

  -- * Converting an ErrDoc into a GhcError in a lossy way
  , ghcErrorRawErrDoc
  ) where

import GHC.Driver.Session (DynFlags)
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Errors.Ppr ()
import GHC.Prelude (String)
import GHC.Tc.Errors.Types (DsError, TcRnError(..))
import GHC.Types.Error (ErrDoc, RenderableError, renderError, errDoc)
import GHC.Unit.Finder (FindResult, cannotFindModule)
import GHC.Unit.Module.Name (ModuleName)
import GHC.Unit.Types (UnitId, Module)
import GHC.Utils.Outputable
import qualified GHC.Parser.Errors as Parser

data GhcError
  = GhcErrorPs Parser.Error
  | GhcErrorTcRn TcRnError
  | GhcErrorDs DsError
  | GhcErrorDriver DriverError
  | GhcErrorRaw ErrDoc

ghcErrorRawErrDoc :: ErrDoc -> GhcError
ghcErrorRawErrDoc = GhcErrorRaw

data DriverError
  = DriverCannotFindModule DynFlags ModuleName FindResult
  | DriverNotAnExpression String
  | DriverParseErrorImport
  | DriverPkgRequiredTrusted UnitId
  | DriverCantLoadIfaceForSafe Module
  | DriverError ErrDoc

instance RenderableError GhcError where
  renderError (GhcErrorPs e)     = renderError e
  renderError (GhcErrorTcRn e)   = renderError e
  renderError (GhcErrorDs e)     = renderError e
  renderError (GhcErrorDriver e) = renderError e
  renderError (GhcErrorRaw d)    = d

instance RenderableError DriverError where
  renderError (DriverError d) = d
  renderError (DriverCannotFindModule dflags m res) =
    errDoc [cannotFindModule dflags m res] [] []
  renderError (DriverNotAnExpression str) =
    errDoc [text "not an expression:" <+> quotes (text str)] [] []
  renderError DriverParseErrorImport =
    errDoc [text "parse error in import declaration"] [] []
  renderError (DriverPkgRequiredTrusted pkg) =
    errDoc [ text "The package (" <> ppr pkg <> text ") is required" <>
             text " to be trusted but it isn't!" ] [] []
  renderError (DriverCantLoadIfaceForSafe m) =
    errDoc [ text "Can't load the interface file for" <+> ppr m
          <> text ", to check that it can be safely imported" ]
           [] []

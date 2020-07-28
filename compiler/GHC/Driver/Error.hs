module GHC.Driver.Error where

import GHC.Prelude
import GHC.Driver.Finder   (cannotFindModule)
import GHC.Driver.Types
import GHC.Parser.Error    (PsError(..))
import GHC.Tc.Errors.Types (TcRnError(..))
import GHC.Tc.Utils.Monad  (DsError)
import GHC.Unit.Module.Name
import GHC.Unit.Types
import GHC.Utils.Error
import GHC.Utils.Outputable

data GhcError
  = GhcErrorPs PsError
  | GhcErrorTcRn TcRnError
  | GhcErrorDs DsError
  | GhcErrorDriver DriverError
  | GhcErrorRaw ErrDoc

ghcErrorRawErrDoc :: ErrDoc -> GhcError
ghcErrorRawErrDoc = GhcErrorRaw

instance RenderableError GhcError where
  renderError dflags (GhcErrorPs e)     = renderError dflags e
  renderError dflags (GhcErrorTcRn e)   = renderError dflags e
  renderError dflags (GhcErrorDs e)     = renderError dflags e
  renderError dflags (GhcErrorDriver e) = renderError dflags e
  renderError _      (GhcErrorRaw d)    = d

data DriverError
  = DriverCannotFindModule ModuleName FindResult
  | DriverNotAnExpression String
  | DriverParseErrorImport
  | DriverPkgRequiredTrusted UnitId
  | DriverCantLoadIfaceForSafe Module
  | DriverError ErrDoc

instance RenderableError DriverError where
  renderError _dflags (DriverError d) = d
  renderError dflags  (DriverCannotFindModule m res) =
    errDoc [cannotFindModule dflags m res] [] []
  renderError _       (DriverNotAnExpression str) =
    errDoc [text "not an expression:" <+> quotes (text str)] [] []
  renderError _       DriverParseErrorImport =
    errDoc [text "parse error in import declaration"] [] []
  renderError _       (DriverPkgRequiredTrusted pkg) =
    errDoc [ text "The package (" <> ppr pkg <> text ") is required" <>
             text " to be trusted but it isn't!" ] [] []
  renderError _       (DriverCantLoadIfaceForSafe m) =
    errDoc [ text "Can't load the interface file for" <+> ppr m
          <> text ", to check that it can be safely imported" ]
           [] []

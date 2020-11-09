
module GHC.Driver.Errors.Types (
    GhcError(..)
  , GhcWarning(..)
  , DriverError(..)

  -- * Converting an ErrDoc into a GhcError in a lossy way
  , ghcErrorRawErrDoc
  ) where

import GHC.Driver.Session (DynFlags)
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Errors.Ppr ()
import GHC.Prelude (String)
import GHC.Tc.Errors.Types (DsError, TcRnError(..))
import GHC.Types.Error (ErrDoc, RenderableDiagnostic, renderDiagnostic, errDoc)
import GHC.Unit.Finder (FindResult, cannotFindModule)
import GHC.Unit.Module.Name (ModuleName)
import GHC.Unit.Types (UnitId, Module)
import GHC.Utils.Outputable
import qualified GHC.Parser.Errors as Parser

data GhcWarning
  = GhcWarningPs  Parser.Warning
  | GhcWarningRaw ErrDoc

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

instance RenderableDiagnostic GhcWarning where
  renderDiagnostic (GhcWarningPs w)  = renderDiagnostic w
  renderDiagnostic (GhcWarningRaw d) = d

instance RenderableDiagnostic GhcError where
  renderDiagnostic (GhcErrorPs e)     = renderDiagnostic e
  renderDiagnostic (GhcErrorTcRn e)   = renderDiagnostic e
  renderDiagnostic (GhcErrorDs e)     = renderDiagnostic e
  renderDiagnostic (GhcErrorDriver e) = renderDiagnostic e
  renderDiagnostic (GhcErrorRaw d)    = d

instance RenderableDiagnostic DriverError where
  renderDiagnostic (DriverError d) = d
  renderDiagnostic (DriverCannotFindModule dflags m res) =
    errDoc [cannotFindModule dflags m res] [] []
  renderDiagnostic (DriverNotAnExpression str) =
    errDoc [text "not an expression:" <+> quotes (text str)] [] []
  renderDiagnostic DriverParseErrorImport =
    errDoc [text "parse error in import declaration"] [] []
  renderDiagnostic (DriverPkgRequiredTrusted pkg) =
    errDoc [ text "The package (" <> ppr pkg <> text ") is required" <>
             text " to be trusted but it isn't!" ] [] []
  renderDiagnostic (DriverCantLoadIfaceForSafe m) =
    errDoc [ text "Can't load the interface file for" <+> ppr m
          <> text ", to check that it can be safely imported" ]
           [] []


{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Driver.Errors.Ppr () where

import GHC.Driver.Errors.Types
import GHC.Parser.Errors.Ppr ()
import GHC.Types.Error ( RenderableDiagnostic, renderDiagnostic, errDoc )
import GHC.Unit.Finder ( cannotFindModule )
import GHC.Utils.Outputable

instance RenderableDiagnostic GhcWarning where
  renderDiagnostic (GhcWarningPs w)      = renderDiagnostic w
  renderDiagnostic (GhcWarningCmdLine w) = renderDiagnostic w
  renderDiagnostic (GhcWarningRaw d)     = d

instance RenderableDiagnostic GhcError where
  renderDiagnostic (GhcErrorPs e)      = renderDiagnostic e
  renderDiagnostic (GhcErrorTcRn e)    = renderDiagnostic e
  renderDiagnostic (GhcErrorDs e)      = renderDiagnostic e
  renderDiagnostic (GhcErrorDriver e)  = renderDiagnostic e
  renderDiagnostic (GhcFatalWarning e) = renderDiagnostic e
  renderDiagnostic (GhcErrorRaw d)     = d

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

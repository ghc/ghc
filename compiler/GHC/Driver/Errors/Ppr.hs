{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Driver.Errors.Ppr where

import GHC.Types.Error
import GHC.Driver.Errors.Types

import GHC.Parser.Errors.Ppr ()
import GHC.Tc.Errors.Ppr ()
import GHC.HsToCore.Errors.Ppr ()

-- This is a totally uninteresting instance will will be populated in the context of #18516.
instance RenderableDiagnostic DriverMessage where
  renderDiagnostic _ = mkDecorated []

instance RenderableDiagnostic GhcMessage where
  renderDiagnostic = \case
    GhcPsMessage m
      -> renderDiagnostic m
    GhcTcRnMessage m
      -> renderDiagnostic m
    GhcDsMessage m
      -> renderDiagnostic m
    GhcDriverMessage m
      -> renderDiagnostic m
    GhcUnknownMessage m
      -> renderDiagnostic m

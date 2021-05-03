{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {DriverMessage, GhcMessage}
                                      -- instance RenderableMessage { DriverMessage, GhcMessage }

module GHC.Driver.Errors.Ppr where

import GHC.Prelude

import GHC.Driver.Errors.Types
import GHC.HsToCore.Errors.Ppr ()
import GHC.Parser.Errors.Ppr
import GHC.Tc.Errors.Ppr ()
import GHC.Types.Error

instance RenderableMessage GhcMessage where
  diagnosticMessage = \case
    GhcPsMessage m
      -> diagnosticMessage m
    GhcTcRnMessage m
      -> diagnosticMessage m
    GhcDsMessage m
      -> diagnosticMessage m
    GhcDriverMessage m
      -> diagnosticMessage m
    GhcUnknownMessage m
      -> diagnosticMessage m

instance Diagnostic GhcMessage where
  diagnosticReason = \case
    GhcPsMessage m
      -> diagnosticReason m
    GhcTcRnMessage m
      -> diagnosticReason m
    GhcDsMessage m
      -> diagnosticReason m
    GhcDriverMessage m
      -> diagnosticReason m
    GhcUnknownMessage m
      -> diagnosticReason m
  diagnosticHints = \case
    GhcPsMessage m
      -> diagnosticHints m
    GhcTcRnMessage m
      -> diagnosticHints m
    GhcDsMessage m
      -> diagnosticHints m
    GhcDriverMessage m
      -> diagnosticHints m
    GhcUnknownMessage m
      -> diagnosticHints m

instance RenderableMessage DriverMessage where
  diagnosticMessage (DriverUnknownMessage m)  = diagnosticMessage m
  diagnosticMessage (DriverPsHeaderMessage desc hints)
    = mkSimpleDecorated $ pprPsError desc hints

instance Diagnostic DriverMessage where
  diagnosticReason (DriverUnknownMessage m)   = diagnosticReason m
  diagnosticReason (DriverPsHeaderMessage {}) = ErrorWithoutFlag

  diagnosticHints  (DriverUnknownMessage m) = diagnosticHints m
  diagnosticHints (DriverPsHeaderMessage _ hints) = hints

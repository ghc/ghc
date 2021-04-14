{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {DriverMessage, GhcMessage}

module GHC.Driver.Errors.Ppr where

import GHC.Prelude

import GHC.Types.Error
import GHC.Driver.Errors.Types
import GHC.Parser.Errors.Ppr
import GHC.Tc.Errors.Ppr ()
import GHC.HsToCore.Errors.Ppr ()

instance Diagnostic GhcMessage where
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

instance Diagnostic DriverMessage where
  diagnosticMessage (DriverUnknownMessage m)  = diagnosticMessage m
  diagnosticMessage (DriverPsHeaderMessage desc hints)
    = mkSimpleDecorated $ pprPsError desc hints

  diagnosticReason (DriverUnknownMessage m)   = diagnosticReason m
  diagnosticReason (DriverPsHeaderMessage {}) = ErrorWithoutFlag

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Types.Error
import GHC.HsToCore.Errors.Types

instance Diagnostic DsMessage where
  diagnosticMessage (DsUnknownMessage m) = diagnosticMessage m
  diagnosticReason  (DsUnknownMessage m) = diagnosticReason m
  diagnosticHints   (DsUnknownMessage m) = diagnosticHints m

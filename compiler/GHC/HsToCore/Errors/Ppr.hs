{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance {Diagnostic, RenderableMessage} DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Types.Error
import GHC.HsToCore.Errors.Types

instance RenderableMessage DsMessage where
  diagnosticMessage (DsUnknownMessage m) = diagnosticMessage m

instance Diagnostic DsMessage where
  diagnosticReason  (DsUnknownMessage m) = diagnosticReason m
  diagnosticHints   (DsUnknownMessage m) = diagnosticHints m

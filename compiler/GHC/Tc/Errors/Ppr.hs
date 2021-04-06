{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {TcRnDsMessage, TcRnMessage}

module GHC.Tc.Errors.Ppr where

import GHC.Prelude

import GHC.Tc.Errors.Types
import GHC.Types.Error
import GHC.HsToCore.Errors.Ppr () -- instance Diagnostic DsMessage

instance Diagnostic TcRnDsMessage where
  diagnosticMessage (TcRnDsMessage m) = either diagnosticMessage diagnosticMessage m
  diagnosticReason  (TcRnDsMessage m) = either diagnosticReason  diagnosticReason m

instance Diagnostic TcRnMessage where
  diagnosticMessage (TcRnUnknownMessage m) = diagnosticMessage m
  diagnosticReason  (TcRnUnknownMessage m) = diagnosticReason m

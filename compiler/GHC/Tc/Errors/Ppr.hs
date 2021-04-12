{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {TcRnDsMessage, TcRnMessage}

module GHC.Tc.Errors.Ppr where

import GHC.Prelude

import GHC.Tc.Errors.Types
import GHC.Types.Error
import GHC.HsToCore.Errors.Ppr () -- instance Diagnostic DsMessage
import GHC.Utils.Outputable
import GHC.Driver.Flags

instance Diagnostic TcRnDsMessage where
  diagnosticMessage (TcRnDsMessage m) = either diagnosticMessage diagnosticMessage m
  diagnosticReason  (TcRnDsMessage m) = either diagnosticReason  diagnosticReason m

instance Diagnostic TcRnMessage where
  diagnosticMessage = \case
    TcRnUnknownMessage m
      -> diagnosticMessage m
    TcRnImplicitLift id_or_name errInfo
      -> mkDecorated [text "The variable" <+> quotes (ppr id_or_name) <+>
                      text "is implicitly lifted in the TH quotation"
                     , getErrInfo errInfo
                     ]
    TcRnUnusedPatternBinds bind
      -> mkDecorated [hang (text "This pattern-binding binds no variables:") 2 (ppr bind)]
  diagnosticReason = \case
    TcRnUnknownMessage m
      -> diagnosticReason m
    TcRnImplicitLift{}
      -> WarningWithFlag Opt_WarnImplicitLift
    TcRnUnusedPatternBinds{}
      -> WarningWithFlag Opt_WarnUnusedPatternBinds

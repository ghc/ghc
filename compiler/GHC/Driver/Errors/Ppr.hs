{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {DriverMessage, GhcMessage}

module GHC.Driver.Errors.Ppr where

import GHC.Prelude

import Data.Typeable
import GHC.Driver.Errors.Types
import GHC.HsToCore.Errors.Ppr ()
import GHC.HsToCore.Errors.Types
import GHC.Parser.Errors.Ppr
import GHC.Parser.Errors.Types
import GHC.Tc.Errors.Ppr ()
import GHC.Tc.Errors.Types
import GHC.Types.Error

instance Diagnostic GhcMessage where
  data Hint GhcMessage where
    GhcPsHint      :: Hint PsMessage     -> Hint GhcMessage
    GhcTcRnHint    :: Hint TcRnMessage   -> Hint GhcMessage
    GhcDsHint      :: Hint DsMessage     -> Hint GhcMessage
    GhcDriverHint  :: Hint DriverMessage -> Hint GhcMessage
    GhcUnknownHint :: forall a. (Diagnostic a, Typeable a) => Hint a -> Hint GhcMessage
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
  diagnosticHints = \case
    GhcPsMessage m
      -> GhcPsHint <$> diagnosticHints m
    GhcTcRnMessage m
      -> GhcTcRnHint <$> diagnosticHints m
    GhcDsMessage m
      -> GhcDsHint <$> diagnosticHints m
    GhcDriverMessage m
      -> GhcDriverHint <$> diagnosticHints m
    GhcUnknownMessage m
      -> GhcUnknownHint <$> diagnosticHints m

instance Diagnostic DriverMessage where
  data Hint DriverMessage = DriverUnknownHint DecoratedSDoc
                          | DriverPsHeaderHint (Hint PsMessage)
  diagnosticMessage (DriverUnknownMessage m)  = diagnosticMessage m
  diagnosticMessage (DriverPsHeaderMessage desc hints)
    = mkSimpleDecorated $ pprPsError desc hints

  diagnosticReason (DriverUnknownMessage m)   = diagnosticReason m
  diagnosticReason (DriverPsHeaderMessage {}) = ErrorWithoutFlag

  diagnosticHints  (DriverUnknownMessage m) = rewrapHints DriverUnknownHint (diagnosticHints m)
  diagnosticHints (DriverPsHeaderMessage _ ps_hints) = DriverPsHeaderHint <$> ps_hints

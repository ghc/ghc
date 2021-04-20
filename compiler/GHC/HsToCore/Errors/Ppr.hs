{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Types.Error
import GHC.HsToCore.Errors.Types

instance Diagnostic DsMessage where
  newtype Hint DsMessage = DsUnknownHint DecoratedSDoc -- Hints will be added as part of #18516.
  diagnosticMessage (DsUnknownMessage m) = diagnosticMessage m
  diagnosticReason  (DsUnknownMessage m) = diagnosticReason m
  diagnosticHints   (DsUnknownMessage m) = rewrapHints DsUnknownHint (diagnosticHints m)

{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage

module GHC.Tc.Errors.Ppr where

import GHC.Tc.Errors.Types
import GHC.Types.Error

instance RenderableMessage TcRnMessage where
  diagnosticMessage (TcRnUnknownMessage m) = diagnosticMessage m

instance Diagnostic TcRnMessage where
  diagnosticReason  (TcRnUnknownMessage m) = diagnosticReason m
  diagnosticHints   (TcRnUnknownMessage m) = diagnosticHints m

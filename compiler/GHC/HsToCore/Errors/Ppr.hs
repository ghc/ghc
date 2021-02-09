
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- instance RenderableDiagnostic DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Types.Error
import GHC.HsToCore.Errors.Types

-- This is a totally uninteresting instance will will be populated in the context of #18516.
instance Diagnostic DsMessage where
  diagnosticMessage  = \case {}
  diagnosticReason _ = ErrorWithoutFlag

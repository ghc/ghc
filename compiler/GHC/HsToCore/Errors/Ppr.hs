
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- instance Diagnostic DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Types.Error
import GHC.HsToCore.Errors.Types

-- This instance will be made a bit more interesting in the context of #18516.
instance Diagnostic DsMessage where
  diagnosticMessage  = \case
    DsUnknownMessage d    -> diagnosticMessage d
  diagnosticReason = \case
    DsUnknownMessage d    -> diagnosticReason d

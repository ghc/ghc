
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- instance RenderableDiagnostic DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Types.Error
import GHC.HsToCore.Errors.Types
import GHC.Tc.Errors.Ppr () -- instance Diagnostic TcRnMessage

-- This is a totally uninteresting instance will will be populated in the context of #18516.
instance Diagnostic DsMessage where
  diagnosticMessage  = \case
    DsUnknownMessage d    -> diagnosticMessage d
    DsLiftedTcRnMessage d -> diagnosticMessage d
  diagnosticReason = \case
    DsUnknownMessage d    -> diagnosticReason d
    DsLiftedTcRnMessage d -> diagnosticReason d

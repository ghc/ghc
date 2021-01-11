
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.HsToCore.Errors.Ppr where

import GHC.Types.Error
import GHC.HsToCore.Errors.Types

-- This is a totally uninteresting instance will will be populated in the context of #18516.
instance RenderableDiagnostic DsMessage where
  renderDiagnostic _ = mkDecorated []

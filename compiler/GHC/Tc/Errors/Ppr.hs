
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Tc.Errors.Ppr where

import GHC.Types.Error
import GHC.Tc.Errors.Types

-- This is a totally uninteresting instance will will be populated in the context of #18516.
instance RenderableDiagnostic TcRnMessage where
  renderDiagnostic _ = ErrDoc [] [] []

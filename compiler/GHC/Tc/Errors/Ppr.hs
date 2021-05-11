{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage

module GHC.Tc.Errors.Ppr where

import GHC.Core.TyCo.Ppr (pprWithTYPE)
import GHC.Core.Type
import GHC.Tc.Errors.Types
import GHC.Types.Error
import GHC.Types.Var.Env
import GHC.Utils.Outputable

instance Diagnostic TcRnMessage where
  diagnosticMessage (TcRnUnknownMessage m) = diagnosticMessage m
  diagnosticReason  (TcRnUnknownMessage m) = diagnosticReason m
  diagnosticHints   (TcRnUnknownMessage m) = diagnosticHints m


formatLevPolyErr :: Type  -- levity-polymorphic type
                 -> SDoc
formatLevPolyErr ty
  = hang (text "A levity-polymorphic type is not allowed here:")
       2 (vcat [ text "Type:" <+> pprWithTYPE tidy_ty
               , text "Kind:" <+> pprWithTYPE tidy_ki ])
  where
    (tidy_env, tidy_ty) = tidyOpenType emptyTidyEnv ty
    tidy_ki             = tidyType tidy_env (tcTypeKind ty)


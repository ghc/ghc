{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Tc.Errors.Ppr () where

import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Rep
import GHC.Core.Type ( isTyVarTy )
import GHC.Prelude
import GHC.Tc.Errors.Types as TcRn
import GHC.Types.Error
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Utils.Outputable

instance RenderableDiagnostic TcRn.Error where
  renderDiagnostic = pprError

instance RenderableDiagnostic TcRn.Warning where
  renderDiagnostic = pprWarning

pprWarning :: TcRn.Warning -> ErrDoc
pprWarning = \case
  WarnTcRnRaw e ->
    e

pprError :: TcRn.Error -> ErrDoc
pprError = \case
  ErrTcRnRaw d ->
    d

  ErrBadTelescope telescope sorted_tvs context ->
    errDoc [m] [context] []
      where m = hang (text "These kind and type variables:" <+> sep (map ppr telescope)
                   $$ text "are out of dependency order. Perhaps try this ordering:")
                2 (pprTyVars sorted_tvs)

  ErrOutOfScope tried_rdr_name suggs contextlines extra ->
    errDoc [m $$ suggestions $$ extra] [] [contextlines]
      where m = hang (text "Not in scope:")
                   2 (what <+> quotes (ppr tried_rdr_name))
            what = pprNonVarNameSpace (occNameSpace (rdrNameOcc tried_rdr_name))
            suggestions = pprOutOfScopeSuggestions (rdrNameOcc tried_rdr_name) suggs

  ErrOutOfScopeHole occ ty suggs ->
    errDoc [m] [] [suggestions]
      where herald | isDataOcc occ = text "Data constructor not in scope:"
                   | otherwise     = text "Variable not in scope:"
            m | isTyVarTy ty = hang herald 2 (ppr occ)
              | otherwise    = hang herald 2 (pp_occ_with_type occ ty)

            pp_occ_with_type :: OccName -> Type -> SDoc
            pp_occ_with_type occ hole_ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType hole_ty)

            suggestions = pprOutOfScopeSuggestions occ suggs

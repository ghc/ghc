{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Tc.Errors.Ppr (
    notInScopeErr
  , exactNameErr
  ) where

import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Rep
import GHC.Core.Type ( isTyVarTy )
import GHC.Data.FastString ( sLit )
import GHC.Prelude
import GHC.Tc.Errors.Types as TcRn
import GHC.Types.Error
import GHC.Types.Name ( Name )
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

notInScopeErr :: RdrName -> SDoc
notInScopeErr rdr_name
  = case isExact_maybe rdr_name of
      Just name -> exactNameErr name
      Nothing -> hang (text "Not in scope:")
                  2 (what <+> quotes (ppr rdr_name))
  where
    what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))

{- Note [When to show/hide the module-not-imported line]           -- #15611
For the error message:
    Not in scope X.Y
    Module X does not export Y
    No module named ‘X’ is imported:
there are 2 cases, where we hide the last "no module is imported" line:
1. If the module X has been imported.
2. If the module X is the current module. There are 2 subcases:
   2.1 If the unknown module name is in a input source file,
       then we can use the getModule function to get the current module name.
       (See test T15611a)
   2.2 If the unknown module name has been entered by the user in GHCi,
       then the getModule function returns something like "interactive:Ghci1",
       and we have to check the current module in the last added entry of
       the HomePackageTable. (See test T15611b)
-}

exactNameErr :: Name -> SDoc
exactNameErr name =
  hang (text "The exact Name" <+> quotes (ppr name) <+> ptext (sLit "is not in scope"))
    2 (vcat [ text "Probable cause: you used a unique Template Haskell name (NameU), "
            , text "perhaps via newName, but did not bind it"
            , text "If that's it, then -ddump-splices might be useful" ])

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
    errDoc [notInScopeErr tried_rdr_name $$ suggestions $$ extra] [] [contextlines]
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

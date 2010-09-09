
module Vectorise.Utils.PRDict (
	prDFunOfTyCon,
	prDictOfType,
	prDictOfTyApp,
	prDFunApply,
	wrapPR
)
where
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Utils.PADict

import CoreSyn
import Type
import TypeRep
import TyCon
import Outputable
import Control.Monad


prDFunOfTyCon :: TyCon -> VM CoreExpr
prDFunOfTyCon tycon
  = liftM Var
  . maybeCantVectoriseM "No PR dictionary for tycon" (ppr tycon)
  $ lookupTyConPR tycon



prDictOfType :: Type -> VM CoreExpr
prDictOfType ty = prDictOfTyApp ty_fn ty_args
  where
    (ty_fn, ty_args) = splitAppTys ty

prDictOfTyApp :: Type -> [Type] -> VM CoreExpr
prDictOfTyApp ty_fn ty_args
  | Just ty_fn' <- coreView ty_fn = prDictOfTyApp ty_fn' ty_args
prDictOfTyApp (TyConApp tc _) ty_args
  = do
      dfun <- liftM Var $ maybeV (lookupTyConPR tc)
      prDFunApply dfun ty_args
prDictOfTyApp _ _ = noV

prDFunApply :: CoreExpr -> [Type] -> VM CoreExpr
prDFunApply dfun tys
  = do
      dicts <- mapM prDictOfType tys
      return $ mkApps (mkTyApps dfun tys) dicts

wrapPR :: Type -> VM CoreExpr
wrapPR ty
  = do
      Just  pa_dict <- paDictOfType ty
      pr_dfun       <- prDFunOfTyCon =<< builtin wrapTyCon
      return $ mkApps pr_dfun [Type ty, pa_dict]

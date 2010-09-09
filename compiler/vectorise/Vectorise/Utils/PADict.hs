
module Vectorise.Utils.PADict (
	mkPADictType,
	paDictArgType,
	paDictOfType,
	paDFunType,
	paDFunApply,
	paMethod	
)
where
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Utils.Base

import CoreSyn
import Type
import TypeRep
import TyCon
import Var
import Outputable
import FastString
import Control.Monad


mkPADictType :: Type -> VM Type
mkPADictType ty = mkBuiltinTyConApp paTyCon [ty]


paDictArgType :: TyVar -> VM (Maybe Type)
paDictArgType tv = go (TyVarTy tv) (tyVarKind tv)
  where
    go ty k | Just k' <- kindView k = go ty k'
    go ty (FunTy k1 k2)
      = do
          tv   <- newTyVar (fsLit "a") k1
          mty1 <- go (TyVarTy tv) k1
          case mty1 of
            Just ty1 -> do
                          mty2 <- go (AppTy ty (TyVarTy tv)) k2
                          return $ fmap (ForAllTy tv . FunTy ty1) mty2
            Nothing  -> go ty k2

    go ty k
      | isLiftedTypeKind k
      = liftM Just (mkPADictType ty)

    go _ _ = return Nothing


-- | Get the PA dictionary for some type, or `Nothing` if there isn't one.
paDictOfType :: Type -> VM (Maybe CoreExpr)
paDictOfType ty 
  = paDictOfTyApp ty_fn ty_args
  where
    (ty_fn, ty_args) = splitAppTys ty

    paDictOfTyApp :: Type -> [Type] -> VM (Maybe CoreExpr)
    paDictOfTyApp ty_fn ty_args
        | Just ty_fn' <- coreView ty_fn 
        = paDictOfTyApp ty_fn' ty_args

    paDictOfTyApp (TyVarTy tv) ty_args
     = do dfun <- maybeV (lookupTyVarPA tv)
          liftM Just $ paDFunApply dfun ty_args

    paDictOfTyApp (TyConApp tc _) ty_args
     = do mdfun <- lookupTyConPA tc
          case mdfun of
	    Nothing	
	     -> pprTrace "VectUtils.paDictOfType"
	                 (vcat [ text "No PA dictionary"
			       , text "for tycon: " <> ppr tc
			       , text "in type:   " <> ppr ty])
	     $ return Nothing

	    Just dfun	-> liftM Just $ paDFunApply (Var dfun) ty_args

    paDictOfTyApp ty _
     = cantVectorise "Can't construct PA dictionary for type" (ppr ty)



paDFunType :: TyCon -> VM Type
paDFunType tc
  = do
      margs <- mapM paDictArgType tvs
      res   <- mkPADictType (mkTyConApp tc arg_tys)
      return . mkForAllTys tvs
             $ mkFunTys [arg | Just arg <- margs] res
  where
    tvs = tyConTyVars tc
    arg_tys = mkTyVarTys tvs

paDFunApply :: CoreExpr -> [Type] -> VM CoreExpr
paDFunApply dfun tys
 = do Just dicts <- liftM sequence $ mapM paDictOfType tys
      return $ mkApps (mkTyApps dfun tys) dicts


paMethod :: (Builtins -> Var) -> String -> Type -> VM CoreExpr
paMethod _ name ty
  | Just tycon <- splitPrimTyCon ty
  = liftM Var
  . maybeCantVectoriseM "No PA method" (text name <+> text "for" <+> ppr tycon)
  $ lookupPrimMethod tycon name

paMethod method _ ty
  = do
      fn        <- builtin method
      Just dict <- paDictOfType ty
      return $ mkApps (Var fn) [Type ty, dict]


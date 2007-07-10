module Vectorise( vectorise )
where

#include "HsVersions.h"

import VectMonad

import DynFlags
import HscTypes

import CoreLint             ( showPass, endPass )
import CoreSyn
import CoreUtils
import CoreFVs
import TyCon
import Type
import TypeRep
import Var
import VarEnv
import Name                 ( mkSysTvName )
import NameEnv
import Id

import DsMonad hiding (mapAndUnzipM)

import PrelNames

import Outputable
import FastString
import Control.Monad        ( liftM, liftM2, mapAndUnzipM )

vectorise :: HscEnv -> ModGuts -> IO ModGuts
vectorise hsc_env guts
  | not (Opt_Vectorise `dopt` dflags) = return guts
  | otherwise
  = do
      showPass dflags "Vectorisation"
      eps <- hscEPS hsc_env
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps
      Just (info', guts') <- initV hsc_env guts info (vectModule guts)
      endPass dflags "Vectorisation" Opt_D_dump_vect (mg_binds guts')
      return $ guts' { mg_vect_info = info' }
  where
    dflags = hsc_dflags hsc_env

vectModule :: ModGuts -> VM ModGuts
vectModule guts = return guts

-- ----------------------------------------------------------------------------
-- Bindings

vectBndr :: Var -> VM (Var, Var)
vectBndr v
  = do
      vty <- vectType (idType v)
      lty <- mkPArrayTy vty
      let vv = v `Id.setIdType` vty
          lv = v `Id.setIdType` lty
      updLEnv (mapTo vv lv)
      return (vv, lv)
  where
    mapTo vv lv env = env { local_vars = extendVarEnv (local_vars env) v (Var vv, Var lv) }

vectBndrIn :: Var -> VM a -> VM (Var, Var, a)
vectBndrIn v p
  = localV
  $ do
      (vv, lv) <- vectBndr v
      x <- p
      return (vv, lv, x)

vectBndrsIn :: [Var] -> VM a -> VM ([Var], [Var], a)
vectBndrsIn vs p
  = localV
  $ do
      (vvs, lvs) <- mapAndUnzipM vectBndr vs
      x <- p
      return (vvs, lvs, x)

-- ----------------------------------------------------------------------------
-- Expressions

replicateP :: CoreExpr -> CoreExpr -> VM CoreExpr
replicateP expr len
  = do
      pa  <- paOfType ty
      rep <- builtin replicatePAVar
      return $ mkApps (Var rep) [Type ty, pa, expr, len]
  where
    ty = exprType expr

capply :: (CoreExpr, CoreExpr) -> (CoreExpr, CoreExpr) -> VM (CoreExpr, CoreExpr)
capply (vfn, lfn) (varg, larg)
  = do
      apply  <- builtin applyClosureVar
      applyP <- builtin applyClosurePVar
      return (mkApps (Var apply)  [Type arg_ty, Type res_ty, vfn, varg],
              mkApps (Var applyP) [Type arg_ty, Type res_ty, lfn, larg])
  where
    fn_ty            = exprType vfn
    (arg_ty, res_ty) = splitClosureTy fn_ty

vectVar :: CoreExpr -> Var -> VM (CoreExpr, CoreExpr)
vectVar lc v = local v `orElseV` global v
  where
    local  v = maybeV (readLEnv $ \env -> lookupVarEnv (local_vars env) v)
    global v = do
                 vexpr <- maybeV (readGEnv $ \env -> lookupVarEnv (global_vars env) v)
                 lexpr <- replicateP vexpr lc
                 return (vexpr, lexpr)
                
vectExpr :: CoreExpr -> CoreExprWithFVs -> VM (CoreExpr, CoreExpr)
vectExpr lc (_, AnnType ty)
  = do
      vty <- vectType ty
      return (Type vty, Type vty)
vectExpr lc (_, AnnVar v)   = vectVar lc v
vectExpr lc (_, AnnLit lit)
  = do
      let vexpr = Lit lit
      lexpr <- replicateP vexpr lc
      return (vexpr, lexpr)
vectExpr lc (_, AnnNote note expr)
  = do
      (vexpr, lexpr) <- vectExpr lc expr
      return (Note note vexpr, Note note lexpr)
vectExpr lc (_, AnnApp fn arg)
  = do
      fn'  <- vectExpr lc fn
      arg' <- vectExpr lc arg
      capply fn' arg'
vectExpr lc (_, AnnCase expr bndr ty alts)
  = panic "vectExpr: case"
vectExpr lc (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
      (vrhs, lrhs) <- vectExpr lc rhs
      (vbndr, lbndr, (vbody, lbody)) <- vectBndrIn bndr (vectExpr lc body)
      return (Let (NonRec vbndr vrhs) vbody,
              Let (NonRec lbndr lrhs) lbody)
vectExpr lc (_, AnnLet (AnnRec prs) body)
  = do
      (vbndrs, lbndrs, (vrhss, vbody, lrhss, lbody)) <- vectBndrsIn bndrs vect
      return (Let (Rec (zip vbndrs vrhss)) vbody,
              Let (Rec (zip lbndrs lrhss)) lbody)
  where
    (bndrs, rhss) = unzip prs
    
    vect = do
             (vrhss, lrhss) <- mapAndUnzipM (vectExpr lc) rhss
             (vbody, lbody) <- vectExpr lc body
             return (vrhss, vbody, lrhss, lbody)
vectExpr lc (_, AnnLam bndr body)
  | isTyVar bndr
  = do
      pa_ty          <- paArgType' (TyVarTy bndr) (tyVarKind bndr)
      pa_var         <- newLocalVar FSLIT("dPA") pa_ty
      (vbody, lbody) <- localV
                      $ do
                          extendTyVarPA bndr (Var pa_var)
                          -- FIXME: what about shadowing here (bndr in lc)?
                          vectExpr lc body
      return (mkLams [bndr, pa_var] vbody,
              mkLams [bndr, pa_var] lbody)

-- ----------------------------------------------------------------------------
-- PA dictionaries

paArgType :: Type -> Kind -> VM (Maybe Type)
paArgType ty k
  | Just k' <- kindView k = paArgType ty k'

-- Here, we assume that for a kind (k1 -> k2) to be valid, k1 and k2 can only
-- be made up of * and (->), i.e., they can't be coercion kinds or #.
paArgType ty (FunTy k1 k2)
  = do
      tv  <- newTyVar FSLIT("a") k1
      ty1 <- paArgType' (TyVarTy tv) k1
      ty2 <- paArgType' (AppTy ty (TyVarTy tv)) k2
      return . Just $ ForAllTy tv (FunTy ty1 ty2)

paArgType ty k
  | isLiftedTypeKind k
  = do
      tc <- builtin paDictTyCon
      return . Just $ TyConApp tc [ty]

  | otherwise
  = return Nothing 

paArgType' :: Type -> Kind -> VM Type
paArgType' ty k
  = do
      r <- paArgType ty k
      case r of
        Just ty' -> return ty'
        Nothing  -> pprPanic "paArgType'" (ppr ty)

paOfTyCon :: TyCon -> VM CoreExpr
-- FIXME: just for now
paOfTyCon tc = maybeV (readGEnv $ \env -> lookupNameEnv (global_tycon_pa env) (tyConName tc))

paOfType :: Type -> VM CoreExpr
paOfType ty | Just ty' <- coreView ty = paOfType ty'

paOfType (TyVarTy tv) = maybeV (readLEnv $ \env -> lookupVarEnv (local_tyvar_pa env) tv)
paOfType (AppTy ty1 ty2)
  = do
      e1 <- paOfType ty1
      e2 <- paOfType ty2
      return $ mkApps e1 [Type ty2, e2]
paOfType (TyConApp tc tys)
  = do
      e  <- paOfTyCon tc
      es <- mapM paOfType tys
      return $ mkApps e [arg | (t,e) <- zip tys es, arg <- [Type t, e]]
paOfType (FunTy ty1 ty2) = paOfType (TyConApp funTyCon [ty1,ty2])
paOfType t@(ForAllTy tv ty) = pprPanic "paOfType:" (ppr t)
paOfType ty = pprPanic "paOfType:" (ppr ty)
        


-- ----------------------------------------------------------------------------
-- Types

vectTyCon :: TyCon -> VM TyCon
vectTyCon tc
  | isFunTyCon tc        = builtin closureTyCon
  | isBoxedTupleTyCon tc = return tc
  | isUnLiftedTyCon tc   = return tc
  | otherwise = do
                  r <- lookupTyCon tc
                  case r of
                    Just tc' -> return tc'

                    -- FIXME: just for now
                    Nothing  -> pprTrace "ccTyCon:" (ppr tc) $ return tc

vectType :: Type -> VM Type
vectType ty | Just ty' <- coreView ty = vectType ty
vectType (TyVarTy tv) = return $ TyVarTy tv
vectType (AppTy ty1 ty2) = liftM2 AppTy (vectType ty1) (vectType ty2)
vectType (TyConApp tc tys) = liftM2 TyConApp (vectTyCon tc) (mapM vectType tys)
vectType (FunTy ty1 ty2)   = liftM2 TyConApp (builtin closureTyCon)
                                             (mapM vectType [ty1,ty2])
vectType (ForAllTy tv ty)
  = do
      r   <- paArgType (TyVarTy tv) (tyVarKind tv)
      ty' <- vectType ty
      return . ForAllTy tv $ case r of { Just paty -> FunTy paty ty'; Nothing -> ty' }

vectType ty = pprPanic "vectType:" (ppr ty)

isClosureTyCon :: TyCon -> Bool
isClosureTyCon tc = tyConUnique tc == closureTyConKey

splitClosureTy :: Type -> (Type, Type)
splitClosureTy ty
  | Just (tc, [arg_ty, res_ty]) <- splitTyConApp_maybe ty
  , isClosureTyCon tc
  = (arg_ty, res_ty)

  | otherwise = pprPanic "splitClosureTy" (ppr ty)

mkPArrayTy :: Type -> VM Type
mkPArrayTy ty = do
                  tc <- builtin parrayTyCon
                  return $ TyConApp tc [ty]


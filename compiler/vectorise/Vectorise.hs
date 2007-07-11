module Vectorise( vectorise )
where

#include "HsVersions.h"

import VectMonad
import VectUtils

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
import Control.Monad        ( liftM, liftM2, mapAndUnzipM, zipWithM_ )
import Data.Maybe           ( maybeToList )

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
      lty <- mkPArrayType vty
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
      dict <- paDictOfType ty
      rep  <- builtin replicatePAVar
      return $ mkApps (Var rep) [Type ty, dict, expr, len]
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

vectPolyVar :: CoreExpr -> Var -> [Type] -> VM (CoreExpr, CoreExpr)
vectPolyVar lc v tys
  = do
      r <- readLEnv $ \env -> lookupVarEnv (local_vars env) v
      case r of
        Just (vexpr, lexpr) -> liftM2 (,) (mk_app vexpr) (mk_app lexpr)
        Nothing ->
          do
            poly  <- maybeV (readGEnv $ \env -> lookupVarEnv (global_vars env) v)
            vexpr <- mk_app poly
            lexpr <- replicateP vexpr lc
            return (vexpr, lexpr)
  where
    mk_app e = do
                 vtys  <- mapM vectType tys
                 dicts <- mapM paDictOfType vtys
                 return $ mkApps e [arg | (vty, dict) <- zip vtys dicts
                                        , arg <- [Type vty, dict]]

abstractOverTyVars :: [TyVar] -> ((CoreExpr -> CoreExpr) -> VM a) -> VM a
abstractOverTyVars tvs p
  = do
      mdicts <- mapM mk_dict_var tvs
      zipWithM_ (\tv -> maybe (deleteTyVarPA tv) (extendTyVarPA tv . Var)) tvs mdicts
      p (mk_lams mdicts)
  where
    mk_dict_var tv = do
                       r <- paDictArgType tv
                       case r of
                         Just ty -> liftM Just (newLocalVar FSLIT("dPA") ty)
                         Nothing -> return Nothing

    mk_lams mdicts = mkLams [arg | (tv, mdict) <- zip tvs mdicts
                                 , arg <- tv : maybeToList mdict]
    

vectPolyExpr :: CoreExpr -> CoreExprWithFVs -> VM (CoreExpr, CoreExpr)
vectPolyExpr lc expr
  = localV
  . abstractOverTyVars tvs $ \mk_lams ->
    -- FIXME: shadowing (tvs in lc)
    do
      (vmono, lmono) <- vectExpr lc mono
      return $ (mk_lams vmono, mk_lams lmono)
  where
    (tvs, mono) = collectAnnTypeBinders expr  
                
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

vectExpr lc e@(_, AnnApp _ arg)
  | isAnnTypeArg arg
  = vectTyAppExpr lc fn tys
  where
    (fn, tys) = collectAnnTypeArgs e

vectExpr lc (_, AnnApp fn arg)
  = do
      fn'  <- vectExpr lc fn
      arg' <- vectExpr lc arg
      capply fn' arg'

vectExpr lc (_, AnnCase expr bndr ty alts)
  = panic "vectExpr: case"

vectExpr lc (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
      (vrhs, lrhs) <- vectPolyExpr lc rhs
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
             (vbody, lbody) <- vectPolyExpr lc body
             return (vrhss, vbody, lrhss, lbody)

vectExpr lc e@(_, AnnLam bndr body)
  | isTyVar bndr = pprPanic "vectExpr" (ppr $ deAnnotate e)

vectTyAppExpr :: CoreExpr -> CoreExprWithFVs -> [Type] -> VM (CoreExpr, CoreExpr)
vectTyAppExpr lc (_, AnnVar v) tys = vectPolyVar lc v tys
vectTyAppExpr lc e tys = pprPanic "vectTyAppExpr" (ppr $ deAnnotate e)

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
      r   <- paDictArgType tv
      ty' <- vectType ty
      return $ ForAllTy tv (wrap r ty')
  where
    wrap Nothing      = id
    wrap (Just pa_ty) = FunTy pa_ty

vectType ty = pprPanic "vectType:" (ppr ty)


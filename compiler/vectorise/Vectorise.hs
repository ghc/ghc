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
import DataCon
import TyCon
import Type
import TypeRep
import Var
import VarEnv
import VarSet
import Name                 ( mkSysTvName, getName )
import NameEnv
import Id
import MkId                 ( unwrapFamInstScrut )
import OccName

import DsMonad hiding (mapAndUnzipM)
import DsUtils              ( mkCoreTup, mkCoreTupTy )

import PrelNames
import TysWiredIn
import BasicTypes           ( Boxity(..) )

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

vectTopBind b@(NonRec var expr)
  = do
      var'  <- vectTopBinder var
      expr' <- vectTopRhs expr
      hs    <- takeHoisted
      return . Rec $ (var, expr) : (var', expr') : hs
  `orElseV`
    return b

vectTopBind b@(Rec bs)
  = do
      vars'  <- mapM vectTopBinder vars
      exprs' <- mapM vectTopRhs exprs
      hs     <- takeHoisted
      return . Rec $ bs ++ zip vars' exprs' ++ hs
  `orElseV`
    return b
  where
    (vars, exprs) = unzip bs

vectTopBinder :: Var -> VM Var
vectTopBinder var
  = do
      vty <- liftM (mkForAllTys tyvars) $ vectType mono_ty
      name <- cloneName mkVectOcc (getName var)
      let var' | isExportedId var = Id.mkExportedLocalId name vty
               | otherwise        = Id.mkLocalId         name vty
      defGlobalVar var var'
      return var'
  where
    (tyvars, mono_ty) = splitForAllTys (idType var)
    
vectTopRhs :: CoreExpr -> VM CoreExpr
vectTopRhs = liftM fst . closedV . vectPolyExpr (panic "Empty lifting context") . freeVars

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
vectVar lc v
  = do
      r <- lookupVar v
      case r of
        Local es     -> return es
        Global vexpr -> do
                          lexpr <- replicateP vexpr lc
                          return (vexpr, lexpr)

vectPolyVar :: CoreExpr -> Var -> [Type] -> VM (CoreExpr, CoreExpr)
vectPolyVar lc v tys
  = do
      r <- lookupVar v
      case r of
        Local (vexpr, lexpr) -> liftM2 (,) (mk_app vexpr) (mk_app lexpr)
        Global poly          -> do
                                  vexpr <- mk_app poly
                                  lexpr <- replicateP vexpr lc
                                  return (vexpr, lexpr)
  where
    mk_app e = applyToTypes e =<< mapM vectType tys

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

applyToTypes :: CoreExpr -> [Type] -> VM CoreExpr
applyToTypes expr tys
  = do
      dicts <- mapM paDictOfType tys
      return $ mkApps expr [arg | (ty, dict) <- zip tys dicts
                                , arg <- [Type ty, dict]]
    

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

vectExpr lc (fvs, AnnLam bndr body)
  = do
      let tyvars = filter isTyVar (varSetElems fvs)
      info <- mkCEnvInfo fvs bndr body
      (poly_vfn, poly_lfn) <- mkClosureFns info tyvars bndr body

      vfn_var <- hoistExpr FSLIT("vfn") poly_vfn
      lfn_var <- hoistExpr FSLIT("lfn") poly_lfn

      let (venv, lenv) = mkClosureEnvs info lc

      let env_ty = cenv_vty info

      pa_dict <- paDictOfType env_ty

      arg_ty <- vectType (varType bndr)
      res_ty <- vectType (exprType $ deAnnotate body)

      -- FIXME: move the functions to the top level
      mono_vfn <- applyToTypes (Var vfn_var) (map TyVarTy tyvars)
      mono_lfn <- applyToTypes (Var lfn_var) (map TyVarTy tyvars)

      mk_clo <- builtin mkClosureVar
      mk_cloP <- builtin mkClosurePVar

      let vclo = Var mk_clo  `mkTyApps` [arg_ty, res_ty, env_ty]
                             `mkApps`   [pa_dict, mono_vfn, mono_lfn, venv]
          
          lclo = Var mk_cloP `mkTyApps` [arg_ty, res_ty, env_ty]
                             `mkApps`   [pa_dict, mono_vfn, mono_lfn, lenv]

      return (vclo, lclo)
       

data CEnvInfo = CEnvInfo {
               cenv_vars         :: [Var]
             , cenv_values       :: [(CoreExpr, CoreExpr)]
             , cenv_vty          :: Type
             , cenv_lty          :: Type
             , cenv_repr_tycon   :: TyCon
             , cenv_repr_tyargs  :: [Type]
             , cenv_repr_datacon :: DataCon
             }

mkCEnvInfo :: VarSet -> Var -> CoreExprWithFVs -> VM CEnvInfo
mkCEnvInfo fvs arg body
  = do
      locals <- readLEnv local_vars
      let
          (vars, vals) = unzip
                 [(var, val) | var      <- varSetElems fvs
                             , Just val <- [lookupVarEnv locals var]]
      vtys <- mapM (vectType . varType) vars

      (vty, repr_tycon, repr_tyargs, repr_datacon) <- mk_env_ty vtys
      lty <- mkPArrayType vty
      
      return $ CEnvInfo {
                 cenv_vars         = vars
               , cenv_values       = vals
               , cenv_vty          = vty
               , cenv_lty          = lty
               , cenv_repr_tycon   = repr_tycon
               , cenv_repr_tyargs  = repr_tyargs
               , cenv_repr_datacon = repr_datacon
               }
  where
    mk_env_ty [vty]
      = return (vty, error "absent cinfo_repr_tycon"
                   , error "absent cinfo_repr_tyargs"
                   , error "absent cinfo_repr_datacon")

    mk_env_ty vtys
      = do
          let ty = mkCoreTupTy vtys
          (repr_tc, repr_tyargs) <- lookupPArrayFamInst ty
          let [repr_con] = tyConDataCons repr_tc
          return (ty, repr_tc, repr_tyargs, repr_con)

    

mkClosureEnvs :: CEnvInfo -> CoreExpr -> (CoreExpr, CoreExpr)
mkClosureEnvs info lc
  | [] <- vals
  = (Var unitDataConId, mkApps (Var $ dataConWrapId (cenv_repr_datacon info))
                               [lc, Var unitDataConId])

  | [(vval, lval)] <- vals
  = (vval, lval)

  | otherwise
  = (mkCoreTup vvals, Var (dataConWrapId $ cenv_repr_datacon info)
                      `mkTyApps` cenv_repr_tyargs info
                      `mkApps`   (lc : lvals))

  where
    vals = cenv_values info
    (vvals, lvals) = unzip vals

mkClosureFns :: CEnvInfo -> [TyVar] -> Var -> CoreExprWithFVs
             -> VM (CoreExpr, CoreExpr)
mkClosureFns info tyvars arg body
  = closedV
  . abstractOverTyVars tyvars
  $ \mk_tlams ->
  do
    (vfn, lfn) <- mkClosureMonoFns info arg body
    return (mk_tlams vfn, mk_tlams lfn)

mkClosureMonoFns :: CEnvInfo -> Var -> CoreExprWithFVs -> VM (CoreExpr, CoreExpr)
mkClosureMonoFns info arg body
  = do
      lc_bndr <- newLocalVar FSLIT("lc") intTy
      (varg : vbndrs, larg : lbndrs, (vbody, lbody))
        <- vectBndrsIn (arg : cenv_vars info)
                       (vectExpr (Var lc_bndr) body)

      venv_bndr <- newLocalVar FSLIT("env") vty
      lenv_bndr <- newLocalVar FSLIT("env") lty

      let vcase = bind_venv (Var venv_bndr) vbody vbndrs
      lcase <- bind_lenv (Var lenv_bndr) lbody lc_bndr lbndrs
      return (mkLams [venv_bndr, varg] vcase, mkLams [lenv_bndr, larg] lcase)
  where
    vty = cenv_vty info
    lty = cenv_lty info

    arity = length (cenv_vars info)

    bind_venv venv vbody []      = vbody
    bind_venv venv vbody [vbndr] = Let (NonRec vbndr venv) vbody
    bind_venv venv vbody vbndrs
      = Case venv (mkWildId vty) (exprType vbody)
             [(DataAlt (tupleCon Boxed arity), vbndrs, vbody)]

    bind_lenv lenv lbody lc_bndr [lbndr]
      = do
          lengthPA <- builtin lengthPAVar
          return . Let (NonRec lbndr lenv)
                 $ Case (mkApps (Var lengthPA) [Type vty, (Var lbndr)])
                        lc_bndr
                        intTy
                        [(DEFAULT, [], lbody)]

    bind_lenv lenv lbody lc_bndr lbndrs
      = return
      $ Case (unwrapFamInstScrut (cenv_repr_tycon info)
                                 (cenv_repr_tyargs info)
                                 lenv)
             (mkWildId lty)
             (exprType lbody)
             [(DataAlt (cenv_repr_datacon info), lc_bndr : lbndrs, lbody)]
          
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


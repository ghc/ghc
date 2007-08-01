module Vectorise( vectorise )
where

#include "HsVersions.h"

import VectMonad
import VectUtils
import VectType
import VectCore

import DynFlags
import HscTypes

import CoreLint             ( showPass, endPass )
import CoreSyn
import CoreUtils
import CoreFVs
import SimplMonad           ( SimplCount, zeroSimplCount )
import Rules                ( RuleBase )
import DataCon
import TyCon
import Type
import FamInstEnv           ( extendFamInstEnvList )
import InstEnv              ( extendInstEnvList )
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

import Literal              ( Literal )
import PrelNames
import TysWiredIn
import TysPrim              ( intPrimTy )
import BasicTypes           ( Boxity(..) )

import Outputable
import FastString
import Control.Monad        ( liftM, liftM2, zipWithM, mapAndUnzipM )

vectorise :: HscEnv -> UniqSupply -> RuleBase -> ModGuts
          -> IO (SimplCount, ModGuts)
vectorise hsc_env _ _ guts
  = do
      showPass dflags "Vectorisation"
      eps <- hscEPS hsc_env
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps
      Just (info', guts') <- initV hsc_env guts info (vectModule guts)
      endPass dflags "Vectorisation" Opt_D_dump_vect (mg_binds guts')
      return (zeroSimplCount dflags, guts' { mg_vect_info = info' })
  where
    dflags = hsc_dflags hsc_env

vectModule :: ModGuts -> VM ModGuts
vectModule guts
  = do
      (types', fam_insts, pa_insts) <- vectTypeEnv (mg_types guts)
      
      let insts         = map painstInstance pa_insts
          fam_inst_env' = extendFamInstEnvList (mg_fam_inst_env guts) fam_insts
          inst_env'     = extendInstEnvList (mg_inst_env guts) insts
      updGEnv (setInstEnvs inst_env' fam_inst_env')
     
      dicts  <- mapM buildPADict pa_insts 
      binds' <- mapM vectTopBind (mg_binds guts)
      return $ guts { mg_types        = types'
                    , mg_binds        = Rec (concat dicts) : binds'
                    , mg_inst_env     = inst_env'
                    , mg_fam_inst_env = fam_inst_env'
                    , mg_insts        = mg_insts guts ++ insts
                    , mg_fam_insts    = mg_fam_insts guts ++ fam_insts
                    }

vectTopBind :: CoreBind -> VM CoreBind
vectTopBind b@(NonRec var expr)
  = do
      var'  <- vectTopBinder var
      expr' <- vectTopRhs var expr
      hs    <- takeHoisted
      return . Rec $ (var, expr) : (var', expr') : hs
  `orElseV`
    return b

vectTopBind b@(Rec bs)
  = do
      vars'  <- mapM vectTopBinder vars
      exprs' <- zipWithM vectTopRhs vars exprs
      hs     <- takeHoisted
      return . Rec $ bs ++ zip vars' exprs' ++ hs
  `orElseV`
    return b
  where
    (vars, exprs) = unzip bs

vectTopBinder :: Var -> VM Var
vectTopBinder var
  = do
      vty <- vectType (idType var)
      name <- cloneName mkVectOcc (getName var)
      let var' | isExportedId var = Id.mkExportedLocalId name vty
               | otherwise        = Id.mkLocalId         name vty
      defGlobalVar var var'
      return var'
    
vectTopRhs :: Var -> CoreExpr -> VM CoreExpr
vectTopRhs var expr
  = do
      lc <- newLocalVar FSLIT("lc") intPrimTy
      closedV . liftM vectorised
              . inBind var
              $ vectPolyExpr lc (freeVars expr)

-- ----------------------------------------------------------------------------
-- Bindings

vectBndr :: Var -> VM VVar
vectBndr v
  = do
      vty <- vectType (idType v)
      lty <- mkPArrayType vty
      let vv = v `Id.setIdType` vty
          lv = v `Id.setIdType` lty
      updLEnv (mapTo vv lv)
      return (vv, lv)
  where
    mapTo vv lv env = env { local_vars = extendVarEnv (local_vars env) v (vv, lv) }

vectBndrIn :: Var -> VM a -> VM (VVar, a)
vectBndrIn v p
  = localV
  $ do
      vv <- vectBndr v
      x <- p
      return (vv, x)

vectBndrsIn :: [Var] -> VM a -> VM ([VVar], a)
vectBndrsIn vs p
  = localV
  $ do
      vvs <- mapM vectBndr vs
      x <- p
      return (vvs, x)

-- ----------------------------------------------------------------------------
-- Expressions

capply :: VExpr -> VExpr -> VM VExpr
capply (vfn, lfn) (varg, larg)
  = do
      apply  <- builtin applyClosureVar
      applyP <- builtin applyClosurePVar
      return (mkApps (Var apply)  [Type arg_ty, Type res_ty, vfn, varg],
              mkApps (Var applyP) [Type arg_ty, Type res_ty, lfn, larg])
  where
    fn_ty            = exprType vfn
    (arg_ty, res_ty) = splitClosureTy fn_ty

vectVar :: Var -> Var -> VM VExpr
vectVar lc v
  = do
      r <- lookupVar v
      case r of
        Local (vv,lv) -> return (Var vv, Var lv)
        Global vv     -> do
                           let vexpr = Var vv
                           lexpr <- replicatePA vexpr (Var lc)
                           return (vexpr, lexpr)

vectPolyVar :: Var -> Var -> [Type] -> VM VExpr
vectPolyVar lc v tys
  = do
      vtys <- mapM vectType tys
      r <- lookupVar v
      case r of
        Local (vv, lv) -> liftM2 (,) (polyApply (Var vv) vtys)
                                     (polyApply (Var lv) vtys)
        Global poly    -> do
                            vexpr <- polyApply (Var poly) vtys
                            lexpr <- replicatePA vexpr (Var lc)
                            return (vexpr, lexpr)

vectLiteral :: Var -> Literal -> VM VExpr
vectLiteral lc lit
  = do
      lexpr <- replicatePA (Lit lit) (Var lc)
      return (Lit lit, lexpr)

vectPolyExpr :: Var -> CoreExprWithFVs -> VM VExpr
vectPolyExpr lc expr
  = polyAbstract tvs $ \abstract ->
    -- FIXME: shadowing (tvs in lc)
    do
      mono' <- vectExpr lc mono
      return $ mapVect abstract mono'
  where
    (tvs, mono) = collectAnnTypeBinders expr  
                
vectExpr :: Var -> CoreExprWithFVs -> VM VExpr
vectExpr lc (_, AnnType ty)
  = liftM vType (vectType ty)

vectExpr lc (_, AnnVar v) = vectVar lc v

vectExpr lc (_, AnnLit lit) = vectLiteral lc lit

vectExpr lc (_, AnnNote note expr)
  = liftM (vNote note) (vectExpr lc expr)

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
      vrhs <- localV . inBind bndr $ vectPolyExpr lc rhs
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr lc body)
      return $ vLet (vNonRec vbndr vrhs) vbody

vectExpr lc (_, AnnLet (AnnRec bs) body)
  = do
      (vbndrs, (vrhss, vbody)) <- vectBndrsIn bndrs
                                $ liftM2 (,)
                                  (zipWithM vect_rhs bndrs rhss)
                                  (vectPolyExpr lc body)
      return $ vLet (vRec vbndrs vrhss) vbody
  where
    (bndrs, rhss) = unzip bs

    vect_rhs bndr rhs = localV
                      . inBind bndr
                      $ vectExpr lc rhs

vectExpr lc e@(fvs, AnnLam bndr _)
  | not (isId bndr) = pprPanic "vectExpr" (ppr $ deAnnotate e)
  | otherwise = vectLam lc fvs bs body
  where
    (bs,body) = collectAnnValBinders e

vectLam :: Var -> VarSet -> [Var] -> CoreExprWithFVs -> VM VExpr
vectLam lc fvs bs body
  = do
      tyvars <- localTyVars
      (vs, vvs) <- readLEnv $ \env ->
                   unzip [(var, vv) | var <- varSetElems fvs
                                    , Just vv <- [lookupVarEnv (local_vars env) var]]

      arg_tys <- mapM (vectType . idType) bs
      res_ty  <- vectType (exprType $ deAnnotate body)

      buildClosures tyvars lc vvs arg_tys res_ty
        . hoistPolyVExpr tyvars
        $ do
            new_lc <- newLocalVar FSLIT("lc") intPrimTy
            (vbndrs, vbody) <- vectBndrsIn (vs ++ bs)
                                           (vectExpr new_lc body)
            return $ vLams new_lc vbndrs vbody
  
vectTyAppExpr :: Var -> CoreExprWithFVs -> [Type] -> VM (CoreExpr, CoreExpr)
vectTyAppExpr lc (_, AnnVar v) tys = vectPolyVar lc v tys
vectTyAppExpr lc e tys = pprPanic "vectTyAppExpr" (ppr $ deAnnotate e)



module Vectorise.Utils.Closure (
	mkClosure,
	mkClosureApp,
	buildClosure,
	buildClosures,
	buildEnv
)
where
import VectUtils
import Vectorise.Builtins
import Vectorise.Vect
import Vectorise.Monad

import CoreSyn
import Type
import Var
import MkCore
import CoreUtils
import TyCon
import DataCon
import MkId
import TysWiredIn
import BasicTypes
import FastString


mkClosure :: Type -> Type -> Type -> VExpr -> VExpr -> VM VExpr
mkClosure arg_ty res_ty env_ty (vfn,lfn) (venv,lenv)
 = do Just dict <- paDictOfType env_ty
      mkv       <- builtin closureVar
      mkl       <- builtin liftedClosureVar
      return (Var mkv `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, venv],
              Var mkl `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, lenv])


mkClosureApp :: Type -> Type -> VExpr -> VExpr -> VM VExpr
mkClosureApp arg_ty res_ty (vclo, lclo) (varg, larg)
 = do vapply <- builtin applyVar
      lapply <- builtin liftedApplyVar
      lc     <- builtin liftingContext
      return (Var vapply `mkTyApps` [arg_ty, res_ty] `mkApps` [vclo, varg],
              Var lapply `mkTyApps` [arg_ty, res_ty] `mkApps` [Var lc, lclo, larg])


buildClosures :: [TyVar] -> [VVar] -> [Type] -> Type -> VM VExpr -> VM VExpr
buildClosures _   _    [] _ mk_body
  = mk_body
buildClosures tvs vars [arg_ty] res_ty mk_body
  = -- liftM vInlineMe $
      buildClosure tvs vars arg_ty res_ty mk_body
buildClosures tvs vars (arg_ty : arg_tys) res_ty mk_body
  = do
      res_ty' <- mkClosureTypes arg_tys res_ty
      arg <- newLocalVVar (fsLit "x") arg_ty
      -- liftM vInlineMe
      buildClosure tvs vars arg_ty res_ty'
        . hoistPolyVExpr tvs (Inline (length vars + 1))
        $ do
            lc <- builtin liftingContext
            clo <- buildClosures tvs (vars ++ [arg]) arg_tys res_ty mk_body
            return $ vLams lc (vars ++ [arg]) clo


-- (clo <x1,...,xn> <f,f^>, aclo (Arr lc xs1 ... xsn) <f,f^>)
--   where
--     f  = \env v -> case env of <x1,...,xn> -> e x1 ... xn v
--     f^ = \env v -> case env of Arr l xs1 ... xsn -> e^ l x1 ... xn v
--
buildClosure :: [TyVar] -> [VVar] -> Type -> Type -> VM VExpr -> VM VExpr
buildClosure tvs vars arg_ty res_ty mk_body
  = do
      (env_ty, env, bind) <- buildEnv vars
      env_bndr <- newLocalVVar (fsLit "env") env_ty
      arg_bndr <- newLocalVVar (fsLit "arg") arg_ty

      fn <- hoistPolyVExpr tvs (Inline 2)
          $ do
              lc    <- builtin liftingContext
              body  <- mk_body
              return -- . vInlineMe
                     . vLams lc [env_bndr, arg_bndr]
                     $ bind (vVar env_bndr)
                            (vVarApps lc body (vars ++ [arg_bndr]))

      mkClosure arg_ty res_ty env_ty fn env


-- Environments ---------------------------------------------------------------
buildEnv :: [VVar] -> VM (Type, VExpr, VExpr -> VExpr -> VExpr)
buildEnv [] = do
             ty    <- voidType
             void  <- builtin voidVar
             pvoid <- builtin pvoidVar
             return (ty, vVar (void, pvoid), \_ body -> body)

buildEnv [v] = return (vVarType v, vVar v,
                    \env body -> vLet (vNonRec v env) body)

buildEnv vs
  = do
      
      (lenv_tc, lenv_tyargs) <- pdataReprTyCon ty

      let venv_con   = tupleCon Boxed (length vs) 
          [lenv_con] = tyConDataCons lenv_tc

          venv       = mkCoreTup (map Var vvs)
          lenv       = Var (dataConWrapId lenv_con)
                       `mkTyApps` lenv_tyargs
                       `mkApps`   map Var lvs

          vbind env body = mkWildCase env ty (exprType body)
                           [(DataAlt venv_con, vvs, body)]

          lbind env body =
            let scrut = unwrapFamInstScrut lenv_tc lenv_tyargs env
            in
            mkWildCase scrut (exprType scrut) (exprType body)
              [(DataAlt lenv_con, lvs, body)]

          bind (venv, lenv) (vbody, lbody) = (vbind venv vbody,
                                              lbind lenv lbody)

      return (ty, (venv, lenv), bind)
  where
    (vvs, lvs) = unzip vs
    tys        = map vVarType vs
    ty         = mkBoxedTupleTy tys

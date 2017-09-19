-- |Utils concerning closure construction and application.

module Vectorise.Utils.Closure
  ( mkClosure
  , mkClosureApp
  , buildClosures
  )
where

import GhcPrelude

import Vectorise.Builtins
import Vectorise.Vect
import Vectorise.Monad
import Vectorise.Utils.Base
import Vectorise.Utils.PADict
import Vectorise.Utils.Hoisting

import CoreSyn
import Type
import MkCore
import CoreUtils
import TyCon
import DataCon
import MkId
import TysWiredIn
import BasicTypes( Boxity(..) )
import FastString


-- |Make a closure.
--
mkClosure :: Type       -- ^ Type of the argument.
          -> Type       -- ^ Type of the result.
          -> Type       -- ^ Type of the environment.
          -> VExpr      -- ^ The function to apply.
          -> VExpr      -- ^ The environment to use.
          -> VM VExpr
mkClosure arg_ty res_ty env_ty (vfn,lfn) (venv,lenv)
 = do dict <- paDictOfType env_ty
      mkv  <- builtin closureVar
      mkl  <- builtin liftedClosureVar
      return (Var mkv `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, venv],
              Var mkl `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, lenv])

-- |Make a closure application.
--
mkClosureApp :: Type      -- ^ Type of the argument.
             -> Type      -- ^ Type of the result.
             -> VExpr     -- ^ Closure to apply.
             -> VExpr     -- ^ Argument to use.
             -> VM VExpr
mkClosureApp arg_ty res_ty (vclo, lclo) (varg, larg)
 = do vapply <- builtin applyVar
      lapply <- builtin liftedApplyVar
      lc     <- builtin liftingContext
      return (Var vapply `mkTyApps` [arg_ty, res_ty] `mkApps` [vclo, varg],
              Var lapply `mkTyApps` [arg_ty, res_ty] `mkApps` [Var lc, lclo, larg])

-- |Build a set of 'n' closures corresponding to an 'n'-ary vectorised function.  The length of
-- the list of types of arguments determines the arity.
--
-- In addition to a set of type variables, a set of value variables is passed during closure
-- /construction/.  In contrast, the closure environment and the arguments are passed during closure
-- application.
--
buildClosures :: [TyVar]    -- ^ Type variables passed during closure construction.
              -> [Var]      -- ^ Variables passed during closure construction.
              -> [VVar]     -- ^ Variables in the environment.
              -> [Type]     -- ^ Type of the arguments.
              -> Type       -- ^ Type of result.
              -> VM VExpr
              -> VM VExpr
buildClosures _tvs _vars _env [] _res_ty mk_body
 = mk_body
buildClosures tvs vars env [arg_ty] res_ty mk_body
 =  buildClosure tvs vars env arg_ty res_ty mk_body
buildClosures tvs vars env (arg_ty : arg_tys) res_ty mk_body
 = do { res_ty' <- mkClosureTypes arg_tys res_ty
      ; arg     <- newLocalVVar (fsLit "x") arg_ty
      ; buildClosure tvs vars env arg_ty res_ty'
          . hoistPolyVExpr tvs vars (Inline (length env + 1))
          $ do { lc     <- builtin liftingContext
               ; clo    <- buildClosures tvs vars (env ++ [arg]) arg_tys res_ty mk_body
               ; return $ vLams lc (env ++ [arg]) clo
               }
      }

-- Build a closure taking one extra argument during closure application.
--
-- (clo <x1,...,xn> <f,f^>, aclo (Arr lc xs1 ... xsn) <f,f^>)
--   where
--     f  = \env v -> case env of <x1,...,xn> -> e x1 ... xn v
--     f^ = \env v -> case env of Arr l xs1 ... xsn -> e^ l x1 ... xn v
--
-- In addition to a set of type variables, a set of value variables is passed during closure
-- /construction/.  In contrast, the closure environment and the closure argument are passed during
-- closure application.
--
buildClosure :: [TyVar]         -- ^Type variables passed during closure construction.
             -> [Var]           -- ^Variables passed during closure construction.
             -> [VVar]          -- ^Variables in the environment.
             -> Type            -- ^Type of the closure argument.
             -> Type            -- ^Type of the result.
             -> VM VExpr
             -> VM VExpr
buildClosure tvs vars vvars arg_ty res_ty mk_body
  = do { (env_ty, env, bind) <- buildEnv vvars
       ; env_bndr <- newLocalVVar (fsLit "env") env_ty
       ; arg_bndr <- newLocalVVar (fsLit "arg") arg_ty

           -- generate the closure function as a hoisted binding
       ; fn <- hoistPolyVExpr tvs vars (Inline 2) $
                 do { lc     <- builtin liftingContext
                    ; body   <- mk_body
                    ; return . vLams lc [env_bndr, arg_bndr]
                             $ bind (vVar env_bndr)
                                    (vVarApps lc body (vvars ++ [arg_bndr]))
                    }

       ; mkClosure arg_ty res_ty env_ty fn env
       }

-- Build the environment for a single closure.
--
buildEnv :: [VVar] -> VM (Type, VExpr, VExpr -> VExpr -> VExpr)
buildEnv []
 = do
      ty    <- voidType
      void  <- builtin voidVar
      pvoid <- builtin pvoidVar
      return (ty, vVar (void, pvoid), \_ body -> body)
buildEnv [v]
 = return (vVarType v, vVar v,
           \env body -> vLet (vNonRec v env) body)
buildEnv vs
 = do (lenv_tc, lenv_tyargs) <- pdataReprTyCon ty

      let venv_con   = tupleDataCon Boxed (length vs)
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

-- | Foreign primitive calls
--
-- This is for `@foreign import prim@' declarations.
--
-- Currently, at the core level we pretend that these primitive calls are
-- foreign calls. It may make more sense in future to have them as a distinct
-- kind of Id, or perhaps to bundle them with PrimOps since semantically and for
-- calling convention they are really prim ops.
module GHC.HsToCore.Foreign.Prim
  ( dsPrimCall
  )
where

import GHC.Prelude

import GHC.Tc.Utils.Monad        -- temp
import GHC.Tc.Utils.TcType

import GHC.Core
import GHC.Core.Type
import GHC.Core.Coercion

import GHC.HsToCore.Monad
import GHC.HsToCore.Foreign.Call

import GHC.Types.Id
import GHC.Types.ForeignStubs
import GHC.Types.ForeignCall

dsPrimCall :: Id -> Coercion -> ForeignCall
           -> DsM ([(Id, Expr TyVar)], CStub)
dsPrimCall fn_id co fcall = do
    let
        ty                   = coercionLKind co
        (tvs, fun_ty)        = tcSplitForAllInvisTyVars ty
        (arg_tys, io_res_ty) = tcSplitFunTys fun_ty

    args <- newSysLocalsDs arg_tys  -- no FFI representation polymorphism

    ccall_uniq <- newUnique
    let
        call_app = mkFCall ccall_uniq fcall (map Var args) io_res_ty
        rhs      = mkLams tvs (mkLams args call_app)
        rhs'     = Cast rhs co
    return ([(fn_id, rhs')], mempty)

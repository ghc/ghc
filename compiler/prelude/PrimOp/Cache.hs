module PrimOp.Cache
  ( PrimOpCache(..)
  , mkPrimOpCache
  , primOpType
  , primOpSig
  , primOpOcc
  , getPrimOpResultInfo
  , isComparisonPrimOp
  , PrimOpResultInfo(..)
  ) where

import GhcPrelude

import TysPrim

import Demand
import OccName          ( OccName )
import TyCon            ( TyCon, isPrimTyCon, PrimRep(..) )
import Type
import RepType          ( typePrimRep1, tyConPrimRep1 )
import BasicTypes       ( Arity )
import Platform
import PrimOp

-- | Memoizes these two functions per platform.
data PrimOpCache = PrimOpCache
  { allThePrimOps :: ![PrimOp]
  , primOpInfo :: !(PrimOp -> PrimOpInfo)
  }

mkPrimOpCache :: Platform -> PrimOpCache
mkPrimOpCache platform = PrimOpCache
  { allThePrimOps = allThePrimOpsPerPlatform platform
  , primOpInfo = primOpInfoPerPlatform platform
  }

{-
************************************************************************
*                                                                      *
               PrimOp types
*                                                                      *
************************************************************************
-}

primOpType :: PrimOpCache -> PrimOp -> Type  -- you may want to use primOpSig instead
primOpType primOpCache op
  = case primOpInfo primOpCache op of
    Dyadic  _occ ty -> dyadic_fun_ty ty
    Monadic _occ ty -> monadic_fun_ty ty
    Compare _occ ty -> compare_fun_ty ty

    GenPrimOp _occ tyvars arg_tys res_ty ->
        mkSpecForAllTys tyvars (mkVisFunTys arg_tys res_ty)

primOpOcc :: PrimOpCache -> PrimOp -> OccName
primOpOcc primOpCache op = primOpInfoOcc $ primOpInfo primOpCache op

isComparisonPrimOp :: PrimOpCache -> PrimOp -> Bool
isComparisonPrimOp primOpCache op = case primOpInfo primOpCache op of
                          Compare {} -> True
                          _          -> False

-- primOpSig is like primOpType but gives the result split apart:
-- (type variables, argument types, result type)
-- It also gives arity, strictness info

primOpSig :: PrimOpCache -> PrimOp -> ([TyVar], [Type], Type, Arity, StrictSig)
primOpSig primOpCache op
  = (tyvars, arg_tys, res_ty, arity, primOpStrictness op arity)
  where
    arity = length arg_tys
    (tyvars, arg_tys, res_ty)
      = case (primOpInfo primOpCache op) of
        Monadic   _occ ty                    -> ([],     [ty],    ty       )
        Dyadic    _occ ty                    -> ([],     [ty,ty], ty       )
        Compare   _occ ty                    -> ([],     [ty,ty], intPrimTy)
        GenPrimOp _occ tyvars arg_tys res_ty -> (tyvars, arg_tys, res_ty   )

data PrimOpResultInfo
  = ReturnsPrim     PrimRep
  | ReturnsAlg      TyCon

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOpCache -> PrimOp -> PrimOpResultInfo
getPrimOpResultInfo primOpCache op
  = case (primOpInfo primOpCache op) of
      Dyadic  _ ty                        -> ReturnsPrim (typePrimRep1 ty)
      Monadic _ ty                        -> ReturnsPrim (typePrimRep1 ty)
      Compare _ _                         -> ReturnsPrim (tyConPrimRep1 intPrimTyCon)
      GenPrimOp _ _ _ ty | isPrimTyCon tc -> ReturnsPrim (tyConPrimRep1 tc)
                         | otherwise      -> ReturnsAlg tc
                         where
                           tc = tyConAppTyCon ty
                        -- All primops return a tycon-app result
                        -- The tycon can be an unboxed tuple or sum, though,
                        -- which gives rise to a ReturnAlg

-- Utils:

dyadic_fun_ty, monadic_fun_ty, compare_fun_ty :: Type -> Type
dyadic_fun_ty  ty = mkVisFunTys [ty, ty] ty
monadic_fun_ty ty = mkVisFunTy  ty ty
compare_fun_ty ty = mkVisFunTys [ty, ty] intPrimTy

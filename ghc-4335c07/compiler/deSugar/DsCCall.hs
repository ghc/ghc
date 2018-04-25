{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998


Desugaring foreign calls
-}

{-# LANGUAGE CPP #-}
module DsCCall
        ( dsCCall
        , mkFCall
        , unboxArg
        , boxResult
        , resultWrapper
        ) where

#include "HsVersions.h"


import GhcPrelude

import CoreSyn

import DsMonad
import CoreUtils
import MkCore
import MkId
import ForeignCall
import DataCon
import DsUtils

import TcType
import Type
import Id   ( Id )
import Coercion
import PrimOp
import TysPrim
import TyCon
import TysWiredIn
import BasicTypes
import Literal
import PrelNames
import DynFlags
import Outputable
import Util

import Data.Maybe

{-
Desugaring of @ccall@s consists of adding some state manipulation,
unboxing any boxed primitive arguments and boxing the result if
desired.

The state stuff just consists of adding in
@PrimIO (\ s -> case s of { S# s# -> ... })@ in an appropriate place.

The unboxing is straightforward, as all information needed to unbox is
available from the type.  For each boxed-primitive argument, we
transform:
\begin{verbatim}
   _ccall_ foo [ r, t1, ... tm ] e1 ... em
   |
   |
   V
   case e1 of { T1# x1# ->
   ...
   case em of { Tm# xm# -> xm#
   ccall# foo [ r, t1#, ... tm# ] x1# ... xm#
   } ... }
\end{verbatim}

The reboxing of a @_ccall_@ result is a bit tricker: the types don't
contain information about the state-pairing functions so we have to
keep a list of \tr{(type, s-p-function)} pairs.  We transform as
follows:
\begin{verbatim}
   ccall# foo [ r, t1#, ... tm# ] e1# ... em#
   |
   |
   V
   \ s# -> case (ccall# foo [ r, t1#, ... tm# ] s# e1# ... em#) of
          (StateAnd<r># result# state#) -> (R# result#, realWorld#)
\end{verbatim}
-}

dsCCall :: CLabelString -- C routine to invoke
        -> [CoreExpr]   -- Arguments (desugared)
                        -- Precondition: none have levity-polymorphic types
        -> Safety       -- Safety of the call
        -> Type         -- Type of the result: IO t
        -> DsM CoreExpr -- Result, of type ???

dsCCall lbl args may_gc result_ty
  = do (unboxed_args, arg_wrappers) <- mapAndUnzipM unboxArg args
       (ccall_result_ty, res_wrapper) <- boxResult result_ty
       uniq <- newUnique
       dflags <- getDynFlags
       let
           target = StaticTarget NoSourceText lbl Nothing True
           the_fcall    = CCall (CCallSpec target CCallConv may_gc)
           the_prim_app = mkFCall dflags uniq the_fcall unboxed_args ccall_result_ty
       return (foldr ($) (res_wrapper the_prim_app) arg_wrappers)

mkFCall :: DynFlags -> Unique -> ForeignCall
        -> [CoreExpr]     -- Args
        -> Type           -- Result type
        -> CoreExpr
-- Construct the ccall.  The only tricky bit is that the ccall Id should have
-- no free vars, so if any of the arg tys do we must give it a polymorphic type.
--      [I forget *why* it should have no free vars!]
-- For example:
--      mkCCall ... [s::StablePtr (a->b), x::Addr, c::Char]
--
-- Here we build a ccall thus
--      (ccallid::(forall a b.  StablePtr (a -> b) -> Addr -> Char -> IO Addr))
--                      a b s x c
mkFCall dflags uniq the_fcall val_args res_ty
  = ASSERT( all isTyVar tyvars )  -- this must be true because the type is top-level
    mkApps (mkVarApps (Var the_fcall_id) tyvars) val_args
  where
    arg_tys = map exprType val_args
    body_ty = (mkFunTys arg_tys res_ty)
    tyvars  = tyCoVarsOfTypeWellScoped body_ty
    ty      = mkInvForAllTys tyvars body_ty
    the_fcall_id = mkFCallId dflags uniq the_fcall ty

unboxArg :: CoreExpr                    -- The supplied argument, not levity-polymorphic
         -> DsM (CoreExpr,              -- To pass as the actual argument
                 CoreExpr -> CoreExpr   -- Wrapper to unbox the arg
                )
-- Example: if the arg is e::Int, unboxArg will return
--      (x#::Int#, \W. case x of I# x# -> W)
-- where W is a CoreExpr that probably mentions x#

-- always returns a non-levity-polymorphic expression

unboxArg arg
  -- Primitive types: nothing to unbox
  | isPrimitiveType arg_ty
  = return (arg, \body -> body)

  -- Recursive newtypes
  | Just(co, _rep_ty) <- topNormaliseNewType_maybe arg_ty
  = unboxArg (mkCastDs arg co)

  -- Booleans
  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` boolTyConKey
  = do dflags <- getDynFlags
       prim_arg <- newSysLocalDs intPrimTy
       return (Var prim_arg,
              \ body -> Case (mkWildCase arg arg_ty intPrimTy
                                       [(DataAlt falseDataCon,[],mkIntLit dflags 0),
                                        (DataAlt trueDataCon, [],mkIntLit dflags 1)])
                                        -- In increasing tag order!
                             prim_arg
                             (exprType body)
                             [(DEFAULT,[],body)])

  -- Data types with a single constructor, which has a single, primitive-typed arg
  -- This deals with Int, Float etc; also Ptr, ForeignPtr
  | is_product_type && data_con_arity == 1
  = ASSERT2(isUnliftedType data_con_arg_ty1, pprType arg_ty)
                        -- Typechecker ensures this
    do case_bndr <- newSysLocalDs arg_ty
       prim_arg <- newSysLocalDs data_con_arg_ty1
       return (Var prim_arg,
               \ body -> Case arg case_bndr (exprType body) [(DataAlt data_con,[prim_arg],body)]
              )

  -- Byte-arrays, both mutable and otherwise; hack warning
  -- We're looking for values of type ByteArray, MutableByteArray
  --    data ByteArray          ix = ByteArray        ix ix ByteArray#
  --    data MutableByteArray s ix = MutableByteArray ix ix (MutableByteArray# s)
  | is_product_type &&
    data_con_arity == 3 &&
    isJust maybe_arg3_tycon &&
    (arg3_tycon ==  byteArrayPrimTyCon ||
     arg3_tycon ==  mutableByteArrayPrimTyCon)
  = do case_bndr <- newSysLocalDs arg_ty
       vars@[_l_var, _r_var, arr_cts_var] <- newSysLocalsDs data_con_arg_tys
       return (Var arr_cts_var,
               \ body -> Case arg case_bndr (exprType body) [(DataAlt data_con,vars,body)]
              )

  | otherwise
  = do l <- getSrcSpanDs
       pprPanic "unboxArg: " (ppr l <+> ppr arg_ty)
  where
    arg_ty                                      = exprType arg
    maybe_product_type                          = splitDataProductType_maybe arg_ty
    is_product_type                             = isJust maybe_product_type
    Just (_, _, data_con, data_con_arg_tys)     = maybe_product_type
    data_con_arity                              = dataConSourceArity data_con
    (data_con_arg_ty1 : _)                      = data_con_arg_tys

    (_ : _ : data_con_arg_ty3 : _) = data_con_arg_tys
    maybe_arg3_tycon               = tyConAppTyCon_maybe data_con_arg_ty3
    Just arg3_tycon                = maybe_arg3_tycon

boxResult :: Type
          -> DsM (Type, CoreExpr -> CoreExpr)

-- Takes the result of the user-level ccall:
--      either (IO t),
--      or maybe just t for a side-effect-free call
-- Returns a wrapper for the primitive ccall itself, along with the
-- type of the result of the primitive ccall.  This result type
-- will be of the form
--      State# RealWorld -> (# State# RealWorld, t' #)
-- where t' is the unwrapped form of t.  If t is simply (), then
-- the result type will be
--      State# RealWorld -> (# State# RealWorld #)

boxResult result_ty
  | Just (io_tycon, io_res_ty) <- tcSplitIOType_maybe result_ty
        -- isIOType_maybe handles the case where the type is a
        -- simple wrapping of IO.  E.g.
        --      newtype Wrap a = W (IO a)
        -- No coercion necessary because its a non-recursive newtype
        -- (If we wanted to handle a *recursive* newtype too, we'd need
        -- another case, and a coercion.)
        -- The result is IO t, so wrap the result in an IO constructor
  = do  { res <- resultWrapper io_res_ty
        ; let extra_result_tys
                = case res of
                     (Just ty,_)
                       | isUnboxedTupleType ty
                       -> let Just ls = tyConAppArgs_maybe ty in tail ls
                     _ -> []

              return_result state anss
                = mkCoreUbxTup
                    (realWorldStatePrimTy : io_res_ty : extra_result_tys)
                    (state : anss)

        ; (ccall_res_ty, the_alt) <- mk_alt return_result res

        ; state_id <- newSysLocalDs realWorldStatePrimTy
        ; let io_data_con = head (tyConDataCons io_tycon)
              toIOCon     = dataConWrapId io_data_con

              wrap the_call =
                              mkApps (Var toIOCon)
                                     [ Type io_res_ty,
                                       Lam state_id $
                                       mkWildCase (App the_call (Var state_id))
                                             ccall_res_ty
                                             (coreAltType the_alt)
                                             [the_alt]
                                     ]

        ; return (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap) }

boxResult result_ty
  = do -- It isn't IO, so do unsafePerformIO
       -- It's not conveniently available, so we inline it
       res <- resultWrapper result_ty
       (ccall_res_ty, the_alt) <- mk_alt return_result res
       let
           wrap = \ the_call -> mkWildCase (App the_call (Var realWorldPrimId))
                                           ccall_res_ty
                                           (coreAltType the_alt)
                                           [the_alt]
       return (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap)
  where
    return_result _ [ans] = ans
    return_result _ _     = panic "return_result: expected single result"


mk_alt :: (Expr Var -> [Expr Var] -> Expr Var)
       -> (Maybe Type, Expr Var -> Expr Var)
       -> DsM (Type, (AltCon, [Id], Expr Var))
mk_alt return_result (Nothing, wrap_result)
  = do -- The ccall returns ()
       state_id <- newSysLocalDs realWorldStatePrimTy
       let
             the_rhs = return_result (Var state_id)
                                     [wrap_result (panic "boxResult")]

             ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy]
             the_alt      = (DataAlt (tupleDataCon Unboxed 1), [state_id], the_rhs)

       return (ccall_res_ty, the_alt)

mk_alt return_result (Just prim_res_ty, wrap_result)
  = -- The ccall returns a non-() value
    ASSERT2( isPrimitiveType prim_res_ty, ppr prim_res_ty )
             -- True because resultWrapper ensures it is so
    do { result_id <- newSysLocalDs prim_res_ty
       ; state_id <- newSysLocalDs realWorldStatePrimTy
       ; let the_rhs = return_result (Var state_id)
                                [wrap_result (Var result_id)]
             ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy, prim_res_ty]
             the_alt      = (DataAlt (tupleDataCon Unboxed 2), [state_id, result_id], the_rhs)
       ; return (ccall_res_ty, the_alt) }


resultWrapper :: Type
              -> DsM (Maybe Type,               -- Type of the expected result, if any
                      CoreExpr -> CoreExpr)     -- Wrapper for the result
-- resultWrapper deals with the result *value*
-- E.g. foreign import foo :: Int -> IO T
-- Then resultWrapper deals with marshalling the 'T' part
-- So if    resultWrapper ty = (Just ty_rep, marshal)
--  then      marshal (e :: ty_rep) :: ty
-- That is, 'marshal' wrape the result returned by the foreign call,
-- of type ty_rep, into the value Haskell expected, of type 'ty'
--
-- Invariant: ty_rep is always a primitive type
--            i.e. (isPrimitiveType ty_rep) is True

resultWrapper result_ty
  -- Base case 1: primitive types
  | isPrimitiveType result_ty
  = return (Just result_ty, \e -> e)

  -- Base case 2: the unit type ()
  | Just (tc,_) <- maybe_tc_app
  , tc `hasKey` unitTyConKey
  = return (Nothing, \_ -> Var unitDataConId)

  -- Base case 3: the boolean type
  | Just (tc,_) <- maybe_tc_app
  , tc `hasKey` boolTyConKey
  = do { dflags <- getDynFlags
       ; let marshal_bool e
               = mkWildCase e intPrimTy boolTy
                   [ (DEFAULT                    ,[],Var trueDataConId )
                   , (LitAlt (mkMachInt dflags 0),[],Var falseDataConId)]
       ; return (Just intPrimTy, marshal_bool) }

  -- Newtypes
  | Just (co, rep_ty) <- topNormaliseNewType_maybe result_ty
  = do { (maybe_ty, wrapper) <- resultWrapper rep_ty
       ; return (maybe_ty, \e -> mkCastDs (wrapper e) (mkSymCo co)) }

  -- The type might contain foralls (eg. for dummy type arguments,
  -- referring to 'Ptr a' is legal).
  | Just (tyvar, rest) <- splitForAllTy_maybe result_ty
  = do { (maybe_ty, wrapper) <- resultWrapper rest
       ; return (maybe_ty, \e -> Lam tyvar (wrapper e)) }

  -- Data types with a single constructor, which has a single arg
  -- This includes types like Ptr and ForeignPtr
  | Just (tycon, tycon_arg_tys) <- maybe_tc_app
  , Just data_con <- isDataProductTyCon_maybe tycon  -- One constructor, no existentials
  , [unwrapped_res_ty] <- dataConInstOrigArgTys data_con tycon_arg_tys  -- One argument
  = do { dflags <- getDynFlags
       ; (maybe_ty, wrapper) <- resultWrapper unwrapped_res_ty
       ; let narrow_wrapper = maybeNarrow dflags tycon
             marshal_con e  = Var (dataConWrapId data_con)
                              `mkTyApps` tycon_arg_tys
                              `App` wrapper (narrow_wrapper e)
       ; return (maybe_ty, marshal_con) }

  | otherwise
  = pprPanic "resultWrapper" (ppr result_ty)
  where
    maybe_tc_app = splitTyConApp_maybe result_ty

-- When the result of a foreign call is smaller than the word size, we
-- need to sign- or zero-extend the result up to the word size.  The C
-- standard appears to say that this is the responsibility of the
-- caller, not the callee.

maybeNarrow :: DynFlags -> TyCon -> (CoreExpr -> CoreExpr)
maybeNarrow dflags tycon
  | tycon `hasKey` int8TyConKey   = \e -> App (Var (mkPrimOpId Narrow8IntOp)) e
  | tycon `hasKey` int16TyConKey  = \e -> App (Var (mkPrimOpId Narrow16IntOp)) e
  | tycon `hasKey` int32TyConKey
         && wORD_SIZE dflags > 4         = \e -> App (Var (mkPrimOpId Narrow32IntOp)) e

  | tycon `hasKey` word8TyConKey  = \e -> App (Var (mkPrimOpId Narrow8WordOp)) e
  | tycon `hasKey` word16TyConKey = \e -> App (Var (mkPrimOpId Narrow16WordOp)) e
  | tycon `hasKey` word32TyConKey
         && wORD_SIZE dflags > 4         = \e -> App (Var (mkPrimOpId Narrow32WordOp)) e
  | otherwise                     = id

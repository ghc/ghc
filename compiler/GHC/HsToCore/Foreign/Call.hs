{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998


Desugaring foreign calls
-}



{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.HsToCore.Foreign.Call
   ( dsCCall
   , mkFCall
   , unboxArg
   , boxResult
   , resultWrapper
   )
where

import GHC.Prelude

import GHC.Core

import GHC.HsToCore.Monad
import GHC.Core.Utils
import GHC.Core.Make
import GHC.Types.SourceText
import GHC.Types.Id.Make
import GHC.Types.ForeignCall
import GHC.Core.DataCon
import GHC.HsToCore.Utils

import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.Coercion
import GHC.Builtin.Types.Prim
import GHC.Core.TyCon
import GHC.Builtin.Types
import GHC.Types.Basic
import GHC.Types.Literal
import GHC.Builtin.Names
import GHC.Driver.DynFlags
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Maybe
import GHC.Types.RepType (typePrimRep1)

{-
Desugaring of @ccall@s consists of adding some state manipulation,
unboxing any boxed primitive arguments and boxing the result if
desired.

The state stuff just consists of adding in
@PrimIO (\ s -> case s of { State# s# -> ... })@ in an appropriate place.

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
        -- Precondition: none have representation-polymorphic types
        -> Safety       -- Safety of the call
        -> Type         -- Type of the result: IO t
        -> DsM CoreExpr -- Result, of type ???

dsCCall lbl args may_gc result_ty
  = do (unboxed_args, arg_wrappers) <- mapAndUnzipM unboxArg args
       (ccall_result_ty, res_wrapper) <- boxResult result_ty
       uniq <- newUnique
       let
           target = StaticTarget NoSourceText lbl Nothing True
           the_fcall    = CCall (CCallSpec target CCallConv may_gc)
           the_prim_app = mkFCall uniq the_fcall unboxed_args ccall_result_ty
       return (foldr ($) (res_wrapper the_prim_app) arg_wrappers)

mkFCall :: Unique -> ForeignCall
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
mkFCall uniq the_fcall val_args res_ty
  = assert (all isTyVar tyvars) $ -- this must be true because the type is top-level
    mkApps (mkVarApps (Var the_fcall_id) tyvars) val_args
  where
    arg_tys = map exprType val_args
    body_ty = (mkVisFunTysMany arg_tys res_ty)
    tyvars  = tyCoVarsOfTypeWellScoped body_ty
    ty      = mkInfForAllTys tyvars body_ty
    the_fcall_id = mkFCallId uniq the_fcall ty

unboxArg :: CoreExpr                    -- The supplied argument, not representation-polymorphic
         -> DsM (CoreExpr,              -- To pass as the actual argument
                 CoreExpr -> CoreExpr   -- Wrapper to unbox the arg
                )
-- Example: if the arg is e::Int, unboxArg will return
--      (x#::Int#, \W. case x of I# x# -> W)
-- where W is a CoreExpr that probably mentions x#

-- always returns a non-representation-polymorphic expression

unboxArg arg
  -- Primitive types: nothing to unbox
  | isPrimitiveType arg_ty ||
    -- Same for (# #)
    (isUnboxedTupleType arg_ty && typePrimRep1 arg_ty == VoidRep)
  = return (arg, \body -> body)

  -- Recursive newtypes
  | Just(co, _rep_ty) <- topNormaliseNewType_maybe arg_ty
  = unboxArg (mkCastDs arg co)

  -- Booleans
  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` boolTyConKey
  = do dflags <- getDynFlags
       let platform = targetPlatform dflags
       prim_arg <- newSysLocalMDs intPrimTy
       return (Var prim_arg,
              \ body -> Case (mkIfThenElse arg (mkIntLit platform 1) (mkIntLit platform 0))
                             prim_arg
                             (exprType body)
                             [Alt DEFAULT [] body])

  -- Data types with a single constructor, which has a single, primitive-typed arg
  -- This deals with Int, Float etc; also Ptr, ForeignPtr
  | is_product_type && data_con_arity == 1
  = assertPpr (isUnliftedType data_con_arg_ty1) (pprType arg_ty) $
                        -- Typechecker ensures this
    do case_bndr <- newSysLocalMDs arg_ty
       prim_arg <- newSysLocalMDs data_con_arg_ty1
       return (Var prim_arg,
               \ body -> Case arg case_bndr (exprType body) [Alt (DataAlt data_con) [prim_arg] body]
              )

  -- Byte-arrays, both mutable and otherwise; hack warning
  -- We're looking for values of type ByteArray, MutableByteArray
  --    data ByteArray          ix = ByteArray        ix ix ByteArray#
  --    data MutableByteArray s ix = MutableByteArray ix ix (MutableByteArray# s)
  | is_product_type &&
    data_con_arity == 3,
    Just arg3_tycon <- maybe_arg3_tycon,
    (arg3_tycon ==  byteArrayPrimTyCon ||
     arg3_tycon ==  mutableByteArrayPrimTyCon)
  = do case_bndr <- newSysLocalMDs arg_ty
       vars@[_l_var, _r_var, arr_cts_var] <- newSysLocalsDs (map unrestricted data_con_arg_tys)
       return (Var arr_cts_var,
               \ body -> Case arg case_bndr (exprType body) [Alt (DataAlt data_con) vars body]
              )

  | otherwise
  = do l <- getSrcSpanDs
       pprPanic "unboxArg: " (ppr l <+> ppr arg_ty)
  where
    arg_ty                                      = exprType arg
    maybe_product_type                          = splitDataProductType_maybe arg_ty
    is_product_type                             = isJust maybe_product_type
    Just (_, _, data_con, scaled_data_con_arg_tys) = maybe_product_type
    data_con_arg_tys                            = map scaledThing scaled_data_con_arg_tys
    data_con_arity                              = dataConSourceArity data_con
    (data_con_arg_ty1 : _)                      = data_con_arg_tys

    (_ : _ : data_con_arg_ty3 : _) = data_con_arg_tys
    maybe_arg3_tycon               = tyConAppTyCon_maybe data_con_arg_ty3

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
        ; let return_result state anss = mkCoreUnboxedTuple [state, anss]

        ; (ccall_res_ty, the_alt) <- mk_alt return_result res

        ; state_id <- newSysLocalMDs realWorldStatePrimTy
        ; let io_data_con = head (tyConDataCons io_tycon)
              toIOCon     = dataConWrapId io_data_con

              wrap the_call =
                              mkApps (Var toIOCon)
                                     [ Type io_res_ty,
                                       Lam state_id $
                                       mkWildCase (App the_call (Var state_id))
                                             (unrestricted ccall_res_ty)
                                             (coreAltType the_alt)
                                             [the_alt]
                                     ]

        ; return (realWorldStatePrimTy `mkVisFunTyMany` ccall_res_ty, wrap) }

boxResult result_ty
  = do -- It isn't IO, so do unsafePerformIO
       -- It's not conveniently available, so we inline it
       res <- resultWrapper result_ty
       (ccall_res_ty, the_alt) <- mk_alt return_result res
       let
           wrap = \ the_call -> mkWildCase (App the_call (Var realWorldPrimId))
                                           (unrestricted ccall_res_ty)
                                           (coreAltType the_alt)
                                           [the_alt]
       return (realWorldStatePrimTy `mkVisFunTyMany` ccall_res_ty, wrap)
  where
    return_result _ ans = ans


mk_alt :: (Expr Var -> Expr Var -> Expr Var)
       -> (Maybe Type, Expr Var -> Expr Var)
       -> DsM (Type, CoreAlt)
mk_alt return_result (Nothing, wrap_result)
  = do -- The ccall returns ()
       state_id <- newSysLocalMDs realWorldStatePrimTy
       let
             the_rhs = return_result (Var state_id)
                                     (wrap_result (panic "boxResult"))

             ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy]
             the_alt      = Alt (DataAlt (tupleDataCon Unboxed 1)) [state_id] the_rhs

       return (ccall_res_ty, the_alt)

mk_alt return_result (Just prim_res_ty, wrap_result)
  = -- The ccall returns a non-() value
    assertPpr (isPrimitiveType prim_res_ty) (ppr prim_res_ty) $
             -- True because resultWrapper ensures it is so
    do { result_id <- newSysLocalMDs prim_res_ty
       ; state_id <- newSysLocalMDs realWorldStatePrimTy
       ; let the_rhs = return_result (Var state_id)
                                (wrap_result (Var result_id))
             ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy, prim_res_ty]
             the_alt      = Alt (DataAlt (tupleDataCon Unboxed 2)) [state_id, result_id] the_rhs
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
  = return (Nothing, \_ -> unitExpr)

  -- Base case 3: the boolean type
  | Just (tc,_) <- maybe_tc_app
  , tc `hasKey` boolTyConKey
  = do { dflags <- getDynFlags
       ; let platform = targetPlatform dflags
       ; let marshal_bool e
               = mkWildCase e (unrestricted intPrimTy) boolTy
                   [ Alt DEFAULT                        [] (Var trueDataConId )
                   , Alt (LitAlt (mkLitInt platform 0)) [] (Var falseDataConId)]
       ; return (Just intPrimTy, marshal_bool) }

  -- Newtypes
  | Just (co, rep_ty) <- topNormaliseNewType_maybe result_ty
  = do { (maybe_ty, wrapper) <- resultWrapper rep_ty
       ; return (maybe_ty, \e -> mkCastDs (wrapper e) (mkSymCo co)) }

  -- The type might contain foralls (eg. for dummy type arguments,
  -- referring to 'Ptr a' is legal).
  | Just (tyvar, rest) <- splitForAllTyCoVar_maybe result_ty
  = do { (maybe_ty, wrapper) <- resultWrapper rest
       ; return (maybe_ty, \e -> Lam tyvar (wrapper e)) }

  -- Data types with a single constructor, which has a single arg
  -- This includes types like Ptr and ForeignPtr
  | Just (tycon, tycon_arg_tys) <- maybe_tc_app
  , Just data_con <- tyConSingleAlgDataCon_maybe tycon  -- One constructor
  , null (dataConExTyCoVars data_con)                   -- no existentials
  , [Scaled _ unwrapped_res_ty] <- dataConInstOrigArgTys data_con tycon_arg_tys  -- One argument
  = do { (maybe_ty, wrapper) <- resultWrapper unwrapped_res_ty
       ; let marshal_con e  = Var (dataConWrapId data_con)
                              `mkTyApps` tycon_arg_tys
                              `App` wrapper e
       ; return (maybe_ty, marshal_con) }

  | otherwise
  = pprPanic "resultWrapper" (ppr result_ty)
  where
    maybe_tc_app = splitTyConApp_maybe result_ty

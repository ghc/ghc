%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[DsCCall]{Desugaring \tr{_ccall_}s and \tr{_casm_}s}

\begin{code}
module DsCCall 
	( dsCCall
	, mkCCall
	, unboxArg
	, boxResult
	, resultWrapper
	) where

#include "HsVersions.h"

import CoreSyn

import DsMonad
import DsUtils

import CoreUtils	( exprType, mkCoerce )
import Id		( Id, mkWildId )
import MkId		( mkCCallOpId, realWorldPrimId )
import Maybes		( maybeToBool )
import PrelInfo		( packStringForCId )
import PrimOp		( PrimOp(..), CCall(..), CCallTarget(..) )
import DataCon		( DataCon, splitProductType_maybe, dataConSourceArity, dataConWrapId )
import CallConv
import Type		( isUnLiftedType, splitAlgTyConApp_maybe, mkFunTys,
			  splitTyConApp_maybe, tyVarsOfType, mkForAllTys, 
			  isNewType, repType, isUnLiftedType, mkFunTy,
			  Type
			)
import TysPrim		( byteArrayPrimTy, realWorldStatePrimTy,
			  byteArrayPrimTyCon, mutableByteArrayPrimTyCon, intPrimTy
			)
import TysWiredIn	( unitDataConId, stringTy,
			  unboxedPairDataCon,
			  mkUnboxedTupleTy, unboxedTupleCon,
			  boolTy, trueDataCon, falseDataCon, trueDataConId, falseDataConId,
			  unitTy
			)
import Literal		( mkMachInt )
import CStrings		( CLabelString )
import Unique		( Unique, Uniquable(..), ioTyConKey )
import VarSet		( varSetElems )
import Outputable
\end{code}

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

\begin{code}
dsCCall :: CLabelString	-- C routine to invoke
	-> [CoreExpr]	-- Arguments (desugared)
	-> Bool		-- True <=> might cause Haskell GC
	-> Bool		-- True <=> really a "_casm_"
	-> Type		-- Type of the result: IO t
	-> DsM CoreExpr

dsCCall lbl args may_gc is_asm result_ty
  = mapAndUnzipDs unboxArg args	`thenDs` \ (unboxed_args, arg_wrappers) ->
    boxResult result_ty		`thenDs` \ (ccall_result_ty, res_wrapper) ->
    getUniqueDs			`thenDs` \ uniq ->
    let
	the_ccall    = CCall (StaticTarget lbl) is_asm may_gc cCallConv
 	the_prim_app = mkCCall uniq the_ccall unboxed_args ccall_result_ty
    in
    returnDs (foldr ($) (res_wrapper the_prim_app) arg_wrappers)

mkCCall :: Unique -> CCall 
	-> [CoreExpr] 	-- Args
	-> Type 	-- Result type
	-> CoreExpr
-- Construct the ccall.  The only tricky bit is that the ccall Id should have
-- no free vars, so if any of the arg tys do we must give it a polymorphic type.
-- 	[I forget *why* it should have no free vars!]
-- For example:
--	mkCCall ... [s::StablePtr (a->b), x::Addr, c::Char]
--
-- Here we build a ccall thus
--	(ccallid::(forall a b.  StablePtr (a -> b) -> Addr -> Char -> IO Addr))
--			a b s x c
mkCCall uniq the_ccall val_args res_ty
  = mkApps (mkVarApps (Var the_ccall_id) tyvars) val_args
  where
    arg_tys = map exprType val_args
    body_ty = (mkFunTys arg_tys res_ty)
    tyvars  = varSetElems (tyVarsOfType body_ty)
    ty 	    = mkForAllTys tyvars body_ty
    the_ccall_id = mkCCallOpId uniq the_ccall ty
\end{code}

\begin{code}
unboxArg :: CoreExpr			-- The supplied argument
	 -> DsM (CoreExpr,		-- To pass as the actual argument
		 CoreExpr -> CoreExpr	-- Wrapper to unbox the arg
		)
-- Example: if the arg is e::Int, unboxArg will return
--	(x#::Int#, \W. case x of I# x# -> W)
-- where W is a CoreExpr that probably mentions x#

unboxArg arg
  -- Unlifted types: nothing to unbox
  | isUnLiftedType arg_ty
  = returnDs (arg, \body -> body)

  -- Newtypes
  | isNewType arg_ty
  = unboxArg (mkCoerce (repType arg_ty) arg_ty arg)
      
  -- Booleans
  | arg_ty == boolTy
  = newSysLocalDs intPrimTy		`thenDs` \ prim_arg ->
    returnDs (Var prim_arg,
	      \ body -> Case (Case arg (mkWildId arg_ty)
  		                       [(DataAlt falseDataCon,[],mkIntLit 0),
	                                (DataAlt trueDataCon, [],mkIntLit 1)])
                             prim_arg 
			     [(DEFAULT,[],body)])

  -- Data types with a single constructor, which has a single, primitive-typed arg
  -- This deals with Int, Float etc
  | is_product_type && data_con_arity == 1 
  = ASSERT(isUnLiftedType data_con_arg_ty1 )	-- Typechecker ensures this
    newSysLocalDs arg_ty		`thenDs` \ case_bndr ->
    newSysLocalDs data_con_arg_ty1	`thenDs` \ prim_arg ->
    returnDs (Var prim_arg,
	      \ body -> Case arg case_bndr [(DataAlt data_con,[prim_arg],body)]
    )

  -- Byte-arrays, both mutable and otherwise; hack warning
  | is_product_type &&
    data_con_arity == 3 &&
    maybeToBool maybe_arg3_tycon &&
    (arg3_tycon ==  byteArrayPrimTyCon ||
     arg3_tycon ==  mutableByteArrayPrimTyCon)
    -- and, of course, it is an instance of CCallable
  = newSysLocalDs arg_ty		`thenDs` \ case_bndr ->
    newSysLocalsDs data_con_arg_tys	`thenDs` \ vars@[l_var, r_var, arr_cts_var] ->
    returnDs (Var arr_cts_var,
	      \ body -> Case arg case_bndr [(DataAlt data_con,vars,body)]
    )

  | otherwise
  = getSrcLocDs `thenDs` \ l ->
    pprPanic "unboxArg: " (ppr l <+> ppr arg_ty)
  where
    arg_ty     = exprType arg
    arg_rep_ty = repType arg_ty

    maybe_product_type 			   	  = splitProductType_maybe arg_ty
    is_product_type			   	  = maybeToBool maybe_product_type
    Just (tycon, _, data_con, data_con_arg_tys)   = maybe_product_type
    data_con_arity				  = dataConSourceArity data_con
    (data_con_arg_ty1 : _)			  = data_con_arg_tys

    (_ : _ : data_con_arg_ty3 : _) = data_con_arg_tys
    maybe_arg3_tycon    	   = splitTyConApp_maybe data_con_arg_ty3
    Just (arg3_tycon,_)		   = maybe_arg3_tycon
\end{code}


\begin{code}
boxResult :: Type -> DsM (Type, CoreExpr -> CoreExpr)

-- Takes the result of the user-level ccall: 
--	either (IO t), 
--	or maybe just t for an side-effect-free call
-- Returns a wrapper for the primitive ccall itself, along with the
-- type of the result of the primitive ccall.  This result type
-- will be of the form  
--	State# RealWorld -> (# State# RealWorld, t' #)
-- where t' is the unwrapped form of t.  If t is simply (), then
-- the result type will be 
--	State# RealWorld -> (# State# RealWorld #)

boxResult result_ty
  = case splitAlgTyConApp_maybe result_ty of

	-- The result is IO t, so wrap the result in an IO constructor
	Just (io_tycon, [io_res_ty], [io_data_con]) | getUnique io_tycon == ioTyConKey
		-> mk_alt return_result 
			  (resultWrapper io_res_ty)	`thenDs` \ (ccall_res_ty, the_alt) ->
		   newSysLocalDs realWorldStatePrimTy	 `thenDs` \ state_id ->
		   let
			wrap = \ the_call -> mkApps (Var (dataConWrapId io_data_con))
						    [Type io_res_ty, Lam state_id $
								     Case (App the_call (Var state_id))
									  (mkWildId ccall_res_ty)
									  [the_alt]]
		   in
		   returnDs (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap)
		where
		   return_result state ans = mkConApp unboxedPairDataCon 
						      [Type realWorldStatePrimTy, Type io_res_ty, 
						       state, ans]

	-- It isn't, so do unsafePerformIO
	-- It's not conveniently available, so we inline it
	other -> mk_alt return_result
			(resultWrapper result_ty) 	`thenDs` \ (ccall_res_ty, the_alt) ->
		 let
		    wrap = \ the_call -> Case (App the_call (Var realWorldPrimId)) 
					      (mkWildId ccall_res_ty)
					      [the_alt]
		 in
		 returnDs (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap)
	      where
		 return_result state ans = ans
  where
    mk_alt return_result (Nothing, wrap_result)
	= 	-- The ccall returns ()
	  newSysLocalDs realWorldStatePrimTy	`thenDs` \ state_id ->
	  let
		the_rhs      = return_result (Var state_id) (wrap_result (panic "boxResult"))
		ccall_res_ty = mkUnboxedTupleTy 1 [realWorldStatePrimTy]
		the_alt      = (DataAlt (unboxedTupleCon 1), [state_id], the_rhs)
	  in
	  returnDs (ccall_res_ty, the_alt)

    mk_alt return_result (Just prim_res_ty, wrap_result)
	=	-- The ccall returns a non-() value
	  newSysLocalDs realWorldStatePrimTy	`thenDs` \ state_id ->
	  newSysLocalDs prim_res_ty 		`thenDs` \ result_id ->
	  let
		the_rhs      = return_result (Var state_id) (wrap_result (Var result_id))
		ccall_res_ty = mkUnboxedTupleTy 2 [realWorldStatePrimTy, prim_res_ty]
		the_alt	     = (DataAlt unboxedPairDataCon, [state_id, result_id], the_rhs)
	  in
	  returnDs (ccall_res_ty, the_alt)


resultWrapper :: Type
   	      -> (Maybe Type,		-- Type of the expected result, if any
		  CoreExpr -> CoreExpr)	-- Wrapper for the result 
resultWrapper result_ty
  -- Base case 1: primitive types
  | isUnLiftedType result_ty
  = (Just result_ty, \e -> e)

  -- Base case 1: the unit type ()
  | result_ty == unitTy
  = (Nothing, \e -> Var unitDataConId)

  | result_ty == boolTy
  = (Just intPrimTy, \e -> Case e (mkWildId intPrimTy)
	                          [(LitAlt (mkMachInt 0),[],Var falseDataConId),
	                           (DEFAULT             ,[],Var trueDataConId )])

  -- Data types with a single constructor, which has a single arg
  | is_product_type && data_con_arity == 1
  = let
        (maybe_ty, wrapper)    = resultWrapper unwrapped_res_ty
	(unwrapped_res_ty : _) = data_con_arg_tys
    in
    (maybe_ty, \e -> mkApps (Var (dataConWrapId data_con)) 
			    (map Type tycon_arg_tys ++ [wrapper e]))

  -- newtypes
  | isNewType result_ty
  = let
	rep_ty		    = repType result_ty
        (maybe_ty, wrapper) = resultWrapper rep_ty
    in
    (maybe_ty, \e -> mkCoerce result_ty rep_ty (wrapper e))

  | otherwise
  = pprPanic "resultWrapper" (ppr result_ty)
  where
    maybe_product_type 					    = splitProductType_maybe result_ty
    is_product_type					    = maybeToBool maybe_product_type
    Just (tycon, tycon_arg_tys, data_con, data_con_arg_tys) = maybe_product_type
    data_con_arity					    = dataConSourceArity data_con
\end{code}

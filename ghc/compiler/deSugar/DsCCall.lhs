%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[DsCCall]{Desugaring \tr{_ccall_}s and \tr{_casm_}s}

\begin{code}
module DsCCall ( dsCCall ) where

#include "HsVersions.h"

import CoreSyn

import DsMonad
import DsUtils

import TcHsSyn		( maybeBoxedPrimType )
import CoreUtils	( coreExprType )
import Id		( Id(..), dataConArgTys, idType )
import Maybes		( maybeToBool )
import PrelVals		( packStringForCId )
import PrimOp		( PrimOp(..) )
import Type		( isUnpointedType, splitAlgTyConApp_maybe, 
			  splitTyConApp_maybe, splitFunTys, splitForAllTys,
			  Type
			)
import TyCon		( tyConDataCons )
import TysPrim		( byteArrayPrimTy, realWorldStatePrimTy,
			  byteArrayPrimTyCon, mutableByteArrayPrimTyCon )
import TysWiredIn	( getStatePairingConInfo,
			  unitDataCon, stringTy,
			  realWorldStateTy, stateDataCon
			)
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
dsCCall :: FAST_STRING	-- C routine to invoke
	-> [CoreExpr]	-- Arguments (desugared)
	-> Bool		-- True <=> might cause Haskell GC
	-> Bool		-- True <=> really a "_casm_"
	-> Type		-- Type of the result (a boxed-prim IO type)
	-> DsM CoreExpr

dsCCall label args may_gc is_asm io_result_ty
  = newSysLocalDs realWorldStatePrimTy	`thenDs` \ old_s ->

    mapAndUnzipDs unboxArg args	`thenDs` \ (unboxed_args, arg_wrappers) ->
    let
	 final_args = Var old_s : unboxed_args
	 (ioOkDataCon, result_ty) = getIoOkDataCon io_result_ty
    in

    boxResult ioOkDataCon result_ty `thenDs` \ (final_result_ty, res_wrapper) ->

    let
	the_ccall_op = CCallOp label is_asm may_gc
			       (map coreExprType final_args)
			       final_result_ty
    in
    mkPrimDs the_ccall_op (map VarArg final_args) `thenDs` \ the_prim_app ->
    let
	the_body = foldr ($) (res_wrapper the_prim_app) arg_wrappers
    in
    returnDs (Lam (ValBinder old_s) the_body)
\end{code}

\begin{code}
unboxArg :: CoreExpr			-- The supplied argument
	 -> DsM (CoreExpr,		-- To pass as the actual argument
		 CoreExpr -> CoreExpr	-- Wrapper to unbox the arg
		)
unboxArg arg

  -- Primitive types
  -- ADR Question: can this ever be used?  None of the PrimTypes are
  -- instances of the CCallable class.
  --
  -- SOF response:
  --    Oh yes they are, I've just added them :-) Having _ccall_ and _casm_
  --  that accept unboxed arguments is a Good Thing if you have a stub generator
  --  which generates the boiler-plate box-unbox code for you, i.e., it may help
  --  us nuke this very module :-)
  --
  | isUnpointedType arg_ty
  = returnDs (arg, \body -> body)

  -- Strings
  | arg_ty == stringTy
  -- ToDo (ADR): - allow synonyms of Strings too?
  = newSysLocalDs byteArrayPrimTy		`thenDs` \ prim_arg ->
    mkAppDs (Var packStringForCId) [VarArg arg]	`thenDs` \ pack_appn ->
    returnDs (Var prim_arg,
	      \body -> Case pack_appn (PrimAlts []
						    (BindDefault prim_arg body))
    )

  | null data_cons
    -- oops: we can't see the data constructors!!!
  = can't_see_datacons_error "argument" arg_ty

  -- Byte-arrays, both mutable and otherwise; hack warning
  | is_data_type &&
    length data_con_arg_tys == 2 &&
    maybeToBool maybe_arg2_tycon &&
    (arg2_tycon ==  byteArrayPrimTyCon ||
     arg2_tycon ==  mutableByteArrayPrimTyCon)
    -- and, of course, it is an instance of CCallable
  = newSysLocalsDs data_con_arg_tys		`thenDs` \ vars@[ixs_var, arr_cts_var] ->
    returnDs (Var arr_cts_var,
	      \ body -> Case arg (AlgAlts [(the_data_con,vars,body)]
					      NoDefault)
    )

  -- Data types with a single constructor, which has a single, primitive-typed arg
  | maybeToBool maybe_boxed_prim_arg_ty
  = newSysLocalDs the_prim_arg_ty		`thenDs` \ prim_arg ->
    returnDs (Var prim_arg,
	      \ body -> Case arg (AlgAlts [(box_data_con,[prim_arg],body)]
					      NoDefault)
    )

  | otherwise
  = getSrcLocDs `thenDs` \ l ->
    pprPanic "unboxArg: " (ppr l <+> ppr arg_ty)
  where
    arg_ty = coreExprType arg

    maybe_boxed_prim_arg_ty = maybeBoxedPrimType arg_ty
    (Just (box_data_con, the_prim_arg_ty)) = maybe_boxed_prim_arg_ty

    maybe_data_type 			   = splitAlgTyConApp_maybe arg_ty
    is_data_type			   = maybeToBool maybe_data_type
    (Just (tycon, tycon_arg_tys, data_cons)) = maybe_data_type
    (the_data_con : other_data_cons)       = data_cons

    data_con_arg_tys = dataConArgTys the_data_con tycon_arg_tys
    (data_con_arg_ty1 : data_con_arg_ty2 : _) = data_con_arg_tys

    maybe_arg2_tycon = splitTyConApp_maybe data_con_arg_ty2
    Just (arg2_tycon,_) = maybe_arg2_tycon

can't_see_datacons_error thing ty
  = pprPanic "ERROR: Can't see the data constructor(s) for _ccall_/_casm_ "
	     (hcat [text thing, text "; type: ", ppr ty, text "(try compiling with -fno-prune-tydecls ..)\n"])
\end{code}


\begin{code}
boxResult :: Id				-- IOok constructor
	  -> Type			-- Type of desired result
	  -> DsM (Type,			-- Type of the result of the ccall itself
		  CoreExpr -> CoreExpr)	-- Wrapper for the ccall
					-- to box the result
boxResult ioOkDataCon result_ty
  | null data_cons
  -- oops! can't see the data constructors
  = can't_see_datacons_error "result" result_ty

  -- Data types with a single constructor, which has a single, primitive-typed arg
  | (maybeToBool maybe_data_type) &&				-- Data type
    (null other_data_cons) &&					-- Just one constr
    not (null data_con_arg_tys) && null other_args_tys	&& 	-- Just one arg
    isUnpointedType the_prim_result_ty				-- of primitive type
  =
    newSysLocalDs realWorldStatePrimTy		`thenDs` \ prim_state_id ->
    newSysLocalDs the_prim_result_ty 		`thenDs` \ prim_result_id ->

    mkConDs the_data_con (map TyArg tycon_arg_tys ++ [VarArg (Var prim_result_id)]) `thenDs` \ the_result ->

    mkConDs ioOkDataCon
	    [TyArg result_ty, VarArg (Var prim_state_id), VarArg the_result]
							`thenDs` \ the_pair ->
    let
	the_alt = (state_and_prim_datacon, [prim_state_id, prim_result_id], the_pair)
    in
    returnDs (state_and_prim_ty,
	      \prim_app -> Case prim_app (AlgAlts [the_alt] NoDefault)
    )

  -- Data types with a single nullary constructor
  | (maybeToBool maybe_data_type) &&				-- Data type
    (null other_data_cons) &&					-- Just one constr
    (null data_con_arg_tys)
  =
    newSysLocalDs realWorldStatePrimTy		`thenDs` \ prim_state_id ->

    mkConDs ioOkDataCon
	    [TyArg result_ty, VarArg (Var prim_state_id), VarArg (Var unitDataCon)]
						`thenDs` \ the_pair ->

    let
	the_alt  = (stateDataCon, [prim_state_id], the_pair)
    in
    returnDs (realWorldStateTy,
	      \prim_app -> Case prim_app (AlgAlts [the_alt] NoDefault)
    )

  | otherwise
  = pprPanic "boxResult: " (ppr result_ty)

  where
    maybe_data_type 			   = splitAlgTyConApp_maybe result_ty
    Just (tycon, tycon_arg_tys, data_cons) = maybe_data_type
    (the_data_con : other_data_cons)       = data_cons

    data_con_arg_tys		           = dataConArgTys the_data_con tycon_arg_tys
    (the_prim_result_ty : other_args_tys)  = data_con_arg_tys

    (state_and_prim_datacon, state_and_prim_ty) = getStatePairingConInfo the_prim_result_ty
\end{code}

This grimy bit of code is for digging out the IOok constructor from an
application of the the IO type.  The constructor is needed for
wrapping the result of a _ccall_.  The alternative is to wire-in IO,
which brings a whole heap of junk with it.

If the representation of IO changes, this will probably have to be
brought in line with the new definition.

newtype IO a = IO (State# RealWorld -> IOResult a)

the constructor IO has type (State# RealWorld -> IOResult a) -> IO a

\begin{code}
getIoOkDataCon :: Type 		-- IO t
	       -> (Id,Type)	-- Returns (IOok, t)

getIoOkDataCon io_ty
  = let 
  	Just (ioTyCon, [t]) 	        = splitTyConApp_maybe io_ty
  	[ioDataCon]    			= tyConDataCons ioTyCon
	ioDataConTy			= idType ioDataCon
	(_, ioDataConTy')               = splitForAllTys ioDataConTy
	([arg_ty], _) 		        = splitFunTys ioDataConTy'
	(_, io_result_ty)		= splitFunTys arg_ty
	Just (io_result_tycon, _)	= splitTyConApp_maybe io_result_ty
	[ioOkDataCon,ioFailDataCon]     = tyConDataCons io_result_tycon
    in
    (ioOkDataCon, t)
\end{code}

Another way to do it, more sensitive:

     case ioDataConTy of
	ForAll _ (FunTy (FunTy _ (AppTy (TyConTy ioResultTyCon _) _)) _) ->
		let [ioOkDataCon,ioFailDataCon] = tyConDataCons ioResultTyCon
		in
		(ioOkDataCon, result_ty)
	_ -> pprPanic "getIoOkDataCon: " (ppr PprDebug ioDataConTy)

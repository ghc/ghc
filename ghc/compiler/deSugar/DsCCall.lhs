%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[DsCCall]{Desugaring \tr{_ccall_}s and \tr{_casm_}s}

\begin{code}
#include "HsVersions.h"

module DsCCall ( dsCCall ) where

IMPORT_Trace

import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring
import DsMonad		-- the monadery used in the desugarer

import AbsPrel
import TysPrim		-- ****** ToDo: PROPERLY
import TysWiredIn
import AbsUniType
import DsUtils
import Id		( getInstantiatedDataConSig, mkTupleCon, DataCon(..) )
import Maybes		( maybeToBool, Maybe(..) )
import Pretty
#if USE_ATTACK_PRAGMAS
import Unique
#endif
import Util
\end{code}

Desugaring of @ccall@s consists of adding some state manipulation,
unboxing any boxed primitive arguments and boxing the result if
desired.

The state stuff just consists of adding in
@\ s -> case s of { S# s# -> ... }@ in an appropriate place.

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
dsCCall :: FAST_STRING		-- C routine to invoke
	-> [PlainCoreExpr]	-- Arguments (desugared)
	-> Bool			-- True <=> might cause Haskell GC
	-> Bool			-- True <=> really a "_casm_"
	-> UniType		-- Type of the result (a boxed-prim type)
	-> DsM PlainCoreExpr

dsCCall label args may_gc is_asm result_ty
  = newSysLocalDs realWorldStateTy	`thenDs` \ old_s ->

    mapAndUnzipDs unboxArg (CoVar old_s : args)	`thenDs` \ (final_args, arg_wrappers) ->

    boxResult result_ty				`thenDs` \ (final_result_ty, res_wrapper) ->

    let
	the_ccall_op = CCallOp label is_asm may_gc
			       (map typeOfCoreExpr final_args)
			       final_result_ty
    in
    mkCoPrimDs the_ccall_op
	       [] -- ***NOTE*** no ty apps; the types are inside the_ccall_op.
	       final_args	`thenDs` \ the_prim_app ->
    let
	the_body = foldr apply (res_wrapper the_prim_app) arg_wrappers
    in
    returnDs (CoLam [old_s] the_body)
  where
    apply f x = f x
\end{code}

\begin{code}
unboxArg :: PlainCoreExpr			-- The supplied argument
	 -> DsM (PlainCoreExpr,			-- To pass as the actual argument
		 PlainCoreExpr -> PlainCoreExpr	-- Wrapper to unbox the arg
		)
unboxArg arg

  -- Primitive types
  -- ADR Question: can this ever be used?  None of the PrimTypes are
  -- instances of the _CCallable class.
  | isPrimType arg_ty 
  = returnDs (arg, \body -> body)

  -- Strings
  | arg_ty == stringTy
  -- ToDo (ADR): - allow synonyms of Strings too?
  = newSysLocalDs byteArrayPrimTy		`thenDs` \ prim_arg ->
    mkCoAppDs (CoVar packStringForCId) arg	`thenDs` \ pack_appn ->
    returnDs (CoVar prim_arg,
	      \body -> CoCase pack_appn (CoPrimAlts [] 
						    (CoBindDefault prim_arg body))
    )

  | null data_cons
    -- oops: we can't see the data constructors!!!
  = can't_see_datacons_error "argument" arg_ty

  -- Byte-arrays, both mutable and otherwise
  -- (HACKy method -- but we really don't want the TyCons wired-in...) [WDP 94/10]
  | is_data_type && 
    length data_con_arg_tys == 2 &&
    not (isPrimType data_con_arg_ty1) &&
    isPrimType data_con_arg_ty2
    -- and, of course, it is an instance of _CCallable
--  ( tycon == byteArrayTyCon || 
--    tycon == mutableByteArrayTyCon )
  = newSysLocalsDs data_con_arg_tys		`thenDs` \ vars@[ixs_var, arr_cts_var] ->
    returnDs (CoVar arr_cts_var,
	      \ body -> CoCase arg (CoAlgAlts [(the_data_con,vars,body)] 
					      CoNoDefault)
    )

  -- Data types with a single constructor, which has a single, primitive-typed arg
  | maybeToBool maybe_boxed_prim_arg_ty
  = newSysLocalDs the_prim_arg_ty		`thenDs` \ prim_arg ->
    returnDs (CoVar prim_arg,
	      \ body -> CoCase arg (CoAlgAlts [(box_data_con,[prim_arg],body)] 
					      CoNoDefault)
    )
  -- ... continued below ....
\end{code}

As an experiment, I'm going to unpack any "acceptably small"
enumeration.  This code will never get used in the main version
because enumerations would have triggered type errors but I've
disabled type-checking in my version. ADR

To Will: It might be worth leaving this in (but commented out) until
we decide what's happening with enumerations. ADR

\begin{code}
#if 0
  -- MAYBE LATER:
  -- Data types with a nullary constructors (enumeration)
  | isEnumerationType arg_ty  &&				-- enumeration
    (length data_cons) <= 5					-- "acceptably short"
  = newSysLocalDs the_prim_arg_ty		`thenDs` \ prim_arg ->

    let
      alts = [ (con, [], mkMachInt i) | (con,i) <- data_cons `zip` [0..] ]
      arg_tag = CoCase arg (CoAlgAlts alts) CoNoDefault
    in

    returnDs (CoVar prim_arg,
	      \ body -> CoCase arg_tag (CoPrimAlts [(prim_arg, body)] CoNoDefault)
    )
#endif
\end{code}

\begin{code}
  -- ... continued from above ....
  | otherwise
  = pprPanic "unboxArg: " (ppr PprDebug arg_ty)
  where
    arg_ty = typeOfCoreExpr arg

    maybe_boxed_prim_arg_ty = maybeBoxedPrimType arg_ty
    (Just (box_data_con, the_prim_arg_ty)) = maybe_boxed_prim_arg_ty

    maybe_data_type 			   = getUniDataTyCon_maybe arg_ty
    is_data_type			   = maybeToBool maybe_data_type
    (Just (tycon, tycon_arg_tys, data_cons)) = maybe_data_type
    (the_data_con : other_data_cons)       = data_cons

    (_, data_con_arg_tys, _) = getInstantiatedDataConSig the_data_con tycon_arg_tys
    (data_con_arg_ty1 : data_con_arg_ty2 : _) = data_con_arg_tys

can't_see_datacons_error thing ty
  = error (ppShow 100 (ppBesides [ppStr "ERROR: Can't see the data constructor(s) for _ccall_/_casm_ ", ppStr thing, ppStr "; type: ", ppr PprForUser ty]))
\end{code}


\begin{code}
tuple_con_2 = mkTupleCon 2 -- out here to avoid CAF (sigh)
covar_tuple_con_0 = CoVar (mkTupleCon 0) -- ditto

boxResult :: UniType				-- Type of desired result
	  -> DsM (UniType,			-- Type of the result of the ccall itself
		  PlainCoreExpr -> PlainCoreExpr)	-- Wrapper for the ccall 
							-- to box the result
boxResult result_ty
  | null data_cons
  -- oops! can't see the data constructors
  = can't_see_datacons_error "result" result_ty

  -- Data types with a single constructor, which has a single, primitive-typed arg
  | (maybeToBool maybe_data_type) &&				-- Data type
    (null other_data_cons) &&					-- Just one constr
    not (null data_con_arg_tys) && null other_args_tys	&& 	-- Just one arg
    isPrimType the_prim_result_ty				-- of primitive type
  = 
    newSysLocalDs realWorldStatePrimTy				`thenDs` \ prim_state_id ->
    newSysLocalDs the_prim_result_ty 				`thenDs` \ prim_result_id ->

    mkCoConDs stateDataCon [realWorldTy] [CoVar prim_state_id]	`thenDs` \ new_state ->
    mkCoConDs the_data_con tycon_arg_tys   [CoVar prim_result_id]	`thenDs` \ the_result ->
    
    mkCoConDs tuple_con_2
	      [result_ty, realWorldStateTy]
	      [the_result, new_state]				`thenDs` \ the_pair ->
    let
	the_alt = (state_and_prim_datacon, [prim_state_id, prim_result_id], the_pair)
    in
    returnDs (state_and_prim_ty,
	      \prim_app -> CoCase prim_app (CoAlgAlts [the_alt] CoNoDefault)
    )

  -- Data types with a single nullary constructor
  | (maybeToBool maybe_data_type) &&				-- Data type
    (null other_data_cons) &&					-- Just one constr
    (null data_con_arg_tys)
  = 
    newSysLocalDs realWorldStatePrimTy				`thenDs` \ prim_state_id ->

    mkCoConDs stateDataCon [realWorldTy] [CoVar prim_state_id]	`thenDs` \ new_state ->
    
    mkCoConDs tuple_con_2
	      [result_ty, realWorldStateTy]
	      [covar_tuple_con_0, new_state]	`thenDs` \ the_pair ->

    let
	the_alt  = (stateDataCon, [prim_state_id], the_pair)
    in
    returnDs (realWorldStateTy,
	      \prim_app -> CoCase prim_app (CoAlgAlts [the_alt] CoNoDefault)
    )

#if 0
    -- MAYBE LATER???

  -- Data types with several nullary constructors (Enumerated types)
  | isEnumerationType result_ty &&				-- Enumeration
    (length data_cons) <= 5					-- fairly short
  = 
    newSysLocalDs realWorldStatePrimTy				`thenDs` \ prim_state_id ->
    newSysLocalDs intPrimTy 					`thenDs` \ prim_result_id ->

    mkCoConDs stateDataCon [realWorldTy] [CoVar prim_state_id]	`thenDs` \ new_state ->

    let
      alts = [ (mkMachInt i, con) | (i, con) <- [0..] `zip` data_cons ]
      the_result = CoCase prim_result_id (CoPrimAlts alts) CoNoDefault
    in

    mkCoConDs (mkTupleCon 2)
	      [result_ty, realWorldStateTy]
	      [the_result, new_state]				`thenDs` \ the_pair ->
    let
	the_alt = (state_and_prim_datacon, [prim_state_id, prim_result_id], the_pair)
    in
    returnDs (state_and_prim_ty,
	      \prim_app -> CoCase prim_app (CoAlgAlts [the_alt] CoNoDefault)
    )
#endif

  | otherwise 
  = pprPanic "boxResult: " (ppr PprDebug result_ty)

  where
    maybe_data_type 			   = getUniDataTyCon_maybe result_ty
    Just (tycon, tycon_arg_tys, data_cons) = maybe_data_type
    (the_data_con : other_data_cons)       = data_cons

    (_, data_con_arg_tys, _)               = getInstantiatedDataConSig the_data_con tycon_arg_tys
    (the_prim_result_ty : other_args_tys)  = data_con_arg_tys

    (state_and_prim_datacon, state_and_prim_ty) = getStatePairingConInfo the_prim_result_ty
\end{code}


%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[WwLib]{A library for the ``worker/wrapper'' back-end to the strictness analyser}

\begin{code}
module WwLib (
	mkWwBodies,
	worthSplitting, setUnpackStrategy
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreUtils	( exprType, mkInlineMe )
import Id		( Id, idType, mkSysLocal, idDemandInfo, setIdDemandInfo,
			  isOneShotLambda, setOneShotLambda,
                          mkWildId, setIdInfo
			)
import IdInfo		( CprInfo(..), noCprInfo, vanillaIdInfo )
import DataCon		( DataCon, splitProductType )
import Demand		( Demand(..), wwLazy, wwPrim )
import PrelInfo		( realWorldPrimId, aBSENT_ERROR_ID )
import TysPrim		( realWorldStatePrimTy )
import TysWiredIn	( unboxedTupleCon, unboxedTupleTyCon )
import Type		( isUnLiftedType, 
			  splitForAllTys, splitFunTys,  isAlgType,
			  splitNewType_maybe,
			  mkTyConApp, mkFunTys,
			  Type
			)
import TyCon            ( isNewTyCon, isProductTyCon, TyCon )
import BasicTypes	( NewOrData(..), Arity )
import Var              ( TyVar, Var, isId )
import UniqSupply	( returnUs, thenUs, getUniqueUs, getUniquesUs, 
                          mapUs, UniqSM )
import Util		( zipWithEqual, zipEqual, lengthExceeds )
import Outputable
import List		( zipWith4 )
\end{code}


%************************************************************************
%*									*
\subsection[mkWrapperAndWorker]{@mkWrapperAndWorker@}
%*									*
%************************************************************************

	************   WARNING  ******************
	these comments are rather out of date
	*****************************************

@mkWrapperAndWorker@ is given:
\begin{enumerate}
\item
The {\em original function} \tr{f}, of the form:
\begin{verbatim}
f = /\ tyvars -> \ args -> body
\end{verbatim}
The original-binder \tr{f}, the \tr{tyvars}, \tr{args}, and \tr{body}
are given separately.

We use the Id \tr{f} mostly to get its type.

\item
Strictness information about \tr{f}, in the form of a list of
@Demands@.

\item
A @UniqueSupply@.
\end{enumerate}

@mkWrapperAndWorker@ produces (A BIT OUT-OF-DATE...):
\begin{enumerate}
\item
Maybe @Nothing@: no worker/wrappering going on in this case. This can
happen (a)~if the strictness info says that there is nothing
interesting to do or (b)~if *any* of the argument types corresponding
to ``active'' arg postitions is abstract or will be to the outside
world (i.e., {\em this} module can see the constructors, but nobody
else will be able to).  An ``active'' arg position is one which the
wrapper has to unpack.  An importing module can't do this unpacking,
so it simply has to give up and call the wrapper only.

\item
Maybe \tr{Just (wrapper_Id, wrapper_body, worker_Id, worker_body)}.

The @wrapper_Id@ is just the one that was passed in, with its
strictness IdInfo updated.
\end{enumerate}

The \tr{body} of the original function may not be given (i.e., it's
BOTTOM), in which case you'd jolly well better not tug on the
worker-body output!

Here's an example.  The original function is:
\begin{verbatim}
g :: forall a . Int -> [a] -> a

g = /\ a -> \ x ys ->
	case x of
	  0 -> head ys
	  _ -> head (tail ys)
\end{verbatim}

From this, we want to produce:
\begin{verbatim}
-- wrapper (an unfolding)
g :: forall a . Int -> [a] -> a

g = /\ a -> \ x ys ->
	case x of
	  I# x# -> g.wrk a x# ys
	    -- call the worker; don't forget the type args!

-- worker
g.wrk :: forall a . Int# -> [a] -> a

g.wrk = /\ a -> \ x# ys ->
	let
	    x = I# x#
	in
	    case x of		    -- note: body of g moved intact
	      0 -> head ys
	      _ -> head (tail ys)
\end{verbatim}

Something we have to be careful about:  Here's an example:
\begin{verbatim}
-- "f" strictness: U(P)U(P)
f (I# a) (I# b) = a +# b

g = f	-- "g" strictness same as "f"
\end{verbatim}
\tr{f} will get a worker all nice and friendly-like; that's good.
{\em But we don't want a worker for \tr{g}}, even though it has the
same strictness as \tr{f}.  Doing so could break laziness, at best.

Consequently, we insist that the number of strictness-info items is
exactly the same as the number of lambda-bound arguments.  (This is
probably slightly paranoid, but OK in practice.)  If it isn't the
same, we ``revise'' the strictness info, so that we won't propagate
the unusable strictness-info into the interfaces.


%************************************************************************
%*									*
\subsection{Functions over Demands}
%*									*
%************************************************************************

\begin{code}
mAX_WORKER_ARGS :: Int		-- ToDo: set via flag
mAX_WORKER_ARGS = 6

setUnpackStrategy :: [Demand] -> [Demand]
setUnpackStrategy ds
  = snd (go (mAX_WORKER_ARGS - nonAbsentArgs ds) ds)
  where
    go :: Int 			-- Max number of args available for sub-components of [Demand]
       -> [Demand]
       -> (Int, [Demand])	-- Args remaining after subcomponents of [Demand] are unpacked

    go n (WwUnpack nd _ cs : ds) | n' >= 0
			         = WwUnpack nd True cs' `cons` go n'' ds
			         | otherwise
			         = WwUnpack nd False cs `cons` go n ds
			         where
			 	   n' = n + 1 - nonAbsentArgs cs
					-- Add one because we don't pass the top-level arg any more
					-- Delete # of non-absent args to which we'll now be committed
				   (n'',cs') = go n' cs
				
    go n (d:ds) = d `cons` go n ds
    go n []     = (n,[])

    cons d (n,ds) = (n, d:ds)

nonAbsentArgs :: [Demand] -> Int
nonAbsentArgs []		 = 0
nonAbsentArgs (WwLazy True : ds) = nonAbsentArgs ds
nonAbsentArgs (d	   : ds) = 1 + nonAbsentArgs ds

worthSplitting :: [Demand]
	       -> Bool	-- Result is bottom
	       -> Bool	-- True <=> the wrapper would not be an identity function
worthSplitting ds result_bot = any worth_it ds
	-- We used not to split if the result is bottom.
	-- [Justification:  there's no efficiency to be gained.]
	-- But it's sometimes bad not to make a wrapper.  Consider
	--	fw = \x# -> let x = I# x# in case e of
	--					p1 -> error_fn x
	--					p2 -> error_fn x
	--					p3 -> the real stuff
	-- The re-boxing code won't go away unless error_fn gets a wrapper too.

  where
    worth_it (WwLazy True)	 = True		-- Absent arg
    worth_it (WwUnpack _ True _) = True		-- Arg to unpack
    worth_it WwStrict		 = False	-- Don't w/w just because of strictness
    worth_it other		 = False

allAbsent :: [Demand] -> Bool
allAbsent ds = all absent ds
  where
    absent (WwLazy is_absent)   = is_absent
    absent (WwUnpack _ True cs) = allAbsent cs
    absent other		= False
\end{code}


%************************************************************************
%*									*
\subsection{The worker wrapper core}
%*									*
%************************************************************************

@mkWwBodies@ is called when doing the worker/wrapper split inside a module.

\begin{code}
mkWwBodies :: Type				-- Type of original function
	   -> Arity				-- Arity of original function
	   -> [Demand]				-- Strictness of original function
	   -> Bool				-- True <=> function returns bottom
	   -> [Bool]				-- One-shot-ness of the function
	   -> CprInfo                           -- Result of CPR analysis 
	   -> UniqSM ([Demand],			-- Demands for worker (value) args
		      Id -> CoreExpr,		-- Wrapper body, lacking only the worker Id
		      CoreExpr -> CoreExpr)	-- Worker body, lacking the original function rhs

mkWwBodies fun_ty arity demands res_bot one_shots cpr_info
  = mkWWargs fun_ty arity demands' res_bot one_shots'	`thenUs` \ (wrap_args, wrap_fn_args,   work_fn_args, res_ty) ->
    mkWWstr wrap_args					`thenUs` \ (work_dmds, wrap_fn_str,    work_fn_str) ->
    mkWWcpr res_ty cpr_info      			`thenUs` \ (wrap_fn_cpr,    work_fn_cpr,  cpr_res_ty) ->
    mkWWfixup cpr_res_ty work_dmds			`thenUs` \ (final_work_dmds, wrap_fn_fixup,  work_fn_fixup) ->

    returnUs (final_work_dmds,
	      Note InlineMe . wrap_fn_args . wrap_fn_cpr . wrap_fn_str . wrap_fn_fixup . Var,
	      work_fn_fixup . work_fn_str . work_fn_cpr . work_fn_args)
	-- We use an INLINE unconditionally, even if the wrapper turns out to be
	-- something trivial like
	--	fw = ...
	--	f = __inline__ (coerce T fw)
	-- The point is to propagate the coerce to f's call sites, so even though
	-- f's RHS is now trivial (size 1) we still want the __inline__ to prevent
	-- fw from being inlined into f's RHS
  where
    demands'   = demands   ++ repeat wwLazy
    one_shots' = one_shots ++ repeat False
\end{code}


%************************************************************************
%*									*
\subsection{Coercion stuff}
%*									*
%************************************************************************


We really want to "look through" coerces.
Reason: I've seen this situation:

	let f = coerce T (\s -> E)
	in \x -> case x of
	   	    p -> coerce T' f
		    q -> \s -> E2
	   	    r -> coerce T' f

If only we w/w'd f, we'd get
	let f = coerce T (\s -> fw s)
	    fw = \s -> E
	in ...

Now we'll inline f to get

	let fw = \s -> E
	in \x -> case x of
	   	    p -> fw
		    q -> \s -> E2
	   	    r -> fw

Now we'll see that fw has arity 1, and will arity expand
the \x to get what we want.

\begin{code}
-- mkWWargs is driven off the function type and arity.
-- It chomps bites off foralls, arrows, newtypes
-- and keeps repeating that until it's satisfied the supplied arity

mkWWargs :: Type -> Arity 
	 -> [Demand] -> Bool -> [Bool]		-- Both these will in due course be derived
						-- from the type.  The [Bool] is True for a one-shot arg.
						-- ** Both are infinite, extended with neutral values if necy **
	 -> UniqSM  ([Var],		-- Wrapper args
		     CoreExpr -> CoreExpr,	-- Wrapper fn
		     CoreExpr -> CoreExpr,	-- Worker fn
		     Type)			-- Type of wrapper body

mkWWargs fun_ty arity demands res_bot one_shots
  | (res_bot || arity > 0) && (not (null tyvars) || n_arg_tys > 0)
	-- If the function returns bottom, we feel free to 
	-- build lots of wrapper args:
	--	  \x. let v=E in \y. bottom
	--	= \xy. let v=E in bottom
  = getUniquesUs n_args		`thenUs` \ wrap_uniqs ->
    let
      val_args	= zipWith4 mk_wrap_arg wrap_uniqs arg_tys demands one_shots
      wrap_args = tyvars ++ val_args
    in
    mkWWargs new_fun_ty
	     (arity - n_args) 
	     (drop n_args demands)
	     res_bot
	     (drop n_args one_shots)	`thenUs` \ (more_wrap_args, wrap_fn_args, work_fn_args, res_ty) ->

    returnUs (wrap_args ++ more_wrap_args,
	      mkLams wrap_args . wrap_fn_args,
	      work_fn_args . applyToVars wrap_args,
	      res_ty)
  where
    (tyvars, tau)      	= splitForAllTys fun_ty
    (arg_tys, body_ty) 	= splitFunTys tau
    n_arg_tys		= length arg_tys
    n_args		| res_bot   = n_arg_tys 
			| otherwise = arity `min` n_arg_tys
    new_fun_ty		| n_args == n_arg_tys = body_ty
			| otherwise  	      = mkFunTys (drop n_args arg_tys) body_ty

mkWWargs fun_ty arity demands res_bot one_shots
  = case splitNewType_maybe fun_ty of
	Nothing     -> returnUs ([], id, id, fun_ty)
	Just rep_ty -> mkWWargs rep_ty arity demands res_bot one_shots	`thenUs` \ (wrap_args, wrap_fn_args, work_fn_args, res_ty) ->
		       returnUs (wrap_args,
				 Note (Coerce fun_ty rep_ty) . wrap_fn_args,
				 work_fn_args . Note (Coerce rep_ty fun_ty),
				 res_ty)


applyToVars :: [Var] -> CoreExpr -> CoreExpr
applyToVars vars fn = mkVarApps fn vars

mk_wrap_arg uniq ty dmd one_shot 
  = set_one_shot one_shot (setIdDemandInfo (mkSysLocal SLIT("w") uniq ty) dmd)
  where
    set_one_shot True  id = setOneShotLambda id
    set_one_shot False id = id
\end{code}


%************************************************************************
%*									*
\subsection{Fixup stuff}
%*									*
%************************************************************************

\begin{code}
mkWWfixup res_ty work_dmds
  | null work_dmds && isUnLiftedType res_ty 
 	-- Horrid special case.  If the worker would have no arguments, and the
	-- function returns a primitive type value, that would make the worker into
	-- an unboxed value.  We box it by passing a dummy void argument, thus:
	--
	--	f = /\abc. \xyz. fw abc void
	-- 	fw = /\abc. \v. body
	--
	-- We use the state-token type which generates no code
  = getUniqueUs 		`thenUs` \ void_arg_uniq ->
    let
	    void_arg = mk_ww_local void_arg_uniq realWorldStatePrimTy
    in
    returnUs ([wwPrim],		
	      \ call_to_worker -> App call_to_worker (Var realWorldPrimId),
	      \ worker_body    -> Lam void_arg worker_body)

  | otherwise
  = returnUs (work_dmds, id, id)
\end{code}


%************************************************************************
%*									*
\subsection{Strictness stuff}
%*									*
%************************************************************************

\begin{code}
mkWWstr :: [Var]				-- Wrapper args; have their demand info on them
						-- *Includes type variables*
        -> UniqSM ([Demand],			-- Demand on worker (value) args
		   CoreExpr -> CoreExpr,	-- Wrapper body, lacking the worker call
						-- and without its lambdas 
						-- This fn adds the unboxing, and makes the
						-- call passing the unboxed things
				
		   CoreExpr -> CoreExpr)	-- Worker body, lacking the original body of the function,
						-- but *with* lambdas

mkWWstr wrap_args
  = mk_ww_str wrap_args		`thenUs` \ (work_args, wrap_fn, work_fn) ->
    returnUs ( [idDemandInfo v | v <- work_args, isId v],
	       \ wrapper_body -> wrap_fn (mkVarApps wrapper_body work_args),
	       \ worker_body  -> mkLams work_args (work_fn worker_body))

	-- Empty case
mk_ww_str []
  = returnUs ([],
	      \ wrapper_body -> wrapper_body,
	      \ worker_body  -> worker_body)


mk_ww_str (arg : ds)
  | isTyVar arg
  = mk_ww_str ds		`thenUs` \ (worker_args, wrap_fn, work_fn) ->
    returnUs (arg : worker_args, wrap_fn, work_fn)

  | otherwise
  = case idDemandInfo arg of

	-- Absent case
      WwLazy True ->
	mk_ww_str ds 		`thenUs` \ (worker_args, wrap_fn, work_fn) ->
	returnUs (worker_args, wrap_fn, mk_absent_let arg . work_fn)

	-- Unpack case
      WwUnpack new_or_data True cs ->
	getUniquesUs (length inst_con_arg_tys)		`thenUs` \ uniqs ->
	let
	  unpk_args	 = zipWith mk_ww_local uniqs inst_con_arg_tys
	  unpk_args_w_ds = zipWithEqual "mk_ww_str" set_worker_arg_info unpk_args cs
	in
	mk_ww_str (unpk_args_w_ds ++ ds)		`thenUs` \ (worker_args, wrap_fn, work_fn) ->
	returnUs (worker_args,
	          mk_unpk_case new_or_data arg unpk_args data_con arg_tycon . wrap_fn,
		  work_fn . mk_pk_let new_or_data arg data_con tycon_arg_tys unpk_args)
	where
	  (arg_tycon, tycon_arg_tys, data_con, inst_con_arg_tys) = splitProductType "mk_ww_str" (idType arg)

	-- Other cases
      other_demand ->
	mk_ww_str ds		`thenUs` \ (worker_args, wrap_fn, work_fn) ->
	returnUs (arg : worker_args, wrap_fn, work_fn)
  where
	-- If the wrapper argument is a one-shot lambda, then
	-- so should (all) the corresponding worker arguments be
	-- This bites when we do w/w on a case join point
    set_worker_arg_info worker_arg demand = set_one_shot (setIdDemandInfo worker_arg demand)

    set_one_shot | isOneShotLambda arg = setOneShotLambda
		 | otherwise	       = \x -> x
\end{code}


%************************************************************************
%*									*
\subsection{CPR stuff}
%*									*
%************************************************************************


@mkWWcpr@ takes the worker/wrapper pair produced from the strictness
info and adds in the CPR transformation.  The worker returns an
unboxed tuple containing non-CPR components.  The wrapper takes this
tuple and re-produces the correct structured output.

The non-CPR results appear ordered in the unboxed tuple as if by a
left-to-right traversal of the result structure.


\begin{code}
mkWWcpr :: Type                              -- function body type
        -> CprInfo                           -- CPR analysis results
        -> UniqSM (CoreExpr -> CoreExpr,             -- New wrapper 
                   CoreExpr -> CoreExpr,	     -- New worker
		   Type)			-- Type of worker's body 

mkWWcpr body_ty NoCPRInfo 
    = returnUs (id, id, body_ty)      -- Must be just the strictness transf.

mkWWcpr body_ty ReturnsCPR
    | not (isAlgType body_ty)
    = WARN( True, text "mkWWcpr: non-algebraic body type" <+> ppr body_ty )
      returnUs (id, id, body_ty)

    | n_con_args == 1 && isUnLiftedType con_arg_ty1
	-- Special case when there is a single result of unlifted type
    = getUniquesUs 2			`thenUs` \ [work_uniq, arg_uniq] ->
      let
	work_wild = mk_ww_local work_uniq body_ty
	arg	  = mk_ww_local arg_uniq  con_arg_ty1
      in
      returnUs (\ wkr_call -> Case wkr_call arg       [(DEFAULT, [], mkConApp data_con (map Type tycon_arg_tys ++ [Var arg]))],
		\ body     -> Case body     work_wild [(DataAlt data_con, [arg], Var arg)],
		con_arg_ty1)

    | otherwise		-- The general case
    = getUniquesUs (n_con_args + 2)  	`thenUs` \ uniqs ->
      let
        (wrap_wild : work_wild : args) = zipWith mk_ww_local uniqs (ubx_tup_ty : body_ty : con_arg_tys)
	arg_vars		       = map Var args
	ubx_tup_con		       = unboxedTupleCon n_con_args
	ubx_tup_ty		       = exprType ubx_tup_app
	ubx_tup_app		       = mkConApp ubx_tup_con (map Type con_arg_tys   ++ arg_vars)
        con_app			       = mkConApp data_con    (map Type tycon_arg_tys ++ arg_vars)
      in
      returnUs (\ wkr_call -> Case wkr_call wrap_wild [(DataAlt ubx_tup_con, args, con_app)],
		\ body     -> Case body     work_wild [(DataAlt data_con,    args, ubx_tup_app)],
		ubx_tup_ty)
    where
      (tycon, tycon_arg_tys, data_con, con_arg_tys) = splitProductType "mkWWcpr" body_ty
      n_con_args  = length con_arg_tys
      con_arg_ty1 = head con_arg_tys
\end{code}


%************************************************************************
%*									*
\subsection{Utilities}
%*									*
%************************************************************************


\begin{code}
mk_absent_let arg body
  | not (isUnLiftedType arg_ty)
  = Let (NonRec arg (mkTyApps (Var aBSENT_ERROR_ID) [arg_ty])) body
  | otherwise
  = panic "WwLib: haven't done mk_absent_let for primitives yet"
  where
    arg_ty = idType arg

mk_unpk_case NewType arg unpk_args boxing_con boxing_tycon body
  	-- A newtype!  Use a coercion not a case
  = ASSERT( null other_args )
    Case (Note (Coerce (idType unpk_arg) (idType arg)) (Var arg))
	 (sanitiseCaseBndr unpk_arg)
	 [(DEFAULT,[],body)]
  where
    (unpk_arg:other_args) = unpk_args

mk_unpk_case DataType arg unpk_args boxing_con boxing_tycon body
	-- A data type
  = Case (Var arg) 
	 (sanitiseCaseBndr arg)
	 [(DataAlt boxing_con, unpk_args, body)]

sanitiseCaseBndr :: Id -> Id
-- The argument we are scrutinising has the right type to be
-- a case binder, so it's convenient to re-use it for that purpose.
-- But we *must* throw away all its IdInfo.  In particular, the argument
-- will have demand info on it, and that demand info may be incorrect for
-- the case binder.  e.g.  	case ww_arg of ww_arg { I# x -> ... }
-- Quite likely ww_arg isn't used in '...'.  The case may get discarded
-- if the case binder says "I'm demanded".  This happened in a situation 
-- like		(x+y) `seq` ....
sanitiseCaseBndr id = id `setIdInfo` vanillaIdInfo

mk_pk_let NewType arg boxing_con con_tys unpk_args body
  = ASSERT( null other_args )
    Let (NonRec arg (Note (Coerce (idType arg) (idType unpk_arg)) (Var unpk_arg))) body
  where
    (unpk_arg:other_args) = unpk_args

mk_pk_let DataType arg boxing_con con_tys unpk_args body
  = Let (NonRec arg (mkConApp boxing_con con_args)) body
  where
    con_args = map Type con_tys ++ map Var unpk_args


mk_ww_local uniq ty = mkSysLocal SLIT("ww") uniq ty

\end{code}

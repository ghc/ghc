%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[WwLib]{A library for the ``worker/wrapper'' back-end to the strictness analyser}

\begin{code}
module WwLib (
	WwBinding(..),

	worthSplitting, setUnpackStrategy,
	mkWwBodies, mkWrapper
    ) where

#include "HsVersions.h"

import CoreSyn
import Id		( Id, idType, mkSysLocal, getIdDemandInfo, setIdDemandInfo )
import Const		( Con(..) )
import DataCon		( dataConArgTys )
import Demand		( Demand(..) )
import PrelVals		( aBSENT_ERROR_ID )
import TysWiredIn	( unitTy, unitDataCon )
import Type		( isUnLiftedType, mkTyVarTys, mkTyVarTy, mkFunTys,
			  splitForAllTys, splitFunTys,
			  splitAlgTyConApp_maybe, 
			  Type
			)
import BasicTypes	( NewOrData(..) )
import Var              ( TyVar )
import UniqSupply	( returnUs, thenUs, getUniqueUs, getUniquesUs, UniqSM )
import Util		( zipWithEqual )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[datatype-WwLib]{@WwBinding@: a datatype for worker/wrapper-ing}
%*									*
%************************************************************************

In the worker/wrapper stuff, we want to carry around @CoreBindings@ in
an ``intermediate form'' that can later be turned into a \tr{let} or
\tr{case} (depending on strictness info).

\begin{code}
data WwBinding
  = WwLet  [CoreBind]
  | WwCase (CoreExpr -> CoreExpr)
		-- the "case" will be a "strict let" of the form:
		--
		--  case rhs of
		--    <blah> -> body
		--
		-- (instead of "let <blah> = rhs in body")
		--
		-- The expr you pass to the function is "body" (the
		-- expression that goes "in the corner").
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

worthSplitting :: [Demand] -> Bool	-- True <=> the wrapper would not be an identity function
worthSplitting ds = any worth_it ds
  where
    worth_it (WwLazy True)	 = True		-- Absent arg
    worth_it (WwUnpack _ True _) = True		-- Arg to unpack
    worth_it WwStrict		 = True
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

@mkWrapper@ is called when importing a function.  We have the type of 
the function and the name of its worker, and we want to make its body (the wrapper).

\begin{code}
mkWrapper :: Type		-- Wrapper type
	  -> [Demand]		-- Wrapper strictness info
	  -> UniqSM (Id -> CoreExpr)	-- Wrapper body, missing worker Id

mkWrapper fun_ty demands
  = let
	n_wrap_args = length demands
    in
    getUniquesUs n_wrap_args	`thenUs` \ wrap_uniqs ->
    let
	(tyvars, tau_ty)   = splitForAllTys fun_ty
	(arg_tys, body_ty) = splitFunTys tau_ty
		-- The "expanding dicts" part here is important, even for the splitForAll
		-- The imported thing might be a dictionary, such as Functor Foo
		-- But Functor Foo = forall a b. (a->b) -> Foo a -> Foo b
		-- and as such might have some strictness info attached.
		-- Then we need to have enough args to zip to the strictness info
	
	wrap_args	   = ASSERT( n_wrap_args <= length arg_tys )
			     zipWith mk_ww_local wrap_uniqs arg_tys

	leftover_arg_tys   = drop n_wrap_args arg_tys
	final_body_ty	   = mkFunTys leftover_arg_tys body_ty
    in
    mkWwBodies tyvars wrap_args final_body_ty demands	`thenUs` \ (wrap_fn, _, _) ->
    returnUs wrap_fn
\end{code}

@mkWwBodies@ is called when doing the worker/wrapper split inside a module.

\begin{code}
mkWwBodies :: [TyVar] -> [Id] -> Type		-- Original fn args and body type
	   -> [Demand]				-- Strictness info for original fn; corresp 1-1 with args
	   -> UniqSM (Id -> CoreExpr,		-- Wrapper body, lacking only the worker Id
		      CoreExpr -> CoreExpr,	-- Worker body, lacking the original function body
		      [Demand])			-- Strictness info for worker

mkWwBodies tyvars args body_ty demands
  | allAbsent demands &&
    isUnLiftedType body_ty
  = 	-- Horrid special case.  If the worker would have no arguments, and the
	-- function returns a primitive type value, that would make the worker into
	-- an unboxed value.  We box it by passing a dummy void argument, thus:
	--
	--	f = /\abc. \xyz. fw abc void
	-- 	fw = /\abc. \v. body
	--
    getUniqueUs 		`thenUs` \ void_arg_uniq ->
    let
	void_arg = mk_ww_local void_arg_uniq unitTy
    in
    returnUs (\ work_id -> mkLams tyvars $ mkLams args $
			   mkApps (Var work_id) 
				  (map (Type . mkTyVarTy) tyvars ++ [mkConApp unitDataCon []]),
	      \ body    -> mkLams (tyvars ++ [void_arg]) body,
	      [WwLazy True])

mkWwBodies tyvars wrap_args body_ty demands
  | otherwise
  = let
	wrap_args_w_demands = zipWithEqual "mkWwBodies" setIdDemandInfo wrap_args demands
    in
    mkWW wrap_args_w_demands 		`thenUs` \ (wrap_fn, work_args_w_demands, work_fn) ->
    returnUs (\ work_id -> mkLams tyvars $ mkLams wrap_args_w_demands $
			   wrap_fn (mkTyApps (Var work_id) (mkTyVarTys tyvars)),

	      \ body    -> mkLams tyvars $ mkLams work_args_w_demands $
			   work_fn body,

	      map getIdDemandInfo work_args_w_demands)
\end{code}    


\begin{code}
mkWW :: [Id]				-- Wrapper args; have their demand info on them
     -> UniqSM (CoreExpr -> CoreExpr,	-- Wrapper body, lacking the inner call to the worker
					-- and without its lambdas
		[Id],			-- Worker args; have their demand info on them
		CoreExpr -> CoreExpr)	-- Worker body, lacking the original body of the function


	-- Empty case
mkWW []
  = returnUs (\ wrapper_body -> wrapper_body,
	      [],
	      \ worker_body  -> worker_body)


mkWW (arg : ds)
  = case getIdDemandInfo arg of

	-- Absent case
      WwLazy True ->
	mkWW ds 		`thenUs` \ (wrap_fn, worker_args, work_fn) ->
	returnUs (\ wrapper_body -> wrap_fn wrapper_body,
		  worker_args,
	      	  \ worker_body  -> mk_absent_let arg (work_fn worker_body))


	-- Unpack case
      WwUnpack new_or_data True cs ->
	getUniquesUs (length inst_con_arg_tys)		`thenUs` \ uniqs ->
	let
	  unpk_args	 = zipWith mk_ww_local uniqs inst_con_arg_tys
	  unpk_args_w_ds = zipWithEqual "mkWW" setIdDemandInfo unpk_args cs
	in
	mkWW (unpk_args_w_ds ++ ds)		`thenUs` \ (wrap_fn, worker_args, work_fn) ->
	returnUs (\ wrapper_body -> mk_unpk_case new_or_data arg unpk_args data_con arg_tycon
					         (wrap_fn wrapper_body),
		  worker_args,
	          \ worker_body  -> work_fn (mk_pk_let new_or_data arg data_con 
						       tycon_arg_tys unpk_args worker_body))
	where
    	  inst_con_arg_tys = dataConArgTys data_con tycon_arg_tys
	  (arg_tycon, tycon_arg_tys, data_con)
	     = case (splitAlgTyConApp_maybe (idType arg)) of

	     	 Just (arg_tycon, tycon_arg_tys, [data_con]) ->
			     -- The main event: a single-constructor data type
			     (arg_tycon, tycon_arg_tys, data_con)

	    	 Just (_, _, data_cons) ->
			pprPanic "mk_ww_arg_processing:" 
				 (text "not one constr (interface files not consistent/up to date?)"
				  $$ (ppr arg <+> ppr (idType arg)))

	         Nothing		->
			panic "mk_ww_arg_processing: not datatype"


	-- Other cases
      other_demand ->
	mkWW ds		`thenUs` \ (wrap_fn, worker_args, work_fn) ->
	returnUs (\ wrapper_body -> wrap_fn (App wrapper_body (Var arg)),
	    	  arg : worker_args, 
		  work_fn)
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
	 unpk_arg
	 [(DEFAULT,[],body)]
  where
    (unpk_arg:other_args) = unpk_args

mk_unpk_case DataType arg unpk_args boxing_con boxing_tycon body
	-- A data type
  = Case (Var arg) arg [(DataCon boxing_con, unpk_args, body)]

mk_pk_let NewType arg boxing_con con_tys unpk_args body
  = ASSERT( null other_args )
    Let (NonRec arg (Note (Coerce (idType arg) (idType unpk_arg)) (Var unpk_arg))) body
  where
    (unpk_arg:other_args) = unpk_args

mk_pk_let DataType arg boxing_con con_tys unpk_args body
  = Let (NonRec arg (Con (DataCon boxing_con) con_args)) body
  where
    con_args = map Type con_tys ++ map Var unpk_args


mk_ww_local uniq ty = mkSysLocal uniq ty
\end{code}

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
import Id		( Id, idType, mkSysLocal, getIdDemandInfo, setIdDemandInfo,
                          mkWildId, setIdInfo
			)
import IdInfo		( CprInfo(..), noCprInfo, vanillaIdInfo )
import Const		( Con(..), DataCon )
import DataCon		( splitProductType_maybe, isExistentialDataCon, dataConArgTys )
import Demand		( Demand(..) )
import PrelInfo		( realWorldPrimId, aBSENT_ERROR_ID )
import TysPrim		( realWorldStatePrimTy )
import TysWiredIn	( unboxedTupleCon, unboxedTupleTyCon )
import Type		( isUnLiftedType, mkTyVarTys, mkTyVarTy, mkFunTys,
			  splitForAllTys, splitFunTys, splitFunTysN,
			  splitAlgTyConApp_maybe, splitAlgTyConApp,
			  mkTyConApp, splitNewType_maybe,
			  Type
			)
import TyCon            ( isNewTyCon, isProductTyCon, TyCon )
import BasicTypes	( NewOrData(..) )
import Var              ( TyVar )
import UniqSupply	( returnUs, thenUs, getUniqueUs, getUniquesUs, 
                          mapUs, UniqSM )
import Util		( zipWithEqual, zipEqual )
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

worthSplitting :: [Demand]
	       -> Bool	-- Result is bottom
	       -> Bool	-- True <=> the wrapper would not be an identity function
worthSplitting ds result_bot = not result_bot && any worth_it ds
	-- Don't split if the result is bottom; there's no efficiency to
	-- be gained, and (worse) the wrapper body may not look like a wrapper
	-- body to getWorkerIdAndCons
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

@mkWrapper@ is called when importing a function.  We have the type of 
the function and the name of its worker, and we want to make its body (the wrapper).

\begin{code}
mkWrapper :: Type		-- Wrapper type
	  -> Int		-- Arity
	  -> [Demand]		-- Wrapper strictness info
	  -> CprInfo            -- Wrapper cpr info
	  -> UniqSM (Id -> CoreExpr)	-- Wrapper body, missing worker Id

mkWrapper fun_ty arity demands cpr_info
  = getUniquesUs arity		`thenUs` \ wrap_uniqs ->
    let
	(tyvars, tau_ty)   = splitForAllTys fun_ty
	(arg_tys, body_ty) = splitFunTysN "mkWrapper" arity tau_ty
		-- The "expanding dicts" part here is important, even for the splitForAll
		-- The imported thing might be a dictionary, such as Functor Foo
		-- But Functor Foo = forall a b. (a->b) -> Foo a -> Foo b
		-- and as such might have some strictness info attached.
		-- Then we need to have enough args to zip to the strictness info
	
	wrap_args	   = zipWith mk_ww_local wrap_uniqs arg_tys
    in
    mkWwBodies tyvars wrap_args body_ty demands cpr_info	`thenUs` \ (wrap_fn, _, _) ->
    returnUs wrap_fn
\end{code}

@mkWwBodies@ is called when doing the worker/wrapper split inside a module.

\begin{code}
mkWwBodies :: [TyVar] -> [Id]			-- Original fn args 
	   -> Type				-- Type of result of original function
	   -> [Demand]				-- Strictness info for original fn; corresp 1-1 with args
	   -> CprInfo                           -- Result of CPR analysis 
	   -> UniqSM (Id -> CoreExpr,		-- Wrapper body, lacking only the worker Id
		      CoreExpr -> CoreExpr,	-- Worker body, lacking the original function body
		      [Demand])			-- Strictness info for worker

mkWwBodies tyvars wrap_args res_ty demands cpr_info
  = let
        -- demands may be longer than number of args.  If we aren't doing w/w
        -- for strictness then demands is an infinite list of 'lazy' args.
	wrap_args_w_demands = zipWith setIdDemandInfo wrap_args demands
	
    in
    mkWWstr wrap_args_w_demands			`thenUs` \ (wrap_fn_str,    work_fn_str,    work_arg_dmds) ->
    mkWWcoerce res_ty				`thenUs` \ (wrap_fn_coerce, work_fn_coerce, coerce_res_ty) ->
    mkWWcpr coerce_res_ty cpr_info      	`thenUs` \ (wrap_fn_cpr,    work_fn_cpr,    cpr_res_ty) ->
    mkWWfixup cpr_res_ty (null work_arg_dmds)	`thenUs` \ (wrap_fn_fixup,  work_fn_fixup) ->

    returnUs (\ work_id -> Note InlineMe $
			   mkLams tyvars $ mkLams wrap_args_w_demands $
			   (wrap_fn_coerce . wrap_fn_cpr . wrap_fn_str . wrap_fn_fixup) $
			   mkVarApps (Var work_id) tyvars,

	      \ work_body  -> mkLams tyvars $ 
			      (work_fn_fixup . work_fn_str . work_fn_cpr . work_fn_coerce) 
			      work_body,

	      work_arg_dmds)
\end{code}


%************************************************************************
%*									*
\subsection{Coercion stuff}
%*									*
%************************************************************************

The "coerce" transformation is
	f :: T1 -> T2 -> R
	f = \xy -> e
===>
	f = \xy -> coerce R R' (fw x y)
	fw = \xy -> coerce R' R e

where R' is the representation type for R.

\begin{code}
mkWWcoerce body_ty 
  = case splitNewType_maybe body_ty of

	Nothing     -> returnUs (id, id, body_ty)

	Just rep_ty -> returnUs (mkNote (Coerce body_ty rep_ty),
			         mkNote (Coerce rep_ty body_ty),
				 rep_ty)
\end{code}    



%************************************************************************
%*									*
\subsection{Fixup stuff}
%*									*
%************************************************************************

\begin{code}
mkWWfixup res_ty no_worker_args
  | no_worker_args && isUnLiftedType res_ty 
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
    returnUs (\ call_to_worker -> App call_to_worker (Var void_arg),
	      \ worker_body    -> Lam void_arg worker_body)

  | otherwise
  = returnUs (id, id)
\end{code}


%************************************************************************
%*									*
\subsection{Strictness stuff}
%*									*
%************************************************************************

\begin{code}
mkWWstr :: [Id]					-- Wrapper args; have their demand info on them
        -> UniqSM (CoreExpr -> CoreExpr,	-- Wrapper body, lacking the worker call
						-- and without its lambdas 
						-- This fn adds the unboxing, and makes the
						-- call passing the unboxed things
				
		   CoreExpr -> CoreExpr,	-- Worker body, lacking the original body of the function,
						-- but *with* lambdas
		   [Demand])			-- Worker arg demands

mkWWstr wrap_args
  = mk_ww_str wrap_args		`thenUs` \ (work_args_w_demands, wrap_fn, work_fn) ->
    returnUs ( \ wrapper_body -> wrap_fn (mkVarApps wrapper_body work_args_w_demands),
	       \ worker_body  -> mkLams work_args_w_demands (work_fn worker_body),
	       map getIdDemandInfo work_args_w_demands)

	-- Empty case
mk_ww_str []
  = returnUs ([],
	      \ wrapper_body -> wrapper_body,
	      \ worker_body  -> worker_body)


mk_ww_str (arg : ds)
  = case getIdDemandInfo arg of

	-- Absent case
      WwLazy True ->
	mk_ww_str ds 		`thenUs` \ (worker_args, wrap_fn, work_fn) ->
	returnUs (worker_args, wrap_fn, mk_absent_let arg . work_fn)

	-- Unpack case
      WwUnpack new_or_data True cs ->
	getUniquesUs (length inst_con_arg_tys)		`thenUs` \ uniqs ->
	let
	  unpk_args	 = zipWith mk_ww_local uniqs inst_con_arg_tys
	  unpk_args_w_ds = zipWithEqual "mk_ww_str" setIdDemandInfo unpk_args cs
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
mkWWcpr body_ty (CPRInfo cpr_args)
    = getUniqueUs 		`thenUs` \ body_arg_uniq ->
      let
        body_var = mk_ww_local body_arg_uniq body_ty
      in
      cpr_reconstruct body_ty cpr_info'                   `thenUs` \reconst_fn ->
      cpr_flatten body_ty cpr_info'                       `thenUs` \(flatten_fn, res_ty) ->
      returnUs (reconst_fn, flatten_fn, res_ty)
    where
	    -- We only make use of the outer level of CprInfo,  otherwise we
	    -- may lose laziness.  :-(  Hopefully,  we will find a use for the
	    -- extra info some day (e.g. creating versions specialized to 
	    -- the use made of the components of the result by the callee)
      cpr_info' = CPRInfo (map (const NoCPRInfo) cpr_args) 
\end{code}


@cpr_flatten@ takes the result type produced by the body and the info
from the CPR analysis and flattens the constructed product components.
These are returned in an unboxed tuple.

\begin{code}
cpr_flatten :: Type -> CprInfo -> UniqSM (CoreExpr -> CoreExpr, Type)
cpr_flatten ty cpr_info
    = mk_cpr_case (ty, cpr_info)       `thenUs` \(res_id, tup_ids, flatten_exp) ->
      let
	(unbx_tuple, unbx_tuple_ty) = mk_unboxed_tuple tup_ids
      in
      returnUs (\body -> Case body res_id [(DEFAULT, [], flatten_exp unbx_tuple)],
		unbx_tuple_ty)



mk_cpr_case :: (Type, CprInfo) -> 
               UniqSM (CoreBndr,                     -- Name of binder for this part of result 
                      [(CoreExpr, Type)],            -- expressions for flattened result
                      CoreExpr -> CoreExpr)          -- add in code to flatten result

mk_cpr_case (ty, NoCPRInfo) 
      -- this component must be returned as a component of the unboxed tuple result
    = getUniqueUs            `thenUs`     \id_uniq   ->
      let id_id = mk_ww_local id_uniq ty in
        returnUs (id_id, [(Var id_id, ty)], id)
mk_cpr_case (ty, cpr_info@(CPRInfo ci_args))
    | isNewTyCon tycon  -- a new type: under the coercions must be a 
                        -- constructed product
    = ASSERT ( null $ tail inst_con_arg_tys )
      mk_cpr_case (target_of_from_type, cpr_info) 
                                 `thenUs`  \(arg, tup, exp) ->
      getUniqueUs                `thenUs`  \id_uniq   ->
      let id_id = mk_ww_local id_uniq ty 
          new_exp_case = \var -> Case (Note (Coerce (idType arg) ty) (Var id_id))
				      arg
				      [(DEFAULT,[], exp var)]
      in
        returnUs (id_id, tup, new_exp_case)

    | otherwise            -- a data type
                           -- flatten components
    = mapUs mk_cpr_case (zip inst_con_arg_tys ci_args) 
                                 `thenUs`  \sub_builds ->
      getUniqueUs                `thenUs`  \id_uniq   ->
      let id_id = mk_ww_local id_uniq ty 
          (args, tup, exp) = unzip3 sub_builds
          -- not used: con_app = mkConApp data_con (map Var args) 
          new_tup = concat tup
          new_exp_case = \var -> Case (Var id_id) (mkWildId ty)
				 [(DataCon data_con, args, 
                                  foldl (\e f -> f e) var exp)]
      in
        returnUs (id_id, new_tup, new_exp_case)
    where
      (tycon, tycon_arg_tys, data_con, inst_con_arg_tys) = splitProductType "mk_cpr_case" ty
      from_type = head inst_con_arg_tys
      -- if coerced from a function 'look through' to find result type
      target_of_from_type = (snd.splitFunTys.snd.splitForAllTys) from_type

\end{code}

@cpr_reconstruct@ does the opposite of @cpr_flatten@.  It takes the unboxed
tuple produced by the worker and reconstructs the structured result.

\begin{code}
cpr_reconstruct :: Type -> CprInfo -> UniqSM (CoreExpr -> CoreExpr)
cpr_reconstruct ty cpr_info
    = mk_cpr_let (ty,cpr_info)     `thenUs`  \(res_id, tup_ids, reconstruct_exp) ->
      returnUs (\worker -> Case worker (mkWildId $ worker_type tup_ids)
                           [(DataCon $ unboxedTupleCon $ length tup_ids,
	                    tup_ids, reconstruct_exp $ Var res_id)])
			     
    where
	worker_type ids = mkTyConApp (unboxedTupleTyCon (length ids)) (map idType ids) 


mk_cpr_let :: (Type, CprInfo) -> 
              UniqSM (CoreBndr,                -- Binder for this component of result 
                      [CoreBndr],              -- Binders which will appear in worker's result
                      CoreExpr -> CoreExpr)    -- Code to produce structured result.
mk_cpr_let (ty, NoCPRInfo)
      -- this component will appear explicitly in the unboxed tuple.
    = getUniqueUs            `thenUs`     \id_uniq   ->
      let
	id_id = mk_ww_local id_uniq ty
      in
      returnUs (id_id, [id_id], id)

mk_cpr_let (ty, cpr_info@(CPRInfo ci_args))

{- Should not be needed now:  mkWWfixup does this job
    | isNewTyCon tycon   -- a new type: must coerce the argument to this type
    = ASSERT ( null $ tail inst_con_arg_tys )
      mk_cpr_let (target_of_from_type, cpr_info) 
                                 `thenUs`  \(arg, tup, exp) ->
      getUniqueUs                `thenUs`  \id_uniq   ->
      let id_id = mk_ww_local id_uniq ty 
          new_exp = \var -> exp (Let (NonRec id_id (Note (Coerce ty (idType arg)) (Var arg))) var) 
      in
        returnUs (id_id, tup, new_exp)

    | otherwise     -- a data type
                    -- reconstruct components then apply data con
-}
    = mapUs mk_cpr_let (zip inst_con_arg_tys ci_args) 
                                 `thenUs`  \sub_builds ->
      getUniqueUs                `thenUs`  \id_uniq   ->
      let id_id = mk_ww_local id_uniq ty 
          (args, tup, exp) = unzip3 sub_builds
          con_app = mkConApp data_con $ (map Type tycon_arg_tys) ++ (map Var args) 
          new_tup = concat tup
          new_exp = \var -> foldl (\e f -> f e) (Let (NonRec id_id con_app) var) exp 
      in
        returnUs (id_id, new_tup, new_exp)
    where
      (tycon, tycon_arg_tys, data_con, inst_con_arg_tys) = splitProductType "mk_cpr_let" ty
      from_type = head inst_con_arg_tys
      -- if coerced from a function 'look through' to find result type
      target_of_from_type = (snd.splitFunTys.snd.splitForAllTys) from_type


splitProductType :: String -> Type -> (TyCon, [Type], DataCon, [Type])
  -- For a tiresome reason, the type might not look like a product type
  -- This happens when compiling the compiler!  The module Name
  -- imports {-# SOURCE #-} TyCon and Id
  -- 	data Name = Name NameSort Unique OccName Provenance
  --    data NameSort = WiredInId Module Id | ...
  -- So Name does not look recursive (because Id is imported via a hi-boot file,
  -- which says nothing about Id's rep) but actually it is, because Ids have Names.
  -- Modules that *import* Name have a more complete view, see that Name is recursive,
  -- and therefore that it isn't a ProductType.  This conflicts with the CPR info
  -- in exports from Name that say "do CPR".
  --
  -- Arguably we should regard Name as a product anyway because it isn't recursive
  -- via products all the way... but we don't have that info to hand, and even if
  -- we did this case might *still* arise.

  -- 
  -- So we hack our way out for now, by trusting the pragma that said "do CPR"
  -- that means we can't use splitProductType_maybe

splitProductType fname ty
   = case splitAlgTyConApp_maybe ty of
	Just (tycon, tycon_args, (con:other_cons))
	  | null other_cons && not (isExistentialDataCon con)
	  -> WARN( not (isProductTyCon tycon),
		   text "splitProductType hack: I happened!" <+> ppr ty )
	     (tycon, tycon_args, con, dataConArgTys con tycon_args)
	     
	Nothing -> pprPanic (fname ++ ": not a product") (ppr ty)
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
	 [(DataCon boxing_con, unpk_args, body)]

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
  = Let (NonRec arg (Con (DataCon boxing_con) con_args)) body
  where
    con_args = map Type con_tys ++ map Var unpk_args


mk_ww_local uniq ty = mkSysLocal SLIT("ww") uniq ty


mk_unboxed_tuple :: [(CoreExpr, Type)] -> (CoreExpr, Type)
mk_unboxed_tuple contents
    = (mkConApp (unboxedTupleCon (length contents)) 
                (map (Type . snd) contents ++
                 map fst contents),
       mkTyConApp (unboxedTupleTyCon (length contents)) 
                  (map snd contents))
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[CoreToStg]{Converts Core to STG Syntax}

And, as we have the info in hand, we may convert some lets to
let-no-escapes.

\begin{code}
module CoreToStg ( coreToStg, coreExprToStg ) where

#include "HsVersions.h"

import CoreSyn
import CoreUtils	( rhsIsStatic, manifestArity, exprType, findDefault )
import StgSyn

import Type
import TyCon		( isAlgTyCon )
import Id
import Var		( Var, globalIdDetails, idType )
import TyCon		( isUnboxedTupleTyCon, isPrimTyCon, isFunTyCon, isHiBootTyCon )
#ifdef ILX
import MkId		( unsafeCoerceId )
#endif
import IdInfo
import DataCon
import CostCentre	( noCCS )
import VarSet
import VarEnv
import Maybes		( maybeToBool )
import Name		( getOccName, isExternalName, nameOccName )
import OccName		( occNameUserString, occNameFS )
import BasicTypes       ( Arity )
import DynFlags		( DynFlags )
import StaticFlags	( opt_RuntimeTypes )
import Outputable

infixr 9 `thenLne`
\end{code}

%************************************************************************
%*									*
\subsection[live-vs-free-doc]{Documentation}
%*									*
%************************************************************************

(There is other relevant documentation in codeGen/CgLetNoEscape.)

The actual Stg datatype is decorated with {\em live variable}
information, as well as {\em free variable} information.  The two are
{\em not} the same.  Liveness is an operational property rather than a
semantic one.  A variable is live at a particular execution point if
it can be referred to {\em directly} again.  In particular, a dead
variable's stack slot (if it has one):
\begin{enumerate}
\item
should be stubbed to avoid space leaks, and
\item
may be reused for something else.
\end{enumerate}

There ought to be a better way to say this.  Here are some examples:
\begin{verbatim}
	let v = [q] \[x] -> e
	in
	...v...	 (but no q's)
\end{verbatim}

Just after the `in', v is live, but q is dead.	If the whole of that
let expression was enclosed in a case expression, thus:
\begin{verbatim}
	case (let v = [q] \[x] -> e in ...v...) of
		alts[...q...]
\end{verbatim}
(ie @alts@ mention @q@), then @q@ is live even after the `in'; because
we'll return later to the @alts@ and need it.

Let-no-escapes make this a bit more interesting:
\begin{verbatim}
	let-no-escape v = [q] \ [x] -> e
	in
	...v...
\end{verbatim}
Here, @q@ is still live at the `in', because @v@ is represented not by
a closure but by the current stack state.  In other words, if @v@ is
live then so is @q@.  Furthermore, if @e@ mentions an enclosing
let-no-escaped variable, then {\em its} free variables are also live
if @v@ is.

%************************************************************************
%*									*
\subsection[caf-info]{Collecting live CAF info}
%*									*
%************************************************************************

In this pass we also collect information on which CAFs are live for 
constructing SRTs (see SRT.lhs).  

A top-level Id has CafInfo, which is

	- MayHaveCafRefs, if it may refer indirectly to
	  one or more CAFs, or
	- NoCafRefs if it definitely doesn't

The CafInfo has already been calculated during the CoreTidy pass.

During CoreToStg, we then pin onto each binding and case expression, a
list of Ids which represents the "live" CAFs at that point.  The meaning
of "live" here is the same as for live variables, see above (which is
why it's convenient to collect CAF information here rather than elsewhere).

The later SRT pass takes these lists of Ids and uses them to construct
the actual nested SRTs, and replaces the lists of Ids with (offset,length)
pairs.


Interaction of let-no-escape with SRTs   [Sept 01]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	let-no-escape x = ...caf1...caf2...
	in
	...x...x...x...

where caf1,caf2 are CAFs.  Since x doesn't have a closure, we 
build SRTs just as if x's defn was inlined at each call site, and
that means that x's CAF refs get duplicated in the overall SRT.

This is unlike ordinary lets, in which the CAF refs are not duplicated.

We could fix this loss of (static) sharing by making a sort of pseudo-closure
for x, solely to put in the SRTs lower down.


%************************************************************************
%*									*
\subsection[binds-StgVarInfo]{Setting variable info: top-level, binds, RHSs}
%*									*
%************************************************************************

\begin{code}
coreToStg :: DynFlags -> [CoreBind] -> IO [StgBinding]
coreToStg dflags pgm
  = return pgm'
  where (_, _, pgm') = coreTopBindsToStg dflags emptyVarEnv pgm

coreExprToStg :: CoreExpr -> StgExpr
coreExprToStg expr 
  = new_expr where (new_expr,_,_) = initLne emptyVarEnv (coreToStgExpr expr)


coreTopBindsToStg
    :: DynFlags
    -> IdEnv HowBound		-- environment for the bindings
    -> [CoreBind]
    -> (IdEnv HowBound, FreeVarsInfo, [StgBinding])

coreTopBindsToStg dflags env [] = (env, emptyFVInfo, [])
coreTopBindsToStg dflags env (b:bs)
  = (env2, fvs2, b':bs')
  where
	-- env accumulates down the list of binds, fvs accumulates upwards
	(env1, fvs2, b' ) = coreTopBindToStg dflags env fvs1 b
  	(env2, fvs1, bs') = coreTopBindsToStg dflags env1 bs


coreTopBindToStg
	:: DynFlags
	-> IdEnv HowBound
	-> FreeVarsInfo		-- Info about the body
	-> CoreBind
	-> (IdEnv HowBound, FreeVarsInfo, StgBinding)

coreTopBindToStg dflags env body_fvs (NonRec id rhs)
  = let 
	env' 	  = extendVarEnv env id how_bound
	how_bound = LetBound TopLet (manifestArity rhs)

        (stg_rhs, fvs') = 
	    initLne env (
              coreToTopStgRhs dflags body_fvs (id,rhs)	`thenLne` \ (stg_rhs, fvs') ->
	      returnLne (stg_rhs, fvs')
           )
	
	bind = StgNonRec id stg_rhs
    in
    ASSERT2(manifestArity rhs == stgRhsArity stg_rhs, ppr id)
    ASSERT2(consistentCafInfo id bind, ppr id)
--    WARN(not (consistent caf_info bind), ppr id <+> ppr cafs <+> ppCafInfo caf_info)
    (env', fvs' `unionFVInfo` body_fvs, bind)

coreTopBindToStg dflags env body_fvs (Rec pairs)
  = let 
	(binders, rhss) = unzip pairs

	extra_env' = [ (b, LetBound TopLet (manifestArity rhs))
		     | (b, rhs) <- pairs ]
	env' = extendVarEnvList env extra_env'

        (stg_rhss, fvs')
	  = initLne env' (
	       mapAndUnzipLne (coreToTopStgRhs dflags body_fvs) pairs
						`thenLne` \ (stg_rhss, fvss') ->
	       let fvs' = unionFVInfos fvss' in
	       returnLne (stg_rhss, fvs')
           )

	bind = StgRec (zip binders stg_rhss)
    in
    ASSERT2(and [manifestArity rhs == stgRhsArity stg_rhs | (rhs,stg_rhs) <- rhss `zip` stg_rhss], ppr binders)
    ASSERT2(consistentCafInfo (head binders) bind, ppr binders)
    (env', fvs' `unionFVInfo` body_fvs, bind)

#ifdef DEBUG
-- Assertion helper: this checks that the CafInfo on the Id matches
-- what CoreToStg has figured out about the binding's SRT.  The
-- CafInfo will be exact in all cases except when CorePrep has
-- floated out a binding, in which case it will be approximate.
consistentCafInfo id bind
  | occNameFS (nameOccName (idName id)) == FSLIT("sat")
  = safe
  | otherwise
  = WARN (not exact, ppr id) safe
  where
	safe  = id_marked_caffy || not binding_is_caffy
	exact = id_marked_caffy == binding_is_caffy
	id_marked_caffy  = mayHaveCafRefs (idCafInfo id)
	binding_is_caffy = stgBindHasCafRefs bind
#endif
\end{code}

\begin{code}
coreToTopStgRhs
	:: DynFlags
	-> FreeVarsInfo		-- Free var info for the scope of the binding
	-> (Id,CoreExpr)
	-> LneM (StgRhs, FreeVarsInfo)

coreToTopStgRhs dflags scope_fv_info (bndr, rhs)
  = coreToStgExpr rhs 		`thenLne` \ (new_rhs, rhs_fvs, _) ->
    freeVarsToLiveVars rhs_fvs	`thenLne` \ lv_info ->
    returnLne (mkTopStgRhs is_static rhs_fvs (mkSRT lv_info) bndr_info new_rhs, rhs_fvs)
  where
    bndr_info = lookupFVInfo scope_fv_info bndr
    is_static = rhsIsStatic dflags rhs

mkTopStgRhs :: Bool -> FreeVarsInfo -> SRT -> StgBinderInfo -> StgExpr
	-> StgRhs

mkTopStgRhs is_static rhs_fvs srt binder_info (StgLam _ bndrs body)
  = ASSERT( is_static )
    StgRhsClosure noCCS binder_info
		  (getFVs rhs_fvs)		 
		  ReEntrant
		  srt
		  bndrs body
	
mkTopStgRhs is_static rhs_fvs srt binder_info (StgConApp con args)
  | is_static	 -- StgConApps can be updatable (see isCrossDllConApp)
  = StgRhsCon noCCS con args

mkTopStgRhs is_static rhs_fvs srt binder_info rhs
  = ASSERT2( not is_static, ppr rhs )
    StgRhsClosure noCCS binder_info
		  (getFVs rhs_fvs)		 
	          Updatable
		  srt
	          [] rhs
\end{code}


-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

\begin{code}
coreToStgExpr
  	:: CoreExpr
	-> LneM (StgExpr,	-- Decorated STG expr
		 FreeVarsInfo,	-- Its free vars (NB free, not live)
		 EscVarsSet)	-- Its escapees, a subset of its free vars;
				-- also a subset of the domain of the envt
				-- because we are only interested in the escapees
				-- for vars which might be turned into
				-- let-no-escaped ones.
\end{code}

The second and third components can be derived in a simple bottom up pass, not
dependent on any decisions about which variables will be let-no-escaped or
not.  The first component, that is, the decorated expression, may then depend
on these components, but it in turn is not scrutinised as the basis for any
decisions.  Hence no black holes.

\begin{code}
coreToStgExpr (Lit l) = returnLne (StgLit l, emptyFVInfo, emptyVarSet)
coreToStgExpr (Var v) = coreToStgApp Nothing v []

coreToStgExpr expr@(App _ _)
  = coreToStgApp Nothing f args
  where
    (f, args) = myCollectArgs expr

coreToStgExpr expr@(Lam _ _)
  = let
	(args, body) = myCollectBinders expr 
	args'	     = filterStgBinders args
    in
    extendVarEnvLne [ (a, LambdaBound) | a <- args' ] $
    coreToStgExpr body  `thenLne` \ (body, body_fvs, body_escs) ->
    let
	fvs		= args' `minusFVBinders` body_fvs
	escs		= body_escs `delVarSetList` args'
	result_expr | null args' = body
		    | otherwise  = StgLam (exprType expr) args' body
    in
    returnLne (result_expr, fvs, escs)

coreToStgExpr (Note (SCC cc) expr)
  = coreToStgExpr expr		`thenLne` ( \ (expr2, fvs, escs) ->
    returnLne (StgSCC cc expr2, fvs, escs) )

#ifdef ILX
-- For ILX, convert (__coerce__ to_ty from_ty e) 
--	    into    (coerce to_ty from_ty e)
-- where coerce is real function
coreToStgExpr (Note (Coerce to_ty from_ty) expr)
  = coreToStgExpr (mkApps (Var unsafeCoerceId) 
			  [Type from_ty, Type to_ty, expr])
#endif

coreToStgExpr (Note other_note expr)
  = coreToStgExpr expr

-- Cases require a little more real work.

coreToStgExpr (Case scrut bndr _ alts)
  = extendVarEnvLne [(bndr, LambdaBound)]	(
	 mapAndUnzip3Lne vars_alt alts	`thenLne` \ (alts2, fvs_s, escs_s) ->
	 returnLne ( alts2,
		     unionFVInfos fvs_s,
		     unionVarSets escs_s )
    )			   		`thenLne` \ (alts2, alts_fvs, alts_escs) ->
    let
	-- Determine whether the default binder is dead or not
	-- This helps the code generator to avoid generating an assignment
	-- for the case binder (is extremely rare cases) ToDo: remove.
	bndr' | bndr `elementOfFVInfo` alts_fvs = bndr
	      | otherwise			= bndr `setIdOccInfo` IAmDead

	-- Don't consider the default binder as being 'live in alts',
	-- since this is from the point of view of the case expr, where
	-- the default binder is not free.
	alts_fvs_wo_bndr  = bndr `minusFVBinder` alts_fvs
	alts_escs_wo_bndr = alts_escs `delVarSet` bndr
    in

    freeVarsToLiveVars alts_fvs_wo_bndr		`thenLne` \ alts_lv_info ->

	-- We tell the scrutinee that everything 
	-- live in the alts is live in it, too.
    setVarsLiveInCont alts_lv_info (
	coreToStgExpr scrut	  `thenLne` \ (scrut2, scrut_fvs, scrut_escs) ->
        freeVarsToLiveVars scrut_fvs `thenLne` \ scrut_lv_info ->
	returnLne (scrut2, scrut_fvs, scrut_escs, scrut_lv_info)
      )    
		`thenLne` \ (scrut2, scrut_fvs, scrut_escs, scrut_lv_info) ->

    returnLne (
      StgCase scrut2 (getLiveVars scrut_lv_info)
		     (getLiveVars alts_lv_info)
		     bndr'
		     (mkSRT alts_lv_info)
		     (mkStgAltType (idType bndr) alts)
		     alts2,
      scrut_fvs `unionFVInfo` alts_fvs_wo_bndr,
      alts_escs_wo_bndr `unionVarSet` getFVSet scrut_fvs
		-- You might think we should have scrut_escs, not 
		-- (getFVSet scrut_fvs), but actually we can't call, and 
		-- then return from, a let-no-escape thing.
      )
  where
    vars_alt (con, binders, rhs)
      = let    	-- Remove type variables
	    binders' = filterStgBinders binders
        in	
        extendVarEnvLne [(b, LambdaBound) | b <- binders']	$
        coreToStgExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
        let
	    	-- Records whether each param is used in the RHS
	    good_use_mask = [ b `elementOfFVInfo` rhs_fvs | b <- binders' ]
        in
        returnLne ( (con, binders', good_use_mask, rhs2),
		    binders' `minusFVBinders` rhs_fvs,
		    rhs_escs `delVarSetList` binders' )
    		-- ToDo: remove the delVarSet;
    		-- since escs won't include any of these binders
\end{code}

Lets not only take quite a bit of work, but this is where we convert
then to let-no-escapes, if we wish.

(Meanwhile, we don't expect to see let-no-escapes...)
\begin{code}
coreToStgExpr (Let bind body)
  = fixLne (\ ~(_, _, _, no_binder_escapes) ->
	coreToStgLet no_binder_escapes bind body
    )				`thenLne` \ (new_let, fvs, escs, _) ->

    returnLne (new_let, fvs, escs)
\end{code}

\begin{code}
mkStgAltType scrut_ty alts
  = case splitTyConApp_maybe (repType scrut_ty) of
	Just (tc,_) | isUnboxedTupleTyCon tc -> UbxTupAlt tc
		    | isPrimTyCon tc	     -> PrimAlt tc
		    | isHiBootTyCon tc	     -> look_for_better_tycon
		    | isAlgTyCon tc 	     -> AlgAlt tc
		    | isFunTyCon tc	     -> PolyAlt
		    | otherwise		     -> pprPanic "mkStgAlts" (ppr tc)
	Nothing				     -> PolyAlt

  where
   -- Sometimes, the TyCon in the type of the scrutinee is an HiBootTyCon,
   -- which may not have any constructors inside it.  If so, then we
   -- can get a better TyCon by grabbing the one from a constructor alternative
   -- if one exists.
   look_for_better_tycon
	| ((DataAlt con, _, _) : _) <- data_alts = 
		AlgAlt (dataConTyCon con)
	| otherwise =
		ASSERT(null data_alts)
		PolyAlt
	where
		(data_alts, deflt) = findDefault alts
\end{code}


-- ---------------------------------------------------------------------------
-- Applications
-- ---------------------------------------------------------------------------

\begin{code}
coreToStgApp
	 :: Maybe UpdateFlag		-- Just upd <=> this application is
					-- the rhs of a thunk binding
					-- 	x = [...] \upd [] -> the_app
					-- with specified update flag
	-> Id				-- Function
	-> [CoreArg]			-- Arguments
	-> LneM (StgExpr, FreeVarsInfo, EscVarsSet)

coreToStgApp maybe_thunk_body f args
  = coreToStgArgs args		`thenLne` \ (args', args_fvs) ->
    lookupVarLne f		`thenLne` \ how_bound ->

    let
	n_val_args	 = valArgCount args
	not_letrec_bound = not (isLetBound how_bound)
	fun_fvs	 	 
          = let fvs = singletonFVInfo f how_bound fun_occ in
            -- e.g. (f :: a -> int) (x :: a) 
            -- Here the free variables are "f", "x" AND the type variable "a"
            -- coreToStgArgs will deal with the arguments recursively
            if opt_RuntimeTypes then
	      fvs `unionFVInfo` tyvarFVInfo (tyVarsOfType (idType f))
	    else fvs

	-- Mostly, the arity info of a function is in the fn's IdInfo
	-- But new bindings introduced by CoreSat may not have no
	-- arity info; it would do us no good anyway.  For example:
	--	let f = \ab -> e in f
	-- No point in having correct arity info for f!
	-- Hence the hasArity stuff below.
	-- NB: f_arity is only consulted for LetBound things
	f_arity   = stgArity f how_bound
	saturated = f_arity <= n_val_args

	fun_occ 
	 | not_letrec_bound	    = noBinderInfo	-- Uninteresting variable
	 | f_arity > 0 && saturated = stgSatOcc	-- Saturated or over-saturated function call
	 | otherwise		    = stgUnsatOcc	-- Unsaturated function or thunk

	fun_escs
	 | not_letrec_bound      = emptyVarSet	-- Only letrec-bound escapees are interesting
	 | f_arity == n_val_args = emptyVarSet	-- A function *or thunk* with an exactly
						-- saturated call doesn't escape
						-- (let-no-escape applies to 'thunks' too)

	 | otherwise 	     = unitVarSet f	-- Inexact application; it does escape

	-- At the moment of the call:

	--  either the function is *not* let-no-escaped, in which case
	--  	   nothing is live except live_in_cont
	--	or the function *is* let-no-escaped in which case the
	--	   variables it uses are live, but still the function
	--	   itself is not.  PS.  In this case, the function's
	--	   live vars should already include those of the
	--	   continuation, but it does no harm to just union the
	--	   two regardless.

	res_ty = exprType (mkApps (Var f) args)
	app = case globalIdDetails f of
      		DataConWorkId dc | saturated -> StgConApp dc args'
	        PrimOpId op  		     -> ASSERT( saturated )
					        StgOpApp (StgPrimOp op) args' res_ty
		FCallId call	 -> ASSERT( saturated )
				    StgOpApp (StgFCallOp call (idUnique f)) args' res_ty
		_other      	 -> StgApp f args'

    in
    returnLne (
	app,
	fun_fvs  `unionFVInfo` args_fvs,
	fun_escs `unionVarSet` (getFVSet args_fvs)
				-- All the free vars of the args are disqualified
				-- from being let-no-escaped.
    )



-- ---------------------------------------------------------------------------
-- Argument lists
-- This is the guy that turns applications into A-normal form
-- ---------------------------------------------------------------------------

coreToStgArgs :: [CoreArg] -> LneM ([StgArg], FreeVarsInfo)
coreToStgArgs []
  = returnLne ([], emptyFVInfo)

coreToStgArgs (Type ty : args)	-- Type argument
  = coreToStgArgs args	`thenLne` \ (args', fvs) ->
    if opt_RuntimeTypes then
 	returnLne (StgTypeArg ty : args', fvs `unionFVInfo` tyvarFVInfo (tyVarsOfType ty))
    else
    returnLne (args', fvs)

coreToStgArgs (arg : args)	-- Non-type argument
  = coreToStgArgs args	`thenLne` \ (stg_args, args_fvs) ->
    coreToStgExpr arg	`thenLne` \ (arg', arg_fvs, escs) ->
    let
	fvs = args_fvs `unionFVInfo` arg_fvs
	stg_arg = case arg' of
		       StgApp v []      -> StgVarArg v
		       StgConApp con [] -> StgVarArg (dataConWorkId con)
		       StgLit lit       -> StgLitArg lit
		       _ 		-> pprPanic "coreToStgArgs" (ppr arg)
    in
    returnLne (stg_arg : stg_args, fvs)


-- ---------------------------------------------------------------------------
-- The magic for lets:
-- ---------------------------------------------------------------------------

coreToStgLet
	 :: Bool	-- True <=> yes, we are let-no-escaping this let
	 -> CoreBind	-- bindings
	 -> CoreExpr	-- body
    	 -> LneM (StgExpr,	-- new let
		  FreeVarsInfo,	-- variables free in the whole let
		  EscVarsSet,	-- variables that escape from the whole let
		  Bool)		-- True <=> none of the binders in the bindings
				-- is among the escaping vars

coreToStgLet let_no_escape bind body
  = fixLne (\ ~(_, _, _, _, _, rec_body_fvs, _, _) ->

	-- Do the bindings, setting live_in_cont to empty if
	-- we ain't in a let-no-escape world
	getVarsLiveInCont		`thenLne` \ live_in_cont ->
	setVarsLiveInCont (if let_no_escape 
				then live_in_cont 
				else emptyLiveInfo)
			  (vars_bind rec_body_fvs bind)
	    `thenLne` \ ( bind2, bind_fvs, bind_escs, bind_lv_info, env_ext) ->

  	-- Do the body
	extendVarEnvLne env_ext (
	  coreToStgExpr body          `thenLne` \(body2, body_fvs, body_escs) ->
	  freeVarsToLiveVars body_fvs `thenLne` \ body_lv_info ->

  	  returnLne (bind2, bind_fvs, bind_escs, getLiveVars bind_lv_info,
		     body2, body_fvs, body_escs, getLiveVars body_lv_info)
	)

    ) `thenLne` (\ (bind2, bind_fvs, bind_escs, bind_lvs, 
		    body2, body_fvs, body_escs, body_lvs) ->


	-- Compute the new let-expression
    let
	new_let | let_no_escape = StgLetNoEscape live_in_whole_let bind_lvs bind2 body2
		| otherwise	= StgLet bind2 body2

	free_in_whole_let
	  = binders `minusFVBinders` (bind_fvs `unionFVInfo` body_fvs)

	live_in_whole_let
	  = bind_lvs `unionVarSet` (body_lvs `delVarSetList` binders)

	real_bind_escs = if let_no_escape then
			    bind_escs
			 else
			    getFVSet bind_fvs
			    -- Everything escapes which is free in the bindings

	let_escs = (real_bind_escs `unionVarSet` body_escs) `delVarSetList` binders

	all_escs = bind_escs `unionVarSet` body_escs	-- Still includes binders of
							-- this let(rec)

	no_binder_escapes = isEmptyVarSet (set_of_binders `intersectVarSet` all_escs)

#ifdef DEBUG
	-- Debugging code as requested by Andrew Kennedy
	checked_no_binder_escapes
		| not no_binder_escapes && any is_join_var binders
		= pprTrace "Interesting!  A join var that isn't let-no-escaped" (ppr binders)
		  False
		| otherwise = no_binder_escapes
#else
	checked_no_binder_escapes = no_binder_escapes
#endif
			    
		-- Mustn't depend on the passed-in let_no_escape flag, since
		-- no_binder_escapes is used by the caller to derive the flag!
    in
    returnLne (
	new_let,
	free_in_whole_let,
	let_escs,
	checked_no_binder_escapes
    ))
  where
    set_of_binders = mkVarSet binders
    binders	   = bindersOf bind

    mk_binding bind_lv_info binder rhs
	= (binder, LetBound (NestedLet live_vars) (manifestArity rhs))
	where
	   live_vars | let_no_escape = addLiveVar bind_lv_info binder
		     | otherwise     = unitLiveVar binder
		-- c.f. the invariant on NestedLet

    vars_bind :: FreeVarsInfo		-- Free var info for body of binding
	      -> CoreBind
	      -> LneM (StgBinding,
		       FreeVarsInfo, 
		       EscVarsSet,  	  -- free vars; escapee vars
		       LiveInfo,	  -- Vars and CAFs live in binding
		       [(Id, HowBound)])  -- extension to environment
					 

    vars_bind body_fvs (NonRec binder rhs)
      = coreToStgRhs body_fvs [] (binder,rhs)
				`thenLne` \ (rhs2, bind_fvs, bind_lv_info, escs) ->
	let
	    env_ext_item = mk_binding bind_lv_info binder rhs
	in
	returnLne (StgNonRec binder rhs2, 
		   bind_fvs, escs, bind_lv_info, [env_ext_item])


    vars_bind body_fvs (Rec pairs)
      = fixLne (\ ~(_, rec_rhs_fvs, _, bind_lv_info, _) ->
	   let
		rec_scope_fvs = unionFVInfo body_fvs rec_rhs_fvs
	        binders = map fst pairs
	        env_ext = [ mk_binding bind_lv_info b rhs 
			  | (b,rhs) <- pairs ]
	   in
	   extendVarEnvLne env_ext (
	      mapAndUnzip4Lne (coreToStgRhs rec_scope_fvs binders) pairs 
					`thenLne` \ (rhss2, fvss, lv_infos, escss) ->
	      let
			bind_fvs = unionFVInfos fvss
			bind_lv_info = foldr unionLiveInfo emptyLiveInfo lv_infos
			escs     = unionVarSets escss
	      in
	      returnLne (StgRec (binders `zip` rhss2),
			 bind_fvs, escs, bind_lv_info, env_ext)
	   )
	)

is_join_var :: Id -> Bool
-- A hack (used only for compiler debuggging) to tell if
-- a variable started life as a join point ($j)
is_join_var j = occNameUserString (getOccName j) == "$j"
\end{code}

\begin{code}
coreToStgRhs :: FreeVarsInfo		-- Free var info for the scope of the binding
	     -> [Id]
	     -> (Id,CoreExpr)
  	     -> LneM (StgRhs, FreeVarsInfo, LiveInfo, EscVarsSet)

coreToStgRhs scope_fv_info binders (bndr, rhs)
  = coreToStgExpr rhs 		`thenLne` \ (new_rhs, rhs_fvs, rhs_escs) ->
    getEnvLne			`thenLne` \ env ->    
    freeVarsToLiveVars (binders `minusFVBinders` rhs_fvs) `thenLne` \ lv_info ->
    returnLne (mkStgRhs rhs_fvs (mkSRT lv_info) bndr_info new_rhs,
	       rhs_fvs, lv_info, rhs_escs)
  where
    bndr_info = lookupFVInfo scope_fv_info bndr

mkStgRhs :: FreeVarsInfo -> SRT -> StgBinderInfo -> StgExpr -> StgRhs

mkStgRhs rhs_fvs srt binder_info (StgConApp con args)
  = StgRhsCon noCCS con args

mkStgRhs rhs_fvs srt binder_info (StgLam _ bndrs body)
  = StgRhsClosure noCCS binder_info
		  (getFVs rhs_fvs)		 
		  ReEntrant
		  srt bndrs body
	
mkStgRhs rhs_fvs srt binder_info rhs
  = StgRhsClosure noCCS binder_info
		  (getFVs rhs_fvs)		 
	          upd_flag srt [] rhs
  where
   upd_flag = Updatable
  {-
    SDM: disabled.  Eval/Apply can't handle functions with arity zero very
    well; and making these into simple non-updatable thunks breaks other
    assumptions (namely that they will be entered only once).

    upd_flag | isPAP env rhs  = ReEntrant
	     | otherwise      = Updatable
  -}

{- ToDo:
          upd = if isOnceDem dem
      		    then (if isNotTop toplev 
                	    then SingleEntry    -- HA!  Paydirt for "dem"
                	    else 
#ifdef DEBUG
                     trace "WARNING: SE CAFs unsupported, forcing UPD instead" $
#endif
                     Updatable)
          	else Updatable
        -- For now we forbid SingleEntry CAFs; they tickle the
        -- ASSERT in rts/Storage.c line 215 at newCAF() re mut_link,
        -- and I don't understand why.  There's only one SE_CAF (well,
        -- only one that tickled a great gaping bug in an earlier attempt
        -- at ClosureInfo.getEntryConvention) in the whole of nofib, 
        -- specifically Main.lvl6 in spectral/cryptarithm2.
        -- So no great loss.  KSW 2000-07.
-}
\end{code}

Detect thunks which will reduce immediately to PAPs, and make them
non-updatable.  This has several advantages:

        - the non-updatable thunk behaves exactly like the PAP,

	- the thunk is more efficient to enter, because it is
	  specialised to the task.

        - we save one update frame, one stg_update_PAP, one update
	  and lots of PAP_enters.

	- in the case where the thunk is top-level, we save building
	  a black hole and futhermore the thunk isn't considered to
	  be a CAF any more, so it doesn't appear in any SRTs.

We do it here, because the arity information is accurate, and we need
to do it before the SRT pass to save the SRT entries associated with
any top-level PAPs.

isPAP env (StgApp f args) = listLengthCmp args arity == LT -- idArity f > length args
			  where
			    arity = stgArity f (lookupBinding env f)
isPAP env _ 	          = False


%************************************************************************
%*									*
\subsection[LNE-monad]{A little monad for this let-no-escaping pass}
%*									*
%************************************************************************

There's a lot of stuff to pass around, so we use this @LneM@ monad to
help.  All the stuff here is only passed *down*.

\begin{code}
type LneM a =  IdEnv HowBound
	    -> LiveInfo		-- Vars and CAFs live in continuation
	    -> a

type LiveInfo = (StgLiveVars, 	-- Dynamic live variables; 
				-- i.e. ones with a nested (non-top-level) binding
		 CafSet)	-- Static live variables;
				-- i.e. top-level variables that are CAFs or refer to them

type EscVarsSet = IdSet
type CafSet     = IdSet

data HowBound
  = ImportBound		-- Used only as a response to lookupBinding; never
			-- exists in the range of the (IdEnv HowBound)

  | LetBound		-- A let(rec) in this module
	LetInfo		-- Whether top level or nested
 	Arity		-- Its arity (local Ids don't have arity info at this point)

  | LambdaBound		-- Used for both lambda and case

data LetInfo
  = TopLet		-- top level things
  | NestedLet LiveInfo	-- For nested things, what is live if this
			-- thing is live?  Invariant: the binder
			-- itself is always a member of
			-- the dynamic set of its own LiveInfo

isLetBound (LetBound _ _) = True
isLetBound other 	  = False

topLevelBound ImportBound	  = True
topLevelBound (LetBound TopLet _) = True
topLevelBound other		  = False
\end{code}

For a let(rec)-bound variable, x, we record LiveInfo, the set of
variables that are live if x is live.  This LiveInfo comprises
	(a) dynamic live variables (ones with a non-top-level binding)
	(b) static live variabes (CAFs or things that refer to CAFs)

For "normal" variables (a) is just x alone.  If x is a let-no-escaped
variable then x is represented by a code pointer and a stack pointer
(well, one for each stack).  So all of the variables needed in the
execution of x are live if x is, and are therefore recorded in the
LetBound constructor; x itself *is* included.

The set of dynamic live variables is guaranteed ot have no further let-no-escaped
variables in it.

\begin{code}
emptyLiveInfo :: LiveInfo
emptyLiveInfo = (emptyVarSet,emptyVarSet)

unitLiveVar :: Id -> LiveInfo
unitLiveVar lv = (unitVarSet lv, emptyVarSet)

unitLiveCaf :: Id -> LiveInfo
unitLiveCaf caf = (emptyVarSet, unitVarSet caf)

addLiveVar :: LiveInfo -> Id -> LiveInfo
addLiveVar (lvs, cafs) id = (lvs `extendVarSet` id, cafs)

unionLiveInfo :: LiveInfo -> LiveInfo -> LiveInfo
unionLiveInfo (lv1,caf1) (lv2,caf2) = (lv1 `unionVarSet` lv2, caf1 `unionVarSet` caf2)

mkSRT :: LiveInfo -> SRT
mkSRT (_, cafs) = SRTEntries cafs

getLiveVars :: LiveInfo -> StgLiveVars
getLiveVars (lvs, _) = lvs
\end{code}


The std monad functions:
\begin{code}
initLne :: IdEnv HowBound -> LneM a -> a
initLne env m = m env emptyLiveInfo



{-# INLINE thenLne #-}
{-# INLINE returnLne #-}

returnLne :: a -> LneM a
returnLne e env lvs_cont = e

thenLne :: LneM a -> (a -> LneM b) -> LneM b
thenLne m k env lvs_cont 
  = k (m env lvs_cont) env lvs_cont

mapAndUnzipLne  :: (a -> LneM (b,c))   -> [a] -> LneM ([b],[c])
mapAndUnzipLne f [] = returnLne ([],[])
mapAndUnzipLne f (x:xs)
  = f x		    	`thenLne` \ (r1,  r2)  ->
    mapAndUnzipLne f xs	`thenLne` \ (rs1, rs2) ->
    returnLne (r1:rs1, r2:rs2)

mapAndUnzip3Lne :: (a -> LneM (b,c,d)) -> [a] -> LneM ([b],[c],[d])
mapAndUnzip3Lne f []	= returnLne ([],[],[])
mapAndUnzip3Lne f (x:xs)
  = f x		    	 `thenLne` \ (r1,  r2,  r3)  ->
    mapAndUnzip3Lne f xs `thenLne` \ (rs1, rs2, rs3) ->
    returnLne (r1:rs1, r2:rs2, r3:rs3)

mapAndUnzip4Lne :: (a -> LneM (b,c,d,e)) -> [a] -> LneM ([b],[c],[d],[e])
mapAndUnzip4Lne f []	= returnLne ([],[],[],[])
mapAndUnzip4Lne f (x:xs)
  = f x		    	 `thenLne` \ (r1,  r2,  r3, r4)  ->
    mapAndUnzip4Lne f xs `thenLne` \ (rs1, rs2, rs3, rs4) ->
    returnLne (r1:rs1, r2:rs2, r3:rs3, r4:rs4)

fixLne :: (a -> LneM a) -> LneM a
fixLne expr env lvs_cont
  = result
  where
    result = expr result env lvs_cont
\end{code}

Functions specific to this monad:

\begin{code}
getVarsLiveInCont :: LneM LiveInfo
getVarsLiveInCont env lvs_cont = lvs_cont

setVarsLiveInCont :: LiveInfo -> LneM a -> LneM a
setVarsLiveInCont new_lvs_cont expr env lvs_cont
  = expr env new_lvs_cont

extendVarEnvLne :: [(Id, HowBound)] -> LneM a -> LneM a
extendVarEnvLne ids_w_howbound expr env lvs_cont
  = expr (extendVarEnvList env ids_w_howbound) lvs_cont

lookupVarLne :: Id -> LneM HowBound
lookupVarLne v env lvs_cont = returnLne (lookupBinding env v) env lvs_cont

getEnvLne :: LneM (IdEnv HowBound)
getEnvLne env lvs_cont = returnLne env env lvs_cont

lookupBinding :: IdEnv HowBound -> Id -> HowBound
lookupBinding env v = case lookupVarEnv env v of
			Just xx -> xx
			Nothing -> ASSERT2( isGlobalId v, ppr v ) ImportBound


-- The result of lookupLiveVarsForSet, a set of live variables, is
-- only ever tacked onto a decorated expression. It is never used as
-- the basis of a control decision, which might give a black hole.

freeVarsToLiveVars :: FreeVarsInfo -> LneM LiveInfo
freeVarsToLiveVars fvs env live_in_cont
  = returnLne live_info env live_in_cont
  where
    live_info    = foldr unionLiveInfo live_in_cont lvs_from_fvs
    lvs_from_fvs = map do_one (allFreeIds fvs)

    do_one (v, how_bound)
      = case how_bound of
	  ImportBound 		          -> unitLiveCaf v	-- Only CAF imports are 
								-- recorded in fvs
	  LetBound TopLet _ 		 
		| mayHaveCafRefs (idCafInfo v) -> unitLiveCaf v
		| otherwise		       -> emptyLiveInfo

	  LetBound (NestedLet lvs) _      -> lvs	-- lvs already contains v
							-- (see the invariant on NestedLet)

	  _lambda_or_case_binding	  -> unitLiveVar v	-- Bound by lambda or case
\end{code}

%************************************************************************
%*									*
\subsection[Free-var info]{Free variable information}
%*									*
%************************************************************************

\begin{code}
type FreeVarsInfo = VarEnv (Var, HowBound, StgBinderInfo)
	-- The Var is so we can gather up the free variables
	-- as a set.
	--
	-- The HowBound info just saves repeated lookups;
	-- we look up just once when we encounter the occurrence.
	-- INVARIANT: Any ImportBound Ids are HaveCafRef Ids
	--	      Imported Ids without CAF refs are simply
	--	      not put in the FreeVarsInfo for an expression.
	--	      See singletonFVInfo and freeVarsToLiveVars
	--
	-- StgBinderInfo records how it occurs; notably, we
	-- are interested in whether it only occurs in saturated 
	-- applications, because then we don't need to build a
	-- curried version.
	-- If f is mapped to noBinderInfo, that means
	-- that f *is* mentioned (else it wouldn't be in the
	-- IdEnv at all), but perhaps in an unsaturated applications.
	--
	-- All case/lambda-bound things are also mapped to
	-- noBinderInfo, since we aren't interested in their
	-- occurence info.
	--
	-- For ILX we track free var info for type variables too;
	-- hence VarEnv not IdEnv
\end{code}

\begin{code}
emptyFVInfo :: FreeVarsInfo
emptyFVInfo = emptyVarEnv

singletonFVInfo :: Id -> HowBound -> StgBinderInfo -> FreeVarsInfo
-- Don't record non-CAF imports at all, to keep free-var sets small
singletonFVInfo id ImportBound info
   | mayHaveCafRefs (idCafInfo id) = unitVarEnv id (id, ImportBound, info)
   | otherwise         		   = emptyVarEnv
singletonFVInfo id how_bound info  = unitVarEnv id (id, how_bound, info)

tyvarFVInfo :: TyVarSet -> FreeVarsInfo
tyvarFVInfo tvs = foldVarSet add emptyFVInfo tvs
        where
	  add tv fvs = extendVarEnv fvs tv (tv, LambdaBound, noBinderInfo)
		-- Type variables must be lambda-bound

unionFVInfo :: FreeVarsInfo -> FreeVarsInfo -> FreeVarsInfo
unionFVInfo fv1 fv2 = plusVarEnv_C plusFVInfo fv1 fv2

unionFVInfos :: [FreeVarsInfo] -> FreeVarsInfo
unionFVInfos fvs = foldr unionFVInfo emptyFVInfo fvs

minusFVBinders :: [Id] -> FreeVarsInfo -> FreeVarsInfo
minusFVBinders vs fv = foldr minusFVBinder fv vs

minusFVBinder :: Id -> FreeVarsInfo -> FreeVarsInfo
minusFVBinder v fv | isId v && opt_RuntimeTypes
		   = (fv `delVarEnv` v) `unionFVInfo` 
		     tyvarFVInfo (tyVarsOfType (idType v))
		   | otherwise = fv `delVarEnv` v
	-- When removing a binder, remember to add its type variables
	-- c.f. CoreFVs.delBinderFV

elementOfFVInfo :: Id -> FreeVarsInfo -> Bool
elementOfFVInfo id fvs = maybeToBool (lookupVarEnv fvs id)

lookupFVInfo :: FreeVarsInfo -> Id -> StgBinderInfo
-- Find how the given Id is used.
-- Externally visible things may be used any old how
lookupFVInfo fvs id 
  | isExternalName (idName id) = noBinderInfo
  | otherwise = case lookupVarEnv fvs id of
			Nothing         -> noBinderInfo
			Just (_,_,info) -> info

allFreeIds :: FreeVarsInfo -> [(Id,HowBound)]	-- Both top level and non-top-level Ids
allFreeIds fvs = [(id,how_bound) | (id,how_bound,_) <- varEnvElts fvs, isId id]

-- Non-top-level things only, both type variables and ids
-- (type variables only if opt_RuntimeTypes)
getFVs :: FreeVarsInfo -> [Var]	
getFVs fvs = [id | (id, how_bound, _) <- varEnvElts fvs, 
		    not (topLevelBound how_bound) ]

getFVSet :: FreeVarsInfo -> VarSet
getFVSet fvs = mkVarSet (getFVs fvs)

plusFVInfo (id1,hb1,info1) (id2,hb2,info2)
  = ASSERT (id1 == id2 && hb1 `check_eq_how_bound` hb2)
    (id1, hb1, combineStgBinderInfo info1 info2)

#ifdef DEBUG
-- The HowBound info for a variable in the FVInfo should be consistent
check_eq_how_bound ImportBound 	      ImportBound 	 = True
check_eq_how_bound LambdaBound 	      LambdaBound 	 = True
check_eq_how_bound (LetBound li1 ar1) (LetBound li2 ar2) = ar1 == ar2 && check_eq_li li1 li2
check_eq_how_bound hb1		      hb2		 = False

check_eq_li (NestedLet _) (NestedLet _) = True
check_eq_li TopLet        TopLet        = True
check_eq_li li1 	  li2		= False
#endif
\end{code}

Misc.
\begin{code}
filterStgBinders :: [Var] -> [Var]
filterStgBinders bndrs
  | opt_RuntimeTypes = bndrs
  | otherwise	     = filter isId bndrs
\end{code}


\begin{code}
	-- Ignore all notes except SCC
myCollectBinders expr
  = go [] expr
  where
    go bs (Lam b e)          = go (b:bs) e
    go bs e@(Note (SCC _) _) = (reverse bs, e) 
    go bs (Note _ e)         = go bs e
    go bs e	             = (reverse bs, e)

myCollectArgs :: CoreExpr -> (Id, [CoreArg])
	-- We assume that we only have variables
	-- in the function position by now
myCollectArgs expr
  = go expr []
  where
    go (Var v)          as = (v, as)
    go (App f a) as        = go f (a:as)
    go (Note (SCC _) e) as = pprPanic "CoreToStg.myCollectArgs" (ppr expr)
    go (Note n e)       as = go e as
    go _		as = pprPanic "CoreToStg.myCollectArgs" (ppr expr)
\end{code}

\begin{code}
stgArity :: Id -> HowBound -> Arity
stgArity f (LetBound _ arity) = arity
stgArity f ImportBound	      = idArity f
stgArity f LambdaBound        = 0
\end{code}

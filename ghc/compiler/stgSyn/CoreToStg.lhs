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
import CoreFVs
import CoreUtils
import SimplUtils
import StgSyn

import Type
import TyCon		( isAlgTyCon )
import Id
import Var		( Var, globalIdDetails )
import IdInfo
import DataCon
import CostCentre	( noCCS )
import VarSet
import VarEnv
import DataCon		( dataConWrapId )
import IdInfo		( OccInfo(..) )
import PrimOp		( PrimOp(..), ccallMayGC )
import TysPrim		( foreignObjPrimTyCon )
import Maybes		( maybeToBool, orElse )
import Name		( getOccName, isExternallyVisibleName )
import Module		( Module )
import OccName		( occNameUserString )
import BasicTypes       ( TopLevelFlag(..), isNotTopLevel )
import CmdLineOpts	( DynFlags, opt_KeepStgTypes )
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
\subsection[binds-StgVarInfo]{Setting variable info: top-level, binds, RHSs}
%*									*
%************************************************************************

\begin{code}
coreToStg :: DynFlags -> Module -> [CoreBind] -> IO [StgBinding]
coreToStg dflags this_mod pgm
  = return (fst (initLne (coreTopBindsToStg pgm)))

coreExprToStg :: CoreExpr -> StgExpr
coreExprToStg expr 
  = new_expr where (new_expr,_,_) = initLne (coreToStgExpr expr)

-- For top-level guys, we basically aren't worried about this
-- live-variable stuff; we do need to keep adding to the environment
-- as we step through the bindings (using @extendVarEnv@).

coreTopBindsToStg :: [CoreBind] -> LneM ([StgBinding], FreeVarsInfo)

coreTopBindsToStg [] = returnLne ([], emptyFVInfo)
coreTopBindsToStg (bind:binds)
  =  let 
         binders = bindersOf bind
	 env_extension = binders `zip` repeat how_bound
    	 how_bound = LetrecBound True {- top level -}
				 emptyVarSet
     in

     extendVarEnvLne env_extension (
       coreTopBindsToStg binds		       `thenLne` \ (binds', fv_binds) ->
       coreTopBindToStg binders fv_binds bind  `thenLne` \ (bind',  fv_bind) ->
       returnLne (
		  (bind' : binds'),
		  binders `minusFVBinders` (fv_binds `unionFVInfo` fv_bind)
		 )
      )


coreTopBindToStg
	:: [Id]			-- New binders (with correct arity)
	-> FreeVarsInfo		-- Info about the body
	-> CoreBind
	-> LneM (StgBinding, FreeVarsInfo)

coreTopBindToStg [binder] body_fvs (NonRec _ rhs)
  = coreToStgRhs body_fvs TopLevel (binder,rhs)	`thenLne` \ (rhs2, fvs, _) ->
    returnLne (StgNonRec binder rhs2, fvs)

coreTopBindToStg binders body_fvs (Rec pairs)
  = fixLne (\ ~(_, rec_rhs_fvs) ->
	let scope_fvs = unionFVInfo body_fvs rec_rhs_fvs
	in
	mapAndUnzip3Lne (coreToStgRhs scope_fvs TopLevel) pairs 
						`thenLne` \ (rhss2, fvss, _) ->
	let fvs = unionFVInfos fvss
	in
	returnLne (StgRec (binders `zip` rhss2), fvs)
    )
\end{code}

\begin{code}
coreToStgRhs
	:: FreeVarsInfo		-- Free var info for the scope of the binding
	-> TopLevelFlag
	-> (Id,CoreExpr)
	-> LneM (StgRhs, FreeVarsInfo, EscVarsSet)

coreToStgRhs scope_fv_info top (binder, rhs)
  = coreToStgExpr rhs  `thenLne` \ (new_rhs, rhs_fvs, rhs_escs) ->
    returnLne (mkStgRhs top rhs_fvs binder_info new_rhs, 
	       rhs_fvs, rhs_escs)
  where
    binder_info = lookupFVInfo scope_fv_info binder

mkStgRhs :: TopLevelFlag -> FreeVarsInfo -> StgBinderInfo
	 -> StgExpr -> StgRhs

mkStgRhs top rhs_fvs binder_info (StgLam _ bndrs body)
  = StgRhsClosure noCCS binder_info noSRT
		  (getFVs rhs_fvs)		 
		  ReEntrant
		  bndrs body
	
mkStgRhs top rhs_fvs binder_info (StgConApp con args)
  | isNotTopLevel top || not (isDllConApp con args)
  = StgRhsCon noCCS con args

mkStgRhs top rhs_fvs binder_info rhs
  = StgRhsClosure noCCS binder_info noSRT
		  (getFVs rhs_fvs)		 
	          (updatable [] rhs)
	          [] rhs
  where
    updatable args body | null args && isPAP body  = ReEntrant
		        | otherwise                = Updatable
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

\begin{code}
isPAP (StgApp f args) = idArity f > length args
isPAP _ 	      = False
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
  = let (args, body) = myCollectBinders expr 
	args'	     = filterStgBinders args
    in
    extendVarEnvLne [ (a, LambdaBound) | a <- args' ] $
    coreToStgExpr body  `thenLne` \ (body, body_fvs, body_escs) ->
    let
	set_of_args	= mkVarSet args'
	fvs		= args' `minusFVBinders` body_fvs
	escs		= body_escs `minusVarSet`    set_of_args
    in
    if null args'
	then returnLne (body, fvs, escs)
	else returnLne (StgLam (exprType expr) args' body, fvs, escs)

coreToStgExpr (Note (SCC cc) expr)
  = coreToStgExpr expr		`thenLne` ( \ (expr2, fvs, escs) ->
    returnLne (StgSCC cc expr2, fvs, escs) )

coreToStgExpr (Note other_note expr)
  = coreToStgExpr expr


-- Cases require a little more real work.

coreToStgExpr (Case scrut bndr alts)
  = getVarsLiveInCont				`thenLne` \ live_in_cont ->
    extendVarEnvLne [(bndr, CaseBound)]	$
    vars_alts (findDefault alts)		`thenLne` \ (alts2, alts_fvs, alts_escs) ->
    lookupLiveVarsForSet alts_fvs		`thenLne` \ alts_lvs ->
    let
	-- determine whether the default binder is dead or not
	-- This helps the code generator to avoid generating an assignment
	-- for the case binder (is extremely rare cases) ToDo: remove.
	bndr'= if (bndr `elementOfFVInfo` alts_fvs) 
		  then bndr
		  else bndr `setIdOccInfo` IAmDead

	 -- for a _ccall_GC_, some of the *arguments* need to live across the
	 -- call (see findLiveArgs comments.), so we annotate them as being live
	 -- in the alts to achieve the desired effect.
	mb_live_across_case =
	  case scrut of
	    -- ToDo: Notes?
	    e@(App _ _) | (v, args) <- myCollectArgs e,
			  PrimOpId (CCallOp ccall) <- globalIdDetails v,
			  ccallMayGC ccall
			  -> Just (filterVarSet isForeignObjArg (exprFreeVars e))
	    _   -> Nothing

	-- Don't consider the default binder as being 'live in alts',
	-- since this is from the point of view of the case expr, where
	-- the default binder is not free.
	live_in_alts = orElse (FMAP unionVarSet mb_live_across_case) id $
		       live_in_cont `unionVarSet` 
		       (alts_lvs `minusVarSet` unitVarSet bndr)
    in
	-- we tell the scrutinee that everything live in the alts
	-- is live in it, too.
    setVarsLiveInCont live_in_alts (
	coreToStgExpr scrut
    )			   `thenLne` \ (scrut2, scrut_fvs, scrut_escs) ->

    lookupLiveVarsForSet scrut_fvs `thenLne` \ scrut_lvs ->
    let
	live_in_whole_case = live_in_alts `unionVarSet` scrut_lvs
    in
    returnLne (
      StgCase scrut2 live_in_whole_case live_in_alts bndr' noSRT alts2,
      bndr `minusFVBinder` (scrut_fvs `unionFVInfo` alts_fvs),
      (alts_escs `minusVarSet` unitVarSet bndr) `unionVarSet` getFVSet scrut_fvs
		-- You might think we should have scrut_escs, not (getFVSet scrut_fvs),
		-- but actually we can't call, and then return from, a let-no-escape thing.
      )
  where
    scrut_ty   = idType bndr
    prim_case  = isUnLiftedType scrut_ty && not (isUnboxedTupleType scrut_ty)

    vars_alts (alts,deflt)
	| prim_case
        = mapAndUnzip3Lne vars_prim_alt alts
			`thenLne` \ (alts2,  alts_fvs_list,  alts_escs_list) ->
	  let
	      alts_fvs  = unionFVInfos alts_fvs_list
	      alts_escs = unionVarSets alts_escs_list
	  in
	  vars_deflt deflt `thenLne` \ (deflt2, deflt_fvs, deflt_escs) ->
	  returnLne (
	      mkStgPrimAlts scrut_ty alts2 deflt2,
	      alts_fvs  `unionFVInfo`   deflt_fvs,
	      alts_escs `unionVarSet` deflt_escs
	  )

	| otherwise
        = mapAndUnzip3Lne vars_alg_alt alts
			`thenLne` \ (alts2,  alts_fvs_list,  alts_escs_list) ->
	  let
	      alts_fvs  = unionFVInfos alts_fvs_list
	      alts_escs = unionVarSets alts_escs_list
	  in
	  vars_deflt deflt `thenLne` \ (deflt2, deflt_fvs, deflt_escs) ->
	  returnLne (
	      mkStgAlgAlts scrut_ty alts2 deflt2,
	      alts_fvs  `unionFVInfo`   deflt_fvs,
	      alts_escs `unionVarSet` deflt_escs
	  )

      where
	vars_prim_alt (LitAlt lit, _, rhs)
	  = coreToStgExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	    returnLne ((lit, rhs2), rhs_fvs, rhs_escs)

	vars_alg_alt (DataAlt con, binders, rhs)
	  = let
		-- remove type variables
		binders' = filterStgBinders binders
	    in	
	    extendVarEnvLne [(b, CaseBound) | b <- binders']	$
	    coreToStgExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	    let
		good_use_mask = [ b `elementOfFVInfo` rhs_fvs | b <- binders' ]
		-- records whether each param is used in the RHS
	    in
	    returnLne (
		(con, binders', good_use_mask, rhs2),
		binders' `minusFVBinders` rhs_fvs,
		rhs_escs `minusVarSet`   mkVarSet binders'
			-- ToDo: remove the minusVarSet;
			-- since escs won't include any of these binders
	    )
	vars_alg_alt other = pprPanic "vars_alg_alt" (ppr other)

     	vars_deflt Nothing
     	   = returnLne (StgNoDefault, emptyFVInfo, emptyVarSet)
     
     	vars_deflt (Just rhs)
     	   = coreToStgExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
     	     returnLne (StgBindDefault rhs2, rhs_fvs, rhs_escs)
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

If we've got a case containing a _ccall_GC_ primop, we need to
ensure that the arguments are kept live for the duration of the
call. This only an issue

\begin{code}
isForeignObjArg :: Id -> Bool
isForeignObjArg x = isId x && isForeignObjPrimTy (idType x)

isForeignObjPrimTy ty
   = case splitTyConApp_maybe ty of
	Just (tycon, _) -> tycon == foreignObjPrimTyCon
	Nothing		-> False
\end{code}

\begin{code}
mkStgAlgAlts ty alts deflt
 =  case alts of
		-- Get the tycon from the data con
	(dc, _, _, _) : _rest
	    -> StgAlgAlts (Just (dataConTyCon dc)) alts deflt

		-- Otherwise just do your best
	[] -> case splitTyConApp_maybe (repType ty) of
		Just (tc,_) | isAlgTyCon tc 
			-> StgAlgAlts (Just tc) alts deflt
		other
			-> StgAlgAlts Nothing alts deflt

mkStgPrimAlts ty alts deflt 
  = StgPrimAlts (tyConAppTyCon ty) alts deflt
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
  = getVarsLiveInCont		`thenLne` \ live_in_cont ->
    coreToStgArgs args		`thenLne` \ (args', args_fvs) ->
    lookupVarLne f		`thenLne` \ how_bound ->

    let
	n_args		 = length args
	not_letrec_bound = not (isLetrecBound how_bound)
	fun_fvs	 	 = singletonFVInfo f how_bound fun_occ

	-- Mostly, the arity info of a function is in the fn's IdInfo
	-- But new bindings introduced by CoreSat may not have no
	-- arity info; it would do us no good anyway.  For example:
	--	let f = \ab -> e in f
	-- No point in having correct arity info for f!
	-- Hence the hasArity stuff below.
	f_arity_info     = idArityInfo f
	f_arity		 = arityLowerBound f_arity_info		-- Zero if no info

	fun_occ 
	 | not_letrec_bound		    = noBinderInfo	-- Uninteresting variable
	 | f_arity > 0 && f_arity <= n_args = stgSatOcc		-- Saturated or over-saturated function call
	 | otherwise			    = stgUnsatOcc	-- Unsaturated function or thunk

	fun_escs
	 | not_letrec_bound  = emptyVarSet	-- Only letrec-bound escapees are interesting
	 | hasArity f_arity_info &&
	   f_arity == n_args = emptyVarSet	-- A function *or thunk* with an exactly
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

	app = case globalIdDetails f of
      		DataConId dc -> StgConApp dc args'
	        PrimOpId op  -> StgPrimApp op args' (exprType (mkApps (Var f) args))
		_other       -> StgApp f args'

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
    if opt_KeepStgTypes then
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
		       StgConApp con [] -> StgVarArg (dataConWrapId con)
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
  = fixLne (\ ~(_, _, _, rec_bind_lvs, _, rec_body_fvs, _, _) ->

	-- Do the bindings, setting live_in_cont to empty if
	-- we ain't in a let-no-escape world
	getVarsLiveInCont		`thenLne` \ live_in_cont ->
	setVarsLiveInCont
		(if let_no_escape then live_in_cont else emptyVarSet)
		(vars_bind rec_bind_lvs rec_body_fvs bind)
			    `thenLne` \ (bind2, bind_fvs, bind_escs, env_ext) ->

	-- The live variables of this binding are the ones which are live
	-- by virtue of being accessible via the free vars of the binding (lvs_from_fvs)
	-- together with the live_in_cont ones
	lookupLiveVarsForSet (binders `minusFVBinders` bind_fvs)
				`thenLne` \ lvs_from_fvs ->
	let
		bind_lvs = lvs_from_fvs `unionVarSet` live_in_cont
	in

	-- bind_fvs and bind_escs still include the binders of the let(rec)
	-- but bind_lvs does not

  	-- Do the body
	extendVarEnvLne env_ext (
		coreToStgExpr body			`thenLne` \ (body2, body_fvs, body_escs) ->
		lookupLiveVarsForSet body_fvs	`thenLne` \ body_lvs ->

		returnLne (bind2, bind_fvs, bind_escs, bind_lvs,
			   body2, body_fvs, body_escs, body_lvs)

    )) `thenLne` (\ (bind2, bind_fvs, bind_escs, bind_lvs,
		     body2, body_fvs, body_escs, body_lvs) ->


	-- Compute the new let-expression
    let
	new_let | let_no_escape = StgLetNoEscape live_in_whole_let bind_lvs bind2 body2
		| otherwise	= StgLet bind2 body2

	free_in_whole_let
	  = binders `minusFVBinders` (bind_fvs `unionFVInfo` body_fvs)

	live_in_whole_let
	  = bind_lvs `unionVarSet` (body_lvs `minusVarSet` set_of_binders)

	real_bind_escs = if let_no_escape then
			    bind_escs
			 else
			    getFVSet bind_fvs
			    -- Everything escapes which is free in the bindings

	let_escs = (real_bind_escs `unionVarSet` body_escs) `minusVarSet` set_of_binders

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
    binders	   = case bind of
			NonRec binder rhs -> [binder]
			Rec pairs         -> map fst pairs

    mk_binding bind_lvs binder
	= (binder,  LetrecBound  False		-- Not top level
			live_vars
	   )
	where
	   live_vars = if let_no_escape then
			    extendVarSet bind_lvs binder
		       else
			    unitVarSet binder

    vars_bind :: StgLiveVars
	      -> FreeVarsInfo			-- Free var info for body of binding
	      -> CoreBind
	      -> LneM (StgBinding,
		       FreeVarsInfo, EscVarsSet,	-- free vars; escapee vars
		       [(Id, HowBound)])
					 -- extension to environment

    vars_bind rec_bind_lvs rec_body_fvs (NonRec binder rhs)
      = coreToStgRhs rec_body_fvs NotTopLevel (binder,rhs)
					`thenLne` \ (rhs2, fvs, escs) ->
	let
	    env_ext_item@(binder', _) = mk_binding rec_bind_lvs binder
	in
	returnLne (StgNonRec binder' rhs2, fvs, escs, [env_ext_item])

    vars_bind rec_bind_lvs rec_body_fvs (Rec pairs)
      = let
	    binders = map fst pairs
	    env_ext = map (mk_binding rec_bind_lvs) binders
	in
	extendVarEnvLne env_ext		  (
	fixLne (\ ~(_, rec_rhs_fvs, _, _) ->
		let
			rec_scope_fvs = unionFVInfo rec_body_fvs rec_rhs_fvs
		in
		mapAndUnzip3Lne (coreToStgRhs rec_scope_fvs NotTopLevel) pairs 
					`thenLne` \ (rhss2, fvss, escss) ->
		let
			fvs  = unionFVInfos      fvss
			escs = unionVarSets escss
		in
		returnLne (StgRec (binders `zip` rhss2), fvs, escs, env_ext)
	))

is_join_var :: Id -> Bool
-- A hack (used only for compiler debuggging) to tell if
-- a variable started life as a join point ($j)
is_join_var j = occNameUserString (getOccName j) == "$j"
\end{code}

%************************************************************************
%*									*
\subsection[LNE-monad]{A little monad for this let-no-escaping pass}
%*									*
%************************************************************************

There's a lot of stuff to pass around, so we use this @LneM@ monad to
help.  All the stuff here is only passed {\em down}.

\begin{code}
type LneM a =  IdEnv HowBound
	    -> StgLiveVars		-- vars live in continuation
	    -> a

data HowBound
  = ImportBound
  | CaseBound
  | LambdaBound
  | LetrecBound
	Bool		-- True <=> bound at top level
	StgLiveVars	-- Live vars... see notes below

isLetrecBound (LetrecBound _ _) = True
isLetrecBound other		= False
\end{code}

For a let(rec)-bound variable, x, we record StgLiveVars, the set of
variables that are live if x is live.  For "normal" variables that is
just x alone.  If x is a let-no-escaped variable then x is represented
by a code pointer and a stack pointer (well, one for each stack).  So
all of the variables needed in the execution of x are live if x is,
and are therefore recorded in the LetrecBound constructor; x itself
*is* included.

The set of live variables is guaranteed ot have no further let-no-escaped
variables in it.

The std monad functions:
\begin{code}
initLne :: LneM a -> a
initLne m = m emptyVarEnv emptyVarSet

{-# INLINE thenLne #-}
{-# INLINE returnLne #-}

returnLne :: a -> LneM a
returnLne e env lvs_cont = e

thenLne :: LneM a -> (a -> LneM b) -> LneM b
thenLne m k env lvs_cont
  = k (m env lvs_cont) env lvs_cont

mapLne  :: (a -> LneM b)   -> [a] -> LneM [b]
mapLne f [] = returnLne []
mapLne f (x:xs)
  = f x		`thenLne` \ r  ->
    mapLne f xs	`thenLne` \ rs ->
    returnLne (r:rs)

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

fixLne :: (a -> LneM a) -> LneM a
fixLne expr env lvs_cont
  = result
  where
    result = expr result env lvs_cont
\end{code}

Functions specific to this monad:

\begin{code}
getVarsLiveInCont :: LneM StgLiveVars
getVarsLiveInCont env lvs_cont = lvs_cont

setVarsLiveInCont :: StgLiveVars -> LneM a -> LneM a
setVarsLiveInCont new_lvs_cont expr env lvs_cont
  = expr env new_lvs_cont

extendVarEnvLne :: [(Id, HowBound)] -> LneM a -> LneM a
extendVarEnvLne ids_w_howbound expr env lvs_cont
  = expr (extendVarEnvList env ids_w_howbound) lvs_cont

lookupVarLne :: Id -> LneM HowBound
lookupVarLne v env lvs_cont
  = returnLne (
      case (lookupVarEnv env v) of
	Just xx -> xx
	Nothing -> ImportBound
    ) env lvs_cont

-- The result of lookupLiveVarsForSet, a set of live variables, is
-- only ever tacked onto a decorated expression. It is never used as
-- the basis of a control decision, which might give a black hole.

lookupLiveVarsForSet :: FreeVarsInfo -> LneM StgLiveVars

lookupLiveVarsForSet fvs env lvs_cont
  = returnLne (unionVarSets (map do_one (getFVs fvs)))
	      env lvs_cont
  where
    do_one v
      = if isLocalId v then
	    case (lookupVarEnv env v) of
	      Just (LetrecBound _ lvs) -> extendVarSet lvs v
	      Just _		       -> unitVarSet v
	      Nothing -> pprPanic "lookupVarEnv/do_one:" (ppr v)
	else
	    emptyVarSet
\end{code}


%************************************************************************
%*									*
\subsection[Free-var info]{Free variable information}
%*									*
%************************************************************************

\begin{code}
type FreeVarsInfo = VarEnv (Var, Bool, StgBinderInfo)
	-- If f is mapped to noBinderInfo, that means
	-- that f *is* mentioned (else it wouldn't be in the
	-- IdEnv at all), but perhaps in an unsaturated applications.
	--
	-- All case/lambda-bound things are also mapped to
	-- noBinderInfo, since we aren't interested in their
	-- occurence info.
	--
	-- The Bool is True <=> the Id is top level letrec bound
	--
	-- For ILX we track free var info for type variables too;
	-- hence VarEnv not IdEnv

type EscVarsSet = IdSet
\end{code}

\begin{code}
emptyFVInfo :: FreeVarsInfo
emptyFVInfo = emptyVarEnv

singletonFVInfo :: Id -> HowBound -> StgBinderInfo -> FreeVarsInfo
singletonFVInfo id ImportBound		     info = emptyVarEnv
singletonFVInfo id (LetrecBound top_level _) info = unitVarEnv id (id, top_level, info)
singletonFVInfo id other		     info = unitVarEnv id (id, False,     info)

tyvarFVInfo :: TyVarSet -> FreeVarsInfo
tyvarFVInfo tvs = foldVarSet add emptyFVInfo tvs
	        where
		  add tv fvs = extendVarEnv fvs tv (tv, False, noBinderInfo)

unionFVInfo :: FreeVarsInfo -> FreeVarsInfo -> FreeVarsInfo
unionFVInfo fv1 fv2 = plusVarEnv_C plusFVInfo fv1 fv2

unionFVInfos :: [FreeVarsInfo] -> FreeVarsInfo
unionFVInfos fvs = foldr unionFVInfo emptyFVInfo fvs

minusFVBinders :: [Id] -> FreeVarsInfo -> FreeVarsInfo
minusFVBinders vs fv = foldr minusFVBinder fv vs

minusFVBinder :: Id -> FreeVarsInfo -> FreeVarsInfo
minusFVBinder v fv | isId v && opt_KeepStgTypes
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
  | isExternallyVisibleName (idName id) = noBinderInfo
  | otherwise = case lookupVarEnv fvs id of
			Nothing         -> noBinderInfo
			Just (_,_,info) -> info

getFVs :: FreeVarsInfo -> [Id]	-- Non-top-level things only
getFVs fvs = [id | (id,False,_) <- rngVarEnv fvs]

getFVSet :: FreeVarsInfo -> IdSet
getFVSet fvs = mkVarSet (getFVs fvs)

plusFVInfo (id1,top1,info1) (id2,top2,info2)
  = ASSERT (id1 == id2 && top1 == top2)
    (id1, top1, combineStgBinderInfo info1 info2)
\end{code}

Misc.
\begin{code}
filterStgBinders :: [Var] -> [Var]
filterStgBinders bndrs
  | opt_KeepStgTypes = bndrs
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

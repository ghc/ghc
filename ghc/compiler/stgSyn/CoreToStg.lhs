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
import CoreUtils
import StgSyn

import Type
import TyCon		( isAlgTyCon )
import Literal
import Id
import Var		( Var, globalIdDetails )
import IdInfo
import DataCon
import CostCentre	( noCCS )
import VarSet
import VarEnv
import DataCon		( dataConWrapId )
import IdInfo		( OccInfo(..) )
import TysPrim		( foreignObjPrimTyCon )
import Maybes		( maybeToBool )
import Name		( getOccName, isExternallyVisibleName, isDllName )
import OccName		( occNameUserString )
import BasicTypes       ( TopLevelFlag(..), isNotTopLevel, Arity )
import CmdLineOpts	( DynFlags, opt_KeepStgTypes )
import FastTypes	hiding ( fastOr )
import Outputable

import List 		( partition )

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

we collect the CafInfo first by analysing the original Core expression, and
also place this information in the environment.

During CoreToStg, we then pin onto each binding and case expression, a
list of Ids which represents the "live" CAFs at that point.  The meaning
of "live" here is the same as for live variables, see above (which is
why it's convenient to collect CAF information here rather than elsewhere).

The later SRT pass takes these lists of Ids and uses them to construct
the actual nested SRTs, and replaces the lists of Ids with (offset,length)
pairs.

%************************************************************************
%*									*
\subsection[binds-StgVarInfo]{Setting variable info: top-level, binds, RHSs}
%*									*
%************************************************************************

\begin{code}
coreToStg :: DynFlags -> [CoreBind] -> IO [StgBinding]
coreToStg dflags pgm
  = return pgm'
  where (env', fvs, pgm') = coreTopBindsToStg emptyVarEnv pgm

coreExprToStg :: CoreExpr -> StgExpr
coreExprToStg expr 
  = new_expr where (new_expr,_,_) = initLne emptyVarEnv (coreToStgExpr expr)


coreTopBindsToStg
    :: IdEnv HowBound		-- environment for the bindings
    -> [CoreBind]
    -> (IdEnv HowBound, FreeVarsInfo, [StgBinding])

coreTopBindsToStg env [] = (env, emptyFVInfo, [])
coreTopBindsToStg env (b:bs)
  = (env2, fvs1, b':bs')
  where
	-- env accumulates down the list of binds, fvs accumulates upwards
	(env1, fvs2, b' ) = coreTopBindToStg env fvs1 b
  	(env2, fvs1, bs') = coreTopBindsToStg env1 bs


coreTopBindToStg
	:: IdEnv HowBound
	-> FreeVarsInfo		-- Info about the body
	-> CoreBind
	-> (IdEnv HowBound, FreeVarsInfo, StgBinding)

coreTopBindToStg env body_fvs (NonRec id rhs)
  = let 
	caf_info = hasCafRefs env rhs
	arity = exprArity rhs

	env' = extendVarEnv env id (LetBound how_bound emptyVarSet arity)

	how_bound | mayHaveCafRefs caf_info = TopLevelHasCafs
		  | otherwise               = TopLevelNoCafs

        (stg_rhs, fvs', cafs) = 
	    initLne env (
              coreToStgRhs body_fvs TopLevel (id,rhs) 
			`thenLne` \ (stg_rhs, fvs', _) ->
	      freeVarsToLiveVars fvs' `thenLne` \ (_, cafs) ->
	      returnLne (stg_rhs, fvs', cafs)
           )
	
	bind = StgNonRec (SRTEntries cafs) id stg_rhs
    in
    ASSERT2(consistent caf_info bind, ppr id)
--    WARN(not (consistent caf_info bind), ppr id <+> ppr cafs <+> ppCafInfo caf_info)
    (env', fvs' `unionFVInfo` body_fvs, bind)

coreTopBindToStg env body_fvs (Rec pairs)
  = let 
	(binders, rhss) = unzip pairs

	-- to calculate caf_info, we initially map all the binders to
	-- TopLevelNoCafs.
	env1 = extendVarEnvList env 
		[ (b, LetBound TopLevelNoCafs emptyVarSet (error "no arity"))
	        | b <- binders ]

	caf_info = hasCafRefss env1{-NB: not env'-} rhss

	env' = extendVarEnvList env 
		[ (b, LetBound how_bound emptyVarSet (exprArity rhs)) 
	        | (b,rhs) <- pairs ]

	how_bound | mayHaveCafRefs caf_info = TopLevelHasCafs
		  | otherwise               = TopLevelNoCafs

        (stg_rhss, fvs', cafs)
	  = initLne env' (
	       mapAndUnzip3Lne (coreToStgRhs body_fvs TopLevel) pairs
			`thenLne` \ (stg_rhss, fvss', _) ->
	       let fvs' = unionFVInfos fvss' in
	       freeVarsToLiveVars fvs'	`thenLne` \ (_, cafs) ->
	       returnLne (stg_rhss, fvs', cafs)
           )

	bind = StgRec (SRTEntries cafs) (zip binders stg_rhss)
    in
    ASSERT2(consistent caf_info bind, ppr binders)
--    WARN(not (consistent caf_info bind), ppr binders <+> ppr cafs <+> ppCafInfo caf_info)
    (env', fvs' `unionFVInfo` body_fvs, bind)

-- assertion helper
consistent caf_info bind = mayHaveCafRefs caf_info == stgBindHasCafRefs bind
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

bogus_rhs = StgRhsClosure noCCS noBinderInfo [] ReEntrant [] bogus_expr
bogus_expr = (StgLit (MachInt 1))

mkStgRhs :: TopLevelFlag -> FreeVarsInfo -> StgBinderInfo
	 -> StgExpr -> StgRhs

mkStgRhs top rhs_fvs binder_info (StgLam _ bndrs body)
  = StgRhsClosure noCCS binder_info
		  (getFVs rhs_fvs)		 
		  ReEntrant
		  bndrs body
	
mkStgRhs top rhs_fvs binder_info (StgConApp con args)
  | isNotTopLevel top || not (isDllConApp con args)
  = StgRhsCon noCCS con args

mkStgRhs top rhs_fvs binder_info rhs
  = StgRhsClosure noCCS binder_info
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
	result_expr | null args' = body
		    | otherwise  = StgLam (exprType expr) args' body
    in
    returnLne (result_expr, fvs, escs)

coreToStgExpr (Note (SCC cc) expr)
  = coreToStgExpr expr		`thenLne` ( \ (expr2, fvs, escs) ->
    returnLne (StgSCC cc expr2, fvs, escs) )

coreToStgExpr (Note other_note expr)
  = coreToStgExpr expr


-- Cases require a little more real work.

coreToStgExpr (Case scrut bndr alts)
  = extendVarEnvLne [(bndr, CaseBound)]	$
    vars_alts (findDefault alts)   `thenLne` \ (alts2, alts_fvs, alts_escs) ->
    freeVarsToLiveVars  alts_fvs   `thenLne` \ (alts_lvs, alts_caf_refs) ->
    let
	-- determine whether the default binder is dead or not
	-- This helps the code generator to avoid generating an assignment
	-- for the case binder (is extremely rare cases) ToDo: remove.
	bndr'= if (bndr `elementOfFVInfo` alts_fvs) 
		  then bndr
		  else bndr `setIdOccInfo` IAmDead

	-- Don't consider the default binder as being 'live in alts',
	-- since this is from the point of view of the case expr, where
	-- the default binder is not free.
	live_in_alts = (alts_lvs `minusVarSet` unitVarSet bndr)
    in
	-- we tell the scrutinee that everything live in the alts
	-- is live in it, too.
    setVarsLiveInCont (live_in_alts,alts_caf_refs) (
	coreToStgExpr scrut	  `thenLne` \ (scrut2, scrut_fvs, scrut_escs) ->
        freeVarsToLiveVars scrut_fvs `thenLne` \ (scrut_lvs, _) ->
	returnLne (scrut2, scrut_fvs, scrut_escs, scrut_lvs)
      )    
		`thenLne` \ (scrut2, scrut_fvs, scrut_escs, scrut_lvs) ->

    let srt = SRTEntries alts_caf_refs
    in
    returnLne (
      StgCase scrut2 scrut_lvs live_in_alts bndr' srt alts2,
      bndr `minusFVBinder` (scrut_fvs `unionFVInfo` alts_fvs),
      (alts_escs `minusVarSet` unitVarSet bndr) `unionVarSet` getFVSet scrut_fvs
		-- You might think we should have scrut_escs, not 
		-- (getFVSet scrut_fvs), but actually we can't call, and 
		-- then return from, a let-no-escape thing.
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
  = coreToStgArgs args		`thenLne` \ (args', args_fvs) ->
    lookupVarLne f		`thenLne` \ how_bound ->

    let
	n_args		 = length args
	not_letrec_bound = not (isLetBound how_bound)
	fun_fvs	 	 = singletonFVInfo f how_bound fun_occ

	-- Mostly, the arity info of a function is in the fn's IdInfo
	-- But new bindings introduced by CoreSat may not have no
	-- arity info; it would do us no good anyway.  For example:
	--	let f = \ab -> e in f
	-- No point in having correct arity info for f!
	-- Hence the hasArity stuff below.
	f_arity = case how_bound of 
			LetBound _ _ arity -> arity
			_                  -> 0

	fun_occ 
	 | not_letrec_bound		    = noBinderInfo	-- Uninteresting variable
	 | f_arity > 0 && f_arity <= n_args = stgSatOcc		-- Saturated or over-saturated function call
	 | otherwise			    = stgUnsatOcc	-- Unsaturated function or thunk

	fun_escs
	 | not_letrec_bound  = emptyVarSet	-- Only letrec-bound escapees are interesting
	 | f_arity == n_args = emptyVarSet	-- A function *or thunk* with an exactly
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
  = fixLne (\ ~(_, _, _, _, _, rec_body_fvs, _, _) ->

	-- Do the bindings, setting live_in_cont to empty if
	-- we ain't in a let-no-escape world
	getVarsLiveInCont		`thenLne` \ live_in_cont ->
	setVarsLiveInCont (if let_no_escape 
				then live_in_cont 
				else (emptyVarSet,emptyVarSet))
			  (vars_bind rec_body_fvs bind)
		  `thenLne` \ (bind2, bind_fvs, bind_escs, bind_lvs, env_ext) ->

  	-- Do the body
	extendVarEnvLne env_ext (
	  coreToStgExpr body          `thenLne` \(body2, body_fvs, body_escs) ->
	  freeVarsToLiveVars body_fvs `thenLne` \(body_lvs, _) ->

  	  returnLne (bind2, bind_fvs, bind_escs, bind_lvs,
		     body2, body_fvs, body_escs, body_lvs)
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

    mk_binding bind_lvs binder rhs
	= (binder,  LetBound  NotTopLevelBound	-- Not top level
			live_vars (exprArity rhs)
	   )
	where
	   live_vars = if let_no_escape then
			    extendVarSet bind_lvs binder
		       else
			    unitVarSet binder

    vars_bind :: FreeVarsInfo		-- Free var info for body of binding
	      -> CoreBind
	      -> LneM (StgBinding,
		       FreeVarsInfo, 
		       EscVarsSet,  	  -- free vars; escapee vars
		       StgLiveVars,	  -- vars live in binding
		       [(Id, HowBound)])  -- extension to environment
					 

    vars_bind body_fvs (NonRec binder rhs)
      = coreToStgRhs body_fvs NotTopLevel (binder,rhs)
				`thenLne` \ (rhs2, bind_fvs, escs) ->

	freeVarsToLiveVars bind_fvs `thenLne` \ (bind_lvs, bind_cafs) ->
	let
	    env_ext_item@(binder', _) = mk_binding bind_lvs binder rhs
	in
	returnLne (StgNonRec (SRTEntries bind_cafs) binder' rhs2, 
			bind_fvs, escs, bind_lvs, [env_ext_item])


    vars_bind body_fvs (Rec pairs)
      = fixLne (\ ~(_, rec_rhs_fvs, _, bind_lvs, _) ->
	   let
		rec_scope_fvs = unionFVInfo body_fvs rec_rhs_fvs
	        binders = map fst pairs
	        env_ext = [ mk_binding bind_lvs b rhs | (b,rhs) <- pairs ]
	   in
	   extendVarEnvLne env_ext (
	      mapAndUnzip3Lne (coreToStgRhs rec_scope_fvs NotTopLevel) pairs 
					`thenLne` \ (rhss2, fvss, escss) ->
	      let
			bind_fvs = unionFVInfos fvss
			escs     = unionVarSets escss
	      in
	      freeVarsToLiveVars (binders `minusFVBinders` bind_fvs)
					`thenLne` \ (bind_lvs, bind_cafs) ->
	      returnLne (StgRec (SRTEntries bind_cafs) (binders `zip` rhss2), 
				bind_fvs, escs, bind_lvs, env_ext)
	   )
	)

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
help.  All the stuff here is only passed *down*.

\begin{code}
type LneM a =  IdEnv HowBound
	    -> (StgLiveVars, 	-- vars live in continuation
		IdSet)		-- cafs live in continuation
	    -> a

data HowBound
  = ImportBound
  | CaseBound
  | LambdaBound
  | LetBound
	TopLevelCafInfo
	StgLiveVars	-- Live vars... see notes below
 	Arity		-- its arity (local Ids don't have arity info at this point)

isLetBound (LetBound _ _ _) = True
isLetBound other 	    = False
\end{code}

For a let(rec)-bound variable, x, we record StgLiveVars, the set of
variables that are live if x is live.  For "normal" variables that is
just x alone.  If x is a let-no-escaped variable then x is represented
by a code pointer and a stack pointer (well, one for each stack).  So
all of the variables needed in the execution of x are live if x is,
and are therefore recorded in the LetBound constructor; x itself
*is* included.

The set of live variables is guaranteed ot have no further let-no-escaped
variables in it.

The std monad functions:
\begin{code}
initLne :: IdEnv HowBound -> LneM a -> a
initLne env m = m env (emptyVarSet,emptyVarSet)

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
getVarsLiveInCont :: LneM (StgLiveVars, IdSet)
getVarsLiveInCont env lvs_cont = lvs_cont

setVarsLiveInCont :: (StgLiveVars,IdSet) -> LneM a -> LneM a
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

freeVarsToLiveVars :: FreeVarsInfo -> LneM (StgLiveVars, IdSet)
freeVarsToLiveVars fvs env live_in_cont
  = returnLne (lvs `unionVarSet` lvs_cont,
	       mkVarSet cafs `unionVarSet` cafs_cont)
	 env live_in_cont
  where
    (lvs_cont, cafs_cont) = live_in_cont -- not a strict pattern match!
    (local, global) = partition isLocalId (allFVs fvs)

    cafs = filter is_caf_one global
    lvs  = unionVarSets (map do_one local)

    do_one v
      = if isLocalId v then
	    case (lookupVarEnv env v) of
	      Just (LetBound _ lvs _) -> extendVarSet lvs v
	      Just _		      -> unitVarSet v
	      Nothing -> pprPanic "lookupLiveVarsForSet/do_one:" (ppr v)
	else
	    emptyVarSet

    is_caf_one v
	 = case lookupVarEnv env v of
		Just (LetBound TopLevelHasCafs lvs _) ->
		    ASSERT( isEmptyVarSet lvs ) True
	        Just (LetBound _ _ _) -> False
		_otherwise 	    -> mayHaveCafRefs (idCafInfo v)
\end{code}

%************************************************************************
%*									*
\subsection[Free-var info]{Free variable information}
%*									*
%************************************************************************

\begin{code}
type FreeVarsInfo = VarEnv (Var, TopLevelCafInfo, StgBinderInfo)
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

data TopLevelCafInfo
  = NotTopLevelBound
  | TopLevelNoCafs
  | TopLevelHasCafs
  deriving Eq

type EscVarsSet = IdSet
\end{code}

\begin{code}
emptyFVInfo :: FreeVarsInfo
emptyFVInfo = emptyVarEnv

singletonFVInfo :: Id -> HowBound -> StgBinderInfo -> FreeVarsInfo
singletonFVInfo id ImportBound info
   | mayHaveCafRefs (idCafInfo id) = unitVarEnv id (id, TopLevelHasCafs, info)
   | otherwise         		   = emptyVarEnv
singletonFVInfo id (LetBound top_level _ _) info 
   = unitVarEnv id (id, top_level, info)
singletonFVInfo id other info
   = unitVarEnv id (id, NotTopLevelBound, info)

tyvarFVInfo :: TyVarSet -> FreeVarsInfo
tyvarFVInfo tvs = foldVarSet add emptyFVInfo tvs
        where
	  add tv fvs = extendVarEnv fvs tv (tv, NotTopLevelBound, noBinderInfo)

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

allFVs :: FreeVarsInfo -> [Id]	-- Non-top-level things only
allFVs fvs = [id | (id,_,_) <- rngVarEnv fvs]

getFVs :: FreeVarsInfo -> [Id]	-- Non-top-level things only
getFVs fvs = [id | (id,NotTopLevelBound,_) <- rngVarEnv fvs]

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

%************************************************************************
%*									*
\subsection{Figuring out CafInfo for an expression}
%*									*
%************************************************************************

hasCafRefs decides whether a top-level closure can point into the dynamic heap.
We mark such things as `MayHaveCafRefs' because this information is
used to decide whether a particular closure needs to be referenced
in an SRT or not.

There are two reasons for setting MayHaveCafRefs:
	a) The RHS is a CAF: a top-level updatable thunk.
	b) The RHS refers to something that MayHaveCafRefs

Possible improvement: In an effort to keep the number of CAFs (and 
hence the size of the SRTs) down, we could also look at the expression and 
decide whether it requires a small bounded amount of heap, so we can ignore 
it as a CAF.  In these cases however, we would need to use an additional
CAF list to keep track of non-collectable CAFs.  

\begin{code}
hasCafRefs  :: IdEnv HowBound -> CoreExpr -> CafInfo
-- Only called for the RHS of top-level lets
hasCafRefss :: IdEnv HowBound -> [CoreExpr] -> CafInfo
	-- predicate returns True for a given Id if we look at this Id when
	-- calculating the result.  Used to *avoid* looking at the CafInfo
 	-- field for an Id that is part of the current recursive group.

hasCafRefs p expr 
  | isCAF expr || isFastTrue (cafRefs p expr) =  MayHaveCafRefs
  | otherwise = NoCafRefs

	-- used for recursive groups.  The whole group is set to
	-- "MayHaveCafRefs" if at least one of the group is a CAF or
	-- refers to any CAFs.
hasCafRefss p exprs
  | any isCAF exprs || isFastTrue (cafRefss p exprs) = MayHaveCafRefs
  | otherwise = NoCafRefs

-- cafRefs compiles to beautiful code :)

cafRefs p (Var id)
  | isLocalId id = fastBool False
  | otherwise = 
      case lookupVarEnv p id of
	Just (LetBound TopLevelHasCafs _ _) -> fastBool True
        Just (LetBound _ _ _) -> fastBool False
	Nothing -> fastBool (cgMayHaveCafRefs (idCgInfo id)) --  imported Ids

cafRefs p (Lit l) 	     = fastBool False
cafRefs p (App f a) 	     = fastOr (cafRefs p f) (cafRefs p) a
cafRefs p (Lam x e) 	     = cafRefs p e
cafRefs p (Let b e) 	     = fastOr (cafRefss p (rhssOfBind b)) (cafRefs p) e
cafRefs p (Case e bndr alts) = fastOr (cafRefs p e) 	
				(cafRefss p) (rhssOfAlts alts)
cafRefs p (Note n e) 	     = cafRefs p e
cafRefs p (Type t) 	     = fastBool False

cafRefss p [] 	  = fastBool False
cafRefss p (e:es) = fastOr (cafRefs p e) (cafRefss p) es

-- hack for lazy-or over FastBool.
fastOr a f x = fastBool (isFastTrue a || isFastTrue (f x))

isCAF :: CoreExpr -> Bool
-- Only called for the RHS of top-level lets
isCAF e = not (rhsIsNonUpd e)
  {- ToDo: check type for onceness, i.e. non-updatable thunks? -}


rhsIsNonUpd :: CoreExpr -> Bool
  -- True => Value-lambda, constructor, PAP
  -- This is a bit like CoreUtils.exprIsValue, with the following differences:
  -- 	a) scc "foo" (\x -> ...) is updatable (so we catch the right SCC)
  --
  --    b) (C x xs), where C is a contructors is updatable if the application is
  --	   dynamic: see isDynConApp
  -- 
  --    c) don't look through unfolding of f in (f x).  I'm suspicious of this one

rhsIsNonUpd (Lam b e)          = isId b || rhsIsNonUpd e
rhsIsNonUpd (Note (SCC _) e)   = False
rhsIsNonUpd (Note _ e)         = rhsIsNonUpd e
rhsIsNonUpd other_expr
  = go other_expr 0 []
  where
    go (Var f) n_args args = idAppIsNonUpd f n_args args
	
    go (App f a) n_args args
	| isTypeArg a = go f n_args args
	| otherwise   = go f (n_args + 1) (a:args)

    go (Note (SCC _) f) n_args args = False
    go (Note _ f) n_args args       = go f n_args args

    go other n_args args = False

idAppIsNonUpd :: Id -> Int -> [CoreExpr] -> Bool
idAppIsNonUpd id n_val_args args
  | Just con <- isDataConId_maybe id = not (isDynConApp con args)
  | otherwise 			     = n_val_args < idArity id

isDynConApp :: DataCon -> [CoreExpr] -> Bool
isDynConApp con args = isDllName (dataConName con) || any isDynArg args
-- Top-level constructor applications can usually be allocated 
-- statically, but they can't if 
-- 	a) the constructor, or any of the arguments, come from another DLL
--	b) any of the arguments are LitLits
-- (because we can't refer to static labels in other DLLs).
-- If this happens we simply make the RHS into an updatable thunk, 
-- and 'exectute' it rather than allocating it statically.
-- All this should match the decision in (see CoreToStg.coreToStgRhs)


isDynArg :: CoreExpr -> Bool
isDynArg (Var v)    = isDllName (idName v)
isDynArg (Note _ e) = isDynArg e
isDynArg (Lit lit)  = isLitLitLit lit
isDynArg (App e _)  = isDynArg e	-- must be a type app
isDynArg (Lam _ e)  = isDynArg e	-- must be a type lam
\end{code}

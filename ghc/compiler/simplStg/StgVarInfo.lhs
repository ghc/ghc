%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[StgVarInfo]{Sets free/live variable info in STG syntax}

And, as we have the info in hand, we may convert some lets to
let-no-escapes.

\begin{code}
module StgVarInfo ( setStgVarInfo ) where

#include "HsVersions.h"

import StgSyn

import Id		( setIdArity, getIdArity, Id )
import VarSet
import VarEnv
import Var
import IdInfo		( ArityInfo(..), InlinePragInfo(..), 
			  setInlinePragInfo )
import Maybes		( maybeToBool )
import Name		( isLocallyDefined )
import BasicTypes       ( Arity )
import Outputable

infixr 9 `thenLne`, `thenLne_`
\end{code}

%************************************************************************
%*									*
\subsection[live-vs-free-doc]{Documentation}
%*									*
%************************************************************************

(There is other relevant documentation in codeGen/CgLetNoEscape.)

March 97: setStgVarInfo guarantees to leave every variable's arity correctly
set.  The lambda lifter makes some let-bound variables (which have arities)
and turns them into lambda-bound ones (which should not, else we get Vap trouble),
so this guarantee is necessary, as well as desirable.

The arity information is used in the code generator, when deciding if
a right-hand side is a saturated application so we can generate a VAP
closure.

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

Top-level:
\begin{code}
setStgVarInfo	:: Bool			-- True <=> do let-no-escapes
		-> [StgBinding]	-- input
		-> [StgBinding]	-- result

setStgVarInfo want_LNEs pgm
  = pgm'
  where
    (pgm', _) = initLne want_LNEs (varsTopBinds pgm)

\end{code}

For top-level guys, we basically aren't worried about this
live-variable stuff; we do need to keep adding to the environment
as we step through the bindings (using @extendVarEnv@).

\begin{code}
varsTopBinds :: [StgBinding] -> LneM ([StgBinding], FreeVarsInfo)

varsTopBinds [] = returnLne ([], emptyFVInfo)
varsTopBinds (bind:binds)
  = extendVarEnvLne env_extension (
	varsTopBinds binds			`thenLne` \ (binds', fv_binds) ->
	varsTopBind binders' fv_binds bind	`thenLne` \ (bind',  fv_bind) ->
    	returnLne ((bind' : binds'),
		   (fv_binds `unionFVInfo` fv_bind) `minusFVBinders` binders'
		  )

    )
  where
    pairs         = case bind of
			StgNonRec binder rhs -> [(binder,rhs)]
			StgRec pairs         -> pairs

    binders' = [ binder `setIdArity` ArityExactly (rhsArity rhs) 
	       | (binder, rhs) <- pairs
	       ]

    env_extension = binders' `zip` repeat how_bound

    how_bound = LetrecBound
			True {- top level -}
			emptyVarSet


varsTopBind :: [Id]			-- New binders (with correct arity)
	    -> FreeVarsInfo		-- Info about the body
	    -> StgBinding
	    -> LneM (StgBinding, FreeVarsInfo)

varsTopBind [binder'] body_fvs (StgNonRec binder rhs)
  = varsRhs body_fvs (binder,rhs)		`thenLne` \ (rhs2, fvs, _) ->
    returnLne (StgNonRec binder' rhs2, fvs)

varsTopBind binders' body_fvs (StgRec pairs)
  = fixLne (\ ~(_, rec_rhs_fvs) ->
	let
		scope_fvs = unionFVInfo body_fvs rec_rhs_fvs
	in
	mapAndUnzip3Lne (varsRhs scope_fvs) pairs `thenLne` \ (rhss2, fvss, _) ->
	let
		fvs = unionFVInfos fvss
	in
	returnLne (StgRec (binders' `zip` rhss2), fvs)
    )

\end{code}

\begin{code}
varsRhs :: FreeVarsInfo		-- Free var info for the scope of the binding
	-> (Id,StgRhs)
	-> LneM (StgRhs, FreeVarsInfo, EscVarsSet)

varsRhs scope_fv_info (binder, StgRhsCon cc con args)
  = varsAtoms args	`thenLne` \ (args', fvs) ->
    returnLne (StgRhsCon cc con args', fvs, getFVSet fvs)

varsRhs scope_fv_info (binder, StgRhsClosure cc _ srt _ upd args body)
  = extendVarEnvLne [ (zapArity a, LambdaBound) | a <- args ] (
    do_body args body	`thenLne` \ (body2, body_fvs, body_escs) ->
    let
	set_of_args	= mkVarSet args
	rhs_fvs		= body_fvs  `minusFVBinders` args
	rhs_escs	= body_escs `minusVarSet`   set_of_args
	binder_info     = lookupFVInfo scope_fv_info binder
	upd'  | null args && isPAP body2 = ReEntrant
	      | otherwise                = upd
    in
    returnLne (StgRhsClosure cc binder_info srt (getFVs rhs_fvs) upd' 
		args body2, rhs_fvs, rhs_escs)
    )
  where
	-- Pick out special case of application in body of thunk
    do_body [] (StgApp f args) = varsApp (Just upd) f args
    do_body _ other_body 	 = varsExpr other_body
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
isPAP (StgApp f args) 
  = case getIdArity f of
	   ArityExactly n -> n > n_args
	   ArityAtLeast n -> n > n_args
	   _              -> False
   where n_args = length args
isPAP _ = False
\end{code}

\begin{code}
varsAtoms :: [StgArg]
	  -> LneM ([StgArg], FreeVarsInfo)
	-- It's not *really* necessary to return fresh arguments,
	-- because the only difference is that the argument variable
	-- arities are correct.  But it seems safer to do so.

varsAtoms atoms
  = mapAndUnzipLne var_atom atoms	`thenLne` \ (args', fvs_lists) ->
    returnLne (args', unionFVInfos fvs_lists)
  where
    var_atom a@(StgConArg _) = returnLne (a, emptyFVInfo)
    var_atom a@(StgVarArg v)
      = lookupVarLne v	`thenLne` \ (v', how_bound) ->
	returnLne (StgVarArg v', singletonFVInfo v' how_bound stgArgOcc)
\end{code}

%************************************************************************
%*									*
\subsection[expr-StgVarInfo]{Setting variable info on expressions}
%*									*
%************************************************************************

@varsExpr@ carries in a monad-ised environment, which binds each
let(rec) variable (ie non top level, not imported, not lambda bound,
not case-alternative bound) to:
	- its STG arity, and
	- its set of live vars.
For normal variables the set of live vars is just the variable
itself.	 For let-no-escaped variables, the set of live vars is the set
live at the moment the variable is entered.  The set is guaranteed to
have no further let-no-escaped vars in it.

\begin{code}
varsExpr :: StgExpr
	 -> LneM (StgExpr,	-- Decorated expr
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
varsExpr (StgApp f args) = varsApp Nothing f args

varsExpr (StgCon con args res_ty)
  = getVarsLiveInCont		`thenLne` \ live_in_cont ->
    varsAtoms args		`thenLne` \ (args', args_fvs) ->
    returnLne (StgCon con args' res_ty, args_fvs, getFVSet args_fvs)

varsExpr (StgSCC label expr)
  = varsExpr expr		`thenLne` ( \ (expr2, fvs, escs) ->
    returnLne (StgSCC label expr2, fvs, escs) )
\end{code}

Cases require a little more real work.
\begin{code}
varsExpr (StgCase scrut _ _ bndr srt alts)
  = getVarsLiveInCont		  `thenLne` \ live_in_cont ->
    extendVarEnvLne [(zapArity bndr, CaseBound)] (
    vars_alts alts		  `thenLne` \ (alts2, alts_fvs, alts_escs) ->
    lookupLiveVarsForSet alts_fvs `thenLne` \ alts_lvs ->
    let
	-- determine whether the default binder is dead or not
	bndr'= if (bndr `elementOfFVInfo` alts_fvs) 
		  then bndr `modifyIdInfo` (setInlinePragInfo NoInlinePragInfo)
		  else bndr `modifyIdInfo` (setInlinePragInfo IAmDead)

	-- don't consider the default binder as being 'live in alts',
	-- since this is from the point of view of the case expr, where
	-- the default binder is not free.
	live_in_alts = live_in_cont `unionVarSet` 
				(alts_lvs `minusVarSet` unitVarSet bndr)
    in
	-- we tell the scrutinee that everything live in the alts
	-- is live in it, too.
    setVarsLiveInCont live_in_alts (
	varsExpr scrut
    )				   `thenLne` \ (scrut2, scrut_fvs, scrut_escs) ->
    lookupLiveVarsForSet scrut_fvs `thenLne` \ scrut_lvs ->
    let
	live_in_whole_case = live_in_alts `unionVarSet` scrut_lvs
    in
    returnLne (
      StgCase scrut2 live_in_whole_case live_in_alts bndr' srt alts2,
      (scrut_fvs `unionFVInfo` alts_fvs) 
	  `minusFVBinders` [bndr],
      (alts_escs `unionVarSet` (getFVSet scrut_fvs))
	  `minusVarSet` unitVarSet bndr
	
    ))
  where
    vars_alts (StgAlgAlts ty alts deflt)
      = mapAndUnzip3Lne vars_alg_alt alts
			`thenLne` \ (alts2,  alts_fvs_list,  alts_escs_list) ->
	let
	    alts_fvs  = unionFVInfos alts_fvs_list
	    alts_escs = unionVarSets alts_escs_list
	in
	vars_deflt deflt `thenLne` \ (deflt2, deflt_fvs, deflt_escs) ->
	returnLne (
	    StgAlgAlts ty alts2 deflt2,
	    alts_fvs  `unionFVInfo`   deflt_fvs,
	    alts_escs `unionVarSet` deflt_escs
	)
      where
	vars_alg_alt (con, binders, worthless_use_mask, rhs)
	  = extendVarEnvLne [(zapArity b, CaseBound) | b <- binders] (
	    varsExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	    let
		good_use_mask = [ b `elementOfFVInfo` rhs_fvs | b <- binders ]
		-- records whether each param is used in the RHS
	    in
	    returnLne (
		(con, binders, good_use_mask, rhs2),
		rhs_fvs	 `minusFVBinders` binders,
		rhs_escs `minusVarSet`   mkVarSet binders	-- ToDo: remove the minusVarSet;
							-- since escs won't include
							-- any of these binders
	    ))

    vars_alts (StgPrimAlts ty alts deflt)
      = mapAndUnzip3Lne vars_prim_alt alts
			`thenLne` \ (alts2,  alts_fvs_list,  alts_escs_list) ->
	let
	    alts_fvs  = unionFVInfos alts_fvs_list
	    alts_escs = unionVarSets alts_escs_list
	in
	vars_deflt deflt `thenLne` \ (deflt2, deflt_fvs, deflt_escs) ->
	returnLne (
	    StgPrimAlts ty alts2 deflt2,
	    alts_fvs  `unionFVInfo`   deflt_fvs,
	    alts_escs `unionVarSet` deflt_escs
	)
      where
	vars_prim_alt (lit, rhs)
	  = varsExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	    returnLne ((lit, rhs2), rhs_fvs, rhs_escs)

    vars_deflt StgNoDefault
      = returnLne (StgNoDefault, emptyFVInfo, emptyVarSet)

    vars_deflt (StgBindDefault rhs)
      = varsExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	returnLne ( StgBindDefault rhs2, rhs_fvs, rhs_escs )
\end{code}

Lets not only take quite a bit of work, but this is where we convert
then to let-no-escapes, if we wish.

(Meanwhile, we don't expect to see let-no-escapes...)
\begin{code}
varsExpr (StgLetNoEscape _ _ _ _) = panic "varsExpr: unexpected StgLetNoEscape"

varsExpr (StgLet bind body)
  = isSwitchSetLne {-StgDoLetNoEscapes-} `thenLne` \ want_LNEs ->

    (fixLne (\ ~(_, _, _, no_binder_escapes) ->
	let
	    non_escaping_let = want_LNEs && no_binder_escapes
	in
	vars_let non_escaping_let bind body
    ))					`thenLne` \ (new_let, fvs, escs, _) ->

    returnLne (new_let, fvs, escs)
\end{code}

Applications:
\begin{code}
varsApp :: Maybe UpdateFlag		-- Just upd <=> this application is
					-- the rhs of a thunk binding
					-- 	x = [...] \upd [] -> the_app
					-- with specified update flag
	-> Id				-- Function
	-> [StgArg]		-- Arguments
	-> LneM (StgExpr, FreeVarsInfo, EscVarsSet)

varsApp maybe_thunk_body f args
  = getVarsLiveInCont		`thenLne` \ live_in_cont ->

    varsAtoms args		`thenLne` \ (args', args_fvs) ->

    lookupVarLne f		`thenLne` \ (f', how_bound) ->

    let
	n_args		 = length args
	not_letrec_bound = not (isLetrecBound how_bound)
	f_arity          = getIdArity f'
	fun_fvs	 	 = singletonFVInfo f' how_bound fun_occ

	fun_occ 
	  | not_letrec_bound
	  = NoStgBinderInfo		-- Uninteresting variable

	  | otherwise			-- Letrec bound; must have its arity
	  = case f_arity of
	      ArityExactly arity
		| n_args == 0 -> stgFakeFunAppOcc   -- Function Application
						    -- with no arguments.
						    -- used by the lambda lifter.
		| arity > n_args -> stgUnsatOcc	    -- Unsaturated


		| arity == n_args &&
		  maybeToBool maybe_thunk_body ->   -- Exactly saturated,
						    -- and rhs of thunk
			case maybe_thunk_body of
				Just Updatable   -> stgStdHeapOcc
				Just SingleEntry -> stgNoUpdHeapOcc
				other		 -> panic "varsApp"

		| otherwise ->  stgNormalOcc
				-- Record only that it occurs free

	myself = unitVarSet f'

	fun_escs | not_letrec_bound = emptyVarSet	-- Only letrec-bound escapees are interesting
		 | otherwise	    = case f_arity of	-- Letrec bound, so must have its arity
				        ArityExactly arity
				          | arity == n_args -> emptyVarSet
					  	-- Function doesn't escape
					  | otherwise -> myself
						-- Inexact application; it does escape

	-- At the moment of the call:

	--  either the function is *not* let-no-escaped, in which case
	--  	   nothing is live except live_in_cont
	--	or the function *is* let-no-escaped in which case the
	--	   variables it uses are live, but still the function
	--	   itself is not.  PS.  In this case, the function's
	--	   live vars should already include those of the
	--	   continuation, but it does no harm to just union the
	--	   two regardless.

	-- XXX not needed?
	-- live_at_call
	--   = live_in_cont `unionVarSet` case how_bound of
	-- 			      LetrecBound _ lvs -> lvs `minusVarSet` myself
	--			   other	     -> emptyVarSet
    in
    returnLne (
	StgApp f' args',
	fun_fvs  `unionFVInfo` args_fvs,
	fun_escs `unionVarSet` (getFVSet args_fvs)
				-- All the free vars of the args are disqualified
				-- from being let-no-escaped.
    )
\end{code}

The magic for lets:
\begin{code}
vars_let :: Bool		-- True <=> yes, we are let-no-escaping this let
	 -> StgBinding	-- bindings
	 -> StgExpr	-- body
    	 -> LneM (StgExpr,	-- new let
		  FreeVarsInfo,	-- variables free in the whole let
		  EscVarsSet,	-- variables that escape from the whole let
		  Bool)		-- True <=> none of the binders in the bindings
				-- is among the escaping vars

vars_let let_no_escape bind body
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
	lookupLiveVarsForSet (bind_fvs `minusFVBinders` binders)	`thenLne` \ lvs_from_fvs ->
	let
		bind_lvs = lvs_from_fvs `unionVarSet` live_in_cont
	in

	-- bind_fvs and bind_escs still include the binders of the let(rec)
	-- but bind_lvs does not

  	-- Do the body
	extendVarEnvLne env_ext (
		varsExpr body			`thenLne` \ (body2, body_fvs, body_escs) ->
		lookupLiveVarsForSet body_fvs	`thenLne` \ body_lvs ->

		returnLne (bind2, bind_fvs, bind_escs, bind_lvs,
			   body2, body_fvs, body_escs, body_lvs)

    )) `thenLne` (\ (bind2, bind_fvs, bind_escs, bind_lvs,
		     body2, body_fvs, body_escs, body_lvs) ->


	-- Compute the new let-expression
    let
	new_let = if let_no_escape then
		     -- trace "StgLetNoEscape!" (
		     StgLetNoEscape live_in_whole_let bind_lvs bind2 body2
		     -- )
		  else
		     StgLet bind2 body2

	free_in_whole_let
	  = (bind_fvs `unionFVInfo` body_fvs) `minusFVBinders` binders

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
		-- Mustn't depend on the passed-in let_no_escape flag, since
		-- no_binder_escapes is used by the caller to derive the flag!
    in
    returnLne (
	new_let,
	free_in_whole_let,
	let_escs,
	no_binder_escapes
    ))
  where
    set_of_binders = mkVarSet binders
    binders	   = case bind of
			StgNonRec binder rhs -> [binder]
			StgRec pairs         -> map fst pairs

    mk_binding bind_lvs (binder,rhs)
	= (binder `setIdArity` ArityExactly (stgArity rhs),
	   LetrecBound  False		-- Not top level
			live_vars
	  )
	where
	   live_vars = if let_no_escape then
			    extendVarSet bind_lvs binder
		       else
			    unitVarSet binder

    vars_bind :: StgLiveVars
	      -> FreeVarsInfo			-- Free var info for body of binding
	      -> StgBinding
	      -> LneM (StgBinding,
		       FreeVarsInfo, EscVarsSet,	-- free vars; escapee vars
		       [(Id, HowBound)])
					 -- extension to environment

    vars_bind rec_bind_lvs rec_body_fvs (StgNonRec binder rhs)
      = varsRhs rec_body_fvs (binder,rhs)	`thenLne` \ (rhs2, fvs, escs) ->
	let
	    env_ext_item@(binder', _) = mk_binding rec_bind_lvs (binder,rhs)
	in
	returnLne (StgNonRec binder' rhs2, fvs, escs, [env_ext_item])

    vars_bind rec_bind_lvs rec_body_fvs (StgRec pairs)
      = let
	    env_ext  = map (mk_binding rec_bind_lvs) pairs
	    binders' = map fst env_ext
	in
	extendVarEnvLne env_ext		  (
	fixLne (\ ~(_, rec_rhs_fvs, _, _) ->
		let
			rec_scope_fvs = unionFVInfo rec_body_fvs rec_rhs_fvs
		in
		mapAndUnzip3Lne (varsRhs rec_scope_fvs) pairs `thenLne` \ (rhss2, fvss, escss) ->
		let
			fvs  = unionFVInfos      fvss
			escs = unionVarSets escss
		in
		returnLne (StgRec (binders' `zip` rhss2), fvs, escs, env_ext)
	))
\end{code}

%************************************************************************
%*									*
\subsection[LNE-monad]{A little monad for this let-no-escaping pass}
%*									*
%************************************************************************

There's a lot of stuff to pass around, so we use this @LneM@ monad to
help.  All the stuff here is only passed {\em down}.

\begin{code}
type LneM a =  Bool			-- True <=> do let-no-escapes
	    -> IdEnv (Id, HowBound)	-- Use the Id at all occurrences; it has correct
					-- 	arity information inside it.
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

For a let(rec)-bound variable, x,  we record what varibles are live if
x is live.  For "normal" variables that is just x alone.  If x is
a let-no-escaped variable then x is represented by a code pointer and
a stack pointer (well, one for each stack).  So all of the variables
needed in the execution of x are live if x is, and are therefore recorded
in the LetrecBound constructor; x itself *is* included.

The std monad functions:
\begin{code}
initLne :: Bool -> LneM a -> a
initLne want_LNEs m = m want_LNEs emptyVarEnv emptyVarSet

{-# INLINE thenLne #-}
{-# INLINE thenLne_ #-}
{-# INLINE returnLne #-}

returnLne :: a -> LneM a
returnLne e sw env lvs_cont = e

thenLne :: LneM a -> (a -> LneM b) -> LneM b
thenLne m k sw env lvs_cont
  = case (m sw env lvs_cont) of
      m_result -> k m_result sw env lvs_cont

thenLne_ :: LneM a -> LneM b -> LneM b
thenLne_ m k sw env lvs_cont
  = case (m sw env lvs_cont) of
      _ -> k sw env lvs_cont

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
fixLne expr sw env lvs_cont = result
  where
    result = expr result sw env lvs_cont
--  ^^^^^^ ------ ^^^^^^
\end{code}

Functions specific to this monad:
\begin{code}
isSwitchSetLne :: LneM Bool
isSwitchSetLne want_LNEs env lvs_cont
  = want_LNEs

getVarsLiveInCont :: LneM StgLiveVars
getVarsLiveInCont sw env lvs_cont = lvs_cont

setVarsLiveInCont :: StgLiveVars -> LneM a -> LneM a
setVarsLiveInCont new_lvs_cont expr sw env lvs_cont
  = expr sw env new_lvs_cont

extendVarEnvLne :: [(Id, HowBound)] -> LneM a -> LneM a
extendVarEnvLne ids_w_howbound expr sw env lvs_cont
  = expr sw (extendVarEnvList env [(id, pair) | pair@(id,_) <- ids_w_howbound]) lvs_cont


lookupVarLne :: Id -> LneM (Id, HowBound)
lookupVarLne v sw env lvs_cont
  = returnLne (
      case (lookupVarEnv env v) of
	Just xx -> xx
	Nothing -> --false:ASSERT(not (isLocallyDefined v))
		   (v, ImportBound)
    ) sw env lvs_cont

-- The result of lookupLiveVarsForSet, a set of live variables, is
-- only ever tacked onto a decorated expression. It is never used as
-- the basis of a control decision, which might give a black hole.

lookupLiveVarsForSet :: FreeVarsInfo -> LneM StgLiveVars

lookupLiveVarsForSet fvs sw env lvs_cont
  = returnLne (unionVarSets (map do_one (getFVs fvs)))
	      sw env lvs_cont
  where
    do_one v
      = if isLocallyDefined v then
	    case (lookupVarEnv env v) of
	      Just (_, LetrecBound _ lvs) -> extendVarSet lvs v
	      Just _		            -> unitVarSet v
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
type FreeVarsInfo = IdEnv (Id, Bool, StgBinderInfo)
			-- If f is mapped to NoStgBinderInfo, that means
			-- that f *is* mentioned (else it wouldn't be in the
			-- IdEnv at all), but only in a saturated applications.
			--
			-- All case/lambda-bound things are also mapped to
			-- NoStgBinderInfo, since we aren't interested in their
			-- occurence info.
			--
			-- The Bool is True <=> the Id is top level letrec bound

type EscVarsSet   = IdSet
\end{code}

\begin{code}
emptyFVInfo :: FreeVarsInfo
emptyFVInfo = emptyVarEnv

singletonFVInfo :: Id -> HowBound -> StgBinderInfo -> FreeVarsInfo
singletonFVInfo id ImportBound		     info = emptyVarEnv
singletonFVInfo id (LetrecBound top_level _) info = unitVarEnv id (id, top_level, info)
singletonFVInfo id other		     info = unitVarEnv id (id, False,     info)

unionFVInfo :: FreeVarsInfo -> FreeVarsInfo -> FreeVarsInfo
unionFVInfo fv1 fv2 = plusVarEnv_C plusFVInfo fv1 fv2

unionFVInfos :: [FreeVarsInfo] -> FreeVarsInfo
unionFVInfos fvs = foldr unionFVInfo emptyFVInfo fvs

minusFVBinders :: FreeVarsInfo -> [Id] -> FreeVarsInfo
minusFVBinders fv ids = fv `delVarEnvList` ids

elementOfFVInfo :: Id -> FreeVarsInfo -> Bool
elementOfFVInfo id fvs = maybeToBool (lookupVarEnv fvs id)

lookupFVInfo :: FreeVarsInfo -> Id -> StgBinderInfo
lookupFVInfo fvs id = case lookupVarEnv fvs id of
			Nothing         -> NoStgBinderInfo
			Just (_,_,info) -> info

getFVs :: FreeVarsInfo -> [Id]	-- Non-top-level things only
getFVs fvs = [id | (id,False,_) <- rngVarEnv fvs]

getFVSet :: FreeVarsInfo -> IdSet
getFVSet fvs = mkVarSet (getFVs fvs)

plusFVInfo (id1,top1,info1) (id2,top2,info2)
  = ASSERT (id1 == id2 && top1 == top2)
    (id1, top1, combineStgBinderInfo info1 info2)
\end{code}

\begin{code}
rhsArity :: StgRhs -> Arity
rhsArity (StgRhsCon _ _ _)              = 0
rhsArity (StgRhsClosure _ _ _ _ _ args _) = length args

zapArity :: Id -> Id
zapArity id = id `setIdArity` UnknownArity
\end{code}




%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[StgVarInfo]{Sets free/live variable info in STG syntax}

And, as we have the info in hand, we may convert some lets to
let-no-escapes.

\begin{code}
#include "HsVersions.h"

module StgVarInfo ( setStgVarInfo ) where

IMPORT_Trace		-- ToDo: rm (debugging only)
import Pretty
import Outputable

import StgSyn

import Id		( getIdArity, externallyVisibleId )
import IdInfo		-- ( arityMaybe, ArityInfo )

import IdEnv
import Maybes		( maybeToBool, Maybe(..) )
import UniqSet
import Util

infixr 9 `thenLne`, `thenLne_`
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

Top-level:
\begin{code}
setStgVarInfo	:: Bool			-- True <=> do let-no-escapes
		-> [PlainStgBinding]	-- input
		-> [PlainStgBinding]	-- result

setStgVarInfo want_LNEs pgm 
  = pgm' 
  where
    (pgm', _) = initLne want_LNEs (varsTopBinds pgm)
    
\end{code}

For top-level guys, we basically aren't worried about this
live-variable stuff; we do need to keep adding to the environment
as we step through the bindings (using @extendVarEnv@).

\begin{code}
varsTopBinds :: [PlainStgBinding] -> LneM ([PlainStgBinding], FreeVarsInfo)

varsTopBinds [] = returnLne ([], emptyFVInfo)
varsTopBinds (bind:binds)
  = extendVarEnv env_extension (
	varsTopBinds binds		`thenLne` \ (binds', fv_binds) ->
	varsTopBind fv_binds bind	`thenLne` \ (bind',  fv_bind) ->
    	returnLne ((bind' : binds'),
		   (fv_binds `unionFVInfo` fv_bind) `minusFVBinders` binders
		  )
		   
    )
  where
    env_extension = [(b, LetrecBound 
				True {- top level -}
				(rhsArity rhs)
				emptyUniqSet)
		    | (b,rhs) <- pairs]

    pairs         = case bind of
			StgNonRec binder rhs -> [(binder,rhs)]
			StgRec pairs         -> pairs

    binders = [b | (b,_) <- pairs]


varsTopBind :: FreeVarsInfo		-- Info about the body
	    -> PlainStgBinding
	    -> LneM (PlainStgBinding, FreeVarsInfo)

varsTopBind body_fvs (StgNonRec binder rhs)
  = varsRhs body_fvs (binder,rhs)		`thenLne` \ (rhs2, fvs, _) ->
    returnLne (StgNonRec binder rhs2, fvs)

varsTopBind body_fvs (StgRec pairs)
  = let
	(binders, rhss) = unzip pairs
    in
    fixLne (\ ~(_, rec_rhs_fvs) ->
	let 
		scope_fvs = unionFVInfo body_fvs rec_rhs_fvs
	in
	mapAndUnzip3Lne (varsRhs scope_fvs) pairs `thenLne` \ (rhss2, fvss, _) ->
	let
		fvs = unionFVInfos fvss
	in
	returnLne (StgRec (binders `zip` rhss2), fvs)
    )

\end{code}

\begin{code}
varsRhs :: FreeVarsInfo		-- Free var info for the scope of the binding
	-> (Id,PlainStgRhs)
	-> LneM (PlainStgRhs, FreeVarsInfo, EscVarsSet)

varsRhs scope_fv_info (binder, StgRhsCon cc con args)
  = varsAtoms args	`thenLne` \ fvs ->
    returnLne (StgRhsCon cc con args, fvs, getFVSet fvs)

varsRhs scope_fv_info (binder, StgRhsClosure cc _ _ upd args body) 
  = extendVarEnv [ (a, LambdaBound) | a <- args ] (
    do_body args body	`thenLne` \ (body2, body_fvs, body_escs) ->
    let
	set_of_args	= mkUniqSet args
	rhs_fvs		= body_fvs  `minusFVBinders` args
	rhs_escs	= body_escs `minusUniqSet`   set_of_args
        binder_info     = lookupFVInfo scope_fv_info binder
    in
    returnLne (StgRhsClosure cc binder_info (getFVs rhs_fvs) upd args body2, 
	       rhs_fvs, rhs_escs)
    )
  where
	-- Pick out special case of application in body of thunk
    do_body [] (StgApp (StgVarAtom f) args _) = varsApp (Just upd) f args
    do_body _ other_body 		      = varsExpr other_body
\end{code}

\begin{code}
varsAtoms :: [PlainStgAtom]
	  -> LneM FreeVarsInfo

varsAtoms atoms
  = mapLne var_atom atoms	`thenLne` \ fvs_lists ->
    returnLne (unionFVInfos fvs_lists)
  where
    var_atom a@(StgLitAtom	   _) = returnLne emptyFVInfo
    var_atom a@(StgVarAtom v)
      = lookupVarEnv v	`thenLne` \ how_bound ->
	returnLne (singletonFVInfo v how_bound stgArgOcc)
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
varsExpr :: PlainStgExpr
	 -> LneM (PlainStgExpr,	-- Decorated expr
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
varsExpr (StgApp lit@(StgLitAtom _) args _)
  = --(if null args then id else (trace (ppShow 80 (ppr PprShowAll args)))) (
    returnLne (StgApp lit [] emptyUniqSet, emptyFVInfo, emptyUniqSet)
    --)

varsExpr (StgApp fun@(StgVarAtom f) args _) = varsApp Nothing f args

varsExpr (StgConApp con args _) 
  = getVarsLiveInCont		`thenLne` \ live_in_cont ->
    varsAtoms args		`thenLne` \ args_fvs ->

    returnLne (StgConApp con args live_in_cont, args_fvs, getFVSet args_fvs)

varsExpr (StgPrimApp op args _) 
  = getVarsLiveInCont		`thenLne` \ live_in_cont ->
    varsAtoms args		`thenLne` \ args_fvs ->

    returnLne (StgPrimApp op args live_in_cont, args_fvs, getFVSet args_fvs)

varsExpr (StgSCC ty label expr)
  = varsExpr expr		`thenLne` ( \ (expr2, fvs, escs) ->
    returnLne (StgSCC ty label expr2, fvs, escs) )
\end{code}

Cases require a little more real work.
\begin{code}
varsExpr (StgCase scrut _ _ uniq alts)
  = getVarsLiveInCont		  `thenLne` \ live_in_cont ->
    vars_alts alts		  `thenLne` \ (alts2, alts_fvs, alts_escs) ->
    lookupLiveVarsForSet alts_fvs `thenLne` \ alts_lvs ->
    let
	live_in_alts = live_in_cont `unionUniqSets` alts_lvs
    in
	-- we tell the scrutinee that everything live in the alts
	-- is live in it, too.
    setVarsLiveInCont live_in_alts (
	varsExpr scrut
    )				   `thenLne` \ (scrut2, scrut_fvs, scrut_escs) ->
    lookupLiveVarsForSet scrut_fvs `thenLne` \ scrut_lvs ->
    let
	live_in_whole_case = live_in_alts `unionUniqSets` scrut_lvs
    in
    returnLne (
      StgCase scrut2 live_in_whole_case live_in_alts uniq alts2,
      scrut_fvs `unionFVInfo` alts_fvs,
      alts_escs `unionUniqSets` (getFVSet scrut_fvs)   -- All free vars in the scrutinee escape
    )
  where
    vars_alts (StgAlgAlts ty alts deflt)
      = mapAndUnzip3Lne vars_alg_alt alts
			`thenLne` \ (alts2,  alts_fvs_list,  alts_escs_list) ->
	let
	    alts_fvs  = unionFVInfos alts_fvs_list
	    alts_escs = unionManyUniqSets alts_escs_list
	in
	vars_deflt deflt `thenLne` \ (deflt2, deflt_fvs, deflt_escs) ->
	returnLne (
	    StgAlgAlts ty alts2 deflt2,
	    alts_fvs  `unionFVInfo`   deflt_fvs,
	    alts_escs `unionUniqSets` deflt_escs
	)
      where
	vars_alg_alt (con, binders, worthless_use_mask, rhs)
	  = extendVarEnv [(b, CaseBound) | b <- binders] (
	    varsExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	    let
		good_use_mask = [ b `elementOfFVInfo` rhs_fvs | b <- binders ]
		-- records whether each param is used in the RHS
	    in
	    returnLne (
		(con, binders, good_use_mask, rhs2),
		rhs_fvs	 `minusFVBinders` binders,
		rhs_escs `minusUniqSet`   mkUniqSet binders	-- ToDo: remove the minusUniqSet;
							-- since escs won't include
							-- any of these binders
	    ))

    vars_alts (StgPrimAlts ty alts deflt)
      = mapAndUnzip3Lne vars_prim_alt alts
			`thenLne` \ (alts2,  alts_fvs_list,  alts_escs_list) ->
	let
	    alts_fvs  = unionFVInfos alts_fvs_list
	    alts_escs = unionManyUniqSets alts_escs_list
	in
	vars_deflt deflt `thenLne` \ (deflt2, deflt_fvs, deflt_escs) ->
	returnLne (
	    StgPrimAlts ty alts2 deflt2,
	    alts_fvs  `unionFVInfo`   deflt_fvs,
	    alts_escs `unionUniqSets` deflt_escs
	)
      where
	vars_prim_alt (lit, rhs)
	  = varsExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	    returnLne ((lit, rhs2), rhs_fvs, rhs_escs)

    vars_deflt StgNoDefault
      = returnLne (StgNoDefault, emptyFVInfo, emptyUniqSet)

    vars_deflt (StgBindDefault binder _ rhs)
      = extendVarEnv [(binder, CaseBound)] (
	varsExpr rhs	`thenLne` \ (rhs2, rhs_fvs, rhs_escs) ->
	let
	    used_in_rhs = binder `elementOfFVInfo` rhs_fvs
	in
	returnLne (
	    StgBindDefault binder used_in_rhs rhs2,
	    rhs_fvs  `minusFVBinders` [binder],
	    rhs_escs `minusUniqSet`   singletonUniqSet binder
	))
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

\begin{code}
#ifdef DPH
-- rest of varsExpr goes here

#endif {- Data Parallel Haskell -}
\end{code}

Applications:
\begin{code}
varsApp :: Maybe UpdateFlag		-- Just upd <=> this application is
					-- the rhs of a thunk binding
					-- 	x = [...] \upd [] -> the_app
					-- with specified update flag
	-> Id				-- Function
	-> [PlainStgAtom]		-- Arguments
	-> LneM (PlainStgExpr, FreeVarsInfo, EscVarsSet)

varsApp maybe_thunk_body f args
  = getVarsLiveInCont		`thenLne` \ live_in_cont ->

    varsAtoms args		`thenLne` \ args_fvs ->
  
    lookupVarEnv f		`thenLne` \ how_bound ->
   
    let
        n_args = length args

	fun_fvs = singletonFVInfo f how_bound fun_occ

	fun_occ =
	  case how_bound of 
	    LetrecBound _ arity _ 
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
				-- record only that it occurs free

	    other ->	NoStgBinderInfo
		-- uninteresting variable

	myself = singletonUniqSet f

	fun_escs = case how_bound of

		     LetrecBound _ arity lvs -> 
		       if arity == n_args then
			  emptyUniqSet -- Function doesn't escape
		       else
			  myself -- Inexact application; it does escape

		     other -> emptyUniqSet	-- Only letrec-bound escapees 
						-- are interesting

	-- At the moment of the call:

	--  either the function is *not* let-no-escaped, in which case
	--  	   nothing is live except live_in_cont
	--	or the function *is* let-no-escaped in which case the
	--	   variables it uses are live, but still the function
	--	   itself is not.  PS.  In this case, the function's
	--	   live vars should already include those of the
	--	   continuation, but it does no harm to just union the
	--	   two regardless.

	live_at_call
	  = live_in_cont `unionUniqSets` case how_bound of
				   LetrecBound _ _ lvs -> lvs `minusUniqSet` myself
				   other	       -> emptyUniqSet
    in
    returnLne (
	StgApp (StgVarAtom f) args live_at_call,
	fun_fvs  `unionFVInfo` args_fvs,
	fun_escs `unionUniqSets` (getFVSet args_fvs)	
				-- All the free vars of the args are disqualified
				-- from being let-no-escaped.
    )
\end{code}

The magic for lets:
\begin{code}
vars_let :: Bool		-- True <=> yes, we are let-no-escaping this let
	 -> PlainStgBinding	-- bindings
	 -> PlainStgExpr	-- body
    	 -> LneM (PlainStgExpr,	-- new let
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
		(if let_no_escape then live_in_cont else emptyUniqSet)
		(vars_bind rec_bind_lvs rec_body_fvs bind)
					`thenLne` \ (bind2, bind_fvs, bind_escs, env_ext) ->

	-- The live variables of this binding are the ones which are live
	-- by virtue of being accessible via the free vars of the binding (lvs_from_fvs)
	-- together with the live_in_cont ones
	lookupLiveVarsForSet (bind_fvs `minusFVBinders` binders)	`thenLne` \ lvs_from_fvs ->
	let 
		bind_lvs = lvs_from_fvs `unionUniqSets` live_in_cont
	in

	-- bind_fvs and bind_escs still include the binders of the let(rec)
	-- but bind_lvs does not

  	-- Do the body
        extendVarEnv env_ext (
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
	  = bind_lvs `unionUniqSets` (body_lvs `minusUniqSet` set_of_binders)

	real_bind_escs = if let_no_escape then 
			    bind_escs
			 else
			    getFVSet bind_fvs
			    -- Everything escapes which is free in the bindings

	let_escs = (real_bind_escs `unionUniqSets` body_escs) `minusUniqSet` set_of_binders

	all_escs = bind_escs `unionUniqSets` body_escs	-- Still includes binders of
						-- this let(rec)

	no_binder_escapes = isEmptyUniqSet (set_of_binders `intersectUniqSets` all_escs)
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
    binders		= case bind of
			    StgNonRec binder rhs -> [binder]
			    StgRec pairs         -> map fst pairs
    set_of_binders	= mkUniqSet binders

    mk_binding bind_lvs (binder,rhs)
	= (binder,
	   LetrecBound  False		-- Not top level
			(stgArity rhs)
			live_vars
	  )
	where
	   live_vars = if let_no_escape then 
			    bind_lvs `unionUniqSets` singletonUniqSet binder
		       else 
			    singletonUniqSet binder

    vars_bind :: PlainStgLiveVars
	      -> FreeVarsInfo			-- Free var info for body of binding
	      -> PlainStgBinding
	      -> LneM (PlainStgBinding,
		       FreeVarsInfo, EscVarsSet,	-- free vars; escapee vars
		       [(Id, HowBound)])
					 -- extension to environment

    vars_bind rec_bind_lvs rec_body_fvs (StgNonRec binder rhs)
      = varsRhs rec_body_fvs (binder,rhs)	`thenLne` \ (rhs2, fvs, escs) ->
	let
	    env_ext = [mk_binding rec_bind_lvs (binder,rhs)]
	in
	returnLne (StgNonRec binder rhs2, fvs, escs, env_ext)

    vars_bind rec_bind_lvs rec_body_fvs (StgRec pairs)
      = let
	    (binders, rhss) = unzip pairs
	    env_ext = map (mk_binding rec_bind_lvs) pairs
	in
	extendVarEnv env_ext		  (
	fixLne (\ ~(_, rec_rhs_fvs, _, _) ->
		let 
			rec_scope_fvs = unionFVInfo rec_body_fvs rec_rhs_fvs
		in
		mapAndUnzip3Lne (varsRhs rec_scope_fvs) pairs `thenLne` \ (rhss2, fvss, escss) ->
		let
			fvs  = unionFVInfos      fvss
			escs = unionManyUniqSets escss
		in
		returnLne (StgRec (binders `zip` rhss2), fvs, escs, env_ext)
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
	    -> IdEnv HowBound
	    -> PlainStgLiveVars		-- vars live in continuation
	    -> a

type Arity = Int

data HowBound
  = ImportBound
  | CaseBound
  | LambdaBound
  | LetrecBound	
	Bool			-- True <=> bound at top level
	Arity			-- Arity
	PlainStgLiveVars	-- Live vars... see notes below
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
initLne want_LNEs m = m want_LNEs nullIdEnv emptyUniqSet

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenLne #-}
{-# INLINE thenLne_ #-}
{-# INLINE returnLne #-}
#endif

returnLne :: a -> LneM a
returnLne e sw env lvs_cont = e

thenLne :: LneM a -> (a -> LneM b) -> LneM b
(m `thenLne` k) sw env lvs_cont
  = case (m sw env lvs_cont) of
      m_result -> k m_result sw env lvs_cont

thenLne_ :: LneM a -> LneM b -> LneM b
(m `thenLne_` k) sw env lvs_cont
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
{- NOT USED:
ifSwitchSetLne :: GlobalSwitch -> LneM a -> LneM a -> LneM a
ifSwitchSetLne switch then_ else_ switch_checker env lvs_cont
  = (if switch_checker switch then then_ else else_) switch_checker env lvs_cont
-}

isSwitchSetLne :: LneM Bool
isSwitchSetLne want_LNEs env lvs_cont
  = want_LNEs

getVarsLiveInCont :: LneM PlainStgLiveVars
getVarsLiveInCont sw env lvs_cont = lvs_cont

setVarsLiveInCont :: PlainStgLiveVars -> LneM a -> LneM a
setVarsLiveInCont new_lvs_cont expr sw env lvs_cont
  = expr sw env new_lvs_cont

extendVarEnv :: [(Id, HowBound)] -> LneM a -> LneM a
extendVarEnv extension expr sw env lvs_cont
  = expr sw (growIdEnvList env extension) lvs_cont

lookupVarEnv :: Id -> LneM HowBound
lookupVarEnv v sw env lvs_cont
  = returnLne (
      case (lookupIdEnv env v) of
	Just xx -> xx
	Nothing -> --false:ASSERT(not (isLocallyDefined v))
		   ImportBound
    ) sw env lvs_cont

-- The result of lookupLiveVarsForSet, a set of live variables, is
-- only ever tacked onto a decorated expression. It is never used as
-- the basis of a control decision, which might give a black hole.

lookupLiveVarsForSet :: FreeVarsInfo -> LneM PlainStgLiveVars

lookupLiveVarsForSet fvs sw env lvs_cont
  = returnLne (unionManyUniqSets (map do_one (getFVs fvs)))
	      sw env lvs_cont
  where
    do_one v
      = if isLocallyDefined v then
	    case (lookupIdEnv env v) of
	      Just (LetrecBound _ _ lvs) -> lvs `unionUniqSets` singletonUniqSet v
	      Just _		         -> singletonUniqSet v
	      Nothing -> pprPanic "lookupVarEnv/do_one:" (ppr PprShowAll v)
	else
	    emptyUniqSet
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

type EscVarsSet   = UniqSet Id
\end{code}

\begin{code}
emptyFVInfo :: FreeVarsInfo
emptyFVInfo = nullIdEnv

singletonFVInfo :: Id -> HowBound -> StgBinderInfo -> FreeVarsInfo
singletonFVInfo id ImportBound		       info = nullIdEnv
singletonFVInfo id (LetrecBound top_level _ _) info = unitIdEnv id (id, top_level, info)
singletonFVInfo id other		       info = unitIdEnv id (id, False,     info)

unionFVInfo :: FreeVarsInfo -> FreeVarsInfo -> FreeVarsInfo
unionFVInfo fv1 fv2 = combineIdEnvs plusFVInfo fv1 fv2

unionFVInfos :: [FreeVarsInfo] -> FreeVarsInfo
unionFVInfos fvs = foldr unionFVInfo emptyFVInfo fvs

minusFVBinders :: FreeVarsInfo -> [Id] -> FreeVarsInfo
minusFVBinders fv ids = fv `delManyFromIdEnv` ids

elementOfFVInfo :: Id -> FreeVarsInfo -> Bool
elementOfFVInfo id fvs = maybeToBool (lookupIdEnv fvs id)

lookupFVInfo :: FreeVarsInfo -> Id -> StgBinderInfo
lookupFVInfo fvs id = case lookupIdEnv fvs id of
			Nothing         -> NoStgBinderInfo
			Just (_,_,info) -> info

getFVs :: FreeVarsInfo -> [Id]	-- Non-top-level things only
getFVs fvs = [id | (id,False,_) <- rngIdEnv fvs]

getFVSet :: FreeVarsInfo -> UniqSet Id
getFVSet fvs = mkUniqSet (getFVs fvs)

plusFVInfo (id1,top1,info1) (id2,top2,info2)
  = ASSERT (id1 == id2 && top1 == top2)
    (id1, top1, combineStgBinderInfo info1 info2)
\end{code}

\begin{code}
rhsArity :: PlainStgRhs -> Arity
rhsArity (StgRhsCon _ _ _)              = 0
rhsArity (StgRhsClosure _ _ _ _ args _) = length args
\end{code}




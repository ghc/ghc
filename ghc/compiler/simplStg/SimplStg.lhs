%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[SimplStg]{Driver for simplifying @STG@ programs}

\begin{code}
#include "HsVersions.h"

module SimplStg ( stg2stg ) where

IMP_Ubiq(){-uitous-}
IMPORT_1_3(IO(hPutStr,stderr))

import StgSyn
import StgUtils

import LambdaLift	( liftProgram )
import Name		( isLocallyDefined )
import SCCfinal		( stgMassageForProfiling )
import SatStgRhs	( satStgRhs )
import StgLint		( lintStgBindings )
import StgSAT		( doStaticArgs )
import StgStats	        ( showStgStats )
import StgVarInfo	( setStgVarInfo )
import UpdAnal		( updateAnalyse )

import CmdLineOpts	( opt_EnsureSplittableC, opt_SccGroup,
			  opt_StgDoLetNoEscapes, opt_D_verbose_stg2stg,
			  StgToDo(..)
			)
import Id		( externallyVisibleId,
			  nullIdEnv, lookupIdEnv, addOneToIdEnv,
			  growIdEnvList, isNullIdEnv, SYN_IE(IdEnv),
			  GenId{-instance Eq/Outputable -}
			)
import Maybes		( maybeToBool )
import PprType		( GenType{-instance Outputable-} )
import Pretty		( ppShow, ppAbove, ppAboves, ppStr )
import UniqSupply	( splitUniqSupply )
import Util		( mapAccumL, panic, assertPanic )

unlocaliseId = panic "SimplStg.unlocaliseId (ToDo)"
\end{code}

\begin{code}
stg2stg :: [StgToDo]		-- spec of what stg-to-stg passes to do
	-> FAST_STRING		-- module name (profiling only)
	-> PprStyle		-- printing style (for debugging only)
	-> UniqSupply		-- a name supply
	-> [StgBinding]		-- input...
	-> IO
	    ([StgBinding],	-- output program...
	     ([CostCentre],	-- local cost-centres that need to be decl'd
	      [CostCentre]))	-- "extern" cost-centres

stg2stg stg_todos module_name ppr_style us binds
  = case (splitUniqSupply us)	of { (us4now, us4later) ->

    (if do_verbose_stg2stg then
	hPutStr stderr "VERBOSE STG-TO-STG:\n" >>
	hPutStr stderr (ppShow 1000
	(ppAbove (ppStr ("*** Core2Stg:"))
		 (ppAboves (map (ppr ppr_style) (setStgVarInfo False binds)))
	))
     else return ()) >>

	-- Do the main business!
    foldl_mn do_stg_pass (binds, us4now, ([],[])) stg_todos
		>>= \ (processed_binds, _, cost_centres) ->
	-- Do essential wind-up: part (a) is SatStgRhs

	-- Not optional, because correct arity information is used by
	-- the code generator.  Afterwards do setStgVarInfo; it gives
	-- the wrong answers if arities are subsequently changed,
	-- which stgSatRhs might do.  Furthermore, setStgVarInfo
	-- decides about let-no-escape things, which in turn do a
	-- better job if arities are correct, which is done by
	-- satStgRhs.

    case (satStgRhs processed_binds us4later) of { saturated_binds ->

	-- Essential wind-up: part (b), eliminate indirections

    let no_ind_binds = elimIndirections saturated_binds in


	-- Essential wind-up: part (c), do setStgVarInfo. It has to
	-- happen regardless, because the code generator uses its
	-- decorations.
	--
	-- Why does it have to happen last?  Because earlier passes
	-- may move things around, which would change the live-var
	-- info.  Also, setStgVarInfo decides about let-no-escape
	-- things, which in turn do a better job if arities are
	-- correct, which is done by satStgRhs.
	--
    let
		-- ToDo: provide proper flag control!
	binds_to_mangle
	  = if not do_unlocalising
	    then no_ind_binds
	    else snd (unlocaliseStgBinds unlocal_tag nullIdEnv no_ind_binds)
    in
    return (setStgVarInfo do_let_no_escapes binds_to_mangle, cost_centres)
    }}
  where
    do_let_no_escapes  = opt_StgDoLetNoEscapes
    do_verbose_stg2stg = opt_D_verbose_stg2stg

    (do_unlocalising, unlocal_tag)
      = case (opt_EnsureSplittableC) of
	      Nothing  -> (False, panic "tag")
	      Just tag -> (True,  _PK_ tag)

    grp_name  = case (opt_SccGroup) of
		  Just xx -> _PK_ xx
		  Nothing -> module_name -- default: module name

    -------------
    stg_linter = if False -- LATER: switch_is_on DoCoreLinting -- ToDo: DoStgLinting flag
		 then lintStgBindings ppr_style
		 else ( \ whodunnit binds -> binds )

    -------------------------------------------
    do_stg_pass (binds, us, ccs) to_do
      =	let
	    (us1, us2) = splitUniqSupply us
	in
	case to_do of
	  StgDoStaticArgs ->
	     ASSERT(null (fst ccs) && null (snd ccs))
	     _scc_ "StgStaticArgs"
	     let
		 binds3 = doStaticArgs binds us1
	     in
	     end_pass us2 "StgStaticArgs" ccs binds3

	  StgDoUpdateAnalysis ->
	     ASSERT(null (fst ccs) && null (snd ccs))
	     _scc_ "StgUpdAnal"
		-- NB We have to do setStgVarInfo first!  (There's one
		-- place free-var info is used) But no let-no-escapes,
		-- because update analysis doesn't care.
	     end_pass us2 "UpdAnal" ccs (updateAnalyse (setStgVarInfo False binds))

	  D_stg_stats ->
	     trace (showStgStats binds)
	     end_pass us2 "StgStats" ccs binds

	  StgDoLambdaLift ->
	     _scc_ "StgLambdaLift"
		-- NB We have to do setStgVarInfo first!
	     let
		binds3 = liftProgram us1 (setStgVarInfo do_let_no_escapes binds)
	     in
	     end_pass us2 "LambdaLift" ccs binds3

	  StgDoMassageForProfiling ->
	     _scc_ "ProfMassage"
	     let
		 (collected_CCs, binds3)
		   = stgMassageForProfiling module_name grp_name us1 binds
	     in
	     end_pass us2 "ProfMassage" collected_CCs binds3

    end_pass us2 what ccs binds2
      = -- report verbosely, if required
	(if do_verbose_stg2stg then
	    hPutStr stderr (ppShow 1000
	    (ppAbove (ppStr ("*** "++what++":"))
		     (ppAboves (map (ppr ppr_style) binds2))
	    ))
	 else return ()) >>
	let
	    linted_binds = stg_linter what binds2
	in
	return (linted_binds, us2, ccs)
	    -- return: processed binds
	    -- 	       UniqueSupply for the next guy to use
	    --	       cost-centres to be declared/registered (specialised)
	    --	       add to description of what's happened (reverse order)

-- here so it can be inlined...
foldl_mn f z []     = return z
foldl_mn f z (x:xs) = f z x	>>= \ zz ->
		     foldl_mn f zz xs
\end{code}

%************************************************************************
%*									*
\subsection[SimplStg-unlocalise]{Unlocalisation in STG code}
%*									*
%************************************************************************

The idea of all this ``unlocalise'' stuff is that in certain (prelude
only) modules we split up the .hc file into lots of separate little
files, which are separately compiled by the C compiler.  That gives
lots of little .o files.  The idea is that if you happen to mention
one of them you don't necessarily pull them all in.  (Pulling in a
piece you don't need can be v bad, because it may mention other pieces
you don't need either, and so on.)

Sadly, splitting up .hc files means that local names (like s234) are
now globally visible, which can lead to clashes between two .hc
files. So unlocaliseWhatnot goes through making all the local things
into global things, essentially by giving them full names so when they
are printed they'll have their module name too.  Pretty revolting
really.

\begin{code}
type UnlocalEnv = IdEnv Id

lookup_uenv :: UnlocalEnv -> Id -> Id
lookup_uenv env id =  case lookupIdEnv env id of
			Nothing     -> id
			Just new_id -> new_id

unlocaliseStgBinds :: FAST_STRING -> UnlocalEnv -> [StgBinding] -> (UnlocalEnv, [StgBinding])

unlocaliseStgBinds mod uenv [] = (uenv, [])

unlocaliseStgBinds mod uenv (b : bs)
  = case (unlocal_top_bind mod uenv b)	      of { (new_uenv, new_b) ->
    case (unlocaliseStgBinds mod new_uenv bs) of { (uenv3, new_bs) ->
    (uenv3, new_b : new_bs) }}

------------------

unlocal_top_bind :: FAST_STRING -> UnlocalEnv -> StgBinding -> (UnlocalEnv, StgBinding)

unlocal_top_bind mod uenv bind@(StgNonRec binder _)
  = let new_uenv = case unlocaliseId mod binder of
			Nothing		-> uenv
			Just new_binder -> addOneToIdEnv uenv binder new_binder
    in
    (new_uenv, renameTopStgBind (lookup_uenv new_uenv) bind)

unlocal_top_bind mod uenv bind@(StgRec pairs)
  = let maybe_unlocaliseds  = [ (b, unlocaliseId mod b) | (b, _) <- pairs ]
	new_uenv	    = growIdEnvList uenv [ (b,new_b)
						 | (b, Just new_b) <- maybe_unlocaliseds]
    in
    (new_uenv, renameTopStgBind (lookup_uenv new_uenv) bind)
\end{code}

%************************************************************************
%*									*
\subsection[SimplStg-indirections]{Eliminating indirections in STG code}
%*									*
%************************************************************************

In @elimIndirections@, we look for things at the top-level of the form...
\begin{verbatim}
    x_local = ....rhs...
    ...
    x_exported = x_local
    ...
\end{verbatim}
In cases we find like this, we go {\em backwards} and replace
\tr{x_local} with \tr{...rhs...}, to produce
\begin{verbatim}
    x_exported = ...rhs...
    ...
    ...
\end{verbatim}
This saves a gratuitous jump
(from \tr{x_exported} to \tr{x_local}), and makes strictness
information propagate better.

If more than one exported thing is equal to a local thing (i.e., the
local thing really is shared), then we eliminate only the first one.  Thus:
\begin{verbatim}
    x_local = ....rhs...
    ...
    x_exported1 = x_local
    ...
    x_exported2 = x_local
    ...
\end{verbatim}
becomes
\begin{verbatim}
    x_exported1 = ....rhs...
    ...
    ...
    x_exported2 = x_exported1
    ...
\end{verbatim}

We also have to watch out for

	f = \xyz -> g x y z

This can arise post lambda lifting; the original might have been

	f = \xyz -> letrec g = [xy] \ [k] -> e
		    in
		    g z

Strategy: first collect the info; then make a \tr{Id -> Id} mapping.
Then blast the whole program (LHSs as well as RHSs) with it.

\begin{code}
elimIndirections :: [StgBinding] -> [StgBinding]

elimIndirections binds_in
  = if isNullIdEnv blast_env then
	binds_in	    -- Nothing to do
    else
	[renameTopStgBind lookup_fn bind | Just bind <- reduced_binds]
  where
    lookup_fn id = case lookupIdEnv blast_env id of
			Just new_id -> new_id
			Nothing     -> id

    (blast_env, reduced_binds) = mapAccumL try_bind nullIdEnv binds_in

    try_bind :: IdEnv Id -> StgBinding -> (IdEnv Id, Maybe StgBinding)
    try_bind env_so_far
	     (StgNonRec exported_binder
		       (StgRhsClosure _ _ _ _
				lambda_args
				(StgApp (StgVarArg local_binder) fun_args _)
	     ))
	| externallyVisibleId exported_binder && -- Only if this is exported
	  not (externallyVisibleId local_binder) && -- Only if this one is defined in this
	  isLocallyDefined local_binder &&  -- module, so that we *can* change its
					    -- binding to be the exported thing!
	  not (in_dom env_so_far local_binder) && -- Only if we havn't seen it before
	  args_match lambda_args fun_args   -- Just an eta-expansion

	= (addOneToIdEnv env_so_far local_binder exported_binder,
	   Nothing)
	where
	  args_match [] [] = True
	  args_match (la:las) (StgVarArg fa:fas) = la == fa && args_match las fas
	  args_match _  _  = False

    try_bind env_so_far bind
	= (env_so_far, Just bind)

    in_dom env id = maybeToBool (lookupIdEnv env id)
\end{code}

@renameTopStgBind@ renames top level binders and all occurrences thereof.

\begin{code}
renameTopStgBind :: (Id -> Id) -> StgBinding -> StgBinding

renameTopStgBind fn (StgNonRec b rhs) = StgNonRec (fn b) (mapStgBindeesRhs fn rhs)
renameTopStgBind fn (StgRec pairs)    = StgRec [ (fn b, mapStgBindeesRhs fn r) | (b, r) <- pairs ]
\end{code}

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

import LambdaLift	( liftProgram )
import Name		( isLocallyDefined )
import UniqSet          ( UniqSet(..), mapUniqSet )
import SCCfinal		( stgMassageForProfiling )
import StgLint		( lintStgBindings )
import StgStats	        ( showStgStats )
import StgVarInfo	( setStgVarInfo )
import UpdAnal		( updateAnalyse )

import CmdLineOpts	( opt_SccGroup, --Not used:opt_EnsureSplittableC,
			  opt_StgDoLetNoEscapes, opt_D_verbose_stg2stg,
			  opt_DoStgLinting,
			  StgToDo(..)
			)
import Id		( nullIdEnv, lookupIdEnv, addOneToIdEnv,
			  growIdEnvList, isNullIdEnv, SYN_IE(IdEnv),
			  setIdVisibility,
			  GenId{-instance Eq/Outputable -}
			)
import Maybes		( maybeToBool )
import PprType		( GenType{-instance Outputable-} )
import Pretty		( ppShow, ppAbove, ppAboves, ppStr, ppPStr )
import UniqSupply	( splitUniqSupply )
import Util		( mapAccumL, panic, assertPanic )

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
	(ppAbove (ppPStr SLIT("*** Core2Stg:"))
		 (ppAboves (map (ppr ppr_style) (setStgVarInfo False binds)))
	))
     else return ()) >>

	-- Do the main business!
    foldl_mn do_stg_pass (binds, us4now, ([],[])) stg_todos
		>>= \ (processed_binds, _, cost_centres) ->

	-- 	Do essential wind-up

{- Nuked for now	SLPJ Dec 96

	-- Essential wind-up: part (a), saturate RHSs
	-- This must occur *after* elimIndirections, because elimIndirections
	-- can change things' arities.  Consider:
	--	x_local = f x
	--	x_global = \a -> x_local a
	-- Then elimIndirections will change the program to
	--	x_global = f x
	-- and lo and behold x_global's arity has changed!
    case (satStgRhs processed_binds us4later) of { saturated_binds ->
-}

	-- Essential wind-up: part (b), do setStgVarInfo. It has to
	-- happen regardless, because the code generator uses its
	-- decorations.
	--
	-- Why does it have to happen last?  Because earlier passes
	-- may move things around, which would change the live-var
	-- info.  Also, setStgVarInfo decides about let-no-escape
	-- things, which in turn do a better job if arities are
	-- correct, which is done by satStgRhs.
	--

{- 
	Done in Core now.  Nuke soon. SLPJ Nov 96

   No, STG passes may introduce toplevel bindings which
   have to be globalised here (later than Core anyway) -- SOF 2/97

   Yes, lambda lifting now does the Right Thing.

    let
		-- ToDo: provide proper flag control!
	binds_to_mangle
	  = if not do_unlocalising
	    then processed_binds
	    else snd (unlocaliseStgBinds unlocal_tag nullIdEnv processed_binds)
    in
-}

    return (setStgVarInfo do_let_no_escapes processed_binds, cost_centres)
   }
  where
    do_let_no_escapes  = opt_StgDoLetNoEscapes
    do_verbose_stg2stg = opt_D_verbose_stg2stg

{-
    (do_unlocalising, unlocal_tag) 
     = case opt_EnsureSplittableC of
         Just tag -> (True, _PK_ tag)
         Nothing  -> (False, panic "tag")
-}
    grp_name  = case (opt_SccGroup) of
		  Just xx -> _PK_ xx
		  Nothing -> module_name -- default: module name

    -------------
    stg_linter = if False --LATER: opt_DoStgLinting (ToDo)
		 then lintStgBindings ppr_style
		 else ( \ whodunnit binds -> binds )

    -------------------------------------------
    do_stg_pass (binds, us, ccs) to_do
      =	let
	    (us1, us2) = splitUniqSupply us
	in
	case to_do of
	  StgDoStaticArgs ->  panic "STG static argument transformation deleted"

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
		binds3 = liftProgram module_name us1 (setStgVarInfo do_let_no_escapes binds)
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
unlocaliseStgBinds :: FAST_STRING
                   -> UnlocalEnv
                   -> [StgBinding] 
		   -> (UnlocalEnv, [StgBinding])
unlocaliseStgBinds mod uenv []       = (uenv, [])
unlocaliseStgBinds mod uenv (b : bs) =
  case unlocal_top_bind mod uenv b        of { (new_uenv, new_b) ->
  case unlocaliseStgBinds mod new_uenv bs of { (uenv3, new_bs) ->
    (uenv3, new_b : new_bs)
  }}

------------------
unlocal_top_bind :: FAST_STRING 
	         -> UnlocalEnv 
		 -> StgBinding 
		 -> (UnlocalEnv, StgBinding)
unlocal_top_bind mod uenv bind@(StgNonRec binder _) =
 let
  new_uenv =
   case lookupIdEnv uenv binder of
    Just global -> uenv
    Nothing     -> new_env
     where
      new_env    = addOneToIdEnv uenv binder new_global
      new_global = setIdVisibility mod binder
 in
 (new_uenv, renameTopStgBind (lookup_uenv new_uenv) bind)

unlocal_top_bind mod uenv bind@(StgRec pairs) =
 let
  new_env binder uenv =
    case lookupIdEnv uenv binder of
      Just global -> uenv
      Nothing     -> env'
        where
         env'       = addOneToIdEnv uenv binder new_global
         new_global = setIdVisibility mod binder

  uenv' = foldr (new_env) uenv (map (fst) pairs)
 in
 (uenv', renameTopStgBind (lookup_uenv uenv') bind)

\end{code}

@renameTopStgBind@ renames top level binders and all occurrences thereof.

\begin{code}
renameTopStgBind :: (Id -> Id) -> StgBinding -> StgBinding
renameTopStgBind fn (StgNonRec b rhs) = StgNonRec (fn b) (mapStgBindeesRhs fn rhs)
renameTopStgBind fn (StgRec pairs)    = StgRec [ (fn b, mapStgBindeesRhs fn r) | (b, r) <- pairs ]
\end{code}

This utility function simply applies the given function to every
bindee in the program.

\begin{code}
mapStgBindeesBind :: (Id -> Id) -> StgBinding -> StgBinding
mapStgBindeesBind fn (StgNonRec b rhs) = StgNonRec b (mapStgBindeesRhs fn rhs)
mapStgBindeesBind fn (StgRec pairs)    = StgRec [ (b, mapStgBindeesRhs fn r) | (b, r) <- pairs ]

------------------
mapStgBindeesRhs :: (Id -> Id) -> StgRhs -> StgRhs
mapStgBindeesRhs fn (StgRhsClosure cc bi fvs u args expr)
  = StgRhsClosure 
	cc bi 
	(map fn fvs) 
	u 
	(map fn args) 
	(mapStgBindeesExpr fn expr)

mapStgBindeesRhs fn (StgRhsCon cc con atoms)
  = StgRhsCon cc con (map (mapStgBindeesArg fn) atoms)

------------------
mapStgBindeesExpr :: (Id -> Id) -> StgExpr -> StgExpr

mapStgBindeesExpr fn (StgApp f args lvs)
  = StgApp (mapStgBindeesArg fn f) 
	   (map (mapStgBindeesArg fn) args) 
	   (mapUniqSet fn lvs)

mapStgBindeesExpr fn (StgCon con atoms lvs)
  = StgCon con (map (mapStgBindeesArg fn) atoms) (mapUniqSet fn lvs)

mapStgBindeesExpr fn (StgPrim op atoms lvs)
  = StgPrim op (map (mapStgBindeesArg fn) atoms) (mapUniqSet fn lvs)

mapStgBindeesExpr fn (StgLet bind expr)
  = StgLet (mapStgBindeesBind fn bind) (mapStgBindeesExpr fn expr)

mapStgBindeesExpr fn (StgLetNoEscape lvs rhss_lvs bind body)
  = StgLetNoEscape (mapUniqSet fn lvs) (mapUniqSet fn rhss_lvs)
		   (mapStgBindeesBind fn bind) (mapStgBindeesExpr fn body)

mapStgBindeesExpr fn (StgSCC ty label expr)
  = StgSCC ty label (mapStgBindeesExpr fn expr)

mapStgBindeesExpr fn (StgCase expr lvs1 lvs2 uniq alts)
  = StgCase (mapStgBindeesExpr fn expr)
	    (mapUniqSet fn lvs1)
	    (mapUniqSet fn lvs2)
	    uniq
	    (mapStgBindeesAlts alts)
  where
    mapStgBindeesAlts (StgAlgAlts ty alts deflt)
      = StgAlgAlts ty (map mapStgBindeesBoxed_alt alts) (mapStgBindeesDeflt deflt)
      where
	mapStgBindeesBoxed_alt (c,ps,use_mask,expr) = (c,ps,use_mask,mapStgBindeesExpr fn expr)

    mapStgBindeesAlts (StgPrimAlts ty alts deflt)
      = StgPrimAlts ty (map mapStgBindeesunboxed_alt alts) (mapStgBindeesDeflt deflt)
      where
	mapStgBindeesunboxed_alt (l,expr) = (l,mapStgBindeesExpr fn expr)

    mapStgBindeesDeflt StgNoDefault		    = StgNoDefault
    mapStgBindeesDeflt (StgBindDefault b used expr) = StgBindDefault b used (mapStgBindeesExpr fn expr)

------------------
mapStgBindeesArg :: (Id -> Id) -> StgArg -> StgArg
mapStgBindeesArg fn a@(StgLitArg _)   = a
mapStgBindeesArg fn a@(StgConArg _)   = a
mapStgBindeesArg fn a@(StgVarArg id)  = StgVarArg (fn id)
\end{code}

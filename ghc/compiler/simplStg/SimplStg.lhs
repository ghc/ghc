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
import CostCentre       ( CostCentre )
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
			  GenId{-instance Eq/Outputable -}, SYN_IE(Id)
			)
import Maybes		( maybeToBool )
import PprType		( GenType{-instance Outputable-} )
import Outputable       ( PprStyle, Outputable(..) )
import Pretty		( Doc, ($$), vcat, text, ptext )
import UniqSupply	( splitUniqSupply, UniqSupply )
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
	hPutStr stderr (show
	(($$) (ptext SLIT("*** Core2Stg:"))
		 (vcat (map (ppr ppr_style) (setStgVarInfo False binds)))
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
	    hPutStr stderr (show
	    (($$) (text ("*** "++what++":"))
		     (vcat (map (ppr ppr_style) binds2))
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

%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[SimplStg]{Driver for simplifying @STG@ programs}

\begin{code}
module SimplStg ( stg2stg ) where

#include "HsVersions.h"

import StgSyn

import LambdaLift	( liftProgram )
import CostCentre       ( CostCentre, CostCentreStack )
import SCCfinal		( stgMassageForProfiling )
import StgLint		( lintStgBindings )
import StgStats	        ( showStgStats )
import StgVarInfo	( setStgVarInfo )
import SRT		( computeSRTs )

import CmdLineOpts	( DynFlags, DynFlag(..), dopt, 
			  opt_StgDoLetNoEscapes,
			  StgToDo(..), dopt_StgToDo
			)
import Id		( Id )
import Module		( Module )
import ErrUtils		( doIfSet_dyn, dumpIfSet_dyn, showPass )
import UniqSupply	( mkSplitUniqSupply, splitUniqSupply, UniqSupply )
import IO		( hPutStr, stdout )
import Outputable
\end{code}

\begin{code}
stg2stg :: DynFlags		-- includes spec of what stg-to-stg passes to do
	-> Module		-- module name (profiling only)
	-> UniqSupply		-- a name supply
	-> [StgBinding]		-- input...
	-> IO
	    ([(StgBinding,[Id])],  -- output program...
	     ([CostCentre],	   -- local cost-centres that need to be decl'd
	      [CostCentre],	   -- "extern" cost-centres
	      [CostCentreStack]))  -- pre-defined "singleton" cost centre stacks

stg2stg dflags module_name us binds
  = do	{ showPass dflags "Stg2Stg"
	; us <- mkSplitUniqSupply 'g'

	; doIfSet_dyn dflags Opt_D_verbose_stg2stg 
		      (printDump (text "VERBOSE STG-TO-STG:"))

	; (binds', us', ccs) <- end_pass us "Core2Stg" ([],[],[]) binds

		-- Do the main business!
	; (processed_binds, _, cost_centres) 
		<- foldl_mn do_stg_pass (binds', us', ccs)
			    (dopt_StgToDo dflags)

		-- Do essential wind-up
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

	; let annotated_binds = setStgVarInfo opt_StgDoLetNoEscapes processed_binds
	      srt_binds       = computeSRTs annotated_binds

	; dumpIfSet_dyn dflags Opt_D_dump_stg "STG syntax:" 
	     		(pprStgBindingsWithSRTs srt_binds)

	; return (srt_binds, cost_centres)
   }

  where
    stg_linter = if dopt Opt_DoStgLinting dflags
		 then lintStgBindings
		 else ( \ whodunnit binds -> binds )

    -------------------------------------------
    do_stg_pass (binds, us, ccs) to_do
      =	let
	    (us1, us2) = splitUniqSupply us
	in
	case to_do of
	  StgDoStaticArgs ->  panic "STG static argument transformation deleted"

	  D_stg_stats ->
	     trace (showStgStats binds)
	     end_pass us2 "StgStats" ccs binds

	  StgDoLambdaLift ->
	     _scc_ "StgLambdaLift"
		-- NB We have to do setStgVarInfo first!
	     let
		binds3 = liftProgram module_name us1 (setStgVarInfo opt_StgDoLetNoEscapes binds)
	     in
	     end_pass us2 "LambdaLift" ccs binds3

	  StgDoMassageForProfiling ->
	     _scc_ "ProfMassage"
	     let
		 (collected_CCs, binds3)
		   = stgMassageForProfiling module_name us1 binds
	     in
	     end_pass us2 "ProfMassage" collected_CCs binds3

    end_pass us2 what ccs binds2
      = -- report verbosely, if required
	(if dopt Opt_D_verbose_stg2stg dflags then
	    hPutStr stdout (showSDoc
	      (text ("*** "++what++":") $$ vcat (map ppr binds2)
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

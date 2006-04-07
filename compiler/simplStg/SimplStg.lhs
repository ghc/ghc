%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[SimplStg]{Driver for simplifying @STG@ programs}

\begin{code}
module SimplStg ( stg2stg ) where

#include "HsVersions.h"

import StgSyn

import CostCentre       ( CollectedCCs )
import SCCfinal		( stgMassageForProfiling )
import StgLint		( lintStgBindings )
import StgStats	        ( showStgStats )
import SRT		( computeSRTs )

import Packages		( HomeModules )
import DynFlags		( DynFlags(..), DynFlag(..), dopt, StgToDo(..),
			  getStgToDo )
import Id		( Id )
import Module		( Module )
import ErrUtils		( doIfSet_dyn, dumpIfSet_dyn, showPass )
import UniqSupply	( mkSplitUniqSupply, splitUniqSupply )
import Outputable
\end{code}

\begin{code}
stg2stg :: DynFlags		     -- includes spec of what stg-to-stg passes to do
	-> HomeModules
	-> Module		     -- module name (profiling only)
	-> [StgBinding]		     -- input...
	-> IO ( [(StgBinding,[(Id,[Id])])]  -- output program...
	      , CollectedCCs)        -- cost centre information (declared and used)

stg2stg dflags pkg_deps module_name binds
  = do	{ showPass dflags "Stg2Stg"
	; us <- mkSplitUniqSupply 'g'

	; doIfSet_dyn dflags Opt_D_verbose_stg2stg 
		      (printDump (text "VERBOSE STG-TO-STG:"))

	; (binds', us', ccs) <- end_pass us "Stg2Stg" ([],[],[]) binds

		-- Do the main business!
	; (processed_binds, _, cost_centres) 
		<- foldl_mn do_stg_pass (binds', us', ccs) (getStgToDo dflags)

	; let srt_binds = computeSRTs processed_binds

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
	  D_stg_stats ->
	     trace (showStgStats binds)
	     end_pass us2 "StgStats" ccs binds

	  StgDoMassageForProfiling ->
	     {-# SCC "ProfMassage" #-}
	     let
		 (collected_CCs, binds3)
		   = stgMassageForProfiling pkg_deps module_name us1 binds
	     in
	     end_pass us2 "ProfMassage" collected_CCs binds3

    end_pass us2 what ccs binds2
      = do -- report verbosely, if required
	   dumpIfSet_dyn dflags Opt_D_verbose_stg2stg what
	      (vcat (map ppr binds2))
	   let linted_binds = stg_linter what binds2
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

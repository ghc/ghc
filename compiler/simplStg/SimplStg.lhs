%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[SimplStg]{Driver for simplifying @STG@ programs}

\begin{code}
{-# LANGUAGE CPP #-}

module SimplStg ( stg2stg ) where

#include "HsVersions.h"

import StgSyn

import CostCentre       ( CollectedCCs )
import SCCfinal         ( stgMassageForProfiling )
import StgLint          ( lintStgBindings )
import StgStats         ( showStgStats )
import UnariseStg       ( unarise )

import DynFlags
import Module           ( Module )
import ErrUtils
import SrcLoc
import UniqSupply       ( mkSplitUniqSupply, splitUniqSupply )
import Outputable
import Control.Monad
\end{code}

\begin{code}
stg2stg :: DynFlags                  -- includes spec of what stg-to-stg passes to do
        -> Module                    -- module name (profiling only)
        -> [StgBinding]              -- input...
        -> IO ( [StgBinding]         -- output program...
              , CollectedCCs)        -- cost centre information (declared and used)

stg2stg dflags module_name binds
  = do  { showPass dflags "Stg2Stg"
        ; us <- mkSplitUniqSupply 'g'

        ; when (dopt Opt_D_verbose_stg2stg dflags)
               (log_action dflags dflags SevDump noSrcSpan defaultDumpStyle (text "VERBOSE STG-TO-STG:"))

        ; (binds', us', ccs) <- end_pass us "Stg2Stg" ([],[],[]) binds

                -- Do the main business!
        ; let (us0, us1) = splitUniqSupply us'
        ; (processed_binds, _, cost_centres)
                <- foldM do_stg_pass (binds', us0, ccs) (getStgToDo dflags)

        ; let un_binds = unarise us1 processed_binds

        ; dumpIfSet_dyn dflags Opt_D_dump_stg "STG syntax:"
                        (pprStgBindings un_binds)

        ; return (un_binds, cost_centres)
   }

  where
    stg_linter = if gopt Opt_DoStgLinting dflags
                 then lintStgBindings
                 else ( \ _whodunnit binds -> binds )

    -------------------------------------------
    do_stg_pass (binds, us, ccs) to_do
      = let
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
                   = stgMassageForProfiling dflags module_name us1 binds
             in
             end_pass us2 "ProfMassage" collected_CCs binds3

    end_pass us2 what ccs binds2
      = do -- report verbosely, if required
           dumpIfSet_dyn dflags Opt_D_verbose_stg2stg what
              (vcat (map ppr binds2))
           let linted_binds = stg_linter what binds2
           return (linted_binds, us2, ccs)
            -- return: processed binds
            --         UniqueSupply for the next guy to use
            --         cost-centres to be declared/registered (specialised)
            --         add to description of what's happened (reverse order)
\end{code}

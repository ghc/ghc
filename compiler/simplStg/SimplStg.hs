{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[SimplStg]{Driver for simplifying @STG@ programs}
-}

{-# LANGUAGE CPP #-}

module SimplStg ( stg2stg ) where

#include "HsVersions.h"

import GhcPrelude

import StgSyn

import CostCentre       ( CollectedCCs )
import SCCfinal         ( stgMassageForProfiling )
import StgLint          ( lintStgTopBindings )
import StgStats         ( showStgStats )
import UnariseStg       ( unarise )
import StgCse           ( stgCse )

import DynFlags
import Module           ( Module )
import ErrUtils
import SrcLoc
import UniqSupply       ( mkSplitUniqSupply, splitUniqSupply )
import Outputable
import Control.Monad

stg2stg :: DynFlags                  -- includes spec of what stg-to-stg passes to do
        -> Module                    -- module name (profiling only)
        -> [StgTopBinding]           -- input...
        -> IO ( [StgTopBinding]      -- output program...
              , CollectedCCs)        -- cost centre information (declared and used)

stg2stg dflags module_name binds
  = do  { showPass dflags "Stg2Stg"
        ; us <- mkSplitUniqSupply 'g'

        ; when (dopt Opt_D_verbose_stg2stg dflags)
               (putLogMsg dflags NoReason SevDump noSrcSpan
                  (defaultDumpStyle dflags) (text "VERBOSE STG-TO-STG:"))

        ; (binds', us', ccs) <- end_pass us "Stg2Stg" ([],[]) binds

                -- Do the main business!
        ; let (us0, us1) = splitUniqSupply us'
        ; (processed_binds, _, cost_centres)
                <- foldM do_stg_pass (binds', us0, ccs) (getStgToDo dflags)

        ; dumpIfSet_dyn dflags Opt_D_dump_stg "Pre unarise:"
                        (pprStgTopBindings processed_binds)

        ; let un_binds = stg_linter True "Unarise"
                         $ unarise us1 processed_binds

        ; dumpIfSet_dyn dflags Opt_D_dump_stg "STG syntax:"
                        (pprStgTopBindings un_binds)

        ; return (un_binds, cost_centres)
   }

  where
    stg_linter unarised
      | gopt Opt_DoStgLinting dflags = lintStgTopBindings unarised
      | otherwise                    = \ _whodunnit binds -> binds

    -------------------------------------------
    do_stg_pass (binds, us, ccs) to_do
      = case to_do of
          D_stg_stats ->
             trace (showStgStats binds)
             end_pass us "StgStats" ccs binds

          StgDoMassageForProfiling ->
             {-# SCC "ProfMassage" #-}
             let
                 (us1, us2) = splitUniqSupply us
                 (collected_CCs, binds3)
                   = stgMassageForProfiling dflags module_name us1 binds
             in
             end_pass us2 "ProfMassage" collected_CCs binds3

          StgCSE ->
             {-# SCC "StgCse" #-}
             let
                 binds' = stgCse binds
             in
             end_pass us "StgCse" ccs binds'

    end_pass us2 what ccs binds2
      = do -- report verbosely, if required
           dumpIfSet_dyn dflags Opt_D_verbose_stg2stg what
              (vcat (map ppr binds2))
           let linted_binds = stg_linter False what binds2
           return (linted_binds, us2, ccs)
            -- return: processed binds
            --         UniqueSupply for the next guy to use
            --         cost-centres to be declared/registered (specialised)
            --         add to description of what's happened (reverse order)

-- -----------------------------------------------------------------------------
-- StgToDo:  abstraction of stg-to-stg passes to run.

-- | Optional Stg-to-Stg passes.
data StgToDo
  = StgCSE
  | StgDoMassageForProfiling  -- should be (next to) last
  | D_stg_stats

-- | Which optional Stg-to-Stg passes to run. Depends on flags, ways etc.
getStgToDo :: DynFlags -> [StgToDo]
getStgToDo dflags
  = [ StgCSE                   | gopt Opt_StgCSE dflags] ++
    [ StgDoMassageForProfiling | WayProf `elem` ways dflags] ++
    [ D_stg_stats              | stg_stats ]
  where
        stg_stats = gopt Opt_StgStats dflags

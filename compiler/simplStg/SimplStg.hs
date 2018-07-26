{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[SimplStg]{Driver for simplifying @STG@ programs}
-}

{-# LANGUAGE CPP #-}

module SimplStg ( stg2stg ) where

#include "HsVersions.h"

import GhcPrelude

import StgSyn

import StgLint          ( lintStgTopBindings )
import StgStats         ( showStgStats )
import UnariseStg       ( unarise )
import StgCse           ( stgCse )

import DynFlags
import ErrUtils
import UniqSupply       ( mkSplitUniqSupply )
import Outputable
import Control.Monad

stg2stg :: DynFlags                  -- includes spec of what stg-to-stg passes to do
        -> [StgTopBinding]           -- input...
        -> IO [StgTopBinding]        -- output program

stg2stg dflags binds
  = do  { showPass dflags "Stg2Stg"
        ; us <- mkSplitUniqSupply 'g'

                -- Do the main business!
        ; dumpIfSet_dyn dflags Opt_D_dump_stg "Pre unarise:"
                        (pprStgTopBindings binds)

        ; stg_linter False "Pre-unarise" binds
        ; let un_binds = unarise us binds
        ; stg_linter True "Unarise" un_binds

        ; dumpIfSet_dyn dflags Opt_D_dump_stg "STG syntax:"
                        (pprStgTopBindings un_binds)

        ; foldM do_stg_pass un_binds (getStgToDo dflags)
        }

  where
    stg_linter unarised
      | gopt Opt_DoStgLinting dflags = lintStgTopBindings dflags unarised
      | otherwise                    = \ _whodunnit _binds -> return ()

    -------------------------------------------
    do_stg_pass binds to_do
      = case to_do of
          D_stg_stats ->
             trace (showStgStats binds) (return binds)

          StgCSE ->
             {-# SCC "StgCse" #-}
             let
                 binds' = stgCse binds
             in
             end_pass "StgCse" binds'

    end_pass what binds2
      = do -- report verbosely, if required
           dumpIfSet_dyn dflags Opt_D_verbose_stg2stg what
              (pprStgTopBindings binds2)
           stg_linter True what binds2
           return binds2

-- -----------------------------------------------------------------------------
-- StgToDo:  abstraction of stg-to-stg passes to run.

-- | Optional Stg-to-Stg passes.
data StgToDo
  = StgCSE
  | D_stg_stats

-- | Which optional Stg-to-Stg passes to run. Depends on flags, ways etc.
getStgToDo :: DynFlags -> [StgToDo]
getStgToDo dflags
  = [ StgCSE                   | gopt Opt_StgCSE dflags] ++
    [ D_stg_stats              | stg_stats ]
  where
        stg_stats = gopt Opt_StgStats dflags

{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[SimplStg]{Driver for simplifying @STG@ programs}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SimplStg ( stg2stg ) where

#include "HsVersions.h"

import GhcPrelude

import StgSyn

import StgLint          ( lintStgTopBindings )
import StgStats         ( showStgStats )
import UnariseStg       ( unarise )
import StgCse           ( stgCse )
import StgLiftLams      ( stgLiftLams )
import Module           ( Module )

import DynFlags
import ErrUtils
import UniqSupply
import Outputable
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

newtype StgM a = StgM { _unStgM :: StateT UniqSupply IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadUnique StgM where
  getUniqueSupplyM = StgM (state splitUniqSupply)
  getUniqueM = StgM (state takeUniqFromSupply)

runStgM :: UniqSupply -> StgM a -> IO a
runStgM us (StgM m) = evalStateT m us

stg2stg :: DynFlags                  -- includes spec of what stg-to-stg passes to do
        -> Module                    -- module being compiled
        -> [StgTopBinding]           -- input program
        -> IO [StgTopBinding]        -- output program

stg2stg dflags this_mod binds
  = do  { showPass dflags "Stg2Stg"
        ; us <- mkSplitUniqSupply 'g'

        -- Do the main business!
        ; binds' <- runStgM us $
            foldM do_stg_pass binds (getStgToDo dflags)

        ; dump_when Opt_D_dump_stg "STG syntax:" binds'

        ; return binds'
   }

  where
    stg_linter what
      | gopt Opt_DoStgLinting dflags
      = lintStgTopBindings dflags this_mod what
      | otherwise
      = \ _whodunnit _binds -> return ()

    -------------------------------------------
    do_stg_pass :: [StgTopBinding] -> StgToDo -> StgM [StgTopBinding]
    do_stg_pass binds to_do
      = case to_do of
          StgDoNothing ->
            return binds

          StgStats ->
            trace (showStgStats binds) (return binds)

          StgCSE -> do
            let binds' = {-# SCC "StgCse" #-} stgCse binds
            end_pass "StgCse" binds'

          StgLiftLams -> do
            us <- getUniqueSupplyM
            let binds' = {-# SCC "StgLiftLams" #-} stgLiftLams dflags us binds
            end_pass "StgLiftLams" binds'

          StgUnarise -> do
            liftIO (dump_when Opt_D_dump_stg "Pre unarise:" binds)
            us <- getUniqueSupplyM
            liftIO (stg_linter False "Pre-unarise" binds)
            let binds' = unarise us binds
            liftIO (stg_linter True "Unarise" binds')
            return binds'

    dump_when flag header binds
      = dumpIfSet_dyn dflags flag header (pprStgTopBindings binds)

    end_pass what binds2
      = liftIO $ do -- report verbosely, if required
          dumpIfSet_dyn dflags Opt_D_verbose_stg2stg what
            (vcat (map ppr binds2))
          stg_linter False what binds2
          return binds2

-- -----------------------------------------------------------------------------
-- StgToDo:  abstraction of stg-to-stg passes to run.

-- | Optional Stg-to-Stg passes.
data StgToDo
  = StgCSE
  -- ^ Common subexpression elimination
  | StgLiftLams
  -- ^ Lambda lifting closure variables, trading stack/register allocation for
  -- heap allocation
  | StgStats
  | StgUnarise
  -- ^ Mandatory unarise pass, desugaring unboxed tuple and sum binders
  | StgDoNothing
  -- ^ Useful for building up 'getStgToDo'
  deriving Eq

-- | Which Stg-to-Stg passes to run. Depends on flags, ways etc.
getStgToDo :: DynFlags -> [StgToDo]
getStgToDo dflags =
  filter (/= StgDoNothing)
    [ mandatory StgUnarise
    -- Important that unarisation comes first
    -- See Note [StgCse after unarisation] in StgCse
    , optional Opt_StgCSE StgCSE
    , optional Opt_StgLiftLams StgLiftLams
    , optional Opt_StgStats StgStats
    ] where
      optional opt = runWhen (gopt opt dflags)
      mandatory = id

runWhen :: Bool -> StgToDo -> StgToDo
runWhen True todo = todo
runWhen _    _    = StgDoNothing

{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[SimplStg]{Driver for simplifying @STG@ programs}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Stg.Pipeline ( stg2stg ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Stg.Syntax

import GHC.Stg.Lint     ( lintStgTopBindings )
import GHC.Stg.Stats    ( showStgStats )
import GHC.Stg.DepAnal  ( depSortStgPgm )
import GHC.Stg.Unarise  ( unarise )
import GHC.Stg.CSE      ( stgCse )
import GHC.Stg.Lift     ( stgLiftLams )
import GHC.Types.Module ( Module )

import GHC.Driver.Session
import ErrUtils
import GHC.Types.Unique.Supply
import Outputable
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

newtype StgM a = StgM { _unStgM :: StateT Char IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadUnique StgM where
  getUniqueSupplyM = StgM $ do { mask <- get
                               ; liftIO $! mkSplitUniqSupply mask}
  getUniqueM = StgM $ do { mask <- get
                         ; liftIO $! uniqFromMask mask}

runStgM :: Char -> StgM a -> IO a
runStgM mask (StgM m) = evalStateT m mask

stg2stg :: DynFlags                  -- includes spec of what stg-to-stg passes to do
        -> Module                    -- module being compiled
        -> [StgTopBinding]           -- input program
        -> IO [CgStgTopBinding]        -- output program

stg2stg dflags this_mod binds
  = do  { dump_when Opt_D_dump_stg "STG:" binds
        ; showPass dflags "Stg2Stg"
        -- Do the main business!
        ; binds' <- runStgM 'g' $
            foldM do_stg_pass binds (getStgToDo dflags)

          -- Dependency sort the program as last thing. The program needs to be
          -- in dependency order for the SRT algorithm to work (see
          -- CmmBuildInfoTables, which also includes a detailed description of
          -- the algorithm), and we don't guarantee that the program is already
          -- sorted at this point. #16192 is for simplifier not preserving
          -- dependency order. We also don't guarantee that StgLiftLams will
          -- preserve the order or only create minimal recursive groups, so a
          -- sorting pass is necessary.
        ; let binds_sorted = depSortStgPgm this_mod binds'
        ; return binds_sorted
   }

  where
    stg_linter unarised
      | gopt Opt_DoStgLinting dflags
      = lintStgTopBindings dflags this_mod unarised
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
            us <- getUniqueSupplyM
            liftIO (stg_linter False "Pre-unarise" binds)
            let binds' = unarise us binds
            liftIO (dump_when Opt_D_dump_stg_unarised "Unarised STG:" binds')
            liftIO (stg_linter True "Unarise" binds')
            return binds'

    dump_when flag header binds
      = dumpIfSet_dyn dflags flag header FormatSTG (pprStgTopBindings binds)

    end_pass what binds2
      = liftIO $ do -- report verbosely, if required
          dumpIfSet_dyn dflags Opt_D_verbose_stg2stg what
            FormatSTG (vcat (map ppr binds2))
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
    -- See Note [StgCse after unarisation] in GHC.Stg.CSE
    , optional Opt_StgCSE StgCSE
    , optional Opt_StgLiftLams StgLiftLams
    , optional Opt_StgStats StgStats
    ] where
      optional opt = runWhen (gopt opt dflags)
      mandatory = id

runWhen :: Bool -> StgToDo -> StgToDo
runWhen True todo = todo
runWhen _    _    = StgDoNothing

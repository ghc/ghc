{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[SimplStg]{Driver for simplifying @STG@ programs}
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Stg.Pipeline ( stg2stg ) where

import GHC.Prelude

import GHC.Stg.Syntax

import GHC.Stg.Lint     ( lintStgTopBindings )
import GHC.Stg.Stats    ( showStgStats )
import GHC.Stg.DepAnal  ( depSortStgPgm )
import GHC.Stg.Unarise  ( unarise )
import GHC.Stg.BcPrep   ( bcPrep )
import GHC.Stg.CSE      ( stgCse )
import GHC.Stg.Lift     ( stgLiftLams )
import GHC.Unit.Module ( Module )
import GHC.Runtime.Context ( InteractiveContext )

import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable
import GHC.Utils.Logger
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

newtype StgM a = StgM { _unStgM :: ReaderT Char IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadUnique StgM where
  getUniqueSupplyM = StgM $ do { mask <- ask
                               ; liftIO $! mkSplitUniqSupply mask}
  getUniqueM = StgM $ do { mask <- ask
                         ; liftIO $! uniqFromMask mask}

runStgM :: Char -> StgM a -> IO a
runStgM mask (StgM m) = runReaderT m mask

stg2stg :: Logger
        -> DynFlags                  -- includes spec of what stg-to-stg passes to do
        -> InteractiveContext
        -> Bool                      -- prepare for bytecode?
        -> Module                    -- module being compiled
        -> [StgTopBinding]           -- input program
        -> IO [StgTopBinding]        -- output program
stg2stg logger dflags ictxt for_bytecode this_mod binds
  = do  { dump_when Opt_D_dump_stg_from_core "Initial STG:" binds
        ; showPass logger "Stg2Stg"
        -- Do the main business!
        ; binds' <- runStgM 'g' $
            foldM do_stg_pass binds (getStgToDo for_bytecode dflags)

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
      = lintStgTopBindings logger dflags ictxt this_mod unarised
      | otherwise
      = \ _whodunnit _binds -> return ()

    -------------------------------------------
    do_stg_pass :: [StgTopBinding] -> StgToDo -> StgM [StgTopBinding]
    do_stg_pass binds to_do
      = case to_do of
          StgStats ->
            logTraceMsg logger "STG stats" (text (showStgStats binds)) (return binds)

          StgCSE -> do
            let binds' = {-# SCC "StgCse" #-} stgCse binds
            end_pass "StgCse" binds'

          StgLiftLams -> do
            us <- getUniqueSupplyM
            let binds' = {-# SCC "StgLiftLams" #-} stgLiftLams dflags us binds
            end_pass "StgLiftLams" binds'

          StgBcPrep -> do
            us <- getUniqueSupplyM
            let binds' = {-# SCC "StgBcPrep" #-} bcPrep us binds
            end_pass "StgBcPrep" binds'

          StgUnarise -> do
            us <- getUniqueSupplyM
            liftIO (stg_linter False "Pre-unarise" binds)
            let binds' = unarise us binds
            liftIO (dump_when Opt_D_dump_stg_unarised "Unarised STG:" binds')
            liftIO (stg_linter True "Unarise" binds')
            return binds'

    opts = initStgPprOpts dflags
    dump_when flag header binds
      = putDumpFileMaybe logger flag header FormatSTG (pprStgTopBindings opts binds)

    end_pass what binds2
      = liftIO $ do -- report verbosely, if required
          putDumpFileMaybe logger Opt_D_verbose_stg2stg what
            FormatSTG (vcat (map (pprStgTopBinding opts) binds2))
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
  | StgBcPrep
  -- ^ Mandatory when compiling to bytecode
  | StgDoNothing
  -- ^ Useful for building up 'getStgToDo'
  deriving Eq

-- | Which Stg-to-Stg passes to run. Depends on flags, ways etc.
getStgToDo :: Bool -> DynFlags -> [StgToDo]
getStgToDo for_bytecode dflags =
  filter (/= StgDoNothing)
    [ mandatory StgUnarise
    -- Important that unarisation comes first
    -- See Note [StgCse after unarisation] in GHC.Stg.CSE
    , optional Opt_StgCSE StgCSE
    , optional Opt_StgLiftLams StgLiftLams
    , runWhen for_bytecode StgBcPrep
    , optional Opt_StgStats StgStats
    ]
    where
      optional opt x
        | gopt opt dflags = Just x
        | otherwise = Nothing
      mandatory = Just

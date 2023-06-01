{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[SimplStg]{Driver for simplifying @STG@ programs}
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Stg.Pipeline
  ( StgPipelineOpts (..)
  , StgToDo (..)
  , stg2stg
  , StgCgInfos
  ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Stg.Syntax

import GHC.Stg.Lint     ( lintStgTopBindings )
import GHC.Stg.Stats    ( showStgStats )
import GHC.Stg.FVs      ( depSortWithAnnotStgPgm )
import GHC.Stg.Unarise  ( unarise )
import GHC.Stg.BcPrep   ( bcPrep )
import GHC.Stg.CSE      ( stgCse )
import GHC.Stg.Lift     ( StgLiftConfig, stgLiftLams )
import GHC.Unit.Module ( Module )

import GHC.Utils.Error
import GHC.Types.Var
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable
import GHC.Utils.Logger
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import GHC.Settings (Platform)
import GHC.Stg.InferTags (inferTags)
import GHC.Types.Name.Env (NameEnv)
import GHC.Stg.InferTags.TagSig (TagSig)

data StgPipelineOpts = StgPipelineOpts
  { stgPipeline_phases      :: ![StgToDo]
  -- ^ Spec of what stg-to-stg passes to do
  , stgPipeline_lint        :: !(Maybe DiagOpts)
  -- ^ Should we lint the STG at various stages of the pipeline?
  , stgPipeline_pprOpts     :: !StgPprOpts
  , stgPlatform             :: !Platform
  , stgPipeline_forBytecode :: !Bool
  }

newtype StgM a = StgM { _unStgM :: ReaderT Char IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Information to be exposed in interface files which is produced
-- by the stg2stg passes.
type StgCgInfos = NameEnv TagSig

instance MonadUnique StgM where
  getUniqueSupplyM = StgM $ do { tag <- ask
                               ; liftIO $! mkSplitUniqSupply tag}
  getUniqueM = StgM $ do { tag <- ask
                         ; liftIO $! uniqFromTag tag}

runStgM :: Char -> StgM a -> IO a
runStgM mask (StgM m) = runReaderT m mask

stg2stg :: Logger
        -> [Var]                     -- ^ extra vars in scope from GHCi
        -> StgPipelineOpts
        -> Module                    -- ^ module being compiled
        -> [StgTopBinding]           -- ^ input program
        -> IO ([CgStgTopBinding], StgCgInfos) -- output program
stg2stg logger extra_vars opts this_mod binds
  = do  { dump_when Opt_D_dump_stg_from_core "Initial STG:" binds
        ; showPass logger "Stg2Stg"
        -- Do the main business!
        ; binds' <- runStgM 'g' $
            foldM (do_stg_pass this_mod) binds (stgPipeline_phases opts)

          -- Dependency sort the program as last thing. The program needs to be
          -- in dependency order for the SRT algorithm to work (see
          -- CmmBuildInfoTables, which also includes a detailed description of
          -- the algorithm), and we don't guarantee that the program is already
          -- sorted at this point. #16192 is for simplifier not preserving
          -- dependency order. We also don't guarantee that StgLiftLams will
          -- preserve the order or only create minimal recursive groups, so a
          -- sorting pass is necessary.
          -- This pass will also augment each closure with non-global free variables
          -- annotations (which is used by code generator to compute offsets into closures)
        ; let binds_sorted_with_fvs = depSortWithAnnotStgPgm this_mod binds'
        -- See Note [Tag inference for interactive contexts]
        ; inferTags (stgPipeline_pprOpts opts) (stgPipeline_forBytecode opts) logger this_mod binds_sorted_with_fvs
   }

  where
    stg_linter unarised
      | Just diag_opts <- stgPipeline_lint opts
      = lintStgTopBindings
          (stgPlatform opts) logger
          diag_opts ppr_opts
          extra_vars this_mod unarised
      | otherwise
      = \ _whodunit _binds -> return ()

    -------------------------------------------
    do_stg_pass :: Module -> [StgTopBinding] -> StgToDo -> StgM [StgTopBinding]
    do_stg_pass this_mod binds to_do
      = case to_do of
          StgDoNothing ->
            return binds

          StgStats ->
            logTraceMsg logger "STG stats" (text (showStgStats binds)) (return binds)

          StgCSE -> do
            let binds' = {-# SCC "StgCse" #-} stgCse binds
            end_pass "StgCse" binds'

          StgLiftLams cfg -> do
            us <- getUniqueSupplyM
            --
            let binds' = {-# SCC "StgLiftLams" #-} stgLiftLams this_mod cfg us binds
            end_pass "StgLiftLams" binds'

          StgBcPrep -> do
            us <- getUniqueSupplyM
            let binds' = {-# SCC "StgBcPrep" #-} bcPrep us binds
            end_pass "StgBcPrep" binds'

          StgUnarise -> do
            us <- getUniqueSupplyM
            liftIO (stg_linter False "Pre-unarise" binds)
            let binds' = {-# SCC "StgUnarise" #-} unarise us binds
            liftIO (dump_when Opt_D_dump_stg_unarised "Unarised STG:" binds')
            liftIO (stg_linter True "Unarise" binds')
            return binds'

    ppr_opts = stgPipeline_pprOpts opts
    dump_when flag header binds
      = putDumpFileMaybe logger flag header FormatSTG (pprStgTopBindings ppr_opts binds)

    end_pass what binds2
      = liftIO $ do -- report verbosely, if required
          putDumpFileMaybe logger Opt_D_verbose_stg2stg what
            FormatSTG (vcat (map (pprStgTopBinding ppr_opts) binds2))
          stg_linter False what binds2
          return binds2

-- -----------------------------------------------------------------------------
-- StgToDo:  abstraction of stg-to-stg passes to run.

-- | Optional Stg-to-Stg passes.
data StgToDo
  = StgCSE
  -- ^ Common subexpression elimination
  | StgLiftLams StgLiftConfig
  -- ^ Lambda lifting closure variables, trading stack/register allocation for
  -- heap allocation
  | StgStats
  | StgUnarise
  -- ^ Mandatory unarise pass, desugaring unboxed tuple and sum binders
  | StgBcPrep
  -- ^ Mandatory when compiling to bytecode
  | StgDoNothing
  -- ^ Useful for building up 'getStgToDo'
  deriving (Show, Read, Eq, Ord)

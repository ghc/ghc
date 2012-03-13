{-# OPTIONS_GHC -XNoMonoLocalBinds #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

module CmmPipeline (
  -- | Converts C-- with an implicit stack and native C-- calls into
  -- optimized, CPS converted and native-call-less C--.  The latter
  -- C-- can be used to generate assembly.
  cmmPipeline
) where

import CLabel
import Cmm
import CmmLint
import CmmLive
import CmmBuildInfoTables
import CmmCommonBlockElim
import CmmProcPoint
import CmmRewriteAssignments
import CmmContFlowOpt
import OptimizationFuel
import CmmLayoutStack
import Hoopl
import CmmUtils

import DynFlags
import ErrUtils
import HscTypes
import Data.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Outputable
import StaticFlags

-----------------------------------------------------------------------------
-- | Top level driver for C-- pipeline
-----------------------------------------------------------------------------
-- There are two complications here:
-- 1. We need to compile the procedures in two stages because we need
--    an analysis of the procedures to tell us what CAFs they use.
--    The first stage returns a map from procedure labels to CAFs,
--    along with a closure that will compute SRTs and attach them to
--    the compiled procedures.
--    The second stage is to combine the CAF information into a top-level
--    CAF environment mapping non-static closures to the CAFs they keep live,
--    then pass that environment to the closures returned in the first
--    stage of compilation.
-- 2. We need to thread the module's SRT around when the SRT tables
--    are computed for each procedure.
--    The SRT needs to be threaded because it is grown lazily.
-- 3. We run control flow optimizations twice, once before any pipeline
--    work is done, and once again at the very end on all of the
--    resulting C-- blocks.  EZY: It's unclear whether or not whether
--    we actually need to do the initial pass.
cmmPipeline  :: HscEnv -- Compilation env including
                       -- dynamic flags: -dcmm-lint -ddump-cps-cmm
             -> TopSRT     -- SRT table and accumulating list of compiled procs
             -> CmmGroup             -- Input C-- with Procedures
             -> IO (TopSRT, CmmGroup) -- Output CPS transformed C--
cmmPipeline hsc_env topSRT prog =
  do let dflags = hsc_dflags hsc_env
     --
     showPass dflags "CPSZ"

     (cafEnvs, tops) <- liftM unzip $ mapM (cpsTop hsc_env) prog
     -- tops :: [[(CmmDecl,CAFSet]]  (one list per group)

     let topCAFEnv = mkTopCAFInfo (concat cafEnvs)

     -- folding over the groups
     (topSRT, tops) <- foldM (toTops hsc_env topCAFEnv) (topSRT, []) tops

     let cmms :: CmmGroup
         cmms = reverse (concat tops)

     dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (pprPlatform (targetPlatform dflags) cmms)

     return (topSRT, cmms)

{- [Note global fuel]
~~~~~~~~~~~~~~~~~~~~~
The identity and the last pass are stored in
mutable reference cells in an 'HscEnv' and are
global to one compiler session.
-}

-- EZY: It might be helpful to have an easy way of dumping the "pre"
-- input for any given phase, besides just turning it all on with
-- -ddump-cmmz

cpsTop :: HscEnv -> CmmDecl -> IO ([(CLabel, CAFSet)], [(CAFSet, CmmDecl)])
cpsTop _ p@(CmmData {}) = return ([], [(Set.empty, p)])
cpsTop hsc_env (CmmProc h@(TopInfo {stack_info=StackInfo {arg_space=entry_off}}) l g) =
    do
       ----------- Control-flow optimisations ---------------
       g <- {-# SCC "cmmCfgOpts(1)" #-} return $ cmmCfgOpts g
       dump Opt_D_dump_cmmz_cfg "Post control-flow optimsations" g

       ----------- Eliminate common blocks -------------------
       g <- {-# SCC "elimCommonBlocks" #-} return $ elimCommonBlocks g
       dump Opt_D_dump_cmmz_cbe "Post common block elimination" g
       -- Any work storing block Labels must be performed _after_
       -- elimCommonBlocks

       ----------- Proc points -------------------
       let callPPs = {-# SCC "callProcPoints" #-} callProcPoints g
       procPoints <- {-# SCC "minimalProcPointSet" #-} run $
                     minimalProcPointSet (targetPlatform dflags) callPPs g

       ----------- Layout the stack and manifest Sp ---------------
       -- (also does: removeDeadAssignments, and lowerSafeForeignCalls)
       (g, stackmaps) <- {-# SCC "layoutStack" #-}
                         run $ cmmLayoutStack procPoints entry_off g
       dump Opt_D_dump_cmmz_sp "Layout Stack" g

--       ----------- Sink and inline assignments -------------------
--       g <- {-# SCC "rewriteAssignments" #-} runOptimization $
--            rewriteAssignments platform g
--       dump Opt_D_dump_cmmz_rewrite "Post rewrite assignments" g

       ------------- Split into separate procedures ------------
       procPointMap  <- {-# SCC "procPointAnalysis" #-} run $
                        procPointAnalysis procPoints g
       dumpWith dflags ppr Opt_D_dump_cmmz_procmap "procpoint map" procPointMap
       gs <- {-# SCC "splitAtProcPoints" #-} run $
             splitAtProcPoints l callPPs procPoints procPointMap (CmmProc h l g)
       dumps Opt_D_dump_cmmz_split "Post splitting" gs

       ------------- More CAFs ------------------------------
       let cafEnv = {-# SCC "cafAnal" #-} cafAnal platform g
       let localCAFs = catMaybes $ map (localCAFInfo platform cafEnv) gs
       mbpprTrace "localCAFs" (pprPlatform platform localCAFs) $ return ()

       -- NO MORE GRAPH TRANSFORMATION AFTER HERE -- JUST MAKING INFOTABLES
       gs <- {-# SCC "setInfoTableStackMap" #-}
             return $ map (setInfoTableStackMap stackmaps) gs
       dumps Opt_D_dump_cmmz_info "after setInfoTableStackMap" gs

       ----------- Control-flow optimisations ---------------
       gs <- {-# SCC "cmmCfgOpts(2)" #-} return $ map cmmCfgOptsProc gs
       dumps Opt_D_dump_cmmz_cfg "Post control-flow optimsations" gs

       gs <- {-# SCC "bundleCAFs" #-} return $ map (bundleCAFs cafEnv) gs
       dumps Opt_D_dump_cmmz_cafs "after bundleCAFs" gs

       return (localCAFs, gs)

              -- gs        :: [ (CAFSet, CmmDecl) ]
              -- localCAFs :: [ (CLabel, CAFSet) ] -- statics filtered out(?)

  where dflags = hsc_dflags hsc_env
        platform = targetPlatform dflags
        mbpprTrace x y z | dopt Opt_D_dump_cmmz dflags = pprTrace x y z
                         | otherwise = z
        dump = dumpGraph dflags

        dumps flag name
           = mapM_ (dumpWith dflags (pprPlatform platform) flag name)

        -- Runs a required transformation/analysis
        run = runInfiniteFuelIO (hsc_OptFuel hsc_env)
        -- Runs an optional transformation/analysis (and should
        -- thus be subject to optimization fuel)
        runOptimization = runFuelIO (hsc_OptFuel hsc_env)


dumpGraph :: DynFlags -> DynFlag -> String -> CmmGraph -> IO ()
dumpGraph dflags flag name g = do
  when (dopt Opt_DoCmmLinting dflags) $ do_lint g
  dumpWith dflags (pprPlatform (targetPlatform dflags)) flag name g
 where
  do_lint g = case cmmLintGraph (targetPlatform dflags) g of
                 Just err -> do { printDump err
                                ; ghcExit dflags 1
                                }
                 Nothing  -> return ()

dumpWith :: DynFlags -> (a -> SDoc) -> DynFlag -> String -> a -> IO ()
dumpWith dflags pprFun flag txt g = do
         -- ToDo: No easy way of say "dump all the cmmz, *and* split
         -- them into files."  Also, -ddump-cmmz doesn't play nicely
         -- with -ddump-to-file, since the headers get omitted.
   dumpIfSet_dyn dflags flag txt (pprFun g)
   when (not (dopt flag dflags)) $
      dumpIfSet_dyn dflags Opt_D_dump_cmmz txt (pprFun g)

-- This probably belongs in CmmBuildInfoTables?
-- We're just finishing the job here: once we know what CAFs are defined
-- in non-static closures, we can build the SRTs.
toTops :: HscEnv -> Map CLabel CAFSet -> (TopSRT, [[CmmDecl]])
                 -> [(CAFSet, CmmDecl)] -> IO (TopSRT, [[CmmDecl]])
toTops hsc_env topCAFEnv (topSRT, tops) gs =
  do let setSRT (topSRT, rst) g =
           do (topSRT, gs) <- setInfoTableSRT topCAFEnv topSRT g
              return (topSRT, gs : rst)
     (topSRT, gs') <- runFuelIO (hsc_OptFuel hsc_env) $ foldM setSRT (topSRT, []) gs
     return (topSRT, concat gs' : tops)

{-# LANGUAGE NoMonoLocalBinds #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this extension in due course

module CmmPipeline (
  -- | Converts C-- with an implicit stack and native C-- calls into
  -- optimized, CPS converted and native-call-less C--.  The latter
  -- C-- can be used to generate assembly.
  cmmPipeline
) where

import CLabel
import Cmm
import CmmLint
import CmmBuildInfoTables
import CmmCommonBlockElim
import CmmProcPoint
import CmmContFlowOpt
import CmmLayoutStack

import UniqSupply
import DynFlags
import ErrUtils
import HscTypes
import Data.Maybe
import Control.Monad
import Outputable

import qualified Data.Set as Set
import Data.Map (Map)

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

     (cafEnvs, tops) <- {-# SCC "tops" #-} liftM unzip $ mapM (cpsTop hsc_env) prog
     -- tops :: [[(CmmDecl,CAFSet]]  (one list per group)

     let topCAFEnv = {-# SCC "topCAFEnv" #-} mkTopCAFInfo (concat cafEnvs)

     -- folding over the groups
     (topSRT, tops) <- {-# SCC "toTops" #-} foldM (toTops topCAFEnv) (topSRT, []) tops

     let cmms :: CmmGroup
         cmms = reverse (concat tops)

     dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (ppr cmms)

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
       procPoints <- {-# SCC "minimalProcPointSet" #-} runUniqSM $
                     minimalProcPointSet (targetPlatform dflags) callPPs g

       ----------- Layout the stack and manifest Sp ---------------
       -- (also does: removeDeadAssignments, and lowerSafeForeignCalls)
       (g, stackmaps) <- {-# SCC "layoutStack" #-}
                         runUniqSM $ cmmLayoutStack procPoints entry_off g
       dump Opt_D_dump_cmmz_sp "Layout Stack" g

--       g <- {-# SCC "sink" #-} runUniqSM $ cmmSink g
--       dump Opt_D_dump_cmmz_rewrite "Sink assignments" g

--       ----------- Sink and inline assignments -------------------
--       g <- {-# SCC "rewriteAssignments" #-} runOptimization $
--            rewriteAssignments platform g
--       dump Opt_D_dump_cmmz_rewrite "Post rewrite assignments" g

       ------------- Split into separate procedures ------------
       procPointMap  <- {-# SCC "procPointAnalysis" #-} runUniqSM $
                        procPointAnalysis procPoints g
       dumpWith dflags Opt_D_dump_cmmz_procmap "procpoint map" procPointMap
       gs <- {-# SCC "splitAtProcPoints" #-} runUniqSM $
             splitAtProcPoints l callPPs procPoints procPointMap (CmmProc h l g)
       dumps Opt_D_dump_cmmz_split "Post splitting" gs

       ------------- More CAFs ------------------------------
       let cafEnv = {-# SCC "cafAnal" #-} cafAnal g
       let localCAFs = {-# SCC "localCAFs" #-} catMaybes $ map (localCAFInfo cafEnv) gs
       mbpprTrace "localCAFs" (ppr localCAFs) $ return ()

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
        mbpprTrace x y z | dopt Opt_D_dump_cmmz dflags = pprTrace x y z
                         | otherwise = z
        dump = dumpGraph dflags

        dumps flag name
           = mapM_ (dumpWith dflags flag name)

runUniqSM :: UniqSM a -> IO a
runUniqSM m = do
  us <- mkSplitUniqSupply 'u'
  return (initUs_ us m)


dumpGraph :: DynFlags -> DynFlag -> String -> CmmGraph -> IO ()
dumpGraph dflags flag name g = do
  when (dopt Opt_DoCmmLinting dflags) $ do_lint g
  dumpWith dflags flag name g
 where
  do_lint g = case cmmLintGraph g of
                 Just err -> do { fatalErrorMsg dflags err
                                ; ghcExit dflags 1
                                }
                 Nothing  -> return ()

dumpWith :: Outputable a => DynFlags -> DynFlag -> String -> a -> IO ()
dumpWith dflags flag txt g = do
         -- ToDo: No easy way of say "dump all the cmmz, *and* split
         -- them into files."  Also, -ddump-cmmz doesn't play nicely
         -- with -ddump-to-file, since the headers get omitted.
   dumpIfSet_dyn dflags flag txt (ppr g)
   when (not (dopt flag dflags)) $
      dumpIfSet_dyn dflags Opt_D_dump_cmmz txt (ppr g)

-- This probably belongs in CmmBuildInfoTables?
-- We're just finishing the job here: once we know what CAFs are defined
-- in non-static closures, we can build the SRTs.
toTops :: Map CLabel CAFSet -> (TopSRT, [[CmmDecl]])
       -> [(CAFSet, CmmDecl)] -> IO (TopSRT, [[CmmDecl]])
toTops topCAFEnv (topSRT, tops) gs =
  do let setSRT (topSRT, rst) g =
           do (topSRT, gs) <- setInfoTableSRT topCAFEnv topSRT g
              return (topSRT, gs : rst)
     (topSRT, gs') <- runUniqSM $ foldM setSRT (topSRT, []) gs
     return (topSRT, concat gs' : tops)

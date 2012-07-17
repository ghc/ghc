{-# LANGUAGE NoMonoLocalBinds #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this extension in due course

module CmmPipeline (
  -- | Converts C-- with an implicit stack and native C-- calls into
  -- optimized, CPS converted and native-call-less C--.  The latter
  -- C-- can be used to generate assembly.
  cmmPipeline
) where

import Cmm
import CmmLint
import CmmBuildInfoTables
import CmmCommonBlockElim
import CmmProcPoint
import CmmContFlowOpt
import CmmLayoutStack
import CmmSink
import Hoopl

import UniqSupply
import DynFlags
import ErrUtils
import HscTypes
import Control.Monad
import Outputable

-----------------------------------------------------------------------------
-- | Top level driver for C-- pipeline
-----------------------------------------------------------------------------

cmmPipeline  :: HscEnv -- Compilation env including
                       -- dynamic flags: -dcmm-lint -ddump-cps-cmm
             -> TopSRT     -- SRT table and accumulating list of compiled procs
             -> CmmGroup             -- Input C-- with Procedures
             -> IO (TopSRT, CmmGroup) -- Output CPS transformed C--

cmmPipeline hsc_env topSRT prog =
  do let dflags = hsc_dflags hsc_env

     showPass dflags "CPSZ"

     tops <- {-# SCC "tops" #-} mapM (cpsTop hsc_env) prog

     (topSRT, cmms) <- {-# SCC "toTops" #-} doSRTs topSRT tops
     dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (ppr cmms)

     return (topSRT, cmms)



cpsTop :: HscEnv -> CmmDecl -> IO (CAFEnv, [CmmDecl])
cpsTop _ p@(CmmData {}) = return (mapEmpty, [p])
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

       g <- if optLevel dflags >= 99
               then do g <- {-# SCC "sink" #-} return (cmmSink g)
                       dump Opt_D_dump_cmmz_rewrite "Sink assignments" g
                       g <- {-# SCC "inline" #-} return (cmmPeepholeInline g)
                       dump Opt_D_dump_cmmz_rewrite "Peephole inline" g
                       return g
               else return g

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

       ------------- CAF analysis ------------------------------
       let cafEnv = {-# SCC "cafAnal" #-} cafAnal g

       ------------- Populate info tables with stack info ------
       gs <- {-# SCC "setInfoTableStackMap" #-}
             return $ map (setInfoTableStackMap stackmaps) gs
       dumps Opt_D_dump_cmmz_info "after setInfoTableStackMap" gs

       ----------- Control-flow optimisations -----------------
       gs <- {-# SCC "cmmCfgOpts(2)" #-} return $ map cmmCfgOptsProc gs
       dumps Opt_D_dump_cmmz_cfg "Post control-flow optimsations" gs

       return (cafEnv, gs)

  where dflags = hsc_dflags hsc_env
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


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
       ----------- Control-flow optimisations ----------------------------------
       g <- {-# SCC "cmmCfgOpts(1)" #-}
            return $ cmmCfgOpts splitting_proc_points g
       dump Opt_D_dump_cmmz_cfg "Post control-flow optimsations" g

       ----------- Eliminate common blocks -------------------------------------
       g <- {-# SCC "elimCommonBlocks" #-}
            condPass Opt_CmmElimCommonBlocks elimCommonBlocks g
                     Opt_D_dump_cmmz_cbe "Post common block elimination"

       -- Any work storing block Labels must be performed _after_
       -- elimCommonBlocks

       ----------- Proc points -------------------------------------------------
       let call_pps = {-# SCC "callProcPoints" #-} callProcPoints g
       proc_points <-
          if splitting_proc_points
             then {-# SCC "minimalProcPointSet" #-} runUniqSM $
                  minimalProcPointSet (targetPlatform dflags) call_pps g
             else
                  return call_pps

       let noncall_pps = proc_points `setDifference` call_pps
       when (not (setNull noncall_pps)) $
         pprTrace "Non-call proc points: " (ppr noncall_pps) $ return ()

       ----------- Sink and inline assignments *before* stack layout -----------
       {-  Maybe enable this later
       g <- {-# SCC "sink1" #-}
            condPass Opt_CmmSink cmmSink g
                     Opt_D_dump_cmmz_rewrite "Sink assignments (1)"
       -}

       ----------- Layout the stack and manifest Sp ----------------------------
       (g, stackmaps) <-
            {-# SCC "layoutStack" #-}
            runUniqSM $ cmmLayoutStack dflags proc_points entry_off g
       dump Opt_D_dump_cmmz_sp "Layout Stack" g

       ----------- Sink and inline assignments *after* stack layout ------------
       g <- {-# SCC "sink2" #-}
            condPass Opt_CmmSink (cmmSink dflags) g
                     Opt_D_dump_cmmz_rewrite "Sink assignments (2)"

       ------------- CAF analysis ----------------------------------------------
       let cafEnv = {-# SCC "cafAnal" #-} cafAnal g

       if splitting_proc_points
          then do
            ------------- Split into separate procedures -----------------------
            pp_map  <- {-# SCC "procPointAnalysis" #-} runUniqSM $
                             procPointAnalysis proc_points g
            dumpWith dflags Opt_D_dump_cmmz_procmap "procpoint map" pp_map
            gs <- {-# SCC "splitAtProcPoints" #-} runUniqSM $
                  splitAtProcPoints l call_pps proc_points pp_map (CmmProc h l g)
            dumps Opt_D_dump_cmmz_split "Post splitting" gs
     
            ------------- Populate info tables with stack info -----------------
            gs <- {-# SCC "setInfoTableStackMap" #-}
                  return $ map (setInfoTableStackMap stackmaps) gs
            dumps Opt_D_dump_cmmz_info "after setInfoTableStackMap" gs
     
            ----------- Control-flow optimisations -----------------------------
            gs <- {-# SCC "cmmCfgOpts(2)" #-}
                  return $ map (cmmCfgOptsProc splitting_proc_points) gs
            dumps Opt_D_dump_cmmz_cfg "Post control-flow optimsations" gs

            return (cafEnv, gs)

          else do
            -- attach info tables to return points
            g <- return $ attachContInfoTables call_pps (CmmProc h l g)

            ------------- Populate info tables with stack info -----------------
            g <- {-# SCC "setInfoTableStackMap" #-}
                  return $ setInfoTableStackMap stackmaps g
            dump' Opt_D_dump_cmmz_info "after setInfoTableStackMap" g
     
            ----------- Control-flow optimisations -----------------------------
            g <- {-# SCC "cmmCfgOpts(2)" #-}
                 return $ cmmCfgOptsProc splitting_proc_points g
            dump' Opt_D_dump_cmmz_cfg "Post control-flow optimsations" g

            return (cafEnv, [g])

  where dflags = hsc_dflags hsc_env
        dump = dumpGraph dflags
        dump' = dumpWith dflags

        dumps flag name
           = mapM_ (dumpWith dflags flag name)

        condPass flag pass g dumpflag dumpname =
            if dopt flag dflags
               then do
                    g <- return $ pass g
                    dump dumpflag dumpname g
                    return g
               else return g


        -- we don't need to split proc points for the NCG, unless
        -- tablesNextToCode is off.  The latter is because we have no
        -- label to put on info tables for basic blocks that are not
        -- the entry point.
        splitting_proc_points = hscTarget dflags /= HscAsm
                             || not (tablesNextToCode dflags)

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


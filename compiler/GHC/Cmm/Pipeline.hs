
module GHC.Cmm.Pipeline (
  cmmPipeline
) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Config
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.CommonBlockElim
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Info.Build
import GHC.Cmm.Lint
import GHC.Cmm.LayoutStack
import GHC.Cmm.Liveness ( cmmLocalLiveness )
import qualified Data.Set as Set
import GHC.Cmm.Utils ( ofBlockMap, cmmIsTagged, cmmIsNotTagged )
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph ( NonLocal(..) )
import GHC.Cmm.BlockId ( newBlockId )
import GHC.Cmm.ProcPoint
import GHC.Cmm.Sink
import GHC.Cmm.Switch.Implement
import GHC.Cmm.ThreadSanitizer

import GHC.Types.Unique.Supply
import GHC.Types.Unique.DSM

import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Misc ( partitionWith )
import GHC.Utils.Panic ( panic )

import GHC.Platform

import Control.Monad
import Data.Maybe ( listToMaybe, mapMaybe )
import GHC.Utils.Monad (mapAccumLM)

-----------------------------------------------------------------------------
-- | Top level driver for C-- pipeline
-----------------------------------------------------------------------------

-- | Converts C-- with an implicit stack and native C-- calls into
-- optimized, CPS converted and native-call-less C--.  The latter
-- C-- can be used to generate assembly.
cmmPipeline
 :: Logger
 -> CmmConfig
 -> ModuleSRTInfo        -- Info about SRTs generated so far
 -> CmmGroup             -- Input C-- with Procedures
 -> DUniqSupply
 -> IO ((ModuleSRTInfo, CmmGroupSRTs), DUniqSupply) -- Output CPS transformed C--

cmmPipeline logger cmm_config srtInfo prog dus0 = do
  let forceRes ((info, group), us) = info `seq` us `seq` foldr seq () group
  let platform = cmmPlatform cmm_config
  withTimingSilent logger (text "Cmm pipeline") forceRes $ do
     (dus1, prog')  <- {-# SCC "tops" #-} mapAccumLM (cpsTop logger platform cmm_config) dus0 prog
     let (procs, data_) = partitionWith id prog'
     (srtInfo, dus, cmms) <- {-# SCC "doSRTs" #-} doSRTs cmm_config srtInfo dus1 procs data_
     dumpWith logger Opt_D_dump_cmm_cps "Post CPS Cmm" FormatCMM (pdoc platform cmms)

     return ((srtInfo, cmms), dus)

-- | The Cmm pipeline for a single 'CmmDecl'. Returns:
--
--   - in the case of a 'CmmProc': 'Left' of the resulting (possibly
--     proc-point-split) 'CmmDecl's and their 'CafEnv'. CAF analysis
--     necessarily happens *before* proc-point splitting, as described in Note
--     [SRTs].
--
--   - in the case of a `CmmData`, the unmodified 'CmmDecl' and a 'CAFSet' containing
cpsTop :: Logger -> Platform -> CmmConfig -> DUniqSupply -> CmmDecl -> IO (DUniqSupply, Either (CAFEnv, [CmmDecl]) (CAFSet, CmmDataDecl))
cpsTop _logger platform _ dus (CmmData section statics) =
  return (dus, Right (cafAnalData platform statics, CmmData section statics))
cpsTop logger platform cfg dus proc =
    do
      ----------- Control-flow optimisations ----------------------------------

      -- The first round of control-flow optimisation speeds up the
      -- later passes by removing lots of empty blocks, so we do it
      -- even when optimisation isn't turned on.
      --
      CmmProc h l v g <- {-# SCC "cmmCfgOpts(1)" #-}
           return $ cmmCfgOptsProc splitting_proc_points proc
      dump Opt_D_dump_cmm_cfg "Post control-flow optimisations (1)" l g

      let !TopInfo {stack_info=StackInfo { arg_space = entry_off
                                         , do_layout = do_layout }} = h

      ----------- Eliminate common blocks -------------------------------------
      g <- {-# SCC "elimCommonBlocks" #-}
           condPass (cmmOptElimCommonBlks cfg) elimCommonBlocks l g
                         Opt_D_dump_cmm_cbe "Post common block elimination"

      -- Any work storing block Labels must be performed _after_
      -- elimCommonBlocks

      ----------- Implement switches ------------------------------------------
      (g, dus) <- if cmmDoCmmSwitchPlans cfg
             then {-# SCC "createSwitchPlans" #-}
                  pure $ runUniqueDSM dus $ cmmImplementSwitchPlans platform g
             else pure (g, dus)
      dump Opt_D_dump_cmm_switch "Post switch plan" l g

      ----------- ThreadSanitizer instrumentation -----------------------------
      g <- {-# SCC "annotateTSAN" #-}
          if cmmOptThreadSanitizer cfg
          then do
             -- TODO(#25273): Use the deterministic UniqDSM (ie `runUniqueDSM`) instead
             -- of UniqSM (see `initUs_`) to guarantee deterministic objects
             -- when doing thread sanitization.
            us <- mkSplitUniqSupply TsanTag
            return $ initUs_ us $
              annotateTSAN platform g
          else return g
      dump Opt_D_dump_cmm_thread_sanitizer "ThreadSanitizer instrumentation" l g

      ----------- Frames before tag tests (probe) -----------------------------
      g <- {-# SCC "frameBeforeTagTest" #-}
           return $ if do_layout && (mwHas "c2s" || mwHas "c2a" || chainMin > 0 || lvMax > 0 || mwHas "lo" || mwHas "rall")
                      then frameBeforeTagTest platform
                             (cit_lbl <$> mapLookup (g_entry g) (info_tbls h)) g
                      else g

      ----------- Copies of chained tag tests (probe) -------------------------
      ((g, after), dus) <- pure $
         if do_layout && gdupMax > 0
           then runUniqueDSM dus (dupRegions platform
                                   (cit_lbl <$> mapLookup (g_entry g) (info_tbls h))
                                   gdupMax g)
         else if do_layout && mwHas "dup"
           then runUniqueDSM dus (dupChainTests platform g)
           else ((g, mapEmpty), dus)

      ----------- Proc points -------------------------------------------------
      let
        call_pps :: ProcPointSet -- LabelMap
        call_pps = {-# SCC "callProcPoints" #-} callProcPoints g
      (proc_points, dus) <-
         if splitting_proc_points
            then do
              let (pp, dus') = {-# SCC "minimalProcPointSet" #-} runUniqueDSM dus $
                    minimalProcPointSet platform call_pps g
              dumpWith logger Opt_D_dump_cmm_proc "Proc points"
                    FormatCMM (pdoc platform l $$ ppr pp $$ pdoc platform g)
              return (pp, dus')
            else
              return (call_pps, dus)

      ----------- Layout the stack and manifest Sp ----------------------------
      -- the procedure's own info label, which its recursive calls target
      let self_lbl = cit_lbl <$> mapLookup (g_entry g) (info_tbls h)
      ((g, stackmaps), dus) <- pure $
         {-# SCC "layoutStack" #-}
         if do_layout
            then runUniqueDSM dus $ cmmLayoutStack cfg self_lbl proc_points entry_off after g
            else ((g, mapEmpty), dus)
      dump Opt_D_dump_cmm_sp "Layout Stack" l g

      ----------- Sink and inline assignments  --------------------------------
      g <- {-# SCC "sink" #-} -- See Note [Sinking after stack layout]
           condPass (cmmOptSink cfg) (cmmSink platform) l g
                    Opt_D_dump_cmm_sink "Sink assignments"

      ------------- CAF analysis ----------------------------------------------
      let cafEnv = {-# SCC "cafAnal" #-} cafAnal platform call_pps l g
      dumpWith logger Opt_D_dump_cmm_caf "CAFEnv" FormatText (pdoc platform cafEnv)

      (g, dus) <- if splitting_proc_points
           then do
             ------------- Split into separate procedures -----------------------
             let pp_map = {-# SCC "procPointAnalysis" #-}
                          procPointAnalysis proc_points g
             dumpWith logger Opt_D_dump_cmm_procmap "procpoint map"
                FormatCMM (ppr pp_map)
             (g, dus) <- {-# SCC "splitAtProcPoints" #-} pure $ runUniqueDSM dus $
                  splitAtProcPoints platform l call_pps proc_points pp_map
                                    (CmmProc h l v g)
             dumps Opt_D_dump_cmm_split "Post splitting" g
             return (g, dus)
           else
             -- attach info tables to return points
             return ([attachContInfoTables call_pps (CmmProc h l v g)], dus)

      ------------- Populate info tables with stack info -----------------
      g <- {-# SCC "setInfoTableStackMap" #-}
           return $ map (setInfoTableStackMap platform stackmaps) g
      dumps Opt_D_dump_cmm_info "after setInfoTableStackMap" g

      ----------- Control-flow optimisations -----------------------------
      g <- {-# SCC "cmmCfgOpts(2)" #-}
           return $ if cmmOptControlFlow cfg
                    then map (cmmCfgOptsProc splitting_proc_points) g
                    else g
      g <- return $ map (removeUnreachableBlocksProc platform) g
           -- See Note [unreachable blocks]
      dumps Opt_D_dump_cmm_cfg "Post control-flow optimisations (2)" g

      return (dus, Left (cafEnv, g))

  where dump = dumpGraph logger platform (cmmDoLinting cfg)

        dumps flag name
           = mapM_ (dumpWith logger flag name FormatCMM . pdoc platform)

        condPass do_opt pass lbl g dumpflag dumpname =
            if do_opt
               then do
                    g <- return $ pass g
                    dump dumpflag dumpname lbl g
                    return g
               else return g

        -- we don't need to split proc points for the NCG, unless
        -- tablesNextToCode is off.  The latter is because we have no
        -- label to put on info tables for basic blocks that are not
        -- the entry point.
        splitting_proc_points = cmmSplitProcPoints cfg

-- Note [Sinking after stack layout]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In the past we considered running sinking pass also before stack
-- layout, but after making some measurements we realized that:
--
--   a) running sinking only before stack layout produces slower
--      code than running sinking only before stack layout
--
--   b) running sinking both before and after stack layout produces
--      code that has the same performance as when running sinking
--      only after stack layout.
--
-- In other words sinking before stack layout doesn't buy as anything.
--
-- An interesting question is "why is it better to run sinking after
-- stack layout"? It seems that the major reason are stores and loads
-- generated by stack layout. Consider this code before stack layout:
--
--  c1E:
--      _c1C::P64 = R3;
--      _c1B::P64 = R2;
--      _c1A::P64 = R1;
--      I64[(young<c1D> + 8)] = c1D;
--      call stg_gc_noregs() returns to c1D, args: 8, res: 8, upd: 8;
--  c1D:
--      R3 = _c1C::P64;
--      R2 = _c1B::P64;
--      R1 = _c1A::P64;
--      call (P64[(old + 8)])(R3, R2, R1) args: 8, res: 0, upd: 8;
--
-- Stack layout pass will save all local variables live across a call
-- (_c1C, _c1B and _c1A in this example) on the stack just before
-- making a call and reload them from the stack after returning from a
-- call:
--
--  c1E:
--      _c1C::P64 = R3;
--      _c1B::P64 = R2;
--      _c1A::P64 = R1;
--      I64[Sp - 32] = c1D;
--      P64[Sp - 24] = _c1A::P64;
--      P64[Sp - 16] = _c1B::P64;
--      P64[Sp - 8] = _c1C::P64;
--      Sp = Sp - 32;
--      call stg_gc_noregs() returns to c1D, args: 8, res: 8, upd: 8;
--  c1D:
--      _c1A::P64 = P64[Sp + 8];
--      _c1B::P64 = P64[Sp + 16];
--      _c1C::P64 = P64[Sp + 24];
--      R3 = _c1C::P64;
--      R2 = _c1B::P64;
--      R1 = _c1A::P64;
--      Sp = Sp + 32;
--      call (P64[Sp])(R3, R2, R1) args: 8, res: 0, upd: 8;
--
-- If we don't run sinking pass after stack layout we are basically
-- left with such code. However, running sinking on this code can lead
-- to significant improvements:
--
--  c1E:
--      I64[Sp - 32] = c1D;
--      P64[Sp - 24] = R1;
--      P64[Sp - 16] = R2;
--      P64[Sp - 8] = R3;
--      Sp = Sp - 32;
--      call stg_gc_noregs() returns to c1D, args: 8, res: 8, upd: 8;
--  c1D:
--      R3 = P64[Sp + 24];
--      R2 = P64[Sp + 16];
--      R1 = P64[Sp + 8];
--      Sp = Sp + 32;
--      call (P64[Sp])(R3, R2, R1) args: 8, res: 0, upd: 8;
--
-- Now we only have 9 assignments instead of 15.
--
-- There is one case when running sinking before stack layout could
-- be beneficial. Consider this:
--
--   L1:
--      x = y
--      call f() returns L2
--   L2: ...x...y...
--
-- Since both x and y are live across a call to f, they will be stored
-- on the stack during stack layout and restored after the call:
--
--   L1:
--      x = y
--      P64[Sp - 24] = L2
--      P64[Sp - 16] = x
--      P64[Sp - 8]  = y
--      Sp = Sp - 24
--      call f() returns L2
--   L2:
--      y = P64[Sp + 16]
--      x = P64[Sp + 8]
--      Sp = Sp + 24
--      ...x...y...
--
-- However, if we run sinking before stack layout we would propagate x
-- to its usage place (both x and y must be local register for this to
-- be possible - global registers cannot be floated past a call):
--
--   L1:
--      x = y
--      call f() returns L2
--   L2: ...y...y...
--
-- Thus making x dead at the call to f(). If we ran stack layout now
-- we would generate less stores and loads:
--
--   L1:
--      x = y
--      P64[Sp - 16] = L2
--      P64[Sp - 8]  = y
--      Sp = Sp - 16
--      call f() returns L2
--   L2:
--      y = P64[Sp + 8]
--      Sp = Sp + 16
--      ...y...y...
--
-- But since we don't see any benefits from running sinking before stack
-- layout, this situation probably doesn't arise too often in practice.
--

{- Note [inconsistent-pic-reg]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
On x86/Darwin, PIC is implemented by inserting a sequence like

    call 1f
 1: popl %reg

at the proc entry point, and then referring to labels as offsets from
%reg.  If we don't split proc points, then we could have many entry
points in a proc that would need this sequence, and each entry point
would then get a different value for %reg.  If there are any join
points, then at the join point we don't have a consistent value for
%reg, so we don't know how to refer to labels.

Hence, on x86/Darwin, we have to split proc points, and then each proc
point will get its own PIC initialisation sequence.

This isn't an issue on x86/ELF, where the sequence is

    call 1f
 1: popl %reg
    addl $_GLOBAL_OFFSET_TABLE_+(.-1b), %reg

so %reg always has a consistent value: the address of
_GLOBAL_OFFSET_TABLE_, regardless of which entry point we arrived via.

-}

{- Note [unreachable blocks]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
The control-flow optimiser sometimes leaves unreachable blocks behind
containing junk code.  These aren't necessarily a problem, but
removing them is good because it might save time in the native code
generator later.

To make unreachable blocks visible in -ddump-cmm-* output, add -dppr-debug.
-}

dumpGraph :: Logger -> Platform -> Bool -> DumpFlag -> String -> CLabel -> CmmGraph -> IO ()
dumpGraph logger platform do_linting flag name lbl g = do
  when do_linting $ do_lint g
  dumpWith logger flag name FormatCMM (pdoc platform lbl $$ pdoc platform g)
 where
  do_lint g = case cmmLintGraph platform g of
                 Just err -> do { fatalErrorMsg logger err
                                ; ghcExit logger 1
                                }
                 Nothing  -> return ()

dumpWith :: Logger -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
dumpWith logger flag txt fmt sdoc = do
  putDumpFileMaybe logger flag txt fmt sdoc
  when (not (logHasDumpFlag logger flag)) $
    -- If `-ddump-cmm-verbose -ddump-to-file` is specified,
    -- dump each Cmm pipeline stage output to a separate file.  #16930
    when (logHasDumpFlag logger Opt_D_dump_cmm_verbose)
      $ logDumpFile logger (mkDumpStyle alwaysQualify) flag txt fmt sdoc
  putDumpFileMaybe logger Opt_D_dump_cmm_verbose_by_proc txt fmt sdoc

{- Note [Frame before the tag test] (probe)
emitEnter's sequence (MW) sets up the frame on the entering path only:

       res = fun; if (res & 7 != 0) goto lcont; else goto lcall;
     lcall: R1 = fun; call [R1] returns to lret;
     lret:  res = R1; goto lcont;

frameBeforeTagTest gives a sequence back the old form, the frame set up
before the test (Note [diamond proc point] in GHC.Cmm.LayoutStack), where
another call returning into the procedure follows lcont: on some path from it
(GHC_MW_VARIANT c2s) or on every path (c2a), following no edge back in
reverse postorder and counting no call of a canned GC entry point.  That
call's frame then finds the values the first one saved where they are.
-}

frameBeforeTagTest :: Platform -> Maybe CLabel -> CmmGraph -> CmmGraph
frameBeforeTagTest platform self_lbl g
  | null sites = g
  | otherwise = ofBlockMap (g_entry g) (foldl' rewrite blocks sites)
  where
    blocks = toBlockMap g
    order = revPostorder g
    rpo = mapFromList (zip (map entryLabel order) [0 :: Int ..])
    all_sites = mwSites platform g
    sites = [ (ltest, lcall, lret, r1, tagged, lcont)
            | (ltest, lcall, lret, lcont, r1, tagged) <- all_sites
            , mwHas "rall"
              || (chainMin > 0 && mapFindWithDefault 0 ltest chains >= chainMin)
              || (lvMax > 0 && Set.size (mapFindWithDefault Set.empty lcont live) > lvMax)
              || ((mwHas "c2s" || mwHas "c2a") && mapFindWithDefault False lcont reach)
              || (mwHas "lo" && not (reaches lcont ltest)
                  && not (mwHas "lor" && selfRec
                          && (not (mwHas "lr8") || Set.size (mapFindWithDefault Set.empty lcont live) <= 8))
                  && not (loMax > 0 && Set.size (mapFindWithDefault Set.empty lcont live) <= loMax
                          && smallProc)) ]
    -- (probe, lsN, lnN) loK only in a procedure of at most N sequences, or
    -- of at most N middle nodes
    smallProc = (loSites == 0 || length all_sites <= loSites)
                && (loNodes == 0 || procNodes <= loNodes)
    procNodes = sum [ length (blockToList mid) | b <- mapElems blocks
                                               , let (_, mid, _) = blockSplit b ]
    -- a non-tail call of the procedure itself (lor)
    selfRec = or [ Just lbl == self_lbl
                 | b <- mapElems blocks
                 , CmmCall { cml_target = CmmLit (CmmLabel lbl), cml_cont = Just _ } <- [lastNode b] ]
    live = cmmLocalLiveness platform g
    allTests = [ lt | (lt, _, _, _, _, _) <- all_sites ]
    predCount = mapFromListWith (+) [ (sc, 1 :: Int) | b <- mapElems blocks
                                                     , sc <- successors b ]
    -- whether the test is on a cycle through the join (lo)
    reaches from to = go setEmpty [from]
      where go _ [] = False
            go seen (x : xs)
              | x == to = True
              | x `setMember` seen = go seen xs
              | otherwise = go (setInsert x seen)
                               (maybe [] successors (mapLookup x blocks) ++ xs)
    -- the length of the chain of sequences each test is in, a sequence's
    -- continuation being the next one's test
    next = mapFromList [ (lt, lc) | (lt, _, _, lc, _, _) <- all_sites
                                  , lc `elem` tests ]
    tests = [ lt | (lt, _, _, _, _, _) <- all_sites ]
    prevs = mapFromList [ (lc, lt) | (lt, lc) <- mapToList next ]
    -- the sequences after (or before) one, at most 64, and whether they
    -- lead back to it
    walk :: LabelMap Label -> Label -> Int -> Label -> (Int, Bool)
    walk m start fuel l = case mapLookup l m of
      Just l' | l' == start -> (0, True)
              | fuel > 0 -> let (n, c) = walk m start (fuel - 1) l' in (n + 1, c)
      _ -> (0, False)
    chains = mapFromList [ (lt, chainLen lt) | lt <- tests ]
    chainLen lt = case walk next lt 64 lt of
      (n, True) -> n + 1
      (n, False) -> 1 + n + fst (walk prevs lt 64 lt)
    isCall b = case lastNode b of
      CmmCall { cml_cont = Just _, cml_target = t } -> not (isGcTarget t)
      CmmForeignCall {} -> True
      _ -> False
    -- whether a call follows each block, computed in postorder along
    -- forward edges
    reach :: LabelMap Bool
    reach = foldr step mapEmpty order
      where
        step b acc = mapInsert (entryLabel b) v acc
          where
            i = mapFindWithDefault 0 (entryLabel b) rpo
            fwd = [ mapFindWithDefault False s acc
                  | s <- successors b, mapFindWithDefault 0 s rpo > i ]
            back = length fwd < length (successors b)
            v | isCall b = True
              | mwHas "c2a" = not (null fwd) && not back && and fwd
              | otherwise = or fwd
    tagTest True = cmmIsTagged platform
    tagTest False = cmmIsNotTagged platform
    rewrite m (ltest, lcall, lret, r1, tagged, lcont) = case mapLookup lcall m of
      Just bcall ->
        let (cfirst, copyout, clast) = blockSplit bcall
        in merge $ mapInsert lcall (blockJoin cfirst emptyBlock clast)
             (mapAdjust (test copyout) ltest m)
      Nothing -> m
      where
        -- (probe, mj) the join, now reached only from lret, merged into it
        merge m'
          | mwHas "mj"
          , not (lcont `elem` allTests)
          , mapFindWithDefault 0 lcont predCount == (2 :: Int)
          , Just bret <- mapLookup lret m'
          , (fret, midr, CmmBranch l) <- blockSplit bret
          , l == lcont
          , Just bcont <- mapLookup lcont m'
          , (_, midc, lastc) <- blockSplit bcont
          = mapDelete lcont (mapInsert lret (blockJoin fret (midr `blockAppend` midc) lastc) m')
          | otherwise = m'
        test :: Block CmmNode O O -> CmmBlock -> CmmBlock
        test copyout b = case blockSplit b of
          (first, mid, CmmCondBranch _ t f l) ->
            blockJoin first (mid `blockAppend` copyout)
              (if tagged then CmmCondBranch (tagTest True (CmmReg r1)) lret f l
                         else CmmCondBranch (tagTest False (CmmReg r1)) t lret l)
          _ -> b

-- | The most local registers live into a sequence's join that keep MW's
-- form (GHC_MW_VARIANT lvN; 0, no bound, where absent).
lvMax :: Int
lvMax = case [ n | (w, n) <- [("lv4",4),("lv6",6),("lv8",8),("lv10",10),("lv12",12),("lv16",16),("lv24",24)], mwHas w ] of
  (n : _) -> n
  [] -> 0

-- | Outside loops, the most local registers live into a sequence's join that
-- keep MW's form under lo (GHC_MW_VARIANT loK; 0 where absent).
loMax :: Int
loMax = case [ n | (w, n) <- [("lo1",1),("lo2",2),("lo3",3),("lo4",4),("lo6",6)], mwHas w ] of
  (n : _) -> n
  [] -> 0

-- | Under loK, the most sequences (lsN) or middle nodes (lnN) a procedure
-- may have for loK to keep MW's form (0, no bound, where absent).
loSites, loNodes :: Int
loSites = case [ n | (w, n) <- [("ls1",1),("ls2",2),("ls3",3),("ls4",4),("ls6",6),("ls8",8),("ls12",12)], mwHas w ] of
  (n : _) -> n
  [] -> 0
loNodes = case [ n | (w, n) <- [("ln16",16),("ln20",20),("ln24",24),("ln28",28),("ln32",32),("ln40",40),("ln48",48),("ln64",64),("ln128",128),("ln256",256)], mwHas w ] of
  (n : _) -> n
  [] -> 0

-- | The most middle nodes dupRegions copies at a sequence off every cycle
-- (GHC_MW_VARIANT goN; 0, gN's bound, where absent).
goMax :: Int
goMax = case [ n | (w, n) <- [("go1",1),("go2",2),("go3",3),("go4",4),("go6",6)], mwHas w ] of
  (n : _) -> n
  [] -> 0

-- | The shortest chain of emitEnter's sequences given the old form
-- (GHC_MW_VARIANT chK, K > 0; 0 where absent).
chainMin :: Int
chainMin = case [ read k | ('c':'h':k@(_:_)) <- mwWords, all (`elem` "0123456789") k ] of
  (k : _) -> k
  [] -> 0
  where mwWords = [ w | w <- ["ch2","ch3","ch4","ch6","ch8","ch12","ch16","ch32"], mwHas w ]

-- | The most middle nodes of a test block dupChainTests copies
-- (GHC_MW_VARIANT dsN; 0, no bound, where absent).
dupMax :: Int
dupMax = case [ n | (w, n) <- [("ds4",4),("ds8",8),("ds12",12),("ds16",16),("ds24",24),("ds32",32)], mwHas w ] of
  (n : _) -> n
  [] -> 0

-- | The most middle nodes dupRegions copies for one sequence
-- (GHC_MW_VARIANT gN; 0, no copies, where absent).
gdupMax :: Int
gdupMax = case [ n | (w, n) <- [("g2",2),("g4",4),("g6",6),("g8",8),("g16",16),("g32",32),("g64",64),("g128",128)], mwHas w ] of
  (n : _) -> n
  [] -> 0

{- Note [Copies of a continuation's region] (probe)
dupRegions generalises dupChainTests: for each of emitEnter's sequences,
the return path lret gets its own copy of the region after the join lcont,
the blocks reached from lcont along forward edges and from nothing outside
the region, up to blocks ending in a call (copied, returning to the same
continuation), at most gN middle nodes in all.  In the copy the call's
frame stays, so its values are reloaded only where the region uses them, and
a call in it finds them in their slots.  Order constraints: a copy after its
original, so that a block outside the region takes the evaluated path's
stack; a call's original after its copy, so that the call's continuation
takes the frame laid out from the first one.
-}

dupRegions :: Platform -> Maybe CLabel -> Int -> CmmGraph
           -> UniqDSM (CmmGraph, LabelMap Label)
dupRegions platform self_lbl maxNodes g = do
    (m, after, _) <- foldM one (blocks0, mapEmpty, setEmpty) (mwSites platform g)
    return (ofBlockMap (g_entry g) m, after)
  where
    blocks0 = toBlockMap g
    order = map entryLabel (revPostorder g)
    isCallEnd b = case lastNode b of
      CmmCall { cml_cont = Just _ } -> True
      CmmForeignCall {} -> True
      _ -> False
    -- predecessors along edges other than a call's to its continuation
    predsNC = mapFromListWith (++)
      [ (s, [entryLabel b]) | b <- mapElems blocks0, not (isCallEnd b)
                            , s <- successors b ]
    predsC = setFromList [ s | b <- mapElems blocks0, isCallEnd b
                             , s <- successors b ]
    size b = length (blockToList (case blockSplit b of (_, mid, _) -> mid))
    region maxN lcont = go (setSingleton lcont) [lcont]
                      (maybe 0 size (mapLookup lcont blocks0))
                      (drop 1 (dropWhile (/= lcont) order))
      where
        go r acc _ [] = (r, acc)
        go r acc n (l : ls)
          | not (setMember l predsC)
          , ps@(_ : _) <- mapFindWithDefault [] l predsNC
          , all (`setMember` r) ps
          , all (\p -> maybe True (not . isCallEnd) (mapLookup p blocks0)) ps
          , Just b <- mapLookup l blocks0
          , n + size b <= maxN
          = go (setInsert l r) (l : acc) (n + size b) ls
          | otherwise = go r acc n ls
    -- (probe, lgn) no copy at a sequence off every cycle in a procedure
    -- making no non-tail call to itself: under lo, one loK kept
    selfRec = or [ Just lbl == self_lbl
                 | b <- mapElems blocks0
                 , CmmCall { cml_target = CmmLit (CmmLabel lbl), cml_cont = Just _ } <- [lastNode b] ]
    reaches from to = go setEmpty [from]
      where go _ [] = False
            go seen (x : xs)
              | x == to = True
              | x `setMember` seen = go seen xs
              | otherwise = go (setInsert x seen)
                               (maybe [] successors (mapLookup x blocks0) ++ xs)
    one (m, after, used) (ltest, _, lret, lcont, _, _)
      | not (mwHas "lgn" && not selfRec && not (reaches lcont ltest))
      -- (probe, goN) copies of at most N nodes at a sequence off every
      -- cycle in a procedure making no non-tail call to itself
      , let offCycle = not selfRec && not (reaches lcont ltest)
            maxN | goMax > 0 && offCycle = goMax
                 | otherwise = maxNodes
      , (_, ls) <- region maxN lcont
      , not (any (`setMember` used) ls)
      , not (mwHas "gc") || any (maybe False isCallEnd . (`mapLookup` m)) ls
      , Just bret <- mapLookup lret m
      , (fret, midr, CmmBranch _) <- blockSplit bret
      = do
          news <- mapM (const newBlockId) ls
          let ren = mapFromList (zip ls news)
              sw l = mapFindWithDefault l l ren
              copy l l' = case mapLookup l m of
                Just b | (CmmEntry _ sc, mid, lst) <- blockSplit b
                  -> blockJoin (CmmEntry l' sc) mid
                       (if isCallEnd b then lst else mapSuccessors sw lst)
                _ -> panic "dupRegions"
              m' = foldl' (\acc (l, l') -> mapInsert l' (copy l l') acc)
                     (mapInsert lret (blockJoin fret midr (CmmBranch (sw lcont))) m)
                     (zip ls news)
              after' = foldl' (\acc (l, l') ->
                         case mapLookup l m of
                           Just b | isCallEnd b -> mapInsert l l' acc
                           _ -> mapInsert l' l acc) after (zip ls news)
          return (m', after', foldl' (flip setInsert) used ls)
      | otherwise = return (m, after, used)

-- | emitEnter's sequences (MW): the test's block, lcall, lret, lcont, R1 and
-- whether the test is for a tag.
mwSites :: Platform -> CmmGraph -> [(Label, Label, Label, Label, CmmReg, Bool)]
mwSites platform g = mapMaybe sequenceOf (mapElems blocks)
  where
    blocks = toBlockMap g
    preds = mapFromListWith (+) [ (s, 1 :: Int) | b <- mapElems blocks
                                               , s <- successors b ]
    sequenceOf :: CmmBlock -> Maybe (Label, Label, Label, Label, CmmReg, Bool)
    sequenceOf b = case lastNode b of
      CmmCondBranch cond t f _ ->
        listToMaybe (mapMaybe (sequenceAt b cond) [(f, t, True), (t, f, False)])
      _ -> Nothing
    sequenceAt :: CmmBlock -> CmmExpr -> (Label, Label, Bool)
               -> Maybe (Label, Label, Label, Label, CmmReg, Bool)
    sequenceAt b cond (lcall, lcont, tagged)
      | Just bcall <- mapLookup lcall blocks
      , mapFindWithDefault 0 lcall preds == 1
      , (_, copyout, CmmCall { cml_cont = Just lret }) <- blockSplit bcall
      , all (movable lret) (blockToList copyout)
      , Just bret <- mapLookup lret blocks
      , (_, mid, CmmBranch lcont') <- blockSplit bret
      , lcont' == lcont
      , [CmmAssign (CmmLocal res) (CmmReg r1@(CmmGlobal _))] <- blockToList mid
      , cond == tagTest tagged (CmmReg (CmmLocal res))
      = Just (entryLabel b, lcall, lret, lcont, r1, tagged)
      | otherwise = Nothing
    -- the copy-out: the return address and the arguments' registers
    movable :: Label -> CmmNode O O -> Bool
    movable lret (CmmStore (CmmStackSlot (Young l) _) _ _) = l == lret
    movable _ (CmmAssign (CmmGlobal _) _) = True
    movable _ _ = False
    tagTest True = cmmIsTagged platform
    tagTest False = cmmIsNotTagged platform

{- Note [Copies of chained tag tests] (probe)
Where an evaluation's continuation lcont1 is itself the tag test of
the next evaluation (a chain, as in a strict constructor's wrapper),
MW's return path from the first call pops its frame at the join and
the second call stores the values again.  dupChainTests gives the return
path lret1 its own copy of the second test, whose call block is a copy of
the second call's block: on that path the first frame stays and the second
call's frame (laid out from it) stores only what is new, while the path on
which both values arrive evaluated stays frameless.  It returns the order
constraints stack layout needs for that: the copy of the test after the
test (so that the second join takes the frameless path's stack), the
original call after the copied one (so that the second frame is laid out
from the first).  GHC_MW_VARIANT dup.
-}

dupChainTests :: Platform -> CmmGraph -> UniqDSM (CmmGraph, LabelMap Label)
dupChainTests platform g
  | null pairs = return (g, mapEmpty)
  | otherwise = do
      (m, after) <- foldM copy (blocks, mapEmpty) pairs
      return (ofBlockMap (g_entry g) m, after)
  where
    blocks = toBlockMap g
    sites = mwSites platform g
    tests = mapFromList [ (ltest, s) | s@(ltest, _, _, _, _, _) <- sites ]
    pairs = [ (lret1, lcont1, lcall2)
            | (ltest1, _, lret1, lcont1, _, _) <- sites
            , Just (ltest2, lcall2, _, _, _, _) <- [mapLookup lcont1 tests]
            , ltest2 /= ltest1 ]
    copy (m, after) (lret1, ltest2, lcall2)
      | Just b2 <- mapLookup ltest2 m
      , Just bcall2 <- mapLookup lcall2 m
      , Just bret1 <- mapLookup lret1 m
      , (CmmEntry _ s2, mid2, last2@CmmCondBranch{}) <- blockSplit b2
      , dupMax == 0 || length (blockToList mid2) <= dupMax
      , (CmmEntry _ sc, midc, lastc) <- blockSplit bcall2
      , (fret, midr, CmmBranch _) <- blockSplit bret1
      = do
          l2' <- newBlockId
          lcall2' <- newBlockId
          let sw l = if l == lcall2 then lcall2' else l
              last2' = case last2 of
                CmmCondBranch c t f lk -> CmmCondBranch c (sw t) (sw f) lk
              m' = mapInsert l2' (blockJoin (CmmEntry l2' s2) mid2 last2')
                 $ mapInsert lcall2' (blockJoin (CmmEntry lcall2' sc) midc lastc)
                 $ mapInsert lret1 (blockJoin fret midr (CmmBranch l2')) m
          return (m', mapInsert l2' ltest2 (mapInsert lcall2 lcall2' after))
      | otherwise = return (m, after)

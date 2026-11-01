{-# LANGUAGE RecordWildCards #-}
module GHC.Cmm.LayoutStack (
       cmmLayoutStack, LiftFrames(..), setInfoTableStackMap
  ) where

import GHC.Prelude hiding ((<*>))

import GHC.Platform
import GHC.Platform.Profile

import GHC.StgToCmm.Monad      ( newTemp ) -- XXX layering violation
import GHC.StgToCmm.Utils      ( callerSaveVolatileRegs  ) -- XXX layering violation
import GHC.StgToCmm.Foreign    ( saveThreadState, loadThreadState ) -- XXX layering violation

import GHC.Cmm
import GHC.Cmm.Info
import GHC.Cmm.BlockId
import GHC.Cmm.Config
import GHC.Cmm.CLabel ( CLabel )
import GHC.Cmm.Utils
import GHC.Cmm.Graph
import GHC.Cmm.Liveness
import GHC.Cmm.ProcPoint
import GHC.Cmm.ContFlowOpt ( callContinuation_maybe )
import GHC.Runtime.Heap.Layout
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Data.Maybe
import GHC.Data.Graph.Directed ( Node(..), SCC(..), flattenSCC
                               , stronglyConnCompFromEdgedVerticesUniq )
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set ( differenceUniqueSet, filterUniqueSet
                            , sizeUniqueSet )
import GHC.Cmm.LRegSet
import GHC.Types.Unique.DSM
import GHC.Utils.Misc

import GHC.Utils.Outputable hiding ( isEmpty )
import GHC.Utils.Panic
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Fix
import Data.Array as Array
import Data.Array.ST ( STUArray, newArray, readArray, writeArray )
import Control.Monad ( when )
import Control.Monad.ST ( ST, runST )
import Data.STRef ( newSTRef, readSTRef, writeSTRef, modifySTRef' )
import Data.List (nub, partition, sortOn)
import Data.List.NonEmpty ( NonEmpty (..) )

{- Note [Stack Layout]
   ~~~~~~~~~~~~~~~~~~~
The job of this pass is to

 - replace references to abstract stack Areas with fixed offsets from Sp.

 - replace the CmmHighStackMark constant used in the stack check with
   the maximum stack usage of the proc.

 - save any variables that are live across a call, and reload them as
   necessary.

Before stack allocation, local variables remain live across native
calls (CmmCall{ cmm_cont = Just _ }), and after stack allocation local
variables are clobbered by native calls.

We want to do stack allocation so that as far as possible
 - stack use is minimized, and
 - unnecessary stack saves and loads are avoided.

The algorithm we use is a variant of linear-scan register allocation,
where the stack is our register file.

We proceed in two passes, see Note [Two pass approach] for why they are not easy
to merge into one.

Pass 1:

 - First, we do a liveness analysis, which annotates every block with
   the variables live on entry to the block.

 - We traverse blocks in reverse postorder DFS; that is, we visit at
   least one predecessor of a block before the block itself.  The
   stack layout flowing from the predecessor of the block will
   determine the stack layout on entry to the block.

 - We maintain a data structure

     Map Label StackMap

   which describes the contents of the stack and the stack pointer on
   entry to each block that is a successor of a block that we have
   visited.

 - For each block we visit:

    - Look up the StackMap for this block.

    - If this block is a proc point (or a call continuation, if we aren't
      splitting proc points), we need to reload all the live variables from the
      stack - but this is done in Pass 2, which calculates more precise liveness
      information (see description of Pass 2).

    - Walk forwards through the instructions:
      - At an assignment  x = Sp[loc]
        - Record the fact that Sp[loc] contains x, so that we won't
          need to save x if it ever needs to be spilled.
      - At an assignment  x = E
        - If x was previously on the stack, it isn't any more
      - At the last node, if it is a call or a jump to a proc point
        - Lay out the stack frame for the call (see setupStackFrame)
        - emit instructions to save all the live variables
        - Remember the StackMaps for all the successors
        - emit an instruction to adjust Sp
      - If the last node is a branch, then the current StackMap is the
        StackMap for the successors.

    - Manifest Sp: replace references to stack areas in this block
      with real Sp offsets. We cannot do this until we have laid out
      the stack area for the successors above.

      In this phase we also eliminate redundant stores to the stack;
      see elimStackStores.

  - There is one important gotcha: sometimes we'll encounter a control
    transfer to a block that we've already processed (a join point),
    and in that case we might need to rearrange the stack to match
    what the block is expecting. (exactly the same as in linear-scan
    register allocation, except here we have the luxury of an infinite
    supply of temporary variables).

  - Finally, we update the magic CmmHighStackMark constant with the
    stack usage of the function, and eliminate the whole stack check
    if there was no stack use. (in fact this is done as part of the
    main traversal, by feeding the high-water-mark output back in as
    an input. I hate cyclic programming, but it's just too convenient
    sometimes.)

  There are plenty of tricky details: update frames, proc points, return
  addresses, foreign calls, and some ad-hoc optimisations that are
  convenient to do here and effective in common cases.  Comments in the
  code below explain these.

Pass 2:

- Calculate live registers, but taking into account that nothing is live at the
  entry to a proc point.

- At each proc point and call continuation insert reloads of live registers from
  the stack (they were saved by Pass 1).


Note [Two pass approach]
~~~~~~~~~~~~~~~~~~~~~~~~
The main reason for Pass 2 is being able to insert only the reloads that are
needed and the fact that the two passes need different liveness information.
Let's consider an example:

  .....
   \ /
    D   <- proc point
   / \
  E   F
   \ /
    G   <- proc point
    |
    X

Pass 1 needs liveness assuming that local variables are preserved across calls.
This is important because it needs to save any local registers to the stack
(e.g., if register a is used in block X, it must be saved before any native
call).
However, for Pass 2, where we want to reload registers from stack (in a proc
point), this is overly conservative and would lead us to generate reloads in D
for things used in X, even though we're going to generate reloads in G anyway
(since it's also a proc point).
So Pass 2 calculates liveness knowing that nothing is live at the entry to a
proc point. This means that in D we only need to reload things used in E or F.
This can be quite important, for an extreme example see testcase for #3294.

Merging the two passes is not trivial - Pass 2 is a backward rewrite and Pass 1
is a forward one. Furthermore, Pass 1 is creating code that uses local registers
(saving them before a call), which the liveness analysis for Pass 2 must see to
be correct.

-}


-- All stack locations are expressed as positive byte offsets from the
-- "base", which is defined to be the address above the return address
-- on the stack on entry to this CmmProc.
--
-- Lower addresses have higher StackLocs.
--
type StackLoc = ByteOff

{-
 A StackMap describes the stack at any given point.  At a continuation
 it has a particular layout, like this:

         |             | <- base
         |-------------|
         |     ret0    | <- base + 8
         |-------------|
         .  upd frame  . <- base + sm_ret_off
         |-------------|
         |             |
         .    vars     .
         . (live/dead) .
         |             | <- base + sm_sp - sm_args
         |-------------|
         |    ret1     |
         .  ret vals   . <- base + sm_sp    (<--- Sp points here)
         |-------------|

Why do we include the final return address (ret0) in our stack map?  I
have absolutely no idea, but it seems to be done that way consistently
in the rest of the code generator, so I played along here. --SDM

Note that we will be constructing an info table for the continuation
(ret1), which needs to describe the stack down to, but not including,
the update frame (or ret0, if there is no update frame).
-}

data StackMap = StackMap
 {  sm_sp   :: StackLoc
       -- ^ the offset of Sp relative to the base on entry
       -- to this block.
 ,  sm_args :: ByteOff
       -- ^ the number of bytes of arguments in the area for this block
       -- Defn: the offset of young(L) relative to the base is given by
       -- (sm_sp - sm_args) of the StackMap for block L.
 ,  sm_ret_off :: ByteOff
       -- ^ Number of words of stack that we do not describe with an info
       -- table, because it contains an update frame.
 ,  sm_regs :: UniqFM LocalReg (LocalReg,StackLoc)
       -- ^ regs on the stack
 }

instance Outputable StackMap where
  ppr StackMap{..} =
     text "Sp = " <> int sm_sp $$
     text "sm_args = " <> int sm_args $$
     text "sm_ret_off = " <> int sm_ret_off $$
     text "sm_regs = " <> pprUFM sm_regs ppr


cmmLayoutStack :: CmmConfig -> Maybe CLabel -> Set.Set CLabel -> LiftFrames
               -> ProcPointSet
               -> ByteOff -> CmmGraph
               -> UniqDSM (CmmGraph, LabelMap StackMap, LabelMap BlockId)
cmmLayoutStack cfg self_lbl rec_lbls lift_frames procpoints entry_args
               graph@(CmmGraph { g_entry = entry })
  = do
    -- We need liveness info. Dead assignments are removed later
    -- by the sinking pass.
    let liveness = cmmLocalLivenessL platform graph
        blocks   = revPostorder graph
        profile  = cmmProfile   cfg
        platform = profilePlatform profile
        -- the variables live on entry to each proc point, the only labels
        -- liveRegs is given, from their uniques, which ascend as Ord LocalReg
        -- orders them; each is defined in some block, none being live on
        -- entry
        liveRegs l = mapFindWithDefault (pprPanic "liveRegs" (ppr l)) l regSets
        regSets = mapFromList
          [ (l, Set.fromDistinctAscList [ expectJust (lookupUFM_Directly regs u)
                                        | u <- elemsLRegSet live ])
          | (l, live) <- ppLive ]
        ppLive = [ (l, mapFindWithDefault emptyLRegSet l liveness)
                 | l <- setElems procpoints ]
        -- those variables by unique
        regs = foldl' (\m (BlockCC _ mid lst) ->
                         foldNodesBwdOO def mid (def lst m)) emptyUFM blocks
        def :: CmmNode e x -> UniqFM LocalReg LocalReg
            -> UniqFM LocalReg LocalReg
        def n m = foldLocalRegsDefd platform add m n
        add m r | r `elemLRegSet` across = addToUFM m r r
                | otherwise              = m
        across = unionsLRegSet (map snd ppLive)
        -- the blocks by label, and the cold ones
        blockmap = toBlockMap graph
        cold = unlikelyOnlyBlocks blockmap blocks
        -- whether the procedure makes a non-tail call of itself
        selfRec = any (selfCallNode . lastNode) blocks
        -- the blocks on a cycle that makes no non-tail call of the
        -- procedure's own recursion and passes no cold block: their diamonds'
        -- early stores must keep the next call's frame no deeper (Note [Tagged
        -- path skips the stores])
        r2set = setFromList (concat (blockCycles
                  (\x -> not (recCall x) && not (x `setMember` cold)) blocks))
        -- a block ending in a non-tail call of the procedure's own recursion
        recCall x = maybe False (`Set.member` rec_lbls) (calleeOf x)
        -- a non-tail call of the procedure itself, and a block ending in one
        selfCallNode n = maybe False isSelf (nonTailCallee n)
        selfCall x = maybe False isSelf (calleeOf x)
        isSelf lab = Just lab == self_lbl
        -- the label the non-tail call ending a block calls
        calleeOf x = nonTailCallee . lastNode =<< mapLookup x blockmap
        -- the blocks' strongly connected components, for loopHeadSeeds and
        -- reservedSeeds
        sccs = map flattenSCC $ stronglyConnCompFromEdgedVerticesUniq
                 [ DigraphNode l l (successors (lastNode b))
                 | b <- blocks, let l = entryLabel b ]
        run seeds keeps =
          mfix $ \ ~(rec_stackmaps, rec_high_sp, _new_blocks, _props, _dias, _edges) ->
            layout cfg selfRec selfCallNode r2set procpoints liveness entry
                   entry_args blockmap liveRegs cold seeds keeps rec_stackmaps
                   rec_high_sp blocks

    -- See Note [Loop head stack maps] and Note [Reserved frames in loops]
    r1@(stackmaps1, _, _, props1, _, _) <- run mapEmpty setEmpty
    let -- the loop heads either may seed: each with the stack maps of its
        -- edges, its own, and the variables in one slot on every edge
        heads = [ (l, p, rest, old, common)
                | (l, ps) <- mapToList props1
                , l /= entry
                , not (l `setMember` procpoints)
                , Just (p : rest) <- [sequence ps]
                , Just old <- [mapLookup l stackmaps1]
                , let common = foldl' commonRegs (sm_regs p) (map sm_regs rest) ]
        seeds0 = loopHeadSeeds selfRec selfCall blockmap sccs heads
        (reserved, keepsOf) = reservedSeeds platform selfCall procpoints
                                blockmap cold liveness stackmaps1 blocks sccs
                                seeds0 heads
        seeds = mapUnion reserved seeds0
        -- the continuations whose frames keep the stack's depth for the
        -- reserved seeds among the given ones
        keepsFor sds = setUnions [ ks | (l, ks) <- mapToList keepsOf
                                      , l `mapMember` sds ]
        -- after a run whose seeds fail to hold, the seeds that held in it
        -- run again alone, the first run kept where none held or they fail
        -- again: Note [Loop head stack maps]
        retry sds props
          | mapNull held = return r1
          | otherwise = do
              r@(_, _, _, props', _, _) <- run held (keepsFor held)
              return $ if seedsHold held props' then r else r1
          where held = mapFilterWithKey
                         (\l sd -> seedsHold (mapSingleton l sd) props) sds
    (final_stackmaps0, _final_high_sp, new_blocks0, _, diamonds, edges) <-
        if mapNull seeds then return r1 else do
          r2@(_, _, _, props2, _, _) <- run seeds (keepsFor seeds)
          if seedsHold seeds props2 then return r2
          -- reserved seeds that do not hold do not take loopHeadSeeds' own
          -- with them
          else if mapNull seeds0 || mapNull reserved
            then retry seeds props2
          else do
            r3@(_, _, _, props3, _, _) <- run seeds0 setEmpty
            if seedsHold seeds0 props3 then return r3
              else retry seeds0 props3

    -- See Note [Tagged path skips the reloads]
    (final_stackmaps1, new_blocks1, bodies, split_bodies) <-
        if cmmSplitProcPoints cfg
          then return (final_stackmaps0, new_blocks0, mapEmpty, mapEmpty)
          else let -- the laid-out blocks' cycles, a GC call's left out, for
                   -- both (liftFrames changes no label, successor or
                   -- condition)
                   cold_l = unlikelyOnlyBlocks (mapFromList [ (entryLabel b, b)
                                                    | b <- new_blocks0 ])
                                       new_blocks0
                   loops_l = blockCycles (not . (`setMember` cold_l))
                                         new_blocks0
                   -- See Note [Diamond frames keep their depth]
                   (stackmaps_l, blocks_l, lifted) =
                     liftFrames cfg lift_frames diamonds final_stackmaps0
                                loops_l new_blocks0
               in tagSkip cfg procpoints entry (not (mapNull props1))
                          stackmaps_l diamonds loops_l blocks_l lifted

    -- See Note [Retries copy their checks]; only where a continuation is split
    -- (run for every procedure, the search's passes over the blocks made the
    -- compiler run 0.38% more instructions and allocate 0.26% more compiling
    -- T9961 at -O0, 0.17% more and 0.059% more compiling !16269's
    -- CmmDiamondChain at -O1, and 0.16% more and 0.058% more at -O2, on GHC
    -- 10.1.20261108)
    (new_blocks, final_stackmaps) <-
        if mapNull split_bodies then return (new_blocks1, final_stackmaps1)
        else gcRetryCopies procpoints split_bodies final_stackmaps1
                           new_blocks1

    blocks_with_reloads <-
        insertReloadsAsNeeded platform procpoints final_stackmaps bodies entry
                              new_blocks
    new_blocks' <- mapM (lowerSafeForeignCall profile) blocks_with_reloads
    -- joinCallEdges only where layout made a block on an edge into a call:
    -- called for every procedure, it made the compiler run 0.006% more
    -- instructions and allocate 0.024% more compiling T9961 at -O0, 0.001% more
    -- and 0.006% more compiling T12707 at -O, and 0.001% more and 0.005% more
    -- at -O2, on GHC 10.1.20261108
    let joined | null edges = new_blocks'
               | otherwise  = joinCallEdges edges new_blocks'
    return (ofBlockList entry joined, final_stackmaps, split_bodies)

{- Note [Retries copy their checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A heap or stack check's GC call returns to a continuation that reloads the
variables and jumps back to the check (the retry). Where a body split off
a continuation (Note [Tagged path skips the reloads]) also jumps to that
check, holding the variables in other registers than the retry's reloads give
them, the check joins the two, and the allocator fixing its registers from
the retry permutes them on the hot path at every pass (in a loop measured, a
seven-register xchg cycle into each of two checks). So, where the block the
check's passing branch leads to ends in a jump, the retry jumps to a copy of the
check, whose passing branch leads to a copy of that block: the hot check keeps
one way in.

Ordering blocks without frequency estimates (linearRA_SCCs in
GHC.CmmToAsm.Reg.Linear), the allocator defers the retry, but in the loop
measured it defers the loop holding the check too, only deferred blocks leading
into it, the continuation entering it among them (GHC #27800), and allocates the
deferred blocks in dependency order, the retry before the loop.
-}

-- | Each retry into a check that a split body also enters, where the check's
-- passing block ends in a jump, given its own copy of the check and of that
-- block: Note [Retries copy their checks]
gcRetryCopies :: ProcPointSet -> LabelMap BlockId
              -> LabelMap StackMap -> [CmmBlock]
              -> UniqDSM ([CmmBlock], LabelMap StackMap)
gcRetryCopies procpoints split_bodies stackmaps blocks
  -- with no copy to make, the blocks are not rebuilt: rebuilt, they made the
  -- compiler run 0.034% more instructions and allocate 0.042% more compiling
  -- nofib's puzzle at -O0, 0.018% more and 0.02% more at -O1, and 0.024% more
  -- and 0.026% more at -O2, on GHC 10.1.20261108
  | null cands = return (blocks, stackmaps)
  | otherwise = do
      copies <- mapM (\c -> do { h' <- newBlockId; a' <- newBlockId
                               ; return (c, h', a') }) cands
      let new = concat
            [ [ blockJoin (CmmEntry h' hs) hmid (retargetTo a a' hlst)
              , blockJoin (CmmEntry a' as) amid alst ]
            | ((_, _, a, hb, ab), h', a') <- copies
            , let (CmmEntry _ hs, hmid, hlst) = blockSplit hb
                  (CmmEntry _ as, amid, alst) = blockSplit ab ]
          retarget = mapFromList [ (g, h') | ((g, _, _, _, _), h', _) <- copies ]
          redirect b = case mapLookup (entryLabel b) retarget of
            Just h' -> replaceLastNode b (CmmBranch h')
            Nothing -> b
          smOf l = mapFindWithDefault (panic "gcRetryCopies") l stackmaps
          sms = foldl' (\m ((_, h, _, _, ab), h', a') ->
                          mapInsert a' (smOf (entryLabel ab))
                                    (mapInsert h' (smOf h) m))
                       stackmaps copies
      return (map redirect blocks ++ new, sms)
  where
    isBody l = mapMember l split_bodies
    -- found by passes over the blocks keeping small sets: the split bodies'
    -- successors, those of them ending in a check's unlikely branch to a call,
    -- those calls' continuations, the retries among them jumping back to their
    -- checks, and the checks' passing blocks (by lookups in a map of all
    -- blocks, the compiler ran 0.007% fewer instructions and allocated 0.099%
    -- more compiling nofib's puzzle at -O0, 0.002% more and 0.049% more at -O1,
    -- and 0.007% more and 0.09% more at -O2, on GHC 10.1.20261108)
    afterBody = setFromList [ s | b <- blocks, isBody (entryLabel b)
                                , s <- successors (lastNode b) ]
    checks = mapFromList
      [ (h, (c, a, b))
      | b <- blocks, let h = entryLabel b, h `setMember` afterBody
      , not (h `setMember` procpoints), not (isBody h)
      , CmmCondBranch{ cml_true = c, cml_false = a, cml_likely = Just False }
          <- [lastNode b]
      , c /= a, not (a `setMember` procpoints), not (isBody a) ]
    callOf = setFromList [ c | (c, _, _) <- mapElems checks ]
    conts = mapFromList [ (g, c) | b <- blocks, let c = entryLabel b
                                 , c `setMember` callOf
                                 , CmmCall{ cml_cont = Just g } <- [lastNode b] ]
    -- the block jumping back to the check: the continuation itself, or,
    -- where its frame is trimmed and tagSkip split it, the body it jumps to
    hop = mapFromList [ (x, entryLabel b) | b <- blocks, mapMember (entryLabel b) conts
                                          , CmmBranch x <- [lastNode b] ]
    retries = [ (r, h, a, hb) | b <- blocks, let r = entryLabel b
              , Just g <- [ if mapMember r conts then Just r else mapLookup r hop ]
              , Just c <- [mapLookup g conts]
              , CmmBranch h <- [lastNode b]
              , Just (c', a, hb) <- [mapLookup h checks], c' == c ]
    -- a passing block that only jumps on is skipped: its target is what
    -- the copy copies, or the join only moves there (the loop measured in
    -- Note [Retries copy their checks], whose allocation follows such a block)
    jumpsOn = mapFromList [ (entryLabel b, x) | b <- blocks
                          , let (_, m, l) = blockSplit b, isEmptyBlock m
                          , CmmBranch x <- [l] ]
    passed a = case mapLookup a jumpsOn of
      Just x | not (x `setMember` procpoints), not (isBody x) -> x
      _ -> a
    passing = setFromList [ passed a | (_, _, a, _) <- retries ]
    passBlocks = mapFromList [ (entryLabel b, b) | b <- blocks
                                                 , entryLabel b `setMember` passing
                                                 , CmmBranch _ <- [lastNode b] ]
    cands = [ (g, h, a, hb, ab) | (g, h, a, hb) <- retries
                                , Just ab <- [mapLookup (passed a) passBlocks] ]

-- | Join each block layout added on an edge into a call, given with the call's
-- block, to that block, where the call has no other way in: Note [Tagged path
-- skips the reloads]. A stack check optStackCheck folds into a jump to a call's
-- block that nothing else enters stays a jump here, which the code generator
-- drops where it places the call's block next, and which cmmCfgOpts removes at
-- -O1 and -O2 by joining the two after layout.
joinCallEdges :: [(BlockId, BlockId)] -> [CmmBlock] -> [CmmBlock]
joinCallEdges edges new
  -- the blocks as given where no call is joined: rebuilt, they made the
  -- compiler run 0.004% more instructions and allocate 0.004% more compiling
  -- nofib's puzzle at -O0, 0.002% more and no more compiling T12707 at -O, and
  -- 0.002% more and no more compiling nofib's puzzle at -O2, on GHC
  -- 10.1.20261108
  | mapNull calls = new
  | otherwise     = [ join b | b <- new, not (entryLabel b `setMember` gone) ]
  where
    callLbls = setFromList (map snd edges)
    callBlocks = mapFromList
      [ (entryLabel b, b) | b <- new, entryLabel b `setMember` callLbls ]
    preds :: LabelMap Int
    preds = mapFromListWith (+)
      [ (s, 1) | b <- new, s <- successors (lastNode b), s `setMember` callLbls ]
    -- each edge's block, to the block of the call it jumps to
    calls = mapFromList
      [ (e, cb) | (e, c) <- edges
                , mapLookup c preds == Just 1
                , Just cb <- [mapLookup c callBlocks] ]
    gone = setFromList (map entryLabel (mapElems calls))
    join b = case mapLookup (entryLabel b) calls of
      Nothing -> b
      Just cb ->
        let (CmmEntry l sb, mb, _) = blockSplit b
            (CmmEntry _ sc, mc) = blockSplitHead cb
        in CmmEntry l (combineTickScopes sb sc) `blockJoinHead` (mb `blockAppend` mc)

-- -----------------------------------------------------------------------------
-- Pass 1
-- -----------------------------------------------------------------------------

layout :: CmmConfig
       -> Bool                          -- the procedure makes a non-tail
                                        -- call of itself
       -> (CmmNode O C -> Bool)         -- a non-tail call of itself
       -> LabelSet                      -- the blocks whose diamonds' early
                                        -- stores must keep the next frame no
                                        -- deeper
       -> LabelSet                      -- proc points
       -> LabelMap CmmLocalLive         -- liveness
       -> BlockId                       -- entry
       -> ByteOff                       -- stack args on entry
       -> LabelMap CmmBlock             -- the blocks by label
       -> (BlockId -> LocalRegSet)      -- the variables live on entry
       -> LabelSet                      -- the cold blocks (Note [Loop head
                                        -- stack maps])
       -> LabelMap StackMap             -- loop heads' stack maps, given
       -> LabelSet                      -- continuations whose frames keep
                                        -- the stack's depth

       -> LabelMap StackMap             -- [final] stack maps
       -> ByteOff                       -- [final] Sp high water mark

       -> [CmmBlock]                    -- [in] blocks

       -> UniqDSM
          ( LabelMap StackMap           -- [out] stack maps
          , ByteOff                     -- [out] Sp high water mark
          , [CmmBlock]                  -- [out] new blocks
          , LabelMap [Maybe StackMap]   -- [out] stack maps on back edges
          , [(BlockId, BlockId, Maybe (BlockId, [LocalReg]))]
            -- [out] diamonds: block, continuation, the call's edge block
            -- and the variables stored there that the tagged path skips,
            -- if any
          , [(BlockId, BlockId)]
            -- [out] the blocks made on edges into calls, each with the
            -- call's block
          )

layout cfg selfRecL selfCallNode r2set procpoints liveness entry entry_args
       blockmap liveRegs cold seeds keeps final_stackmaps final_sp_high blocks
  = go blocks (mapUnion init_stackmap seeds) entry_args [] setEmpty mapEmpty []
       mapEmpty []
  where
    -- See Note [Tagged path skips the stores]
    -- how many later calls need a variable in a slot, from a continuation: 0,
    -- 1, or 2 for more, by a search over the blocks' Steps that ends where the
    -- variable is redefined or dies or two calls need it, the Steps linked so
    -- that a search looks up only its first (by a search over the blocks by
    -- label, walking the middle of each block entered for a redefinition, the
    -- compiler ran 0.16% more instructions and allocated 0.084% more compiling
    -- T12707 at -O0, 0.47% more and 0.29% more compiling nofib's puzzle at -O1,
    -- and 1.9% more and 1.2% more at -O2, on GHC 10.1.20261108)
    need l r = case mapLookup l steps of
      Just st -> laterSearch fc_res st r
      Nothing -> 0
    steps = blockSteps liveness blockmap
    -- laterSavesBy's count of the later non-tail calls of the procedure
    -- itself alone, for the slot order (allocate): only a frame a self call
    -- trims recurs a level at a time
    selfSaves = laterSavesBy selfCallNode liveRegs entry blockmap
    selfNeed = countAt selfSaves
    -- a call's slot order: allocate's for a non-tail self call where
    -- the procedure loops
    needOf n | selfCallNode n && selfRecL && has_loop = Just selfNeed
             | otherwise = Nothing
    countAt m l r = Map.findWithDefault 0 r (mapFindWithDefault Map.empty l m)
    -- a branch back to a block no later in reverse postorder, a GC call's left
    -- out: the block's place tested first, so that the cold blocks are computed
    -- only once a back edge is found
    has_loop = or [ True
                  | (i, b) <- zip [0 :: Int ..] blocks
                  , s <- successors (lastNode b)
                  , maybe False (<= i) (mapLookup s pos)
                  , not (entryLabel b `setMember` cold)
                  , not (s `setMember` cold) ]
      where pos = mapFromList (zip (map entryLabel blocks) [0 ..])
    -- the variables a safe foreign call defines: at such a call one is
    -- needed where it is not yet live
    fc_res = Set.fromList [ r | b <- blocks
                              , CmmForeignCall{ res = rs } <- [lastNode b]
                              , r <- rs ]
    lazy_ok = not (cmmSplitProcPoints cfg)

    (updfr, cont_info)  = collectContInfo blocks

    -- how many blocks return to each continuation
    cont_callers = foldl' (\m l -> mapInsertWith (+) l (1 :: Int) m) mapEmpty
                     [ l | b <- blocks
                         , Just l <- [callContinuation_maybe (lastNode b)] ]

    init_stackmap = mapSingleton entry
                      StackMap{ sm_sp   = entry_args
                              , sm_args = entry_args
                              , sm_ret_off = updfr
                              , sm_regs = emptyUFM
                              }

    go :: [Block CmmNode C C]
       -> LabelMap StackMap
       -> StackLoc
       -> [CmmBlock]
       -> LabelSet
       -> LabelMap [Maybe StackMap]
       -> [(BlockId, BlockId, Maybe (BlockId, [LocalReg]))]
       -> LabelMap [LocalReg]
       -> [(BlockId, BlockId)]
       -> UniqDSM ( LabelMap StackMap, StackLoc, [CmmBlock]
                  , LabelMap [Maybe StackMap]
                  , [(BlockId, BlockId, Maybe (BlockId, [LocalReg]))]
                  , [(BlockId, BlockId)] )
    go [] acc_stackmaps acc_hwm acc_blocks _done acc_props acc_dias _lazies
       acc_edges
      = return (acc_stackmaps, acc_hwm, acc_blocks, acc_props, acc_dias, acc_edges)

    go (b0 : bs) acc_stackmaps acc_hwm acc_blocks done acc_props acc_dias
       lazies acc_edges
      = do
       let (entry0@(CmmEntry entry_lbl tscope), middle0, last0) = blockSplit b0

       -- a continuation whose diamond stores some variables only on the
       -- call's path: its body cannot assume them in their slots
       let stack0@StackMap { sm_sp = sp0 }
               = (case mapLookup entry_lbl lazies of
                    Nothing -> id
                    Just rs -> \sm -> sm { sm_regs = delListFromUFM (sm_regs sm) rs })
               $ mapFindWithDefault
                     (pprPanic "no stack map for" (ppr entry_lbl))
                     entry_lbl acc_stackmaps

       -- (a) Update the stack map to include the effects of
       --     assignments in this block
       let stack1 = foldBlockNodesF (procMiddle acc_stackmaps) middle0 stack0

       -- (b) Look at the last node and if we are making a call or
       --     jumping to a proc point, we must save the live
       --     variables, adjust Sp, and construct the StackMaps for
       --     each of the successor blocks.  See handleLastNode for
       --     details.
       (middle1, sp_off, last1, fixup_blocks, out, mb_dia)
           <- handleLastNode cfg (selfRecL || (has_loop && entry_lbl `setMember` r2set)) procpoints liveness cont_info keeps
                             cont_callers blockmap liveRegs need needOf
                             lazy_ok acc_stackmaps
                             stack1 tscope middle0 last0

       -- (c) Manifest Sp: run over the nodes in the block and replace
       --     CmmStackSlot with CmmLoad from Sp with a concrete offset.
       --
       -- our block:
       --    middle0          -- the original middle nodes
       --    middle1          -- live variable saves from handleLastNode
       --    Sp = Sp + sp_off -- Sp adjustment goes here
       --    last1            -- the last node
       --
       -- less the return address store a diamond moved to the call's edge
       let middle0' = case mb_dia of
             Just (l, Just _, _) -> filter (not . isRetStore l) (blockToList middle0)
             _              -> blockToList middle0
           -- copied by (++) only where something is appended: always copied,
           -- the nodes made the compiler run 0.23% more instructions and
           -- allocate 0.5% more compiling T3294 at -O0, 0.086% more and 0.17%
           -- more at -O, and 0.081% more and 0.17% more at -O2, on GHC
           -- 10.1.20261108
           middle_pre | null middle1 = middle0'
                      | otherwise    = middle0' ++ middle1

       -- See Note [Loop head stack maps]
       let done' = setInsert entry_lbl done
           -- the cold blocks only once a back edge is found, which a procedure
           -- without a loop has none of (with them tested first, the compiler
           -- ran 0.67% more instructions and allocated 0.71% more compiling
           -- T783 at -O0, 0.14% more and 0.13% more at -O, and 0.13% more and
           -- 0.12% more at -O2, on GHC 10.1.20261108)
           back  = [ l | isBranchNode last0
                       , l <- successors last0
                       , l `setMember` done'
                       , not (entry_lbl `setMember` cold) ]
           prop l
             | isJust mb_dia = Nothing
             | otherwise     = Just (liveOnly (mapFindWithDefault emptyLRegSet
                                                 l liveness) stack1)
           acc_props' = foldl' (\m l -> mapInsertWith (++) l [prop l] m)
                               acc_props back
           acc_dias' = case mb_dia of
             Just (l, _, mb_c) | mapFindWithDefault 0 l cont_info
                                   == platformWordSizeInBytes (cmmPlatform cfg)
                               -> (entry_lbl, l, mb_c) : acc_dias
             _ -> acc_dias
           lazies' = case mb_dia of
             -- several diamonds may share a continuation
             Just (l, _, Just (_, rs)) -> mapInsertWith (++) l rs lazies
             _ -> lazies
           !edges' = case mb_dia of
             Just (_, Just e, _) -> e : acc_edges
             _ -> acc_edges

       let final_blocks =
               manifestSp cfg final_stackmaps stack0 sp0 final_sp_high
                          entry0 middle_pre sp_off last1 fixup_blocks

       let acc_stackmaps' = mapUnion acc_stackmaps out

           -- If this block jumps to the GC, then we do not take its
           -- stack usage into account for the high-water mark.
           -- Otherwise, if the only stack usage is in the stack-check
           -- failure block itself, we will do a redundant stack
           -- check.  The stack has a buffer designed to accommodate
           -- the largest amount of stack needed for calling the GC.
           --
           this_sp_hwm | isGcJump last0 = 0
                       | otherwise      = sp0 - sp_off

           hwm' = maximum (acc_hwm :| this_sp_hwm : map sm_sp (mapElems out))

       go bs acc_stackmaps' hwm' (final_blocks ++ acc_blocks)
          done' acc_props' acc_dias' lazies' edges'

-- | The cycles of the blocks the predicate keeps: the strongly connected
-- components with a cycle of the graph of their edges among themselves.
blockCycles :: (BlockId -> Bool) -> [CmmBlock] -> [[BlockId]]
blockCycles keep bs =
  [ xs | CyclicSCC xs <- stronglyConnCompFromEdgedVerticesUniq
                           [ DigraphNode l l (successors (lastNode b))
                           | b <- bs, let l = entryLabel b, keep l ] ]

-- | A last node with its edges to the first label sent to the second.
retargetTo :: BlockId -> BlockId -> CmmNode O C -> CmmNode O C
retargetTo a b = mapSuccessors (\x -> if x == a then b else x)

-- | The blocks reached only through unlikely branches, as a heap or stack
-- check's call to the GC and its continuation are: those every edge into
-- which is the unlikely side of a conditional branch, the side a branch
-- on a literal never takes, or leaves another such block. A check of two
-- conditions, as the RTS's MAYBE_GC tests two with @||@, enters its failure
-- block by an unlikely branch from each, and the @if (0)@ by which MAYBE_GC
-- shares its GC call with a stack check never takes its edge.
unlikelyOnlyBlocks :: LabelMap CmmBlock -> [CmmBlock] -> LabelSet
unlikelyOnlyBlocks blockmap blocks = spread (setElems seeds) seeds warm0
  where
    coldEdge :: CmmNode O C -> BlockId -> Bool
    coldEdge CmmCondBranch{ cml_pred = CmmLit (CmmInt n _)
                          , cml_true = t, cml_false = f } s
      | t /= f = s == (if n == 0 then t else f)
    coldEdge CmmCondBranch{ cml_true = t, cml_false = f
                          , cml_likely = Just lk } s
      | t /= f = s == (if lk then f else t)
    coldEdge _ _ = False
    -- the number of edges into each block that are not cold by themselves,
    -- for the blocks with any
    warm0 :: LabelMap Int
    warm0 = foldl' (\m b -> let l = lastNode b
                            in foldl' (warmEdge l) m (successors l))
                   mapEmpty blocks
    warmEdge l m s | coldEdge l s = m
                   | otherwise    = mapInsertWith (+) s 1 m
    -- the blocks only edges cold by themselves enter
    seeds = setFromList [ s | b <- blocks, let l = lastNode b
                            , s <- successors l, coldEdge l s
                            , not (s `mapMember` warm0) ]
    -- a block found cold makes the edges out of it cold, and a block left
    -- with no other edge into it is cold in turn, each edge counted down
    -- once
    spread [] cs _ = cs
    spread (p : ps) cs warm = case mapLookup p blockmap of
      Just b  -> let l = lastNode b in down l (successors l) ps cs warm
      Nothing -> spread ps cs warm
    down _ [] ps cs warm = spread ps cs warm
    down l (s : ss) ps cs warm
      | coldEdge l s = down l ss ps cs warm
      | n == 0       = down l ss (s : ps) (setInsert s cs) warm'
      | otherwise    = down l ss ps cs warm'
      where
        n = mapFindWithDefault 1 s warm - 1
        !warm' = mapInsert s n warm

-- | The label a non-tail call to a label calls
nonTailCallee :: CmmNode O C -> Maybe CLabel
nonTailCallee CmmCall{ cml_target = CmmLit (CmmLabel lab), cml_cont = Just _ }
  = Just lab
nonTailCallee _ = Nothing

isBranchNode :: CmmNode O C -> Bool
isBranchNode CmmBranch{}     = True
isBranchNode CmmCondBranch{} = True
isBranchNode CmmSwitch{}     = True
isBranchNode _               = False

-- | The store of continuation @l@'s return address
isRetStore :: BlockId -> CmmNode O O -> Bool
isRetStore l (CmmStore (CmmStackSlot (Young l1) _) (CmmLit (CmmBlock l2)) _)
  = l1 == l && l2 == l
isRetStore _ _ = False

{- Note [Loop head stack maps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A block takes the stack map of the first of its predecessors that layout
visits, and every later edge into it gets a fixup making the stack look
the same. For a loop head that is the edge into the loop, so where the loop's
body sets up a frame, say for a case's continuation (Note [diamond proc
point]), the back edge pops it and every iteration stores the loop's
invariants and moves Sp twice.

So layout runs twice where that happens: the second run gives each loop head
(not a proc point) the stack map common to its back edges in the first run
(their Sp, and the variables each has in the same slot), and the edge into
the loop pays the difference once. A back edge from a block reached only
through unlikely branches, as a GC call's continuation returning to a heap
or stack check is, proposes nothing: seeded from it, the stack check of
stg_decodeDouble_Int64# had every call store its argument and move Sp twice. And
a head is not seeded where every cycle through it makes a non-tail call to
its own procedure: the seed's cost is paid on each entry and exit, which in a
recursion whose loop turns about once an entry costs more than it saves. nofib's
tak, a self tail call around three calls of itself, is entered three times an
iteration; containers' foldrWithKey', recursing into a Map's right subtree and
looping into its left, is entered once an iteration, though its loop holds a
diamond. Where a cycle avoids those calls, as Data.Graph's dfs passes a visited
vertex without one, the loop can turn many times an entry, as one calling other
code (fannkuch-redux's) does. The second run is kept only if its back edges then
need no fixup; otherwise the heads whose back edges need none run again with
their seeds alone (after the third run of Note [Reserved frames in loops], where
that runs), and that run is kept if its back edges need no fixup, the first
otherwise.
A diamond on a cycle that makes no non-tail call into the procedure's recursion
stores a variable before its tag test only where that keeps the next call's
frame no deeper (Note [Tagged path skips the stores]): a deeper frame would
change the depth at which the back edges arrive in the second run, and the seed
would not hold.
-}

loopHeadSeeds :: Bool -> (BlockId -> Bool) -> LabelMap CmmBlock -> [[BlockId]]
              -> [( BlockId, StackMap, [StackMap], StackMap
                  , UniqFM LocalReg (LocalReg, StackLoc) )]
              -> LabelMap StackMap
loopHeadSeeds selfRec selfCall blockmap sccs heads
  = foldl' add mapEmpty heads
  where
    -- a head on a cycle that makes no non-tail call of the procedure: a
    -- head's loop is its strongly connected component (the blocks the head
    -- reaches that reach it), and the cycles avoiding those calls are the
    -- components of the loop without them, computed once for all heads
    loopOk = setUnions
      [ if any selfCall xs then free xs else setFromList xs | xs <- sccs ]
    free xs = setFromList (concat (blockCycles (not . selfCall)
                [ b | x <- xs, Just b <- [mapLookup x blockmap] ]))
    -- without a non-tail call of the procedure, loopOk is every block
    inLoop l = not selfRec || l `setMember` loopOk
    add acc (l, p, rest, old, common)
      | all ((== sm_sp p) . sm_sp) rest
      , any (needsFixup old) (p : rest)
      , inLoop l
      = mapInsert l p { sm_regs = common } acc
      | otherwise = acc

{- Note [Reserved frames in loops]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loopHeadSeeds leaves a loop head alone where its back edges differ in depth
or need no fixup. A call returning into such a loop still moves Sp twice an
iteration where its return path joins a path that made no call: down at the
call, up at the fixup on the join's edge. So the second run gives such a
loop's head the depth of the deepest continuation in its loop, with the loop's
invariants (live at the head, assigned nowhere in the loop) in the slots that
continuation's frame gave them, leaving out a continuation reached only through
unlikely branches: the GC call of a heap check returns into many a loop that
makes no other call, whose every entry and exit would pay for a frame only that
rare path uses. The calls returning into the loop keep the stack's depth (Note
[Diamond frames keep their depth]): the frame is reserved, and the edge into the
loop pays the Sp move and the invariants' stores once. A loop that only permutes
the variables live at its head, as integerAdd's swap of its arguments, turns at
most a few times an entry and would pay that edge, and an Sp move at each exit,
every time its procedure is called, so a loop is reserved only where it advances
one of them: assigns it from a load, arithmetic, a literal or a foreign call's
result, or from a copy of such a variable. Nor is a loop making a non-tail call
to its own procedure, diamond or none: its head is entered once a level of the
recursion, and each level would hold the reserved frame, a stack the GC then
scans (containers' folds over a line-shaped tree). Nor is a loop holding a head
loopHeadSeeds seeded: that seed is the stack map of frames the first run pushed,
each putting its first variable in the youngest slot, and allocate fills the
reserved holes oldest slot first, so a diamond's frame in the reserved loop
puts every variable in another slot than the seed expects, and the edge into
the seeded loop moves them all and swaps the registers at its join (nofib's
puzzle). The second run is kept only if its seeds hold (Note [Loop head stack
maps]); where they do not and loopHeadSeeds seeded some head, a third run tries
its seeds alone.
-}

-- | The reserved seeds, none of them loopHeadSeeds' own, and for each the
-- continuations whose frames keep the stack's depth.
-- See Note [Reserved frames in loops]
reservedSeeds :: Platform -> (BlockId -> Bool) -> ProcPointSet
              -> LabelMap CmmBlock -> LabelSet -> LabelMap CmmLocalLive
              -> LabelMap StackMap -> [CmmBlock] -> [[BlockId]]
              -> LabelMap StackMap
              -> [( BlockId, StackMap, [StackMap], StackMap
                  , UniqFM LocalReg (LocalReg, StackLoc) )]
              -> (LabelMap StackMap, LabelMap LabelSet)
reservedSeeds platform selfCall procpoints blockmap cold liveness
              stackmaps blocks sccs seeds0 heads
  = ( mapFromList [ (l, sd) | (l, sd, _) <- found ]
    , mapFromList [ (l, ks) | (l, _, ks) <- found ] )
  where
    sccOf = mapFromList [ (x, xs) | xs <- sccs, x <- xs ]
    middles xs = [ n | x <- xs, Just blk <- [mapLookup x blockmap]
                     , let (_, mid, _) = blockSplit blk
                     , n <- blockToList mid ]
    assignedIn xs = Set.fromList [ r | n <- middles xs
                                     , r <- foldRedefined (:) n [] ]
    -- the variables a loop assigns from something other than a variable
    -- (a load, arithmetic, a literal, a foreign call's result), or from
    -- a copy of such a variable; an assignment from a global register
    -- (a return's R1) counts as neither
    advancedIn xs =
      let ns = middles xs
          asgs = [ (r, e) | CmmAssign (CmmLocal r) e <- ns ]
          base = Set.fromList ([ r | CmmUnsafeForeignCall _ res _ <- ns
                                   , r <- res ]
                               ++ [ r | (r, e) <- asgs, fresh e ])
          fresh (CmmReg _) = False
          fresh _          = True
          copies = [ (r', r) | (r, CmmReg (CmmLocal r')) <- asgs, r /= r' ]
          grow s0 = let s1 = foldl' (\acc (a, b) -> if a `Set.member` acc
                                                      then Set.insert b acc
                                                      else acc) s0 copies
                    in if Set.size s1 == Set.size s0 then s0 else grow s1
      in grow base
    wordsOf (r, off) = [ off, off - platformWordSizeInBytes platform
                         .. off - localRegBytes platform r + 1 ]
    -- a head is reserved only for a proc point on its loop, which an edge
    -- enters: where no edge enters a proc point that is not cold, no head is,
    -- and the components are not forced here, though loopHeadSeeds forces them
    -- once a head passes its other tests (without this test, the compiler ran
    -- 0.8% more instructions and allocated 1% more compiling !16269's
    -- CmmDiamondChain at -O0, 0.66% more and 0.81% more at -O1, and 0.66% more
    -- and 0.81% more at -O2, on GHC 10.1.20261108)
    entered = or [ True | b <- blocks, s <- successors (lastNode b)
                        , s `setMember` procpoints, not (s `setMember` cold) ]
    found =
      [ (l, seed, setFromList conts)
      | (l, _, _, old, common) <- heads
      , entered
      , not (mapMember l seeds0)
      , Just xs <- [mapLookup l sccOf]
        -- no non-tail call of its own procedure, and no head loopHeadSeeds
        -- seeded, in the loop
      , not (any selfCall xs)
      , not (any (`mapMember` seeds0) xs)
      , let conts = [ k | k <- xs, k `setMember` procpoints
                        , not (k `setMember` cold) ]
            deep = [ sm | k <- conts, Just sm <- [mapLookup k stackmaps] ]
        -- the deepest, the first of several as deep
      , d : ds <- [deep]
      , let smk = foldl' (\a b -> if sm_sp b > sm_sp a then b else a) d ds
      , sm_sp smk > sm_sp old
      , let live = mapFindWithDefault emptyLRegSet l liveness
      , any (`elemLRegSet` live) (Set.toList (advancedIn xs))
      , let assigned = assignedIn xs
            occupied = Set.fromList (concatMap wordsOf (nonDetEltsUFM common))
            inv = [ (v, (v, off)) | (v, off) <- nonDetEltsUFM (sm_regs smk)
                                  , v `elemLRegSet` live
                                  , not (v `Set.member` assigned)
                                  , not (v `elemUFM` common)
                                  , not (any (`Set.member` occupied)
                                             (wordsOf (v, off))) ]
            seed = old { sm_sp = sm_sp smk, sm_regs = addListToUFM common inv }
      ]

seedsHold :: LabelMap StackMap -> LabelMap [Maybe StackMap] -> Bool
seedsHold seeds props = and
  [ maybe False (not . needsFixup seed) mp
  | (l, seed) <- mapToList seeds
  , mp <- mapFindWithDefault [] l props ]

-- | The variables in the same slot in both
commonRegs :: UniqFM LocalReg (LocalReg, StackLoc)
           -> UniqFM LocalReg (LocalReg, StackLoc)
           -> UniqFM LocalReg (LocalReg, StackLoc)
commonRegs a b = filterUFM same a
  where same (r, off) = case lookupUFM b r of
                          Just (_, off') -> off == off'
                          Nothing        -> False

-- | Whether a jump with stack map @from@ to a block with stack map @to@
-- needs a fixup
needsFixup :: StackMap -> StackMap -> Bool
needsFixup to from
  = sm_sp to /= sm_sp from || not (null (fixupStack from to))


-- -----------------------------------------------------------------------------

-- Not foolproof, but GCFun is the culprit we most want to catch
isGcJump :: CmmNode O C -> Bool
isGcJump (CmmCall { cml_target = CmmReg (CmmGlobal (GlobalRegUse l _)) })
  = l == GCFun || l == GCEnter1
isGcJump _something_else = False

-- -----------------------------------------------------------------------------

-- This doesn't seem right somehow.  We need to find out whether this
-- proc will push some update frame material at some point, so that we
-- can avoid using that area of the stack for spilling. Ideally we would
-- capture this information in the CmmProc (e.g. in CmmStackInfo; see #18232
-- for details on one ill-fated attempt at this).
--
-- So we'll just take the max of all the cml_ret_offs.  This could be
-- unnecessarily pessimistic, but probably not in the code we
-- generate.

collectContInfo :: [CmmBlock] -> (ByteOff, LabelMap ByteOff)
collectContInfo blocks
  = (maximum (expectNonEmpty ret_offs), mapFromList (catMaybes mb_argss))
 where
  (mb_argss, ret_offs) = mapAndUnzip get_cont blocks

  get_cont :: Block CmmNode x C -> (Maybe (Label, ByteOff), ByteOff)
  get_cont b =
     case lastNode b of
        CmmCall { cml_cont = Just l, .. }
           -> (Just (l, cml_ret_args), cml_ret_off)
        CmmForeignCall { .. }
           -> (Just (succ, ret_args), ret_off)
        _other -> (Nothing, 0)


-- -----------------------------------------------------------------------------
-- Updating the StackMap from middle nodes

-- Look for loads from stack slots, and update the StackMap.  This is
-- purely for optimisation reasons, so that we can avoid saving a
-- variable back to a different stack slot if it is already on the
-- stack.
--
-- This happens a lot: for example when function arguments are passed
-- on the stack and need to be immediately saved across a call, we
-- want to just leave them where they are on the stack.
--
procMiddle :: LabelMap StackMap -> CmmNode e x -> StackMap -> StackMap
procMiddle stackmaps node sm
  = procMiddleWith (\area off -> Just (getStackLoc area off stackmaps)) node sm

-- | procMiddle given each stack slot's location, a slot it gives Nothing for
-- taken as unknown
procMiddleWith :: (Area -> ByteOff -> Maybe StackLoc)
               -> CmmNode e x -> StackMap -> StackMap
procMiddleWith slotLoc node sm
  = case node of
     CmmAssign (CmmLocal r) (CmmLoad (CmmStackSlot area off) _ _)
       | Just loc <- slotLoc area off
       -> sm { sm_regs = addToUFM (sm_regs sm) r (r,loc) }
     CmmAssign (CmmLocal r) _other
       -> sm { sm_regs = delFromUFM (sm_regs sm) r }
     CmmUnsafeForeignCall _ results _
       -> sm { sm_regs = delListFromUFM (sm_regs sm) results }
     _other
       -> sm
{-# INLINE procMiddleWith #-}

getStackLoc :: Area -> ByteOff -> LabelMap StackMap -> StackLoc
getStackLoc Old       n _         = n
getStackLoc (Young l) n stackmaps =
  case mapLookup l stackmaps of
    Nothing -> pprPanic "getStackLoc" (ppr l)
    Just sm -> sm_sp sm - sm_args sm + n

-- | A fold over the variables a node redefines
foldRedefined :: (LocalReg -> a -> a) -> CmmNode e x -> a -> a
foldRedefined f (CmmAssign (CmmLocal r) _) z = f r z
foldRedefined f (CmmUnsafeForeignCall _ res _) z = foldr f z res
foldRedefined _ _ z = z
{-# INLINE foldRedefined #-}


-- -----------------------------------------------------------------------------
-- Handling stack allocation for a last node

-- We take a single last node and turn it into:
--
--    C1 (some statements)
--    Sp = Sp + N
--    C2 (some more statements)
--    call f()          -- the actual last node
--
-- plus possibly some more blocks (we may have to add some fixup code
-- between the last node and the continuation).
--
-- C1: is the code for saving the variables across this last node onto
-- the stack, if the continuation is a call or jumps to a proc point.
--
-- C2: if the last node is a safe foreign call, we have to inject some
-- extra code that goes *after* the Sp adjustment.

handleLastNode
   :: CmmConfig -> Bool -> ProcPointSet -> LabelMap CmmLocalLive -> LabelMap ByteOff
   -> LabelSet -> LabelMap Int -> LabelMap CmmBlock -> (BlockId -> LocalRegSet)
   -> (BlockId -> LocalReg -> Int)
   -> (CmmNode O C -> Maybe (BlockId -> LocalReg -> Int)) -> Bool
   -> LabelMap StackMap -> StackMap -> CmmTickScope
   -> Block CmmNode O O
   -> CmmNode O C
   -> UniqDSM
      ( [CmmNode O O]      -- nodes to go *before* the Sp adjustment
      , ByteOff            -- amount to adjust Sp
      , CmmNode O C        -- new last node
      , [CmmBlock]         -- new blocks
      , LabelMap StackMap  -- stackmaps for the continuations
      , Maybe (BlockId, Maybe (BlockId, BlockId), Maybe (BlockId, [LocalReg]))
        -- a diamond's continuation, the block on the call's edge its return
        -- address store moved to with the call's block, and that edge's
        -- block with the variables stored there only, if any
      )

handleLastNode cfg aheadNoDeeper procpoints liveness cont_info keeps
               cont_callers blockmap liveRegs need needOf lazy_ok stackmaps
               stack0@StackMap { sm_sp = sp0 } tscp middle last
  = case last of
      --  At each return / tail call,
      --  adjust Sp to point to the last argument pushed, which
      --  is cml_args, after popping any other junk from the stack.
      CmmCall{ cml_cont = Nothing, .. } -> do
        let sp_off = sp0 - cml_args
        return ([], sp_off, last, [], mapEmpty, Nothing)

      --  At each CmmCall with a continuation:
      CmmCall{ cml_cont = Just cont_lbl, .. } ->
        return $ lastCall cont_lbl cml_args cml_ret_args cml_ret_off

      CmmForeignCall{ succ = cont_lbl, .. } ->
        return $ lastCall cont_lbl (platformWordSizeInBytes platform) ret_args ret_off
              -- one word of args: the return address

      CmmBranch {}     ->  handleBranches
      CmmCondBranch {} ->  handleBranches
      CmmSwitch {}     ->  handleBranches
  where
     platform = cmmPlatform cfg
     slot_need = needOf last
     -- Calls and ForeignCalls are handled the same way:
     lastCall :: BlockId -> ByteOff -> ByteOff -> ByteOff
              -> ( [CmmNode O O]
                 , ByteOff
                 , CmmNode O C
                 , [CmmBlock]
                 , LabelMap StackMap
                 , Maybe (BlockId, Maybe (BlockId, BlockId), Maybe (BlockId, [LocalReg]))
                 )
     lastCall lbl cml_args cml_ret_args cml_ret_off
      =  ( assignments
         , spOffsetForCall sp0 cont_stack cml_args
         , last
         , [] -- no new blocks
         , mapSingleton lbl cont_stack
         , Nothing )
      where
         (assignments, cont_stack) = prepareStack (lbl `setMember` keeps) lbl
                                                  cml_ret_args cml_ret_off


     -- the variables that the call L's code reaches first, through
     -- unconditional branches and at most eight blocks, needs in a slot, L's
     -- code not redefining them on the way, with that call and L's middle
     -- nodes on the way, in order: stored on the call's edge only, such
     -- a variable would be stored again at that call on both paths, the
     -- call's path storing it twice, whether or not L's code reads it. See
     -- Note [Tagged path skips the stores]
     aheadSaves :: BlockId
                -> Maybe ( LRegSet
                         , (BlockId, ByteOff, ByteOff, Maybe (LocalReg -> Int))
                         , [CmmNode O O] )
     -- (eight blocks, bounding the walk, is arbitrary, and may be raised where
     -- a case needs more at little cost to the compiler: lifted to 1000, it
     -- changed the compile cost and code of programs of 50 to 400 loops,
     -- calls, cases or diamonds by under 0.1%)
     aheadSaves = go (8 :: Int) emptyLRegSet []
       where
         go 0 _ _ _ = Nothing
         go n seen mids l = case mapLookup l blockmap of
           Nothing -> Nothing
           Just blk ->
             let (_, mid, lst) = blockSplit blk
                 seen' = foldBlockNodesF touch mid seen
                 mids' = mids ++ blockToList mid
                 -- the call's continuation, return values, update frame
                 -- and slot order
                 atCall k ra ro = Just ( mapFindWithDefault emptyLRegSet k
                                                            liveness
                                           `differenceUniqueSet` touch lst seen'
                                       , (k, ra, ro, fmap ($ k) (needOf lst))
                                       , mids' )
             in case lst of
                  CmmCall{ cml_cont = Just k, cml_ret_args = ra
                         , cml_ret_off = ro }                 -> atCall k ra ro
                  CmmForeignCall{ succ = k, ret_args = ra
                                , ret_off = ro }              -> atCall k ra ro
                  CmmBranch s | not (s `setMember` procpoints)
                              -> go (n - 1) seen' mids' s
                  _ -> Nothing
         -- every variable the node defines
         touch :: CmmNode e x -> LRegSet -> LRegSet
         touch node acc =
           foldLocalRegsDefd platform (flip insertLRegSet) acc node

     prepareStack keep lbl cml_ret_args cml_ret_off
       | Just cont_stack <- mapLookup lbl stackmaps
             -- If we have already seen this continuation before, then
             -- we just have to make the stack look the same:
       = (fixupStack stack0 cont_stack, cont_stack)
             -- Otherwise, we have to allocate the stack frame
       | otherwise
       = (save_assignments, new_cont_stack)
       where
        (new_cont_stack, save_assignments)
           = setupStackFrame platform keep (fmap ($ lbl) slot_need) lbl liveRegs
                             cml_ret_off cml_ret_args stack0


     -- For other last nodes (branches), if any of the targets is a
     -- proc point, we have to set up the stack to match what the proc
     -- point is expecting.
     --
     handleBranches :: UniqDSM ( [CmmNode O O]
                                , ByteOff
                                , CmmNode O C
                                , [CmmBlock]
                                , LabelMap StackMap
                                , Maybe (BlockId, Maybe (BlockId, BlockId), Maybe (BlockId, [LocalReg])) )

     handleBranches
         -- See Note [diamond proc point]
       | Just l <- futureContinuation middle
       , (nub $ filter (`setMember` procpoints) $ successors last) == [l]
       = do
         let cont_args = mapFindWithDefault 0 l cont_info
             -- See Note [Diamond frames keep their depth]
             (assigs, cont_stack) = prepareStack True l cont_args (sm_ret_off stack0)
             out = mapFromList [ (l', cont_stack)
                               | l' <- successors last ]
             sp_off = spOffsetForCall sp0 cont_stack (platformWordSizeInBytes platform)
             -- The return address is needed only by the call, if it is
             -- the only block returning to l: no jump to the canned GC
             -- routine in l's alternatives relies on it. See Note
             -- [Tagged path skips the reloads]
             move_ret = case [ c | c <- successors last, c /= l ] of
               [c] | Just cb <- mapLookup c blockmap
                   , CmmCall{ cml_cont = Just l2 } <- lastNode cb
                   , l2 == l
                   , mapLookup l cont_callers == Just 1
                   , [st] <- filter (isRetStore l) (blockToList middle)
                   -> Just (c, st)
               _ -> Nothing
             -- See Note [Tagged path skips the stores]
             later = need l
             storedReg :: CmmNode O O -> Maybe LocalReg
             storedReg (CmmStore _ (CmmReg (CmmLocal r)) _) = Just r
             storedReg _                                    = Nothing
             -- the variables that at most one later call needs in a slot
             lz0 | lazy_ok, cont_args == platformWordSizeInBytes platform
                 = [ r | Just r <- map storedReg assigs, later r <= 1 ]
                 | otherwise = []
             -- all stored before the test where that call's frame, set up
             -- there, is no deeper with them all, and otherwise each only where
             -- that frame has it in the slot l's frame gives it, in allocate's
             -- order or, where the frame is then no deeper and its youngest
             -- slot of a variable that one or more later self calls need, and
             -- of one that two or more need, lies no younger than in that
             -- order, in the plain one
             ahead_call = aheadSaves l
             cand = case ahead_call of
               Just (ahead_set, _, _) -> [ r | r <- lz0
                                             , r `elemLRegSet` ahead_set ]
               Nothing                -> []
             -- all of them unless aheadNoDeeper, set in a procedure that
             -- makes a non-tail call of itself or on a cycle that makes no
             -- non-tail call into the procedure's recursion: see
             -- Note [Tagged path skips the stores]
             ahead
               | null cand || not aheadNoDeeper = cand
               | Just (_, call@(k, ra, ro, k_need), mids) <- ahead_call
               , let (fresh, f0) = nextFrame lz0 call mids
                     (_, f1) = nextFrame (filter (`notElem` cand) lz0) call mids
                     -- the frame in allocate's plain order
                     f0p = snd (nextFrame lz0 (k, ra, ro, Nothing) mids)
                     inSlot f r = case lookupUFM (sm_regs cont_stack) r of
                       Just (_, a) -> fmap snd (lookupUFM (sm_regs f) r) == Just a
                       Nothing     -> False
                     later = fromMaybe (const 0) k_need
                     own = [ r | r <- cand, inSlot f0 r ]
                     anyOrder = [ r | r <- cand, inSlot f0 r || inSlot f0p r ]
                     fa = snd (nextFrame (filter (`notElem` anyOrder) lz0) call mids)
                     -- the youngest slot of a variable n or more later self
                     -- calls need
                     youngest n f = foldl' max 0 [ o | (r, o) <- nonDetEltsUFM (sm_regs f)
                                                    , later r >= n ]
               = if fresh && sm_sp f1 == sm_sp f0 then cand
                 else if length anyOrder == length own
                         || sm_sp fa > sm_sp f0
                         || or [ youngest n fa > youngest n f0 | n <- [1, 2] ]
                      then own else anyOrder
               | otherwise = []
             -- the frame that call sets up where the variables given are in
             -- no slot at l, and whether it sets it up there first
             nextFrame unslotted (k, ra, ro, k_need) mids
               = case mapLookup k stackmaps of
                 Just sm -> (False, sm)
                 Nothing -> (True, fst (setupStackFrame platform
                                          (k `setMember` keeps) k_need k
                                          liveRegs ro ra at_call))
               where
                 at_l = cont_stack { sm_regs = delListFromUFM
                                                 (sm_regs cont_stack) unslotted }
                 at_call = foldl' (flip (procMiddleWith areaLoc)) at_l mids
             -- getStackLoc, a young area with no stack map yet taken as
             -- unknown
             areaLoc Old n = Just n
             areaLoc (Young y) n
               | y == l = Just (sm_sp cont_stack - sm_args cont_stack + n)
               | Just sm <- mapLookup y stackmaps
               = Just (sm_sp sm - sm_args sm + n)
               | otherwise = Nothing
             lazy_regs = filter (`notElem` ahead) lz0
             (lazy_assigs, eager_assigs) =
               partition (maybe False (`elem` lazy_regs) . storedReg) assigs
         case move_ret of
           Nothing ->
             return (assigs, sp_off, last, [], out, Just (l, Nothing, Nothing))
           Just (c, st) -> do
             tmp <- newBlockId
             -- the stores after the unwind at the depth Sp is adjusted to,
             -- as in makeFixupBlock's blocks
             let blk = blockJoin (CmmEntry tmp tscp)
                                 (maybeAddSpAdj cfg (sp0 - sp_off) 0
                                    (blockFromList (st : lazy_assigs)))
                                 (CmmBranch c)
                 last' = retargetTo c tmp last
             return ( eager_assigs, sp_off, last', [blk], out
                    , Just (l, Just (tmp, c), if null lazy_regs then Nothing
                                     else Just (tmp, lazy_regs)) )

        | otherwise = do
          pps <- mapM handleBranch (successors last)
          let lbl_map :: LabelMap Label
              lbl_map = mapFromList [ (l,tmp) | (l,tmp,_,_) <- pps ]
              fix_lbl l = mapFindWithDefault l l lbl_map
          return ( []
                 , 0
                 , mapSuccessors fix_lbl last
                 , concat [ blk | (_,_,_,blk) <- pps ]
                 , mapFromList [ (l, sm) | (l,_,sm,_) <- pps ]
                 , Nothing )

     -- For each successor of this block
     handleBranch :: BlockId -> UniqDSM (BlockId, BlockId, StackMap, [CmmBlock])
     handleBranch l
        --   (a) if the successor already has a stackmap, we need to
        --       shuffle the current stack to make it look the same.
        --       We have to insert a new block to make this happen.
        | Just stack2 <- mapLookup l stackmaps
        = do
             let assigs = fixupStack stack0 stack2
             (tmp_lbl, block) <- makeFixupBlock cfg sp0 l stack2 tscp assigs
             return (l, tmp_lbl, stack2, block)

        --   (b) if the successor is a proc point, save everything
        --       on the stack.
        | l `setMember` procpoints
        = do
             let cont_args = mapFindWithDefault 0 l cont_info
                 (stack2, assigs) =
                      setupStackFrame platform False Nothing l liveRegs
                                      (sm_ret_off stack0)
                                                        cont_args stack0
             (tmp_lbl, block) <- makeFixupBlock cfg sp0 l stack2 tscp assigs
             return (l, tmp_lbl, stack2, block)

        --   (c) otherwise, the current StackMap is the StackMap for
        --       the continuation.  But we must remember to remove any
        --       variables from the StackMap that are *not* live at
        --       the destination, because this StackMap might be used
        --       by fixupStack if this is a join point.
        | otherwise = return (l, l, stack1, [])
        where live = mapFindWithDefault (panic "handleBranch") l liveness
              stack1 = liveOnly live stack0


-- | The stack map with only the variables live given
liveOnly :: CmmLocalLive -> StackMap -> StackMap
liveOnly live sm
  = sm { sm_regs = filterUFM (\(r, _) -> r `elemLRegSet` live) (sm_regs sm) }

makeFixupBlock :: CmmConfig -> ByteOff -> Label -> StackMap
               -> CmmTickScope -> [CmmNode O O]
               -> UniqDSM (Label, [CmmBlock])
makeFixupBlock cfg sp0 l stack tscope assigs
  | null assigs && sp0 == sm_sp stack = return (l, [])
  | otherwise = do
    tmp_lbl <- newBlockId
    let sp_off = sp0 - sm_sp stack
        block = blockJoin (CmmEntry tmp_lbl tscope)
                          ( maybeAddSpAdj cfg sp0 sp_off
                           $ blockFromList assigs )
                          (CmmBranch l)
    return (tmp_lbl, [block])


-- Sp is currently pointing to current_sp,
-- we want it to point to
--    (sm_sp cont_stack - sm_args cont_stack + args)
-- so the difference is
--    sp0 - (sm_sp cont_stack - sm_args cont_stack + args)
spOffsetForCall :: ByteOff -> StackMap -> ByteOff -> ByteOff
spOffsetForCall current_sp cont_stack args
  = current_sp - (sm_sp cont_stack - sm_args cont_stack + args)


-- | create a sequence of assignments to establish the new StackMap,
-- given the old StackMap.
fixupStack :: StackMap -> StackMap -> [CmmNode O O]
fixupStack old_stack new_stack = concatMap move new_locs
 where
     old_map  = sm_regs old_stack
     new_locs = stackSlotRegs new_stack

     move (r,n)
       | Just (_,m) <- lookupUFM old_map r, n == m = []
       | otherwise = [CmmStore (CmmStackSlot Old n)
                               (CmmReg (CmmLocal r))
                               NaturallyAligned]



setupStackFrame
             :: Platform
             -> Bool                    -- keep the stack's depth
             -> Maybe (LocalReg -> Int) -- how many later calls need each
                                        -- variable in a slot: see allocate
             -> BlockId                 -- label of continuation
             -> (BlockId -> LocalRegSet) -- liveness
             -> ByteOff      -- updfr
             -> ByteOff      -- bytes of return values on stack
             -> StackMap     -- current StackMap
             -> (StackMap, [CmmNode O O])

setupStackFrame platform keep later lbl liveness updfr_off ret_args stack0
  = (cont_stack, assignments)
  where
      -- get the set of LocalRegs live in the continuation
      live = liveness lbl

      -- the stack from the base to updfr_off is off-limits.
      -- our new stack frame contains:
      --   * saved live variables
      --   * the return address [young(C) + 8]
      --   * the args for the call,
      --     which are replaced by the return values at the return
      --     point.

      -- everything up to updfr_off is off-limits
      -- stack1 contains updfr_off, plus everything we need to save
      (stack1, assignments) = allocate platform keep later updfr_off live stack0

      -- And the Sp at the continuation is:
      --   sm_sp stack1 + ret_args
      cont_stack = stack1{ sm_sp = sm_sp stack1 + ret_args
                         , sm_args = ret_args
                         , sm_ret_off = updfr_off
                         }


-- Note [diamond proc point]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- This special case looks for the pattern we get from a typical
-- tagged case expression:
--
--    Sp[young(L1)] = L1
--    if (R1 & 7) != 0 goto L1 else goto L2
--  L2:
--    call [R1] returns to L1
--  L1: live: {y}
--    x = R1
--
-- If we let the generic case handle this, we get
--
--    Sp[-16] = L1
--    if (R1 & 7) != 0 goto L1a else goto L2
--  L2:
--    Sp[-8] = y
--    Sp = Sp - 16
--    call [R1] returns to L1
--  L1a:
--    Sp[-8] = y
--    Sp = Sp - 16
--    goto L1
--  L1:
--    x = R1
--
-- The code for saving the live vars is duplicated in each branch, and
-- furthermore there is an extra jump in the fast path (assuming L1 is
-- a proc point, which it probably is if there is a heap check).
--
-- So to fix this we want to set up the stack frame before the
-- conditional jump.  How do we know when to do this, and when it is
-- safe?  The basic idea is, when we see the assignment
--
--   Sp[young(L)] = L
--
-- we know that
--   * we are definitely heading for L
--   * there can be no more reads from another stack area, because young(L)
--     overlaps with it.
--
-- We don't necessarily know that everything live at L is live now
-- (some might be assigned between here and the jump to L).  So we
-- simplify and only do the optimisation when we see
--
--   (1) a block containing an assignment of a return address L
--   (2) ending in a branch where one (and only) continuation goes to L,
--       and no other continuations go to proc points.
--
-- then we allocate the stack frame for L at the end of the block,
-- before the branch.
--
-- We could generalise (2), but that would make it a bit more
-- complicated to handle, and this currently catches the common case.

futureContinuation :: Block CmmNode O O -> Maybe BlockId
futureContinuation middle = foldBlockNodesB f middle Nothing
   where f :: CmmNode a b -> Maybe BlockId -> Maybe BlockId
         f (CmmStore (CmmStackSlot (Young l) _) (CmmLit (CmmBlock _)) _) _
               = Just l
         f _ r = r

-- -----------------------------------------------------------------------------
-- Saving live registers

-- | Given a set of live registers and a StackMap, save all the registers
-- on the stack and return the new StackMap and the assignments to do
-- the saving.
--
-- Where the slots allocate gives the variables it saves would put one that
-- no later call needs in a slot (@later@) older than one that a later call
-- needs, those later calls need take the older slots instead, so that the frame
-- of the next call, once the others have died, trims from the young end rather
-- than keeping their slots as holes, a word each level of a recursion through
-- that call holds (containers' Data.Graph.dfs on a star). @later@ is given only
-- for the frame of a non-tail call of the procedure itself in a procedure that
-- loops, and counts the later such calls; variables with equal counts keep
-- their order. Without it, allocate neither counts nor looks for such a hole: a
-- count of 0 for every other frame instead made the compiler run 0.54% more
-- instructions and allocate 0.17% more compiling T3294 at -O0, 0.38% more and
-- 0.11% more at -O, and 0.36% more and 0.1% more at -O2, on GHC 10.1.20261108.
allocate :: Platform -> Bool -> Maybe (LocalReg -> Int) -> ByteOff -> LocalRegSet
         -> StackMap -> (StackMap, [CmmNode O O])
allocate platform keep Nothing ret_off live stackmap
  = allocateIn platform keep False (const 0) ret_off live stackmap
allocate platform keep (Just later) ret_off live stackmap
  | holeOlder = allocateIn platform keep True later ret_off live stackmap
  | otherwise = plain
  where
    plain = allocateIn platform keep False later ret_off live stackmap
    -- the variables given slots, each with its count
    counted = [ (later r, off) | (r, off) <- nonDetEltsUFM (sm_regs (fst plain))
                               , not (r `elemUFM` sm_regs stackmap) ]
    holeOlder = or [ o0 < o1 | (0, o0) <- counted, (n1, o1) <- counted, n1 > 0 ]

allocateIn :: Platform -> Bool -> Bool -> (LocalReg -> Int) -> ByteOff
           -> LocalRegSet -> StackMap -> (StackMap, [CmmNode O O])
allocateIn platform keep ordered later ret_off live
           stackmap@StackMap{ sm_sp = sp0, sm_regs = regs0 }
 =
   -- we only have to save regs that are not already in a slot
   let to_save = byLater (negate . later)
                   (filter (not . (`elemUFM` regs0)) (Set.elems live))
       byLater f xs | ordered = sortOn f xs
       byLater _ xs           = xs
       regs1   = filterUFM (\(r,_) -> elemRegSet r live) regs0
   in

   -- make a map of the stack
   let stack = reverse $ Array.elems $
               accumArray (\_ x -> x) Empty (1, toWords platform (max sp0 ret_off)) $
                 ret_words ++ live_words
            where ret_words =
                   [ (x, Occupied)
                   | x <- [ 1 .. toWords platform ret_off] ]
                  live_words =
                   [ (toWords platform x, Occupied)
                   | (r,off) <- nonDetEltsUFM regs1,
                   -- See Note [Unique Determinism and code generation]
                     let w = localRegBytes platform r,
                     x <- [ off, off - platformWordSizeInBytes platform .. off - w + 1] ]
   in

   -- Pass over the stack: find slots to save all the new live variables,
   -- choosing the oldest slots first (hence a foldr).
   let
       save slot ([], stack, n, assigs, regs) -- no more regs to save
          = ([], slot:stack, plusW platform n 1, assigs, regs)
       save slot (to_save, stack, n, assigs, regs)
          = case slot of
               Occupied ->  (to_save, Occupied:stack, plusW platform n 1, assigs, regs)
               Empty
                 | Just (stack', r, to_save') <-
                       select_save to_save (slot:stack)
                 -> let assig = CmmStore (CmmStackSlot Old n')
                                         (CmmReg (CmmLocal r))
                                         NaturallyAligned
                        n' = plusW platform n 1
                   in
                        (to_save', stack', n', assig : assigs, (r,(r,n')):regs)

                 | otherwise
                 -> (to_save, slot:stack, plusW platform n 1, assigs, regs)

       -- we should do better here: right now we'll fit the smallest first,
       -- but it would make more sense to fit the biggest first.
       select_save :: [LocalReg] -> [StackSlot]
                   -> Maybe ([StackSlot], LocalReg, [LocalReg])
       select_save regs stack = go regs []
         where go []     _no_fit = Nothing
               go (r:rs) no_fit
                 | Just rest <- dropEmpty words stack
                 = Just (replicate words Occupied ++ rest, r, rs++no_fit)
                 | otherwise
                 = go rs (r:no_fit)
                 where words = localRegWords platform r

       -- fill in empty slots as much as possible
       (still_to_save, save_stack, n, save_assigs, save_regs)
          = foldr save (to_save, [], 0, [], []) stack

       -- push any remaining live vars on the stack
       (push_sp, push_assigs, push_regs)
          = foldr push (n, [], []) (byLater later still_to_save)
          where
              push r (n, assigs, regs)
                = (n', assig : assigs, (r,(r,n')) : regs)
                where
                  n' = n + localRegBytes platform r
                  assig = CmmStore (CmmStackSlot Old n')
                                   (CmmReg (CmmLocal r))
                                   NaturallyAligned

       trim_sp
          | not (null push_regs) = push_sp
          -- with keep, trim at most the word the return address takes
          | keep, Empty : _ <- save_stack = plusW platform n (-1)
          | keep = n
          | otherwise
          = plusW platform n (- length (takeWhile isEmpty save_stack))

       final_regs = regs1 `addListToUFM` push_regs
                          `addListToUFM` save_regs

   in
  -- XXX should be an assert
   if ( n /= max sp0 ret_off ) then pprPanic "allocate" (ppr n <+> ppr sp0 <+> ppr ret_off) else

   if (trim_sp .&. (platformWordSizeInBytes platform - 1)) /= 0  then pprPanic "allocate2" (ppr trim_sp <+> ppr final_regs <+> ppr push_sp) else

   ( stackmap { sm_regs = final_regs , sm_sp = trim_sp }
   , push_assigs ++ save_assigs )


-- -----------------------------------------------------------------------------
-- Manifesting Sp

-- | Manifest Sp: turn all the CmmStackSlots into CmmLoads from Sp.  The
-- block looks like this:
--
--    middle_pre       -- the middle nodes
--    Sp = Sp + sp_off -- Sp adjustment goes here
--    last             -- the last node
--
-- And we have some extra blocks too (that don't contain Sp adjustments)
--
-- The adjustment for middle_pre will be different from that for
-- middle_post, because the Sp adjustment intervenes.
--
manifestSp
   :: CmmConfig
   -> LabelMap StackMap  -- StackMaps for other blocks
   -> StackMap           -- StackMap for this block
   -> ByteOff            -- Sp on entry to the block
   -> ByteOff            -- SpHigh
   -> CmmNode C O        -- first node
   -> [CmmNode O O]      -- middle
   -> ByteOff            -- sp_off
   -> CmmNode O C        -- last node
   -> [CmmBlock]         -- new blocks
   -> [CmmBlock]         -- final blocks with Sp manifest

manifestSp cfg stackmaps stack0 sp0 sp_high
           first middle_pre sp_off last fixup_blocks
  = final_block : fixup_blocks'
  where
    area_off = getAreaOff stackmaps
    platform = cmmPlatform cfg

    adj_pre_sp, adj_post_sp :: CmmNode e x -> CmmNode e x
    adj_pre_sp  = mapExpDeep (areaToSp platform sp0            sp_high area_off)
    adj_post_sp = mapExpDeep (areaToSp platform (sp0 - sp_off) sp_high area_off)

    final_middle = maybeAddSpAdj cfg sp0 sp_off
                 . blockFromList
                 . map adj_pre_sp
                 . elimStackStores stack0 stackmaps area_off
                 $ middle_pre
    final_last    = optStackCheck (adj_post_sp last)

    final_block   = blockJoin first final_middle final_last

    fixup_blocks' = map (mapBlock3' (id, adj_post_sp, id)) fixup_blocks

getAreaOff :: LabelMap StackMap -> (Area -> StackLoc)
getAreaOff _ Old = 0
getAreaOff stackmaps (Young l) =
  case mapLookup l stackmaps of
    Just sm -> sm_sp sm - sm_args sm
    Nothing -> pprPanic "getAreaOff" (ppr l)


maybeAddSpAdj
  :: CmmConfig -> ByteOff -> ByteOff -> Block CmmNode O O -> Block CmmNode O O
maybeAddSpAdj cfg sp0 sp_off block =
  add_initial_unwind $ add_adj_unwind $ adj block
  where
    platform             = cmmPlatform            cfg
    do_stk_unwinding_gen = cmmGenStackUnwindInstr cfg
    adj block
      | sp_off /= 0
      = block `blockSnoc` CmmAssign (spReg platform) (cmmOffset platform (spExpr platform) sp_off)
      | otherwise = block
    -- Add unwind pseudo-instruction at the beginning of each block to
    -- document Sp level for debugging
    add_initial_unwind block
      | do_stk_unwinding_gen
      = CmmUnwind [(Sp, Just sp_unwind)] `blockCons` block
      | otherwise
      = block
      where sp_unwind = CmmRegOff (spReg platform) (sp0 - platformWordSizeInBytes platform)

    -- Add unwind pseudo-instruction right after the Sp adjustment
    -- if there is one.
    add_adj_unwind block
      | do_stk_unwinding_gen
      , sp_off /= 0
      = block `blockSnoc` CmmUnwind [(Sp, Just sp_unwind)]
      | otherwise
      = block
      where sp_unwind = CmmRegOff (spReg platform) (sp0 - platformWordSizeInBytes platform - sp_off)

{- Note [SP old/young offsets]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sp(L) is the Sp offset on entry to block L relative to the base of the
OLD area.

SpArgs(L) is the size of the young area for L, i.e. the number of
arguments.

 - in block L, each reference to [old + N] turns into
   [Sp + Sp(L) - N]

 - in block L, each reference to [young(L') + N] turns into
   [Sp + Sp(L) - Sp(L') + SpArgs(L') - N]

 - be careful with the last node of each block: Sp has already been adjusted
   to be Sp + Sp(L) - Sp(L')
-}

areaToSp :: Platform -> ByteOff -> ByteOff -> (Area -> StackLoc) -> CmmExpr -> CmmExpr

areaToSp platform sp_old _sp_hwm area_off (CmmStackSlot area n)
  = cmmOffset platform (spExpr platform) (sp_old - area_off area - n)
    -- Replace (CmmStackSlot area n) with an offset from Sp

areaToSp platform _ sp_hwm _ (CmmLit CmmHighStackMark)
  = mkIntExpr platform (toTargetInt sp_hwm)
    -- Replace CmmHighStackMark with the number of bytes of stack used,
    -- the sp_hwm.   See Note [Stack usage] in GHC.StgToCmm.Heap

areaToSp platform _ _ _ (CmmMachOp (MO_U_Lt _) args)
  | falseStackCheck args
  = zeroExpr platform
areaToSp platform _ _ _ (CmmMachOp (MO_U_Ge _) args)
  | falseStackCheck args
  = mkIntExpr platform 1
    -- Replace a stack-overflow test that cannot fail with a no-op
    -- See Note [Always false stack check]

areaToSp _ _ _ _ other = other

-- | Determine whether a stack check cannot fail.
falseStackCheck :: [CmmExpr] -> Bool
falseStackCheck [ CmmMachOp (MO_Sub _)
                      [ CmmRegOff (CmmGlobal (GlobalRegUse Sp _)) x_off
                      , CmmLit (CmmInt y_lit _)]
                , CmmReg (CmmGlobal (GlobalRegUse SpLim _))]
  = fromIntegral x_off >= y_lit
falseStackCheck _ = False

-- Note [Always false stack check]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We can optimise stack checks of the form
--
--   if ((Sp + x) - y < SpLim) then .. else ..
--
-- where are non-negative integer byte offsets.  Since we know that
-- SpLim <= Sp (remember the stack grows downwards), this test must
-- yield False if (x >= y), so we can rewrite the comparison to False.
-- A subsequent sinking pass will later drop the dead code.
-- Optimising this away depends on knowing that SpLim <= Sp, so it is
-- really the job of the stack layout algorithm, hence we do it now.
--
-- The control flow optimiser may negate a conditional to increase
-- the likelihood of a fallthrough if the branch is not taken.  But
-- not every conditional is inverted as the control flow optimiser
-- places some requirements on the predecessors of both branch targets.
-- So we better look for the inverted comparison too.

optStackCheck :: CmmNode O C -> CmmNode O C
optStackCheck n = -- Note [Always false stack check]
 case n of
   CmmCondBranch (CmmLit (CmmInt 0 _)) _true false _ -> CmmBranch false
   CmmCondBranch (CmmLit (CmmInt _ _)) true _false _ -> CmmBranch true
   other -> other


-- -----------------------------------------------------------------------------

-- | Eliminate stores of the form
--
--    Sp[area+n] = r
--
-- when we know that r is already in the same slot as Sp[area+n].  We
-- could do this in a later optimisation pass, but that would involve
-- a separate analysis and we already have the information to hand
-- here.  It helps clean up some extra stack stores in common cases.
--
-- Note that we may have to modify the StackMap as we walk through the
-- code using procMiddle, since an assignment to a variable in the
-- StackMap will invalidate its mapping there.
--
elimStackStores :: StackMap
                -> LabelMap StackMap
                -> (Area -> ByteOff)
                -> [CmmNode O O]
                -> [CmmNode O O]
elimStackStores stackmap stackmaps area_off nodes
  = go stackmap nodes
  where
    go _stackmap [] = []
    go stackmap (n:ns)
     = case n of
         CmmStore (CmmStackSlot area m) (CmmReg (CmmLocal r)) _
            | Just (_,off) <- lookupUFM (sm_regs stackmap) r
            , area_off area + m == off
            -> go stackmap ns
         _otherwise
            -> n : go (procMiddle stackmaps n stackmap) ns


-- -----------------------------------------------------------------------------
-- Update info tables to include stack liveness


setInfoTableStackMap :: Platform -> LabelMap StackMap -> CmmDecl -> CmmDecl
setInfoTableStackMap platform stackmaps (CmmProc top_info@TopInfo{..} l v g)
  = CmmProc top_info{ info_tbls = mapMapWithKey fix_info info_tbls } l v g
  where
    fix_info lbl info_tbl@CmmInfoTable{ cit_rep = StackRep _ } =
       info_tbl { cit_rep = StackRep (get_liveness lbl) }
    fix_info _ other = other

    get_liveness :: BlockId -> Liveness
    get_liveness lbl
      = case mapLookup lbl stackmaps of
          Nothing -> pprPanic "setInfoTableStackMap" (ppr lbl <+> pdoc platform info_tbls)
          Just sm -> stackMapToLiveness platform sm

setInfoTableStackMap _ _ d = d


stackMapToLiveness :: Platform -> StackMap -> Liveness
stackMapToLiveness platform StackMap{..} =
   reverse $ Array.elems $
        accumArray (\_ x -> x) True (toWords platform sm_ret_off + 1,
                                     toWords platform (sm_sp - sm_args)) live_words
   where
     live_words =  [ (toWords platform off, False)
                   | (r,off) <- nonDetEltsUFM sm_regs
                   , isGcPtrType (localRegType r) ]
                   -- See Note [Unique Determinism and code generation]

-- -----------------------------------------------------------------------------
-- Between the passes: the tagged path into a continuation
-- -----------------------------------------------------------------------------

{- Note [Tagged path skips the reloads]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a diamond (Note [diamond proc point]) the frame is set up before the
tag test, so the tagged path reaches the continuation L with every live
variable in its register, and in its slot unless stored on the call's edge only
(Note [Tagged path skips the stores]); only the call's return clobbers the
registers. Yet L reloads them all (Pass 2) on either path. So, where proc points
are not split, L's body moves to a new block L', L becomes its reloads and a
jump to L', and the tagged path jumps to L'.

On a loop, as in a traversal of evaluated data, L' takes every variable in its
register but one stored before the test and live across an inner loop that does
not read it: Pass 2 reloaded that at L each turn of the outer loop, and kept in
a register it crowds the inner one (vector's spectral, evaluating an index each
turn of its outer loop, spilled one more value to the C stack and reloaded it
every turn of the inner loop, which sums a row). That holds on a loop with at
most sixteen diamonds' continuations: on one with more, the bodies hold their
variables in registers across so many joins that the allocator spills them to
the C stack and shuffles them with xchg (nofib's puzzle, whose go1 loop holds
over sixteen continuations of fifteen to twenty variables each), and L' takes
in registers only what it takes off loops. Caps on the variables instead, by
their count or against the allocator's registers, cost vector's quickhull 11 to
13%: its loops hold one to five continuations of eight to seventeen variables
each. Elsewhere, as in a chain of evaluations of thunks, L' takes in registers
only the variables stored on the call's edge, and a diamond storing none is not
rewritten: reloaded at L, the others would be loaded on the call's path ahead of
their uses, where Pass 2's reloads at an unsplit L sink to them (megaparsec's
skipSomeTill reloaded six variables above a heap check, and spilled one of them
to the C stack). L' reloads from their slots itself (Pass 2) the variables it
does not take in registers, so that each reload sinks to its uses as at L.

That makes what L' takes in registers live at the tag test, and so possibly at
an earlier proc point, where Pass 2 would then reload it too: in a chain of
evaluations (#3294) every continuation would reload everything live. So the
diamonds are rewritten only where the reloads this adds at proc points total at
most two a diamond; failing that, those whose tag test follows no proc point
whose reloads grow; failing that, none. In a loop whose continuation leads back
to the tag test nothing grows.

The return address store moves off the tagged path too (to the edge into the
call) where the call is the only block returning to L: a canned GC routine
jumped to from L's alternatives returns through it.

The edge's block jumps to the call's, which nothing else may jump to, so
joinCallEdges joins the two where layout ends. Unjoined, each tag test adds a
block to the passes after layout, which keep maps of blocks, and at -O0, where
no later pass joins blocks, a jump wherever the code generator does not place
the call's block next.
-}

tagSkip :: CmmConfig -> ProcPointSet -> BlockId -> Bool -> LabelMap StackMap
        -> [(BlockId, BlockId, Maybe (BlockId, [LocalReg]))] -> [[BlockId]]
        -> [CmmBlock] -> LabelMap ByteOff
        -> UniqDSM (LabelMap StackMap, [CmmBlock], LabelMap StackMap, LabelMap BlockId)
-- no diamond: the inputs as given (by the general equation, the compiler ran
-- 0.23% more instructions and allocated 0.35% more compiling T9961 at -O0,
-- 0.037% more and 0.044% more compiling !16269's CmmDiamondChain at -O1, and
-- 0.028% more and 0.044% more at -O2, on GHC 10.1.20261108)
tagSkip _ _ _ _ stackmaps [] _ blocks _ = return (stackmaps, blocks, mapEmpty, mapEmpty)
tagSkip cfg procpoints entry loopy stackmaps dias0 cycles blocks lifted = do
    -- one body label for each continuation, which several diamonds may share
    -- once common block elimination has merged their continuations
    bodies <- mapM (\l -> do { l' <- newBlockId; return (l, l') })
                   (setElems (setFromList [ l | (_, l, _) <- dias0 ]))
    let body_of = mapFromList bodies
        -- the variables some diamond into l stores on the call's edge only
        lazy = mapFromListWith (++) [ (l, rs) | (_, l, Just (_, rs)) <- dias0 ]
        -- the loops, a GC call's left out: each block's strongly connected
        -- component, where layout found a back edge
        loops = mapMap fst loopsWith
        -- with each loop block's number in its loop and the loop's own
        -- numbering, built once a loop: see across
        loopsWith
          -- none where layout found no back edge (built from the cycles alone,
          -- the map made the compiler run 0.65% more instructions and allocate
          -- 0.79% more compiling T12707 at -O0, 0.35% more and 0.42% more at
          -- -O, and 0.31% more and 0.38% more at -O2, on GHC 10.1.20261108)
          | not loopy = mapEmpty
          | otherwise = mapFromList
              [ (x, (xs, (i, lix)))
              | xs <- cycles
              , let lix = loopIx xs
              , (i, x) <- zip [0 ..] xs ]
        -- a loop's blocks numbered in order, with their successors in the loop,
        -- and for each block the variables live at its entry and those it reads
        loopIx xs = (succA, liveA, readA)
          where
            n = length xs
            ix = mapFromList (zip xs [0 :: Int ..])
            succA = listArray (0, n - 1)
              [ [ j | Just b <- [mapLookup y blockmap]
                    , s <- successors (lastNode b)
                    , Just j <- [mapLookup s ix] ]
              | y <- xs ] :: Array Int [Int]
            liveA = listArray (0, n - 1) [ liveIn y | y <- xs ]
              :: Array Int LRegSet
            readA = listArray (0, n - 1)
              [ maybe emptyLRegSet reads (mapLookup y blockmap) | y <- xs ]
              :: Array Int LRegSet
        -- liveness with each diamond's continuation passing its variables
        -- through, as its split body takes them
        live = analyzeCmmBwd liveLatticeL
                 (reloadXfer platform (\l -> l `setMember` procpoints
                                             && not (mapMember l body_of))
                             mapEmpty)
                 graph_l mapEmpty
        -- the variables live across a loop inside l's loop, one avoiding l,
        -- that the inner loop does not read, as a test on l's frame variables:
        -- the inner loops are the strongly connected components of l's loop
        -- without l, found on the loop's numbering
        across l = case mapLookup l loopsWith of
          Nothing -> const False
          Just (_, (i, (succA, liveA, readA))) ->
            let comps = cyclicSccsWithout i succA
                through r ys =
                  any (\y -> r `elemLRegSet` (liveA ! y)) ys
                  && not (any (\y -> r `elemLRegSet` (readA ! y)) ys)
            in \r -> any (through r) comps
        -- a block's live-in: `live`'s own fact, but at a proc point that
        -- is no split continuation, to which `live` passes nothing
        liveIn y = case mapLookup y blockmap of
          Just b | y `setMember` procpoints, not (mapMember y body_of)
                 -> atMiddle platform b live
          _      -> mapFindWithDefault emptyLRegSet y live
        reads b = let (_, middle, last) = blockSplit b
                  in foldNodesBwdOO (gen_killL platform) middle
                                    (gen_killL platform last emptyLRegSet)
        -- the variables l's body takes in registers on the tagged path: the
        -- ones stored on the call's edge only, and on a loop the others but
        -- those live across an inner loop that does not read them
        inRegs = mapFromList
          [ (l, if null lz && not onLoop then []
                else [ r | (r, _) <- stackSlotRegs sm
                         , r `elem` lz || (onLoop && not (acr r)) ])
          | (l, _) <- bodies, Just sm <- [mapLookup l stackmaps]
          , let lz = mapFindWithDefault [] l lazy
                onLoop = fewOnLoop l
                acr = across l ]
        -- on a loop holding at most sixteen diamonds' continuations: on more,
        -- the bodies hold their variables in registers across many joins,
        -- which shuffle them (spills to the C stack and xchg in puzzle's go1;
        -- sixteen is arbitrary, and may be revised where a case needs it: with
        -- no bound, nofib's puzzle ran 3.9% more instructions at -O1 and 2.2%
        -- more at -O2, and with a bound of 0, vector's quickhull 8.4% more)
        fewOnLoop l = case mapLookup l loops of
          Nothing -> False
          Just xs -> mapFindWithDefault 0 (head xs) loopBodies <= (16 :: Int)
        loopBodies = mapFromListWith (+)
          [ (head xs, 1) | (l, _) <- bodies, Just xs <- [mapLookup l loops] ]
        regsOf l = mapFindWithDefault [] l inRegs
        cands = [ (b, l, mapFindWithDefault l l body_of) | (b, l, _) <- dias0
                , not (null (regsOf l)) ]
        -- the stack map of the variables each body reloads itself
        slottedWith regs cs = mapFromList
          [ (l', sm')
          | (_, l, l') <- cs, Just sm <- [mapLookup l stackmaps]
          , let sm' = (bodyMap l sm) { sm_regs = delListFromUFM (sm_regs sm)
                                                                (regs l) }
          , not (isNullUFM (sm_regs sm')) ]
        slotted = slottedWith regsOf
    let reloads0 = reloadSets mapEmpty graph_l
        -- the proc points whose reloads grow, and by how many variables
        grown cs = [ (p, n) | (p, s) <- mapToList (reloadSets (slotted cs)
                                         (ofBlockList entry (apply cs)))
                            , let n = sizeUniqueSet (s `differenceUniqueSet`
                                    mapFindWithDefault emptyLRegSet p reloads0)
                            , n > 0 ]
        -- at most two reloads more for each diamond rewritten
        cheap cs g = sum (map snd g) <= 2 * length cs
        grown_all = grown cands
        -- how many proc points' regions hold a block, found by walking back
        -- through the blocks that are not proc points; a large count where
        -- the walk goes far (64, bounding the walk, is arbitrary, and may be
        -- raised where a case needs more at little cost to the compiler:
        -- lifted to 1000, compiling 50 to 400 loops over lists in sequence
        -- ran 1.002 to 1.061 times the instructions, the excess growing as N
        -- squared)
        owners b = walk (64 :: Int) 0 (setSingleton b) [b]
          where walk _ n _ [] = n
                walk 0 _ _ _  = 1000
                walk k n seen (x : xs)
                  | x `setMember` procpoints = walk (k - 1) (n + 1) seen xs
                  | otherwise =
                      let new = [ y | y <- mapFindWithDefault [] x preds
                                    , not (y `setMember` seen) ]
                      in walk (k - 1) n (foldr setInsert seen new) (new ++ xs)
        preds = mapFromListWith (++) [ (s, [entryLabel blk]) | blk <- blocks
                                     , s <- successors (lastNode blk) ]
        -- the blocks reached from a proc point before any other one
        region p = go (setSingleton p) [p]
          where go seen [] = seen
                go seen (x : xs) =
                  let new = [ s | Just blk <- [mapLookup x blockmap]
                                , s <- successors (lastNode blk)
                                , not (s `setMember` procpoints)
                                , not (s `setMember` seen) ]
                  in go (foldr setInsert seen new) (new ++ xs)
        -- a bound on grown's total without the analyses: a proc point's reloads
        -- grow only by the variables taken in registers by the bodies of
        -- the diamonds whose tag tests its region holds (without the bound,
        -- the compiler ran 1.0% more instructions and allocated 0.81% more
        -- compiling nofib's puzzle at -O0, 0.53% more and 0.42% more at -O1,
        -- and 0.48% more and 0.44% more compiling T12707 at -O2, and with it
        -- only where every candidate's tag test is a proc point, owners and
        -- preds gone, 1.0% more and 0.81% more at -O0, 0.53% more and 0.42%
        -- more at -O1, and 0.43% more and 0.39% more compiling T12707 at -O2,
        -- on GHC 10.1.20261108)
        bound = sum [ length (regsOf l) * owners b | (b, l, _) <- cands ]
        accepted0
          | bound <= 2 * length cands = cands
          | cheap cands grown_all = cands
          | otherwise =
              let bad = setUnions (map (region . fst) grown_all)
                  cs' = [ c | c@(b, _, _) <- cands, not (b `setMember` bad) ]
              in if cheap cs' (grown cs') then cs' else []
        -- a continuation whose frame is trimmed is split even where not
        -- all of its diamonds are rewritten, its body then reloading every
        -- variable itself (Note [Diamond frames keep their depth])
        taken0 = setFromList [ b | (b, _, _) <- accepted0 ]
        forced = setFromList [ l | (b, l, _) <- dias0, mapMember l lifted
                                 , not (b `setMember` taken0) ]
        accepted = [ c | c@(_, l, _) <- accepted0, not (l `setMember` forced) ]
        forcedCs = [ (b, l, mapFindWithDefault l l body_of, mb_c)
                   | (b, l, mb_c) <- dias0, l `setMember` forced ]
        splits = accepted ++ [ (b, l, l') | (b, l, l', _) <- forcedCs ]
        regsFinal l | l `setMember` forced = []
                    | otherwise            = regsOf l
        stackmaps' = foldl' (\m (_, l, l') -> case mapLookup l m of
                                Just sm -> mapInsert l' (bodyMap l sm) m
                                Nothing -> m) stackmaps splits
        taken = setFromList [ b | (b, _, _) <- accepted ]
        -- the block holding a test block's last node once a diamond whose
        -- continuation it is has been rewritten
        holder b = mapFindWithDefault b b splitOf
        splitOf = mapFromList [ (l, l') | (_, l, l') <- splits ]
        -- where a refused diamond's tagged path goes once it has stored
        target l | l `setMember` forced = mapFindWithDefault l l body_of
                 | otherwise            = l
    -- a diamond left out stores on its tagged path too what it stores only
    -- on the call's: see Note [Tagged path skips the stores]
    refused <- sequence [ do { t <- newBlockId; return (b, l, c, t) }
                        | (b, l, Just (c, _)) <- dias0, not (b `setMember` taken) ]
    return ( stackmaps'
           , storeTagged holder target refused
               (applyWith splits
                  (accepted ++ [ (b, l, l')
                               | (b, l, l', Nothing) <- forcedCs ]))
           , slottedWith regsFinal splits
             -- each split body's continuation, for cafAnal
           , mapFromList [ (l', l) | (_, l, l') <- splits ] )
  where
    platform = cmmPlatform cfg
    -- the stack map a continuation's body is laid out with, deeper than its
    -- frame where the frame is trimmed
    bodyMap l sm = case mapLookup l lifted of
                     Just d  -> sm { sm_sp = d }
                     Nothing -> sm
    -- the unwind at such a frame's depth (under -g), without which its
    -- return point would be described by the Sp rule of the block laid out
    -- before it, and the Sp adjustment to its body's depth where the frame
    -- is trimmed
    entryAdj l = case (mapLookup l lifted, mapLookup l stackmaps) of
      (Just d, Just sm) ->
        maybeAddSpAdj cfg (sm_sp sm) (sm_sp sm - d) emptyBlock
      (Nothing, Just sm) -> maybeAddSpAdj cfg (sm_sp sm) 0 emptyBlock
      _                  -> emptyBlock

    -- no refused diamond: the blocks as given (by the general equation, the
    -- compiler ran 0.054% more instructions and allocated 0.057% more compiling
    -- nofib's puzzle at -O0, 0.027% more and 0.028% more at -O1, and 0.029%
    -- more and 0.029% more at -O2, on GHC 10.1.20261108)
    storeTagged _ _ [] bs = bs
    storeTagged holder target refused bs = new ++ map retarget bs
      where
        tests = mapFromList [ (holder b, (l, t)) | (b, l, _, t) <- refused ]
        -- the stores after the unwind at the depth of the test's tagged
        -- branch, as on move_ret's edge block (under -g)
        new = [ blockJoin (CmmEntry t tscp)
                          (maybeAddSpAdj cfg depth 0 (blockFromList stores))
                          (CmmBranch (target l))
              | (_, l, c, t) <- refused
              , Just cblk <- [mapLookup c blockmap]
              , let (CmmEntry _ tscp, cmid, _) = blockSplit cblk
                    stores = [ n | n@(CmmStore _ (CmmReg _) _) <- blockToList cmid ]
                    depth = case mapLookup l stackmaps of
                      Just sm -> sm_sp (bodyMap l sm)
                      Nothing -> panic "storeTagged" ]
        retarget blk
          | Just (l, t) <- mapLookup (entryLabel blk) tests
          = replaceLastNode blk (retargetTo l t (lastNode blk))
          | otherwise = blk

    graph_l = ofBlockList entry blocks
    blockmap = toBlockMap graph_l

    apply cs = applyWith cs cs
    applyWith cs tcs = concatMap one blocks
      where
        conts = mapFromList [ (l, l') | (_, l, l') <- cs ]
        tests = mapFromList [ (b, (l, l')) | (b, l, l') <- tcs ]
        one blk = case mapLookup lbl conts of
            Just l' -> [ blockJoin (CmmEntry lbl tscp) (entryAdj lbl)
                                   (CmmBranch l')
                       , blockJoin (CmmEntry l' tscp) middle last' ]
            Nothing | mapMember lbl tests -> [ blockJoin e middle last' ]
                    | otherwise           -> [blk]
          where
            lbl = entryLabel blk
            (e@(CmmEntry _ tscp), middle, last) = blockSplit blk
            -- a continuation's body may hold the next diamond's tag test
            last' = case mapLookup lbl tests of
              Just (l, l') -> retargetTo l l' last
              Nothing      -> last

    -- what Pass 2 reloads at each proc point, given the bodies' own reloads
    reloadSets :: LabelMap StackMap -> CmmGraph -> LabelMap LRegSet
    reloadSets slots g = mapFromList
      [ (entryLabel blk, atMiddle platform blk facts)
      | blk <- toBlockList g, entryLabel blk `setMember` procpoints ]
      where
        facts = analyzeCmmBwd liveLatticeL
                  (reloadXfer platform (`setMember` procpoints) slots)
                  g mapEmpty

-- | The variables live at the start of a block's middle nodes
atMiddle :: Platform -> CmmBlock -> FactBase LRegSet -> LRegSet
atMiddle platform blk fb =
  let (_, middle, last) = blockSplit blk
      joined = gen_killL platform last $! joinOutFacts liveLatticeL last fb
  in foldNodesBwdOO (gen_killL platform) middle joined

-- | Pass 2's liveness, as insertReloadsAsNeeded's rewriteCC passes it on:
-- nothing into a block the predicate holds of, and into a split body
-- the variables live at its middle's start that its stack map does not reload
reloadXfer :: Platform -> (BlockId -> Bool) -> LabelMap StackMap
           -> TransferFun LRegSet
reloadXfer platform stops slots = \blk fb ->
  let l = entryLabel blk
      live = atMiddle platform blk fb
  in mapSingleton l $!
       if stops l then emptyLRegSet
       else case mapLookup l slots of
              Just sm -> filterUniqueSet
                           (\u -> not (u `elemUFM_Directly` sm_regs sm)) live
              Nothing -> live
{-# INLINE reloadXfer #-}

-- | The strongly connected components holding a cycle of the graph on
-- 0 .. n-1 with the given successors, node x and its edges left out
-- (Tarjan's algorithm; by GHC.Data.Graph.Directed.Internal's scc instead, the
-- compiler ran 0.003% more instructions and allocated 0.001% more compiling
-- nofib's puzzle at -O0, 0.025% more and 0.027% more at -O1, and 0.14% more and
-- 0.17% more at -O2, on GHC 10.1.20261108)
--
-- TODO: Tarjan's algorithm here can serve as a general strongly connected
-- components function with Data.Graph.scc's documented contract: any graph,
-- every component, each as the tree of its depth-first search's edges, in
-- reverse topological order (the order the algorithm finds them in), in O(V+E)
-- time, scc documenting no laziness; the trees' shapes and the order of
-- components neither of which reaches the other, which scc leaves unspecified,
-- may differ from its present ones. Propose it for containers' scc, which
-- GHC.Data.Graph.Directed.Internal.scc wraps, and once GHC's containers carries
-- it, find these components by that scc on the graph with x's edges left out,
-- keeping those with a cycle (the caller ORs over them, so their order does not
-- matter), and remove this hand-written version.
cyclicSccsWithout :: Int -> Array Int [Int] -> [[Int]]
cyclicSccsWithout x succA = runST $ do
  let n = rangeSize (bounds succA)
  idx <- newArray (0, n - 1) (-1) :: ST s (STUArray s Int Int)
  low <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Int)
  on  <- newArray (0, n - 1) False :: ST s (STUArray s Int Bool)
  counter <- newSTRef (0 :: Int)
  stack <- newSTRef []
  out <- newSTRef []
  let visit v = do
        c <- readSTRef counter
        writeSTRef counter $! c + 1
        writeArray idx v c
        writeArray low v c
        modifySTRef' stack (v :)
        writeArray on v True
        mapM_ (\w -> when (w /= x) $ do
                 iw <- readArray idx w
                 if iw < 0
                   then do visit w
                           lw <- readArray low w
                           lv <- readArray low v
                           writeArray low v (min lv lw)
                   else do o <- readArray on w
                           when o $ do lv <- readArray low v
                                       writeArray low v (min lv iw))
              (succA ! v)
        lv <- readArray low v
        when (lv == c) $ do
          let pop acc = do
                st <- readSTRef stack
                case st of
                  w : rest -> do writeSTRef stack rest
                                 writeArray on w False
                                 if w == v then return (w : acc)
                                           else pop (w : acc)
                  [] -> return acc
          comp <- pop []
          let cyclic = case comp of
                [u] -> u `elem` (succA ! u)
                _   -> True
          when cyclic $ modifySTRef' out (comp :)
  mapM_ (\v -> when (v /= x) $ do
           iv <- readArray idx v
           when (iv < 0) (visit v))
        [0 .. n - 1]
  readSTRef out

{- Note [Diamond frames keep their depth]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A diamond's frame (Note [diamond proc point]) is set up before the tag test,
Sp moved on both paths. Trimmed to its live variables, as a call's frame is,
it moves Sp whenever two diamonds in a loop differ in depth, on the evaluated
path too: up at one, down at the next. So a diamond's frame keeps the stack's
depth, trimming at most the word its return address takes, and the dead
slots stay as holes.

The frame the call pushes is trimmed to its live variables all the same, where
proc points are not split, every branch into the continuation L is a diamond's
tag test, every call returning to L passes only the return address on the stack,
and either the procedure is called by a thunk of its group, as a lazy list's
producer is by the thunk of the list's tail, or the holes are at least half
as many words as the live variables and the procedure is a thunk or L is on
no loop, GC calls left out: the call can then evaluate a thunk that
leads back to such a frame, and each level of the recursion would hold the holes
(nofib's primes, whose nested lazy filters stood twice as deep and crossed the
stack's chunks, and multiplier, whose gates evaluate their inputs in frames
holding a return address and a hole). liftFrames trims it after layout: L's
info table describes the trimmed frame, whose youngest word holds L's return
address; each call returning to L moves Sp up to it, and L moves Sp back down to
the stack's depth before jumping to its body, which tagSkip splits off L even
where the body takes no variable in a register (Note [Tagged path skips the
reloads]). The tagged path, jumping to the body, moves Sp nowhere. Elsewhere
the holes stay in the frame, described as non-pointers: trimmed, the frames in
loops consuming a list, as knights' notElem's and base's eqString's, cost two
instructions an evaluation, 4% of knights and 2.7% of hartel/typecheck.
-}

-- | Which diamonds' frames liftFrames trims, among those it can: see Note
-- [Diamond frames keep their depth]
data LiftFrames
  = LiftAll      -- ^ every one
  | LiftSized    -- ^ those whose holes are at least half the live words
  | LiftOffLoop  -- ^ those, where the continuation is on no loop

-- | The frames the calls into a diamond's continuation push, trimmed below
-- the depth its body keeps, with their stack maps, and for each continuation
-- so trimmed that depth. See Note [Diamond frames keep their depth]
liftFrames :: CmmConfig -> LiftFrames
           -> [(BlockId, BlockId, Maybe (BlockId, [LocalReg]))]
           -> LabelMap StackMap -> [[BlockId]] -> [CmmBlock]
           -> (LabelMap StackMap, [CmmBlock], LabelMap ByteOff)
liftFrames cfg which dias stackmaps cycles blocks
  -- nothing trimmed: the inputs as given (by the general equation, the compiler
  -- ran 1.6% more instructions and allocated 1.5% more compiling T3294 at -O0,
  -- 0.58% more and 0.48% more at -O, and 0.57% more and 0.47% more at -O2, on
  -- GHC 10.1.20261108)
  | mapNull lifts = (stackmaps, blocks, mapEmpty)
  | otherwise
  = ( mapFoldlWithKey (\m l (t, _) -> mapAdjust (\sm -> sm { sm_sp = t }) l m)
                      stackmaps lifts
    , map liftBlock blocks
    , mapMap snd lifts )
  where
    platform = cmmPlatform cfg
    wordsz = platformWordSizeInBytes platform
    tests = mapFromListWith setUnion [ (l, setSingleton b) | (b, l, _) <- dias ]
    -- the continuations a block other than their diamonds' tests branches
    -- into, a foreign call counting as a branch into its continuation, and
    -- those a call returns to with stack arguments other than a word
    badInto = setFromList [ s | b <- blocks, let n = lastNode b
                              , isBranchNode n || isForeignCall n
                              , s <- successors n, Just bs <- [mapLookup s tests]
                              , not (entryLabel b `setMember` bs) ]
    isForeignCall CmmForeignCall{} = True
    isForeignCall _                = False
    badArgs = setFromList [ l | b <- blocks
                              , CmmCall{ cml_cont = Just l, cml_args = a } <- [lastNode b]
                              , mapMember l tests, a /= wordsz ]
    -- the blocks on a loop, a GC call's left out
    on_loop = setFromList (concat cycles)
    lifts = mapFromList
      [ (l, (t, d))
      | l <- mapKeys tests
      , not (l `setMember` badInto)
      , not (l `setMember` badArgs)
      , Just sm <- [mapLookup l stackmaps]
      , sm_args sm == wordsz
      , let d = sm_sp sm
            -- the frame's youngest word holding a variable or the update
            -- frame, and the return address below it
            t = wordsz + nonDetStrictFoldUFM (\(_, o) m -> max o m)
                                             (sm_ret_off sm) (sm_regs sm)
            dead = (d - t) `quot` wordsz
            live = (t - wordsz - sm_ret_off sm) `quot` wordsz
      , t < d
      , case which of
          LiftAll     -> True
          LiftSized   -> 2 * dead >= live
          -- the size rule first, so that on_loop is computed only where a frame
          -- passes it (with on_loop tested first, the compiler ran 0.16% more
          -- instructions and allocated 0.2% more compiling T12707 at -O, 0.15%
          -- more and 0.19% more at -O2, and nothing more at -O0, on GHC
          -- 10.1.20261108)
          LiftOffLoop -> 2 * dead >= live && not (l `setMember` on_loop) ]
    -- a block neither calling nor storing the return address of such a
    -- continuation stays as it is (every block rebuilt, the compiler ran 0.014%
    -- fewer instructions and allocated 0.049% more compiling nofib's puzzle at
    -- -O0, no more and 0.021% more at -O1, and 0.004% fewer and 0.067% more at
    -- -O2, on GHC 10.1.20261108)
    liftBlock blk
      | not calls, not (foldBlockNodesF lifted mid False) = blk
      | otherwise = blockJoin e (blockFromList (map retStore (blockToList mid)
                                                ++ adj)) lst'
      where
        (e, mid, lst) = blockSplit blk
        calls = case lst of
          CmmCall{ cml_cont = Just l } -> mapMember l lifts
          _                            -> False
        lifted :: CmmNode e x -> Bool -> Bool
        lifted n acc = acc || case n of
          CmmStore addr (CmmLit (CmmBlock l)) _ ->
            isSpRel addr && mapMember l lifts
          _ -> False
        (adj, lst') = case lst of
          CmmCall{ cml_cont = Just l, cml_args = a }
            | Just (t, d) <- mapLookup l lifts
            -> ( blockToList (maybeAddSpAdj cfg (d - wordsz + a) (d - t)
                                            emptyBlock)
               , mapExpDeep (shiftSp (d - t)) lst )
          _ -> ([], lst)
    -- the return address goes to the trimmed frame's youngest word
    retStore n@(CmmStore addr v@(CmmLit (CmmBlock l)) al)
      | isSpRel addr, Just (t, d) <- mapLookup l lifts
      = CmmStore (cmmOffset platform addr (d - t)) v al
      | otherwise = n
    retStore n = n
    isSpRel (CmmRegOff r _) = r == spReg platform
    isSpRel (CmmReg r)      = r == spReg platform
    isSpRel _               = False
    shiftSp k (CmmRegOff r o) | r == spReg platform = CmmRegOff r (o - k)
    shiftSp k (CmmReg r)      | r == spReg platform = CmmRegOff r (- k)
    shiftSp _ x = x

{- Note [Tagged path skips the stores]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A diamond (Note [diamond proc point]) stores the variables live in its
continuation L before the tag test. A variable that at most one later call needs
in a slot before it is redefined, such as a loop's index, is stored on the
call's edge only, where the call is the only block returning to L (as the return
address is, Note [Tagged path skips the reloads]). L's body then does not assume
it in its slot, so that one later call stores it on its own path. A variable
more later calls need is stored before the test as before, so that they share
the store (#3294's chain of evaluations), and so is one that L's code saves at
the first call it reaches without a conditional branch, within eight blocks,
not redefining it on the way: the tagged path would store it there anyway, and
the call's path would reload it at L to store it again, the body holding it in
a register at its join (nofib's fasta, whose loop then carried its seed in the
exit's register, two moves an iteration). That store can keep the call's frame
deeper than the call needs. In two places the stores are therefore all made only
where that call's frame, set up there, is no deeper with them all, and otherwise
each only where that frame has its variable in the slot L's frame gives it, in
allocate's order for the call or, where the frame is then no deeper and its
youngest slot of a variable that a later non-tail call of the procedure itself
needs, and of one that two or more such calls need, lies no younger than in that
order, in the plain one: in a procedure that makes a non-tail call of itself,
each level of whose recursion would hold the deeper frame (containers' PostOrder
sum, a node's value kept in a slot older than its children's frame), and in
a diamond on a cycle of its procedure's blocks, blocks reached only through
unlikely branches left out, that makes no non-tail call into the procedure's
recursion (its strongly connected component in its Cmm group's call graph). On
such a cycle the store would make the back edges of layout's first run arrive at
another depth than those of its second, so that the loop's seed (Note [Loop head
stack maps]) would not hold and the loop would set up and drop its frame every
iteration (the foldl' loops of containers' insert and delete benchmarks). Where
every cycle through the diamond makes such a call, the loop turns about once an
entry, and an Sp move an iteration costs less than a seeded frame, set up at
each entry and dropped at each exit (containers' Tree folds, whose loop over a
node's children recurses into each). A diamond whose tagged path still goes to
L, whose reloads read every slot, stores on that path too. Not where proc points
are split.

The cycles here are those of the blocks before layout; the loops of Note [Tagged
path skips the reloads] and Note [Diamond frames keep their depth] are those
after it, where a fixup block on the unlikely side of a branch is reached only
through unlikely branches and so cuts the cycles through it. The two can differ
safely: the first decides only which variables are stored before the test, the
second only which the body takes in registers and which frames are trimmed, and
the body takes in a register, or the tagged path stores, every variable stored
on the call's edge only.
-}

-- | For each block, the variables that the calls the predicate gives after its
-- entry need in a slot before they are redefined, with how many such calls: 1,
-- or 2 for more, the other calls passing the counts after them through
laterSavesBy :: (CmmNode O C -> Bool) -> (BlockId -> LocalRegSet) -> BlockId
             -> LabelMap CmmBlock -> LabelMap (Map.Map LocalReg Int)
laterSavesBy counts liveness entry blockmap
  = analyzeCmmBwd lattice xfer (ofBlockMap entry blockmap) mapEmpty
  where
    lattice = DataflowLattice Map.empty joinF
    joinF (OldFact old) (NewFact new)
      | Map.isSubmapOfBy (<=) new old = NotChanged old
      | otherwise = Changed $! Map.unionWith max old new
    xfer blk fb = mapSingleton (entryLabel blk) $!
        foldNodesBwdOO mid middle (atLast last)
      where
        (_, middle, last) = blockSplit blk
        fact l = mapFindWithDefault Map.empty l fb
        atLast :: CmmNode O C -> Map.Map LocalReg Int
        atLast n = case callContinuation_maybe n of
          Just k | counts n  -> atCall k
                 | otherwise -> fact k
          Nothing -> Map.unionsWith max (map fact (successors n))
        atCall k =
          Map.fromSet (\r -> min 2 (1 + Map.findWithDefault 0 r (fact k)))
                      (liveness k)
        mid :: CmmNode O O -> Map.Map LocalReg Int -> Map.Map LocalReg Int
        mid n f = foldRedefined Map.delete n f

-- | A block's label, its last node, the variables its middle redefines, those
-- live on entry to it, and its successors' Steps, all but the label found where
-- a search first needs them
data Step = Step BlockId (CmmNode O C) LRegSet CmmLocalLive [Step]

-- | The blocks' 'Step's
blockSteps :: LabelMap CmmLocalLive -> LabelMap CmmBlock -> LabelMap Step
blockSteps liveness blockmap = steps
  where
    steps = mapMapWithKey step blockmap
    step l b =
      let (_, middle, last) = blockSplit b
      in Step l last
              (foldNodesBwdOO (foldRedefined insertLRegSet) middle emptyLRegSet)
              (mapFindWithDefault emptyLRegSet l liveness)
              [ st | s <- successors last, Just st <- [mapLookup s steps] ]

-- | How many calls after a block's entry need a variable in a slot before it is
-- redefined: 0, 1, or 2 for more, found by a search from the block's Step over
-- the Steps paired with the calls counted on the way there, each path ending
-- where the variable is redefined, at a call whose continuation does not need
-- it, or, unless a safe foreign call defines it, where it is dead
laterSearch :: Set.Set LocalReg -> Step -> LocalReg -> Int
laterSearch fc_res st0 r = go setEmpty setEmpty [(st0, False)] 0
  where
    prune = not (r `Set.member` fc_res)
    -- the blocks entered with no call counted and with one, the blocks to
    -- enter, and the most calls counted
    go :: LabelSet -> LabelSet -> [(Step, Bool)] -> Int -> Int
    go _ _ [] best = best
    go s0 s1 ((Step l last defs live succs, one) : rest) best
      | l `setMember` (if one then s1 else s0) = go s0 s1 rest best
      | prune, not (r `elemLRegSet` live) = go s0 s1 rest best
      | r `elemLRegSet` defs = go s0' s1' rest best
      | Just _ <- callContinuation_maybe last = case succs of
          [c@(Step _ _ _ c_live _)]
            | r `elemLRegSet` c_live
            -> if one then 2 else go s0' s1' ((c, True) : rest) (max best 1)
          _ -> go s0' s1' rest best
      | otherwise = go s0' s1' ([ (s, one) | s <- succs ] ++ rest) best
      where
        s0' = if one then s0 else setInsert l s0
        s1' = if one then setInsert l s1 else s1

-- -----------------------------------------------------------------------------
-- Pass 2
-- -----------------------------------------------------------------------------

insertReloadsAsNeeded
    :: Platform
    -> ProcPointSet
    -> LabelMap StackMap
    -> LabelMap StackMap  -- split bodies' own reloads
    -> BlockId
    -> [CmmBlock]
    -> UniqDSM [CmmBlock]
insertReloadsAsNeeded platform procpoints final_stackmaps bodies entry blocks =
    toBlockList . fst <$>
        rewriteCmmBwd liveLatticeL rewriteCC (ofBlockList entry blocks) mapEmpty
  where
    rewriteCC :: RewriteFun CmmLocalLive
    rewriteCC (BlockCC e_node middle0 x_node) fact_base0 = do
        let entry_label = entryLabel e_node
            stackmap = case mapLookup entry_label final_stackmaps of
                Just sm -> sm
                Nothing -> panic "insertReloadsAsNeeded: rewriteCC: stackmap"

            -- Merge the liveness from successor blocks and analyse the last
            -- node.
            joined = gen_killL platform x_node $!
                         joinOutFacts liveLatticeL x_node fact_base0
            -- What is live at the start of middle0.
            live_at_middle0 = foldNodesBwdOO (gen_killL platform) middle0 joined

            -- If this is a procpoint we need to add the reloads, but only if
            -- they're actually live. Furthermore, nothing is live at the entry
            -- to a proc point.
            (middle1, live_with_reloads)
                | entry_label `setMember` procpoints
                = let reloads = insertReloads platform stackmap live_at_middle0
                  in (withReloads reloads middle0, emptyLRegSet)
                -- A body split off a continuation reloads itself the
                -- variables in their slots on both paths, Note [Tagged path
                -- skips the reloads]
                | Just sm <- mapLookup entry_label bodies
                = let reloads = insertReloads platform sm live_at_middle0
                  in ( withReloads reloads middle0
                     , filterUniqueSet
                         (\r -> not (r `elemUFM_Directly` sm_regs sm))
                         live_at_middle0 )
                | otherwise
                = (middle0, live_at_middle0)

            -- Final liveness for this block.
            !fact_base2 = mapSingleton entry_label live_with_reloads

        return (BlockCC e_node middle1 x_node, fact_base2)

insertReloads :: Platform -> StackMap -> CmmLocalLive -> [CmmNode O O]
insertReloads platform stackmap live =
     [ CmmAssign (CmmLocal reg)
                 -- This cmmOffset basically corresponds to manifesting
                 -- @CmmStackSlot Old sp_off@, see Note [SP old/young offsets]
                 (CmmLoad (cmmOffset platform (spExpr platform) (sp_off - reg_off))
                          (localRegType reg)
                          NaturallyAligned)
     | (reg, reg_off) <- stackSlotRegs stackmap
     , reg `elemLRegSet` live
     ]
   where
     sp_off = sm_sp stackmap

-- | The reloads put at the start of a block's middle, after the unwind
-- it starts with under -g ('maybeAddSpAdj'): before it, they would run under
-- the Sp rule of the block laid out before this one, which at a return point
-- would also describe its return address (Note [Info Offset] in
-- "GHC.CmmToAsm.Dwarf.Types"), as at -O0, where no sinking moves them.
withReloads :: [CmmNode O O] -> Block CmmNode O O -> Block CmmNode O O
withReloads reloads middle = foldr reload middle reloads
  where
    -- one foldr, which fuses with a list comprehension giving the reloads
    reload r b = case b of
      BCons u@(CmmUnwind {}) rest -> u `BCons` (r `blockCons` rest)
      BMiddle u@(CmmUnwind {})    -> u `BCons` (r `BCons` BNil)
      BNil                        -> BMiddle r
      _                           -> r `BCons` b
{-# INLINE withReloads #-}

-- -----------------------------------------------------------------------------
-- Lowering safe foreign calls

{-
Note [Lower safe foreign calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We start with

   Sp[young(L1)] = L1
 ,-----------------------
 | r1 = foo(x,y,z) returns to L1
 '-----------------------
 L1:
   R1 = r1 -- copyIn, inserted by mkSafeCall
   ...

the stack layout algorithm will arrange to save and reload everything
live across the call.  Our job now is to expand the call so we get

   Sp[young(L1)] = L1
 ,-----------------------
 | SAVE_THREAD_STATE()
 | token = suspendThread(BaseReg, interruptible)
 | r = foo(x,y,z)
 | BaseReg = resumeThread(token)
 | LOAD_THREAD_STATE()
 | R1 = r  -- copyOut
 | jump Sp[0]
 '-----------------------
 L1:
   r = R1 -- copyIn, inserted by mkSafeCall
   ...

Note the copyOut, which saves the results in the places that L1 is
expecting them (see Note [safe foreign call convention]). Note also
that safe foreign call is replace by an unsafe one in the Cmm graph.
-}

lowerSafeForeignCall :: Profile -> CmmBlock -> UniqDSM CmmBlock
lowerSafeForeignCall profile block
  | (entry@(CmmEntry _ tscp), middle, CmmForeignCall { .. }) <- blockSplit block
  = do
    let platform = profilePlatform profile
    -- Both 'id' and 'new_base' are KindNonPtr because they're
    -- RTS-only objects and are not subject to garbage collection
    id <- newTemp (bWord platform)
    new_base <- newTemp (cmmRegType $ baseReg platform)
    let (caller_save, caller_load) = callerSaveVolatileRegs platform
    save_state_code <- saveThreadState profile
    load_state_code <- loadThreadState profile
    let suspend = save_state_code  <*>
                  caller_save <*>
                  mkMiddle (callSuspendThread platform id intrbl)
        midCall = mkUnsafeCall tgt res args
        resume  = mkMiddle (callResumeThread new_base id) <*>
                  -- Assign the result to BaseReg: we
                  -- might now have a different Capability!
                  mkAssign (baseReg platform) (CmmReg (CmmLocal new_base)) <*>
                  caller_load <*>
                  load_state_code

        (_, regs, copyout) =
             copyOutOflow profile NativeReturn Jump (Young succ)
                            (map (CmmReg . CmmLocal) res)
                            ret_off []

        -- NB. after resumeThread returns, the top-of-stack probably contains
        -- the stack frame for succ, but it might not: if the current thread
        -- received an exception during the call, then the stack might be
        -- different.  Hence we continue by jumping to the top stack frame,
        -- not by jumping to succ.
        jump = CmmCall { cml_target    = entryCode platform $
                                         cmmLoadBWord platform (spExpr platform)
                       , cml_cont      = Just succ
                       , cml_args_regs = regs
                       , cml_args      = widthInBytes (wordWidth platform)
                       , cml_ret_args  = ret_args
                       , cml_ret_off   = ret_off }

    graph' <- lgraphOfAGraph ( suspend <*>
                               midCall <*>
                               resume  <*>
                               copyout <*>
                               mkLast jump, tscp)

    case toBlockList (removeDetermGraph graph') of
      [one] -> let (_, middle', last) = blockSplit one
               in return (blockJoin entry (middle `blockAppend` middle') last)
      _ -> panic "lowerSafeForeignCall0"

  -- Block doesn't end in a safe foreign call:
  | otherwise = return block


callSuspendThread :: Platform -> LocalReg -> Bool -> CmmNode O O
callSuspendThread platform id intrbl =
  CmmUnsafeForeignCall (PrimTarget MO_SuspendThread)
       [id] [baseExpr platform, mkIntExpr platform (toTargetInt (fromEnum intrbl))]

callResumeThread :: LocalReg -> LocalReg -> CmmNode O O
callResumeThread new_base id =
  CmmUnsafeForeignCall (PrimTarget MO_ResumeThread)
       [new_base] [CmmReg (CmmLocal id)]

-- -----------------------------------------------------------------------------

plusW :: Platform -> ByteOff -> WordOff -> ByteOff
plusW platform b w = b + w * platformWordSizeInBytes platform

data StackSlot = Occupied | Empty
     -- Occupied: a return address or part of an update frame

instance Outputable StackSlot where
  ppr Occupied = text "XXX"
  ppr Empty    = text "---"

dropEmpty :: WordOff -> [StackSlot] -> Maybe [StackSlot]
dropEmpty 0 ss           = Just ss
dropEmpty n (Empty : ss) = dropEmpty (n-1) ss
dropEmpty _ _            = Nothing

isEmpty :: StackSlot -> Bool
isEmpty Empty = True
isEmpty _ = False

localRegBytes :: Platform -> LocalReg -> ByteOff
localRegBytes platform r
    = roundUpToWords platform (widthInBytes (typeWidth (localRegType r)))

localRegWords :: Platform -> LocalReg -> WordOff
localRegWords platform = toWords platform . localRegBytes platform

toWords :: Platform -> ByteOff -> WordOff
toWords platform x = x `quot` platformWordSizeInBytes platform


stackSlotRegs :: StackMap -> [(LocalReg, StackLoc)]
stackSlotRegs sm = nonDetEltsUFM (sm_regs sm)
  -- See Note [Unique Determinism and code generation]

{-# LANGUAGE RecordWildCards #-}
module GHC.Cmm.LayoutStack (
       cmmLayoutStack, setInfoTableStackMap, mwHas, isGcTarget
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
import GHC.Cmm.CLabel ( CLabel, mkCmmCodeLabel )
import GHC.Unit.Types ( rtsUnitId )
import GHC.Data.FastString ( fsLit )
import GHC.Cmm.Utils
import GHC.Cmm.Graph
import GHC.Cmm.Liveness
import GHC.Cmm.ProcPoint
import GHC.Runtime.Heap.Layout
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Data.Maybe
import GHC.Data.Graph.Directed ( Node(..), flattenSCC
                               , stronglyConnCompFromEdgedVerticesUniq )
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSM
import GHC.Utils.Misc

import GHC.Utils.Outputable hiding ( isEmpty )
import GHC.Utils.Panic
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Fix
import Data.Array as Array
import Data.List (nub, partition, sortOn)
import Data.List.NonEmpty ( NonEmpty (..) )
import System.IO.Unsafe ( unsafePerformIO )
import System.Environment ( lookupEnv )

-- | Probe switch: the comma-separated words of GHC_MW_VARIANT.
{-# NOINLINE mwVariant #-}
mwVariant :: [String]
mwVariant = unsafePerformIO $
  words . map (\c -> if c == ',' then ' ' else c) . fromMaybe "g8,ra4,ch8,lv12,lo,lor,mr,lo2,ln32,rfa"
    <$> lookupEnv "GHC_MW_VARIANT"

mwHas :: String -> Bool
mwHas w = w `elem` mwVariant

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


cmmLayoutStack :: CmmConfig -> Maybe CLabel -> ProcPointSet -> ByteOff
               -> LabelMap Label
               -> CmmGraph -> UniqDSM (CmmGraph, LabelMap StackMap)
cmmLayoutStack cfg self_lbl procpoints entry_args after
               graph@(CmmGraph { g_entry = entry })
  = do
    -- We need liveness info. Dead assignments are removed later
    -- by the sinking pass.
    let liveness = cmmLocalLiveness platform graph
        blocks   = layoutOrder entry after (revPostorder graph)
        profile  = cmmProfile   cfg
        platform = profilePlatform profile
        run seeds keeps =
          mfix $ \ ~(rec_stackmaps, rec_high_sp, _new_blocks, _props, _dias) ->
            layout cfg procpoints liveness entry entry_args seeds keeps
                   rec_stackmaps rec_high_sp blocks

    -- See Note [Loop head stack maps]
    r1@(stackmaps1, _, _, props1, dias1) <- run mapEmpty setEmpty
    let seeds0 = loopHeadSeeds self_lbl entry procpoints stackmaps1 props1
                  [ b | (b, _, _) <- dias1 ] blocks
        -- (probe, rf) See Note [Reserved frames in loops]
        (seeds, keeps)
          | mwHas "rf" || mwHas "rfx" || mwHas "rfi" || mwHas "rfa" = reservedSeeds platform self_lbl entry procpoints liveness
                           stackmaps1 props1 [ b | (b, _, _) <- dias1 ]
                           blocks seeds0
          | otherwise  = (seeds0, setEmpty)
    (final_stackmaps0, _final_high_sp, new_blocks0, _, diamonds) <-
        if mapNull seeds then return r1 else do
          r2@(_, _, _, props2, _) <- run seeds keeps
          if seedsHold seeds props2 then return r2
          -- (probe, rf) reserved seeds that do not hold do not take
          -- loopHeadSeeds' own with them
          else if mapNull seeds0 || mapSize seeds0 == mapSize seeds
            then return r1
          else do
            r3@(_, _, _, props3, _) <- run seeds0 setEmpty
            return $ if seedsHold seeds0 props3 then r3 else r1

    -- See Note [Tagged path skips the reloads]
    (final_stackmaps, new_blocks) <-
        if cmmSplitProcPoints cfg
          then return (final_stackmaps0, new_blocks0)
          else tagSkip platform procpoints entry final_stackmaps0 diamonds
                       new_blocks0

    blocks_with_reloads <-
        insertReloadsAsNeeded platform procpoints final_stackmaps entry new_blocks
    new_blocks' <- mapM (lowerSafeForeignCall profile) blocks_with_reloads
    return (ofBlockList entry new_blocks', final_stackmaps)

-- | The order in which layout visits the blocks: reverse postorder, but
-- a block named in @after@ only once the block it names is visited, and
-- every block only once one of its predecessors is (probe; GHC_MW_VARIANT
-- dup, see Note [Copies of chained tag tests] in GHC.Cmm.Pipeline).
layoutOrder :: BlockId -> LabelMap Label -> [CmmBlock] -> [CmmBlock]
layoutOrder entry after blocks
  | mapNull after = blocks
  | otherwise = go setEmpty blocks
  where
    preds = mapFromListWith (++) [ (s, [entryLabel b]) | b <- blocks
                                                       , s <- successors b ]
    ready done strict b =
      let l = entryLabel b
      in (l == entry || any (`setMember` done) (mapFindWithDefault [] l preds))
         && (not strict || maybe True (`setMember` done) (mapLookup l after))
    go _ [] = []
    go done bs = case break (ready done True) bs of
      (pre, b : post) -> b : go (setInsert (entryLabel b) done) (pre ++ post)
      (_, []) -> case break (ready done False) bs of
        (pre, b : post) -> b : go (setInsert (entryLabel b) done) (pre ++ post)
        (_, []) -> bs


-- | A call of one of the canned GC entry points (GHC.StgToCmm.Heap).
isGcTarget :: CmmExpr -> Bool
isGcTarget (CmmLit (CmmLabel l)) = l `elem` gcLabels
  where gcLabels = map (mkCmmCodeLabel rtsUnitId . fsLit)
          [ "stg_gc_noregs", "stg_gc_unpt_r1", "stg_gc_f1", "stg_gc_d1"
          , "stg_gc_unbx_r1", "stg_gc_l1", "stg_gc_pp", "stg_gc_pi"
          , "stg_gc_ip", "stg_gc_ii", "stg_gc_ppp", "stg_gc_bpp"
          , "stg_gc_pppp", "stg_gc_ppppp" ]
isGcTarget _ = False

-- -----------------------------------------------------------------------------
-- Pass 1
-- -----------------------------------------------------------------------------

layout :: CmmConfig
       -> LabelSet                      -- proc points
       -> LabelMap CmmLocalLive         -- liveness
       -> BlockId                       -- entry
       -> ByteOff                       -- stack args on entry
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
          , [(BlockId, BlockId, Maybe BlockId)]
            -- [out] diamonds: block, continuation, the call's edge block
            -- if it holds stores the tagged path skips
          )

layout cfg procpoints liveness entry entry_args seeds keeps final_stackmaps final_sp_high blocks
  = go blocks (mapUnion init_stackmap seeds) entry_args [] setEmpty mapEmpty []
       mapEmpty
  where
    -- See Note [Tagged path skips the stores]
    need = laterSaves liveness entry blocks
    lazy_ok = not (cmmSplitProcPoints cfg)

    (updfr, cont_info)  = collectContInfo blocks

    -- the blocks by label, and how many blocks return to each continuation
    blockmap = mapFromList [ (entryLabel b, b) | b <- blocks ]
    cont_callers = foldl' (\m l -> mapInsertWith (+) l (1 :: Int) m) mapEmpty
                     [ l | b <- blocks, Just l <- [contOf (lastNode b)]
                         -- (probe, mr) a call storing its own return
                         -- address does not rely on the diamond's
                         , not (mwHas "mr"
                                && any (isRetStore l)
                                       (case blockSplit b of (_, m, _) -> blockToList m)) ]
    -- See Note [Loop head stack maps]: the blocks reached only through
    -- an unlikely branch, as a heap or stack check's call to the GC and its
    -- continuation are, each the only successor of the one before
    preds = foldl' (\m l -> mapInsertWith (+) l (1 :: Int) m) mapEmpty
              [ s | b <- blocks, s <- successors (lastNode b) ]
    cold = coldFrom setEmpty
             [ u | b <- blocks
                 , CmmCondBranch{ cml_true = t, cml_false = f
                                , cml_likely = Just lk } <- [lastNode b]
                 , let u = if lk then f else t
                 , mapLookup u preds == Just 1 ]
    coldFrom seen [] = seen
    coldFrom seen (x : xs)
      | x `setMember` seen = coldFrom seen xs
      | otherwise = coldFrom (setInsert x seen)
          ([ s | Just blk <- [mapLookup x blockmap]
               , s <- successors (lastNode blk)
               , mapLookup s preds == Just 1 ] ++ xs)
    contOf :: CmmNode O C -> Maybe BlockId
    contOf CmmCall{ cml_cont = Just l } = Just l
    contOf CmmForeignCall{ succ = l }   = Just l
    contOf _                            = Nothing

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
       -> [(BlockId, BlockId, Maybe BlockId)]
       -> LabelMap [LocalReg]
       -> UniqDSM ( LabelMap StackMap, StackLoc, [CmmBlock]
                  , LabelMap [Maybe StackMap]
                  , [(BlockId, BlockId, Maybe BlockId)] )
    go [] acc_stackmaps acc_hwm acc_blocks _done acc_props acc_dias _lazies
      = return (acc_stackmaps, acc_hwm, acc_blocks, acc_props, acc_dias)

    go (b0 : bs) acc_stackmaps acc_hwm acc_blocks done acc_props acc_dias
       lazies
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
           <- handleLastNode cfg procpoints liveness cont_info keeps cont_callers
                             blockmap need lazy_ok acc_stackmaps stack1 tscope
                             middle0 last0

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
             Just (l, True, _, _) -> filter (not . isRetStore l) (blockToList middle0)
             _              -> blockToList middle0
           middle_pre = middle0' ++ middle1

       -- See Note [Loop head stack maps]
       let done' = setInsert entry_lbl done
           back  = [ l | isBranchNode last0
                       , not (entry_lbl `setMember` cold)
                       , l <- successors last0
                       , l `setMember` done' ]
           prop l
             | isJust mb_dia = Nothing
             | otherwise     = Just stack1 { sm_regs = filterUFM is_live
                                                         (sm_regs stack1) }
             where live = mapFindWithDefault emptyRegSet l liveness
                   is_live (r,_) = r `elemRegSet` live
           acc_props' = foldl' (\m l -> mapInsertWith (++) l [prop l] m)
                               acc_props back
           acc_dias' = case mb_dia of
             Just (l, _, _, mb_c) | mapFindWithDefault 0 l cont_info
                                      == platformWordSizeInBytes (cmmPlatform cfg)
                                  -> (entry_lbl, l, mb_c) : acc_dias
             _ -> acc_dias
           lazies' = case mb_dia of
             -- several diamonds may share a continuation
             Just (l, _, rs@(_:_), _) -> mapInsertWith (++) l rs lazies
             _ -> lazies

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
          done' acc_props' acc_dias' lazies'

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
through an unlikely branch, as a GC call's continuation returning to a heap
or stack check is, proposes nothing: seeded from it, the stack check of
stg_decodeDouble_Int64# had every call store its argument and move Sp
twice. And a head is not seeded where its loop makes a non-tail call to its
own procedure and holds no diamond: the seed's cost is paid on each entry
and exit, which in a recursion whose loop rarely turns (nofib's tak, a self
tail call around three calls of itself, entered three times an iteration)
costs more than it saves; a loop over data, or calling other code
(fannkuch-redux's), turns many times an entry. The second run is kept only if its back
edges then need no fixup; otherwise the first is.
-}

loopHeadSeeds :: Maybe CLabel -> BlockId -> ProcPointSet -> LabelMap StackMap
              -> LabelMap [Maybe StackMap] -> [BlockId] -> [CmmBlock]
              -> LabelMap StackMap
loopHeadSeeds self_lbl entry procpoints stackmaps props dia_blocks blocks
  = mapFoldlWithKey add mapEmpty props
  where
    blockmap = mapFromList [ (entryLabel b, b) | b <- blocks ]
    -- a non-tail call of the procedure itself
    selfCall x = case mapLookup x blockmap of
      Just blk | CmmCall{ cml_target = CmmLit (CmmLabel lbl)
                        , cml_cont = Just _ } <- lastNode blk
               -> Just lbl == self_lbl
      _ -> False
    -- a loop holding a diamond, or one not recursing into its procedure:
    -- a head's loop is its strongly connected component (the blocks
    -- the head reaches that reach it), computed once for all heads
    dias = setFromList dia_blocks
    loopOk = mapFromList
      [ (x, ok) | scc <- stronglyConnCompFromEdgedVerticesUniq
                           [ DigraphNode l l (successors (lastNode b))
                           | b <- blocks, let l = entryLabel b ]
                , let xs = flattenSCC scc
                      ok = any (`setMember` dias) xs || not (any selfCall xs)
                , x <- xs ]
    inLoop l = mapFindWithDefault False l loopOk
    add acc l ps
      | l /= entry
      , not (l `setMember` procpoints)
      , Just (p : rest) <- sequence ps
      , all ((== sm_sp p) . sm_sp) rest
      , Just old <- mapLookup l stackmaps
      , let seed = p { sm_regs = foldl' commonRegs (sm_regs p)
                                        (map sm_regs rest) }
      , any (needsFixup old) (p : rest)
      , inLoop l
      = mapInsert l seed acc
      | otherwise = acc

{- Note [Reserved frames in loops] (probe, GHC_MW_VARIANT rf)
In a loop that loopHeadSeeds does not seed, as where its back edges need no
fixup or differ in depth, a call returning into the loop moves Sp twice an
iteration where its return path joins a path that made no call, as emitEnter's
sequence (MW) does at its join: down at the call, up at the join. So the second
run gives such a loop's head the depth of the deepest continuation in its loop,
with the loop's invariants (live at the head, assigned nowhere in the loop) in
the slots that continuation's frame gave them, and the calls returning into
the loop keep the stack's depth (Note [Diamond frames keep their depth]):
the frame is reserved, and the edge into the loop pays the Sp move and the
invariants' stores once. The join then takes the depth from the path that made
no call, and the registers it has in slots. The second run is kept only if its
seeds hold (Note [Loop head stack maps]); where they do not and loopHeadSeeds
seeded some head, a third run tries its seeds alone. rfa reserves only in a
loop that advances a variable live at its head: one that only permutes them, as
integerAdd's swap of its arguments, turns at most a few times an entry, and pays
the Sp moves on the edge in and at each exit every time its procedure is called.
-}

reservedSeeds :: Platform -> Maybe CLabel -> BlockId -> ProcPointSet
              -> LabelMap CmmLocalLive -> LabelMap StackMap
              -> LabelMap [Maybe StackMap] -> [BlockId] -> [CmmBlock]
              -> LabelMap StackMap -> (LabelMap StackMap, LabelSet)
reservedSeeds platform self_lbl entry procpoints liveness stackmaps props
              dia_blocks blocks seeds0
  = ( foldl' (\m (l, sd, _) -> mapInsert l sd m) seeds0 found
    , setUnions [ ks | (_, _, ks) <- found ] )
  where
    blockmap = mapFromList [ (entryLabel b, b) | b <- blocks ]
    selfCall x = case mapLookup x blockmap of
      Just blk | CmmCall{ cml_target = CmmLit (CmmLabel lbl)
                        , cml_cont = Just _ } <- lastNode blk
               -> Just lbl == self_lbl
      _ -> False
    dias = setFromList dia_blocks
    sccs = zip [0 :: Int ..]
             [ flattenSCC scc | scc <- stronglyConnCompFromEdgedVerticesUniq
                                  [ DigraphNode l l (successors (lastNode b))
                                  | b <- blocks, let l = entryLabel b ] ]
    sccOf = mapFromList [ (x, xs) | (_, xs) <- sccs, x <- xs ]
    assignedIn xs = Set.fromList
      [ r | x <- xs, Just blk <- [mapLookup x blockmap]
          , let (_, mid, _) = blockSplit blk
          , n <- blockToList mid
          , r <- case n of
                   CmmAssign (CmmLocal r) e | e /= CmmReg (CmmLocal r) -> [r]
                   CmmUnsafeForeignCall _ res _ -> res
                   _ -> [] ]
    -- the variables a loop assigns from something other than a copy of
    -- a variable (a load, arithmetic, a literal, a foreign call's result),
    -- or from a copy of such a variable; an assignment from a global
    -- register (a return's R1) counts as neither
    advancedIn xs =
      let asgs = [ (r, e) | x <- xs, Just blk <- [mapLookup x blockmap]
                          , let (_, mid, _) = blockSplit blk
                          , CmmAssign (CmmLocal r) e <- blockToList mid ]
          fcres = [ r | x <- xs, Just blk <- [mapLookup x blockmap]
                      , let (_, mid, _) = blockSplit blk
                      , CmmUnsafeForeignCall _ res _ <- blockToList mid, r <- res ]
          base = Set.fromList (fcres ++ [ r | (r, e) <- asgs, fresh e ])
          fresh (CmmReg (CmmLocal _)) = False
          fresh (CmmReg (CmmGlobal _)) = False
          fresh _ = True
          copies = [ (r', r) | (r, CmmReg (CmmLocal r')) <- asgs, r /= r' ]
          grow s0 = let s1 = foldl' (\acc (a, b) -> if a `Set.member` acc
                                                      then Set.insert b acc
                                                      else acc) s0 copies
                    in if Set.size s1 == Set.size s0 then s0 else grow s1
      in grow base
    wordsOf (r, off) = [ off, off - platformWordSizeInBytes platform
                         .. off - localRegBytes platform r + 1 ]
    found =
      [ (l, seed, setFromList conts)
      | (l, ps) <- mapToList props
      , not (mapMember l seeds0)
      , l /= entry
      , not (l `setMember` procpoints)
      , Just (p : rest) <- [sequence ps]
      , Just old <- [mapLookup l stackmaps]
        -- (probe, rfx) only where a back edge needs a fixup in the first run
      , not (mwHas "rfx") || any (needsFixup old) (p : rest)
      , Just xs <- [mapLookup l sccOf]
      , any (`setMember` dias) xs || not (any selfCall xs)
      , let conts = [ k | k <- xs, k `setMember` procpoints ]
            deep = [ sm | k <- conts, Just sm <- [mapLookup k stackmaps] ]
      , smk : _ <- [ sortOn (negate . sm_sp) deep ]
      , sm_sp smk > sm_sp old
      , let common = foldl' commonRegs (sm_regs p) (map sm_regs rest)
            live = mapFindWithDefault emptyRegSet l liveness
            assigned = assignedIn xs
            occupied = Set.fromList (concatMap wordsOf (nonDetEltsUFM common))
            inv = [ (v, (v, off)) | (v, off) <- nonDetEltsUFM (sm_regs smk)
                                  , v `elemRegSet` live
                                  , not (v `Set.member` assigned)
                                  , not (v `elemUFM` common)
                                  , not (any (`Set.member` occupied)
                                             (wordsOf (v, off))) ]
            seed = old { sm_sp = sm_sp smk, sm_regs = addListToUFM common inv }
        -- (probe, rfi) only in a loop with an invariant: one that permutes
        -- its variables, as integerAdd's swap of its arguments, turns at
        -- most a few times an entry
      , not (mwHas "rfi") || any (\v -> v `elemRegSet` live && not (v `Set.member` assigned))
                                 (map fst (nonDetEltsUFM (sm_regs smk)))
        -- (probe, rfa) only in a loop that advances a variable live at its
        -- head: one that only permutes them, as integerAdd's swap of its
        -- arguments, turns at most a few times an entry
      , not (mwHas "rfa") || any (\v -> v `elemRegSet` live) (Set.toList (advancedIn xs))
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
  = case node of
     CmmAssign (CmmLocal r) (CmmLoad (CmmStackSlot area off) _ _)
       -> sm { sm_regs = addToUFM (sm_regs sm) r (r,loc) }
        where loc = getStackLoc area off stackmaps
     -- r = r, as a self-recursive call passes an argument unchanged,
     -- leaves r's slot valid
     CmmAssign (CmmLocal r) (CmmReg (CmmLocal r')) | r == r'
       -> sm
     CmmAssign (CmmLocal r) _other
       -> sm { sm_regs = delFromUFM (sm_regs sm) r }
     CmmUnsafeForeignCall _ results _
       -> sm { sm_regs = delListFromUFM (sm_regs sm) results }
     _other
       -> sm

getStackLoc :: Area -> ByteOff -> LabelMap StackMap -> StackLoc
getStackLoc Old       n _         = n
getStackLoc (Young l) n stackmaps =
  case mapLookup l stackmaps of
    Nothing -> pprPanic "getStackLoc" (ppr l)
    Just sm -> sm_sp sm - sm_args sm + n


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
   :: CmmConfig -> ProcPointSet -> LabelMap CmmLocalLive -> LabelMap ByteOff
   -> LabelSet -> LabelMap Int -> LabelMap CmmBlock
   -> LabelMap (Map.Map LocalReg Int) -> Bool
   -> LabelMap StackMap -> StackMap -> CmmTickScope
   -> Block CmmNode O O
   -> CmmNode O C
   -> UniqDSM
      ( [CmmNode O O]      -- nodes to go *before* the Sp adjustment
      , ByteOff            -- amount to adjust Sp
      , CmmNode O C        -- new last node
      , [CmmBlock]         -- new blocks
      , LabelMap StackMap  -- stackmaps for the continuations
      , Maybe (BlockId, Bool, [LocalReg], Maybe BlockId)
        -- a diamond's continuation, whether its return address store
        -- moved to the call's edge, the variables stored there only,
        -- and that edge's block if it holds them
      )

handleLastNode cfg procpoints liveness cont_info keeps cont_callers blockmap need
               lazy_ok stackmaps
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
     -- Calls and ForeignCalls are handled the same way:
     lastCall :: BlockId -> ByteOff -> ByteOff -> ByteOff
              -> ( [CmmNode O O]
                 , ByteOff
                 , CmmNode O C
                 , [CmmBlock]
                 , LabelMap StackMap
                 , Maybe (BlockId, Bool, [LocalReg], Maybe BlockId)
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
           = setupStackFrame platform keep lbl liveness cml_ret_off cml_ret_args stack0


     -- For other last nodes (branches), if any of the targets is a
     -- proc point, we have to set up the stack to match what the proc
     -- point is expecting.
     --
     handleBranches :: UniqDSM ( [CmmNode O O]
                                , ByteOff
                                , CmmNode O C
                                , [CmmBlock]
                                , LabelMap StackMap
                                , Maybe (BlockId, Bool, [LocalReg], Maybe BlockId) )

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
             later = mapFindWithDefault Map.empty l need
             lazy_store (CmmStore _ (CmmReg (CmmLocal r)) _)
               = lazy_ok && cont_args == platformWordSizeInBytes platform
                 && Map.findWithDefault 0 r later <= 1
             lazy_store _ = False
             (lazy_assigs, eager_assigs) = partition lazy_store assigs
             lazy_regs = [ r | CmmStore _ (CmmReg (CmmLocal r)) _ <- lazy_assigs ]
         case move_ret of
           Nothing ->
             return (assigs, sp_off, last, [], out, Just (l, False, [], Nothing))
           Just (c, st) -> do
             tmp <- newBlockId
             let blk = blockJoin (CmmEntry tmp tscp)
                                 (blockFromList (st : lazy_assigs))
                                 (CmmBranch c)
                 last' = mapSuccessors (\x -> if x == c then tmp else x) last
             return ( eager_assigs, sp_off, last', [blk], out
                    , Just (l, True, lazy_regs
                           , if null lazy_regs then Nothing else Just tmp) )

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
                      setupStackFrame platform False l liveness (sm_ret_off stack0)
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
              stack1 = stack0 { sm_regs = filterUFM is_live (sm_regs stack0) }
              is_live (r,_) = r `elemRegSet` live


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
             -> BlockId                 -- label of continuation
             -> LabelMap CmmLocalLive   -- liveness
             -> ByteOff      -- updfr
             -> ByteOff      -- bytes of return values on stack
             -> StackMap     -- current StackMap
             -> (StackMap, [CmmNode O O])

setupStackFrame platform keep lbl liveness updfr_off ret_args stack0
  = (cont_stack, assignments)
  where
      -- get the set of LocalRegs live in the continuation
      live = mapFindWithDefault Set.empty lbl liveness

      -- the stack from the base to updfr_off is off-limits.
      -- our new stack frame contains:
      --   * saved live variables
      --   * the return address [young(C) + 8]
      --   * the args for the call,
      --     which are replaced by the return values at the return
      --     point.

      -- everything up to updfr_off is off-limits
      -- stack1 contains updfr_off, plus everything we need to save
      (stack1, assignments) = allocate platform keep updfr_off live stack0

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
allocate :: Platform -> Bool -> ByteOff -> LocalRegSet -> StackMap
         -> (StackMap, [CmmNode O O])
allocate platform keep ret_off live stackmap@StackMap{ sm_sp = sp0
                                              , sm_regs = regs0 }
 =
   -- we only have to save regs that are not already in a slot
   let to_save = filter (not . (`elemUFM` regs0)) (Set.elems live)
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
          = foldr push (n, [], []) still_to_save
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
          | keep  -- trim at most the word the return address takes
          = plusW platform n (- min 1 (length (takeWhile isEmpty save_stack)))
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
In a diamond (Note [diamond proc point]) the frame is set up before the tag
test, so the tagged path reaches the continuation L with every live variable
both in its slot and in its register; only the call's return clobbers the
registers. Yet L reloads them all (Pass 2) on either path. So, where proc
points are not split, L's body moves to a new block L', L becomes its reloads
and a jump to L', and the tagged path jumps to L'.

That makes what L' needs live at the tag test, and so possibly at an earlier
proc point, where Pass 2 would then reload it too: in a chain of evaluations
(#3294) every continuation would reload everything live. So the diamonds are
rewritten only where the reloads this adds at proc points total at most two
a diamond; failing that, those whose tag test follows no proc point whose
reloads grow; failing that, none. In a loop whose continuation leads back to
the tag test nothing grows.

The return address store moves off the tagged path too (to the edge into the
call) where the call is the only block returning to L: a canned GC routine
jumped to from L's alternatives returns through it.
-}

tagSkip :: Platform -> ProcPointSet -> BlockId -> LabelMap StackMap
        -> [(BlockId, BlockId, Maybe BlockId)] -> [CmmBlock]
        -> UniqDSM (LabelMap StackMap, [CmmBlock])
tagSkip _ _ _ stackmaps [] blocks = return (stackmaps, blocks)
tagSkip platform procpoints entry stackmaps dias0 blocks = do
    -- one body label for each continuation, which several diamonds may share
    -- once common block elimination has merged their continuations
    bodies <- mapM (\l -> do { l' <- newBlockId; return (l, l') })
                   (setElems (setFromList [ l | (_, l, _) <- dias0 ]))
    let body_of = mapFromList bodies
        cands = [ (b, l, mapFindWithDefault l l body_of) | (b, l, _) <- dias0 ]
    let reloads0 = reloadSets blocks
        -- the proc points whose reloads grow, and by how many variables
        grown cs = [ (p, n) | (p, s) <- mapToList (reloadSets (apply cs))
                            , let n = Set.size (s `Set.difference`
                                    mapFindWithDefault Set.empty p reloads0)
                            , n > 0 ]
        -- at most two reloads more for each diamond rewritten
        cheap cs g = sum (map snd g) <= 2 * length cs
        grown_all = grown cands
        -- the blocks reached from a proc point before any other one
        region p = go (setSingleton p) [p]
          where go seen [] = seen
                go seen (x : xs) =
                  let new = [ s | Just blk <- [mapLookup x blockmap]
                                , s <- successors (lastNode blk)
                                , not (s `setMember` procpoints)
                                , not (s `setMember` seen) ]
                  in go (foldr setInsert seen new) (new ++ xs)
        accepted
          | cheap cands grown_all = cands
          | otherwise =
              let bad = setUnions (map (region . fst) grown_all)
                  cs' = [ c | c@(b, _, _) <- cands, not (b `setMember` bad) ]
              in if cheap cs' (grown cs') then cs' else []
        stackmaps' = foldl' (\m (_, l, l') -> case mapLookup l m of
                                Just sm -> mapInsert l' sm m
                                Nothing -> m) stackmaps accepted
        taken = setFromList [ b | (b, _, _) <- accepted ]
        -- the block holding a test block's last node once a diamond whose
        -- continuation it is has been rewritten
        holder b = mapFindWithDefault b b
                     (mapFromList [ (l, l') | (_, l, l') <- accepted ])
    -- a diamond left out stores on its tagged path too what it stores only
    -- on the call's: see Note [Tagged path skips the stores]
    refused <- sequence [ do { t <- newBlockId; return (b, l, c, t) }
                        | (b, l, Just c) <- dias0, not (b `setMember` taken) ]
    return (stackmaps', storeTagged holder refused (apply accepted))
  where
    storeTagged _ [] bs = bs
    storeTagged holder refused bs = new ++ map retarget bs
      where
        tests = mapFromList [ (holder b, (l, t)) | (b, l, _, t) <- refused ]
        new = [ blockJoin (CmmEntry t tscp) (blockFromList stores) (CmmBranch l)
              | (_, l, c, t) <- refused
              , Just cblk <- [mapLookup c blockmap]
              , let (CmmEntry _ tscp, cmid, _) = blockSplit cblk
                    stores = [ n | n@(CmmStore _ (CmmReg _) _) <- blockToList cmid ] ]
        retarget blk
          | Just (l, t) <- mapLookup (entryLabel blk) tests
          , (e, middle, last) <- blockSplit blk
          = blockJoin e middle (mapSuccessors (\x -> if x == l then t else x) last)
          | otherwise = blk

    blockmap = mapFromList [ (entryLabel b, b) | b <- blocks ]

    apply cs = concatMap one blocks
      where
        conts = mapFromList [ (l, l') | (_, l, l') <- cs ]
        tests = mapFromList [ (b, (l, l')) | (b, l, l') <- cs ]
        one blk = case mapLookup lbl conts of
            Just l' -> [ blockJoin (CmmEntry lbl tscp) emptyBlock (CmmBranch l')
                       , blockJoin (CmmEntry l' tscp) middle last' ]
            Nothing | mapMember lbl tests -> [ blockJoin e middle last' ]
                    | otherwise           -> [blk]
          where
            lbl = entryLabel blk
            (e@(CmmEntry _ tscp), middle, last) = blockSplit blk
            -- a continuation's body may hold the next diamond's tag test
            last' = case mapLookup lbl tests of
              Just (l, l') -> mapSuccessors (\x -> if x == l then l' else x) last
              Nothing      -> last

    -- what Pass 2 reloads at each proc point
    reloadSets :: [CmmBlock] -> LabelMap CmmLocalLive
    reloadSets bs = mapFromList [ (entryLabel blk, atMiddle blk facts)
                                | blk <- bs
                                , entryLabel blk `setMember` procpoints ]
      where
        facts = analyzeCmmBwd liveLattice xfer (ofBlockList entry bs) mapEmpty
        xfer blk fb = mapSingleton (entryLabel blk) $!
          if entryLabel blk `setMember` procpoints then emptyRegSet
                                                   else atMiddle blk fb

    atMiddle :: CmmBlock -> FactBase CmmLocalLive -> CmmLocalLive
    atMiddle blk fb =
      let (_, middle, last) = blockSplit blk
          joined = gen_kill platform last $! joinOutFacts liveLattice last fb
      in foldNodesBwdOO (gen_kill platform) middle joined

{- Note [Diamond frames keep their depth]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A diamond's frame (Note [diamond proc point]) is set up before the tag test,
Sp moved on both paths. Trimmed to its live variables, as a call's frame is,
it moves Sp whenever two diamonds in a loop differ in depth, on the evaluated
path too: up at one, down at the next. So a diamond's frame keeps the stack's
depth, trimming at most the word its return address takes, and the dead
slots stay as holes, described as non-pointers.
-}

{- Note [Tagged path skips the stores]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A diamond (Note [diamond proc point]) stores the variables live in its
continuation L before the tag test. A variable that at most one later call
needs in a slot before it is redefined, such as a loop's index, is stored
on the call's edge only, where the call is the only block returning to L
(as the return address is, Note [Tagged path skips the reloads]). L's body
then does not assume it in its slot, so that one later call stores it on its
own path. A variable more later calls need is stored before the test as
before, so that they share the store (#3294's chain of evaluations).
A diamond whose tagged path still goes to L, whose reloads read every slot,
stores on that path too. Not where proc points are split.
-}

-- | For each block, the variables that calls after its entry need in a slot
-- before they are redefined, with how many such calls: 1, or 2 for more.
isLitTarget :: CmmExpr -> Bool
isLitTarget (CmmLit _) = True
isLitTarget _ = False

laterSaves :: LabelMap CmmLocalLive -> BlockId -> [CmmBlock]
           -> LabelMap (Map.Map LocalReg Int)
laterSaves liveness entry blocks
  = analyzeCmmBwd lattice xfer (ofBlockList entry blocks) mapEmpty
  where
    lattice = DataflowLattice Map.empty joinF
    joinF (OldFact old) (NewFact new) =
      let !j = Map.unionWith max old new
      in if j == old then NotChanged old else Changed j
    xfer blk fb = mapSingleton (entryLabel blk) $!
        foldNodesBwdOO mid middle (atLast last)
      where
        (_, middle, last) = blockSplit blk
        fact l = mapFindWithDefault Map.empty l fb
        atLast :: CmmNode O C -> Map.Map LocalReg Int
        atLast n = case n of
          -- an evaluation's call (probe, nev): not counted
          CmmCall{ cml_cont = Just k, cml_target = t }
            | mwHas "nev", not (isLitTarget t) -> fact k
          CmmCall{ cml_cont = Just k } -> atCall k
          CmmForeignCall{ succ = k }   -> atCall k
          _ -> Map.unionsWith max (map fact (successors n))
        atCall k =
          Map.fromSet (\r -> min 2 (1 + Map.findWithDefault 0 r (fact k)))
                      (mapFindWithDefault Set.empty k liveness)
        mid :: CmmNode O O -> Map.Map LocalReg Int -> Map.Map LocalReg Int
        mid (CmmAssign (CmmLocal r) e) f
          | e /= CmmReg (CmmLocal r) = Map.delete r f
        mid (CmmUnsafeForeignCall _ res _) f = foldl' (flip Map.delete) f res
        mid _ f = f

-- -----------------------------------------------------------------------------
-- Pass 2
-- -----------------------------------------------------------------------------

insertReloadsAsNeeded
    :: Platform
    -> ProcPointSet
    -> LabelMap StackMap
    -> BlockId
    -> [CmmBlock]
    -> UniqDSM [CmmBlock]
insertReloadsAsNeeded platform procpoints final_stackmaps entry blocks =
    toBlockList . fst <$>
        rewriteCmmBwd liveLattice rewriteCC (ofBlockList entry blocks) mapEmpty
  where
    rewriteCC :: RewriteFun CmmLocalLive
    rewriteCC (BlockCC e_node middle0 x_node) fact_base0 = do
        let entry_label = entryLabel e_node
            stackmap = case mapLookup entry_label final_stackmaps of
                Just sm -> sm
                Nothing -> panic "insertReloadsAsNeeded: rewriteCC: stackmap"

            -- Merge the liveness from successor blocks and analyse the last
            -- node.
            joined = gen_kill platform x_node $!
                         joinOutFacts liveLattice x_node fact_base0
            -- What is live at the start of middle0.
            live_at_middle0 = foldNodesBwdOO (gen_kill platform) middle0 joined

            -- If this is a procpoint we need to add the reloads, but only if
            -- they're actually live. Furthermore, nothing is live at the entry
            -- to a proc point.
            (middle1, live_with_reloads)
                | entry_label `setMember` procpoints
                = let reloads = insertReloads platform stackmap live_at_middle0
                  in (foldr blockCons middle0 reloads, emptyRegSet)
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
     , reg `elemRegSet` live
     ]
   where
     sp_off = sm_sp stackmap

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

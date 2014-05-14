{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- The register liveness determinator
--
-- (c) The University of Glasgow 2004-2013
--
-----------------------------------------------------------------------------

module RegAlloc.Liveness (
        RegSet,
        RegMap, emptyRegMap,
        BlockMap, emptyBlockMap,
        LiveCmmDecl,
        InstrSR   (..),
        LiveInstr (..),
        Liveness (..),
        LiveInfo (..),
        LiveBasicBlock,

        mapBlockTop,    mapBlockTopM,   mapSCCM,
        mapGenBlockTop, mapGenBlockTopM,
        stripLive,
        stripLiveBlock,
        slurpConflicts,
        slurpReloadCoalesce,
        eraseDeltasLive,
        patchEraseLive,
        patchRegsLiveInstr,
        reverseBlocksInTops,
        regLiveness,
        natCmmTopToLive
  ) where
import Reg
import Instruction

import BlockId
import Cmm hiding (RegSet)
import PprCmm()

import Digraph
import DynFlags
import Outputable
import Platform
import UniqSet
import UniqFM
import UniqSupply
import Bag
import State
import FastString

import Data.List
import Data.Maybe
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map

-----------------------------------------------------------------------------
type RegSet = UniqSet Reg

type RegMap a = UniqFM a

emptyRegMap :: UniqFM a
emptyRegMap = emptyUFM

type BlockMap a = BlockEnv a


-- | A top level thing which carries liveness information.
type LiveCmmDecl statics instr
        = GenCmmDecl
                statics
                LiveInfo
                [SCC (LiveBasicBlock instr)]


-- | The register allocator also wants to use SPILL/RELOAD meta instructions,
--   so we'll keep those here.
data InstrSR instr
        -- | A real machine instruction
        = Instr  instr

        -- | spill this reg to a stack slot
        | SPILL  Reg Int

        -- | reload this reg from a stack slot
        | RELOAD Int Reg

instance Instruction instr => Instruction (InstrSR instr) where
        regUsageOfInstr platform i
         = case i of
                Instr  instr    -> regUsageOfInstr platform instr
                SPILL  reg _    -> RU [reg] []
                RELOAD _ reg    -> RU [] [reg]

        patchRegsOfInstr i f
         = case i of
                Instr instr     -> Instr (patchRegsOfInstr instr f)
                SPILL  reg slot -> SPILL (f reg) slot
                RELOAD slot reg -> RELOAD slot (f reg)

        isJumpishInstr i
         = case i of
                Instr instr     -> isJumpishInstr instr
                _               -> False

        jumpDestsOfInstr i
         = case i of
                Instr instr     -> jumpDestsOfInstr instr
                _               -> []

        patchJumpInstr i f
         = case i of
                Instr instr     -> Instr (patchJumpInstr instr f)
                _               -> i

        mkSpillInstr            = error "mkSpillInstr[InstrSR]: Not making SPILL meta-instr"
        mkLoadInstr             = error "mkLoadInstr[InstrSR]: Not making LOAD meta-instr"

        takeDeltaInstr i
         = case i of
                Instr instr     -> takeDeltaInstr instr
                _               -> Nothing

        isMetaInstr i
         = case i of
                Instr instr     -> isMetaInstr instr
                _               -> False

        mkRegRegMoveInstr platform r1 r2
            = Instr (mkRegRegMoveInstr platform r1 r2)

        takeRegRegMoveInstr i
         = case i of
                Instr instr     -> takeRegRegMoveInstr instr
                _               -> Nothing

        mkJumpInstr target      = map Instr (mkJumpInstr target)

        mkStackAllocInstr platform amount =
             Instr (mkStackAllocInstr platform amount)

        mkStackDeallocInstr platform amount =
             Instr (mkStackDeallocInstr platform amount)


-- | An instruction with liveness information.
data LiveInstr instr
        = LiveInstr (InstrSR instr) (Maybe Liveness)

-- | Liveness information.
--   The regs which die are ones which are no longer live in the *next* instruction
--   in this sequence.
--   (NB. if the instruction is a jump, these registers might still be live
--   at the jump target(s) - you have to check the liveness at the destination
--   block to find out).

data Liveness
        = Liveness
        { liveBorn      :: RegSet       -- ^ registers born in this instruction (written to for first time).
        , liveDieRead   :: RegSet       -- ^ registers that died because they were read for the last time.
        , liveDieWrite  :: RegSet }     -- ^ registers that died because they were clobbered by something.


-- | Stash regs live on entry to each basic block in the info part of the cmm code.
data LiveInfo
        = LiveInfo
                (BlockEnv CmmStatics)                   -- cmm info table static stuff
                (Maybe BlockId)                         -- id of the first block
                (Maybe (BlockMap RegSet))               -- argument locals live on entry to this block
                (Map BlockId (Set Int))                 -- stack slots live on entry to this block


-- | A basic block with liveness information.
type LiveBasicBlock instr
        = GenBasicBlock (LiveInstr instr)


instance Outputable instr
      => Outputable (InstrSR instr) where

        ppr (Instr realInstr)
           = ppr realInstr

        ppr (SPILL reg slot)
           = hcat [
                ptext (sLit "\tSPILL"),
                char ' ',
                ppr reg,
                comma,
                ptext (sLit "SLOT") <> parens (int slot)]

        ppr (RELOAD slot reg)
           = hcat [
                ptext (sLit "\tRELOAD"),
                char ' ',
                ptext (sLit "SLOT") <> parens (int slot),
                comma,
                ppr reg]

instance Outputable instr
      => Outputable (LiveInstr instr) where

        ppr (LiveInstr instr Nothing)
         = ppr instr

        ppr (LiveInstr instr (Just live))
         =  ppr instr
                $$ (nest 8
                        $ vcat
                        [ pprRegs (ptext (sLit "# born:    ")) (liveBorn live)
                        , pprRegs (ptext (sLit "# r_dying: ")) (liveDieRead live)
                        , pprRegs (ptext (sLit "# w_dying: ")) (liveDieWrite live) ]
                    $+$ space)

         where  pprRegs :: SDoc -> RegSet -> SDoc
                pprRegs name regs
                 | isEmptyUniqSet regs  = empty
                 | otherwise            = name <> (hcat $ punctuate space $ map ppr $ uniqSetToList regs)

instance Outputable LiveInfo where
    ppr (LiveInfo mb_static firstId liveVRegsOnEntry liveSlotsOnEntry)
        =  (ppr mb_static)
        $$ text "# firstId          = " <> ppr firstId
        $$ text "# liveVRegsOnEntry = " <> ppr liveVRegsOnEntry
        $$ text "# liveSlotsOnEntry = " <> text (show liveSlotsOnEntry)



-- | map a function across all the basic blocks in this code
--
mapBlockTop
        :: (LiveBasicBlock instr -> LiveBasicBlock instr)
        -> LiveCmmDecl statics instr -> LiveCmmDecl statics instr

mapBlockTop f cmm
        = evalState (mapBlockTopM (\x -> return $ f x) cmm) ()


-- | map a function across all the basic blocks in this code (monadic version)
--
mapBlockTopM
        :: Monad m
        => (LiveBasicBlock instr -> m (LiveBasicBlock instr))
        -> LiveCmmDecl statics instr -> m (LiveCmmDecl statics instr)

mapBlockTopM _ cmm@(CmmData{})
        = return cmm

mapBlockTopM f (CmmProc header label live sccs)
 = do   sccs'   <- mapM (mapSCCM f) sccs
        return  $ CmmProc header label live sccs'

mapSCCM :: Monad m => (a -> m b) -> SCC a -> m (SCC b)
mapSCCM f (AcyclicSCC x)
 = do   x'      <- f x
        return  $ AcyclicSCC x'

mapSCCM f (CyclicSCC xs)
 = do   xs'     <- mapM f xs
        return  $ CyclicSCC xs'


-- map a function across all the basic blocks in this code
mapGenBlockTop
        :: (GenBasicBlock             i -> GenBasicBlock            i)
        -> (GenCmmDecl d h (ListGraph i) -> GenCmmDecl d h (ListGraph i))

mapGenBlockTop f cmm
        = evalState (mapGenBlockTopM (\x -> return $ f x) cmm) ()


-- | map a function across all the basic blocks in this code (monadic version)
mapGenBlockTopM
        :: Monad m
        => (GenBasicBlock            i  -> m (GenBasicBlock            i))
        -> (GenCmmDecl d h (ListGraph i) -> m (GenCmmDecl d h (ListGraph i)))

mapGenBlockTopM _ cmm@(CmmData{})
        = return cmm

mapGenBlockTopM f (CmmProc header label live (ListGraph blocks))
 = do   blocks' <- mapM f blocks
        return  $ CmmProc header label live (ListGraph blocks')


-- | Slurp out the list of register conflicts and reg-reg moves from this top level thing.
--   Slurping of conflicts and moves is wrapped up together so we don't have
--   to make two passes over the same code when we want to build the graph.
--
slurpConflicts
        :: Instruction instr
        => LiveCmmDecl statics instr
        -> (Bag (UniqSet Reg), Bag (Reg, Reg))

slurpConflicts live
        = slurpCmm (emptyBag, emptyBag) live

 where  slurpCmm   rs  CmmData{}                = rs
        slurpCmm   rs (CmmProc info _ _ sccs)
                = foldl' (slurpSCC info) rs sccs

        slurpSCC  info rs (AcyclicSCC b)
                = slurpBlock info rs b

        slurpSCC  info rs (CyclicSCC bs)
                = foldl'  (slurpBlock info) rs bs

        slurpBlock info rs (BasicBlock blockId instrs)
                | LiveInfo _ _ (Just blockLive) _ <- info
                , Just rsLiveEntry                <- mapLookup blockId blockLive
                , (conflicts, moves)              <- slurpLIs rsLiveEntry rs instrs
                = (consBag rsLiveEntry conflicts, moves)

                | otherwise
                = panic "Liveness.slurpConflicts: bad block"

        slurpLIs rsLive (conflicts, moves) []
                = (consBag rsLive conflicts, moves)

        slurpLIs rsLive rs (LiveInstr _ Nothing     : lis)
                = slurpLIs rsLive rs lis

        slurpLIs rsLiveEntry (conflicts, moves) (LiveInstr instr (Just live) : lis)
         = let
                -- regs that die because they are read for the last time at the start of an instruction
                --      are not live across it.
                rsLiveAcross    = rsLiveEntry `minusUniqSet` (liveDieRead live)

                -- regs live on entry to the next instruction.
                --      be careful of orphans, make sure to delete dying regs _after_ unioning
                --      in the ones that are born here.
                rsLiveNext      = (rsLiveAcross `unionUniqSets` (liveBorn     live))
                                                `minusUniqSet`  (liveDieWrite live)

                -- orphan vregs are the ones that die in the same instruction they are born in.
                --      these are likely to be results that are never used, but we still
                --      need to assign a hreg to them..
                rsOrphans       = intersectUniqSets
                                        (liveBorn live)
                                        (unionUniqSets (liveDieWrite live) (liveDieRead live))

                --
                rsConflicts     = unionUniqSets rsLiveNext rsOrphans

          in    case takeRegRegMoveInstr instr of
                 Just rr        -> slurpLIs rsLiveNext
                                        ( consBag rsConflicts conflicts
                                        , consBag rr moves) lis

                 Nothing        -> slurpLIs rsLiveNext
                                        ( consBag rsConflicts conflicts
                                        , moves) lis


-- | For spill\/reloads
--
--   SPILL  v1, slot1
--   ...
--   RELOAD slot1, v2
--
--   If we can arrange that v1 and v2 are allocated to the same hreg it's more likely
--   the spill\/reload instrs can be cleaned and replaced by a nop reg-reg move.
--
--
slurpReloadCoalesce
        :: forall statics instr. Instruction instr
        => LiveCmmDecl statics instr
        -> Bag (Reg, Reg)

slurpReloadCoalesce live
        = slurpCmm emptyBag live

 where
        slurpCmm :: Bag (Reg, Reg)
                 -> GenCmmDecl t t1 [SCC (LiveBasicBlock instr)]
                 -> Bag (Reg, Reg)
        slurpCmm cs CmmData{}   = cs
        slurpCmm cs (CmmProc _ _ _ sccs)
                = slurpComp cs (flattenSCCs sccs)

        slurpComp :: Bag (Reg, Reg)
                     -> [LiveBasicBlock instr]
                     -> Bag (Reg, Reg)
        slurpComp  cs blocks
         = let  (moveBags, _)   = runState (slurpCompM blocks) emptyUFM
           in   unionManyBags (cs : moveBags)

        slurpCompM :: [LiveBasicBlock instr]
                   -> State (UniqFM [UniqFM Reg]) [Bag (Reg, Reg)]
        slurpCompM blocks
         = do   -- run the analysis once to record the mapping across jumps.
                mapM_   (slurpBlock False) blocks

                -- run it a second time while using the information from the last pass.
                --      We /could/ run this many more times to deal with graphical control
                --      flow and propagating info across multiple jumps, but it's probably
                --      not worth the trouble.
                mapM    (slurpBlock True) blocks

        slurpBlock :: Bool -> LiveBasicBlock instr
                   -> State (UniqFM [UniqFM Reg]) (Bag (Reg, Reg))
        slurpBlock propagate (BasicBlock blockId instrs)
         = do   -- grab the slot map for entry to this block
                slotMap         <- if propagate
                                        then getSlotMap blockId
                                        else return emptyUFM

                (_, mMoves)     <- mapAccumLM slurpLI slotMap instrs
                return $ listToBag $ catMaybes mMoves

        slurpLI :: UniqFM Reg                           -- current slotMap
                -> LiveInstr instr
                -> State (UniqFM [UniqFM Reg])          -- blockId -> [slot -> reg]
                                                        --      for tracking slotMaps across jumps

                         ( UniqFM Reg                   -- new slotMap
                         , Maybe (Reg, Reg))            -- maybe a new coalesce edge

        slurpLI slotMap li

                -- remember what reg was stored into the slot
                | LiveInstr (SPILL reg slot) _  <- li
                , slotMap'                      <- addToUFM slotMap slot reg
                = return (slotMap', Nothing)

                -- add an edge between the this reg and the last one stored into the slot
                | LiveInstr (RELOAD slot reg) _ <- li
                = case lookupUFM slotMap slot of
                        Just reg2
                         | reg /= reg2  -> return (slotMap, Just (reg, reg2))
                         | otherwise    -> return (slotMap, Nothing)

                        Nothing         -> return (slotMap, Nothing)

                -- if we hit a jump, remember the current slotMap
                | LiveInstr (Instr instr) _     <- li
                , targets                       <- jumpDestsOfInstr instr
                , not $ null targets
                = do    mapM_   (accSlotMap slotMap) targets
                        return  (slotMap, Nothing)

                | otherwise
                = return (slotMap, Nothing)

        -- record a slotmap for an in edge to this block
        accSlotMap slotMap blockId
                = modify (\s -> addToUFM_C (++) s blockId [slotMap])

        -- work out the slot map on entry to this block
        --      if we have slot maps for multiple in-edges then we need to merge them.
        getSlotMap blockId
         = do   map             <- get
                let slotMaps    = fromMaybe [] (lookupUFM map blockId)
                return          $ foldr mergeSlotMaps emptyUFM slotMaps

        mergeSlotMaps :: UniqFM Reg -> UniqFM Reg -> UniqFM Reg
        mergeSlotMaps map1 map2
                = listToUFM
                $ [ (k, r1)     | (k, r1)       <- ufmToList map1
                                , case lookupUFM map2 k of
                                        Nothing -> False
                                        Just r2 -> r1 == r2 ]


-- | Strip away liveness information, yielding NatCmmDecl
stripLive
        :: (Outputable statics, Outputable instr, Instruction instr)
        => DynFlags
        -> LiveCmmDecl statics instr
        -> NatCmmDecl statics instr

stripLive dflags live
        = stripCmm live

 where  stripCmm :: (Outputable statics, Outputable instr, Instruction instr)
                 => LiveCmmDecl statics instr -> NatCmmDecl statics instr
        stripCmm (CmmData sec ds)       = CmmData sec ds
        stripCmm (CmmProc (LiveInfo info (Just first_id) _ _) label live sccs)
         = let  final_blocks    = flattenSCCs sccs

                -- make sure the block that was first in the input list
                --      stays at the front of the output. This is the entry point
                --      of the proc, and it needs to come first.
                ((first':_), rest')
                                = partition ((== first_id) . blockId) final_blocks

           in   CmmProc info label live
                          (ListGraph $ map (stripLiveBlock dflags) $ first' : rest')

        -- procs used for stg_split_markers don't contain any blocks, and have no first_id.
        stripCmm (CmmProc (LiveInfo info Nothing _ _) label live [])
         =      CmmProc info label live (ListGraph [])

        -- If the proc has blocks but we don't know what the first one was, then we're dead.
        stripCmm proc
                 = pprPanic "RegAlloc.Liveness.stripLive: no first_id on proc" (ppr proc)

-- | Strip away liveness information from a basic block,
--   and make real spill instructions out of SPILL, RELOAD pseudos along the way.

stripLiveBlock
        :: Instruction instr
        => DynFlags
        -> LiveBasicBlock instr
        -> NatBasicBlock instr

stripLiveBlock dflags (BasicBlock i lis)
 =      BasicBlock i instrs'

 where  (instrs', _)
                = runState (spillNat [] lis) 0

        spillNat acc []
         =      return (reverse acc)

        spillNat acc (LiveInstr (SPILL reg slot) _ : instrs)
         = do   delta   <- get
                spillNat (mkSpillInstr dflags reg delta slot : acc) instrs

        spillNat acc (LiveInstr (RELOAD slot reg) _ : instrs)
         = do   delta   <- get
                spillNat (mkLoadInstr dflags reg delta slot : acc) instrs

        spillNat acc (LiveInstr (Instr instr) _ : instrs)
         | Just i <- takeDeltaInstr instr
         = do   put i
                spillNat acc instrs

        spillNat acc (LiveInstr (Instr instr) _ : instrs)
         =      spillNat (instr : acc) instrs


-- | Erase Delta instructions.

eraseDeltasLive
        :: Instruction instr
        => LiveCmmDecl statics instr
        -> LiveCmmDecl statics instr

eraseDeltasLive cmm
        = mapBlockTop eraseBlock cmm
 where
        eraseBlock (BasicBlock id lis)
                = BasicBlock id
                $ filter (\(LiveInstr i _) -> not $ isJust $ takeDeltaInstr i)
                $ lis


-- | Patch the registers in this code according to this register mapping.
--   also erase reg -> reg moves when the reg is the same.
--   also erase reg -> reg moves when the destination dies in this instr.
patchEraseLive
        :: Instruction instr
        => (Reg -> Reg)
        -> LiveCmmDecl statics instr -> LiveCmmDecl statics instr

patchEraseLive patchF cmm
        = patchCmm cmm
 where
        patchCmm cmm@CmmData{}  = cmm

        patchCmm (CmmProc info label live sccs)
         | LiveInfo static id (Just blockMap) mLiveSlots <- info
         = let
                patchRegSet set = mkUniqSet $ map patchF $ uniqSetToList set
                blockMap'       = mapMap patchRegSet blockMap

                info'           = LiveInfo static id (Just blockMap') mLiveSlots
           in   CmmProc info' label live $ map patchSCC sccs

         | otherwise
         = panic "RegAlloc.Liveness.patchEraseLive: no blockMap"

        patchSCC (AcyclicSCC b)  = AcyclicSCC (patchBlock b)
        patchSCC (CyclicSCC  bs) = CyclicSCC  (map patchBlock bs)

        patchBlock (BasicBlock id lis)
                = BasicBlock id $ patchInstrs lis

        patchInstrs []          = []
        patchInstrs (li : lis)

                | LiveInstr i (Just live)       <- li'
                , Just (r1, r2) <- takeRegRegMoveInstr i
                , eatMe r1 r2 live
                = patchInstrs lis

                | otherwise
                = li' : patchInstrs lis

                where   li'     = patchRegsLiveInstr patchF li

        eatMe   r1 r2 live
                -- source and destination regs are the same
                | r1 == r2      = True

                -- destination reg is never used
                | elementOfUniqSet r2 (liveBorn live)
                , elementOfUniqSet r2 (liveDieRead live) || elementOfUniqSet r2 (liveDieWrite live)
                = True

                | otherwise     = False


-- | Patch registers in this LiveInstr, including the liveness information.
--
patchRegsLiveInstr
        :: Instruction instr
        => (Reg -> Reg)
        -> LiveInstr instr -> LiveInstr instr

patchRegsLiveInstr patchF li
 = case li of
        LiveInstr instr Nothing
         -> LiveInstr (patchRegsOfInstr instr patchF) Nothing

        LiveInstr instr (Just live)
         -> LiveInstr
                (patchRegsOfInstr instr patchF)
                (Just live
                        { -- WARNING: have to go via lists here because patchF changes the uniq in the Reg
                          liveBorn      = mkUniqSet $ map patchF $ uniqSetToList $ liveBorn live
                        , liveDieRead   = mkUniqSet $ map patchF $ uniqSetToList $ liveDieRead live
                        , liveDieWrite  = mkUniqSet $ map patchF $ uniqSetToList $ liveDieWrite live })


--------------------------------------------------------------------------------
-- | Convert a NatCmmDecl to a LiveCmmDecl, with empty liveness information

natCmmTopToLive
        :: Instruction instr
        => NatCmmDecl statics instr
        -> LiveCmmDecl statics instr

natCmmTopToLive (CmmData i d)
        = CmmData i d

natCmmTopToLive (CmmProc info lbl live (ListGraph []))
        = CmmProc (LiveInfo info Nothing Nothing Map.empty) lbl live []

natCmmTopToLive proc@(CmmProc info lbl live (ListGraph blocks@(first : _)))
 = let  first_id        = blockId first
        sccs            = sccBlocks blocks (entryBlocks proc)
        sccsLive        = map (fmap (\(BasicBlock l instrs) ->
                                        BasicBlock l (map (\i -> LiveInstr (Instr i) Nothing) instrs)))
                        $ sccs

   in   CmmProc (LiveInfo info (Just first_id) Nothing Map.empty) lbl live sccsLive


--
-- Compute the liveness graph of the set of basic blocks.  Important:
-- we also discard any unreachable code here, starting from the entry
-- points (the first block in the list, and any blocks with info
-- tables).  Unreachable code arises when code blocks are orphaned in
-- earlier optimisation passes, and may confuse the register allocator
-- by referring to registers that are not initialised.  It's easy to
-- discard the unreachable code as part of the SCC pass, so that's
-- exactly what we do. (#7574)
--
sccBlocks
        :: Instruction instr
        => [NatBasicBlock instr]
        -> [BlockId]
        -> [SCC (NatBasicBlock instr)]

sccBlocks blocks entries = map (fmap get_node) sccs
  where
        sccs = stronglyConnCompFromG graph roots

        graph = graphFromEdgedVertices nodes

        -- nodes :: [(NatBasicBlock instr, Unique, [Unique])]
        nodes = [ (block, id, getOutEdges instrs)
                | block@(BasicBlock id instrs) <- blocks ]

        get_node (n, _, _) = n

        getOutEdges :: Instruction instr => [instr] -> [BlockId]
        getOutEdges instrs = concat $ map jumpDestsOfInstr instrs

        -- This is truly ugly, but I don't see a good alternative.
        -- Digraph just has the wrong API.  We want to identify nodes
        -- by their keys (BlockId), but Digraph requires the whole
        -- node: (NatBasicBlock, BlockId, [BlockId]).  This takes
        -- advantage of the fact that Digraph only looks at the key,
        -- even though it asks for the whole triple.
        roots = [(panic "sccBlocks",b,panic "sccBlocks") | b <- entries ]



--------------------------------------------------------------------------------
-- Annotate code with register liveness information
--
regLiveness
        :: (Outputable instr, Instruction instr)
        => Platform
        -> LiveCmmDecl statics instr
        -> UniqSM (LiveCmmDecl statics instr)

regLiveness _ (CmmData i d)
        = return $ CmmData i d

regLiveness _ (CmmProc info lbl live [])
        | LiveInfo static mFirst _ _    <- info
        = return $ CmmProc
                        (LiveInfo static mFirst (Just mapEmpty) Map.empty)
                        lbl live []

regLiveness platform (CmmProc info lbl live sccs)
        | LiveInfo static mFirst _ liveSlotsOnEntry     <- info
        = let   (ann_sccs, block_live)  = computeLiveness platform sccs

          in    return $ CmmProc (LiveInfo static mFirst (Just block_live) liveSlotsOnEntry)
                           lbl live ann_sccs


-- -----------------------------------------------------------------------------
-- | Check ordering of Blocks
--   The computeLiveness function requires SCCs to be in reverse
--   dependent order.  If they're not the liveness information will be
--   wrong, and we'll get a bad allocation.  Better to check for this
--   precondition explicitly or some other poor sucker will waste a
--   day staring at bad assembly code..
--
checkIsReverseDependent
        :: Instruction instr
        => [SCC (LiveBasicBlock instr)]         -- ^ SCCs of blocks that we're about to run the liveness determinator on.
        -> Maybe BlockId                        -- ^ BlockIds that fail the test (if any)

checkIsReverseDependent sccs'
 = go emptyUniqSet sccs'

 where  go _ []
         = Nothing

        go blocksSeen (AcyclicSCC block : sccs)
         = let  dests           = slurpJumpDestsOfBlock block
                blocksSeen'     = unionUniqSets blocksSeen $ mkUniqSet [blockId block]
                badDests        = dests `minusUniqSet` blocksSeen'
           in   case uniqSetToList badDests of
                 []             -> go blocksSeen' sccs
                 bad : _        -> Just bad

        go blocksSeen (CyclicSCC blocks : sccs)
         = let  dests           = unionManyUniqSets $ map slurpJumpDestsOfBlock blocks
                blocksSeen'     = unionUniqSets blocksSeen $ mkUniqSet $ map blockId blocks
                badDests        = dests `minusUniqSet` blocksSeen'
           in   case uniqSetToList badDests of
                 []             -> go blocksSeen' sccs
                 bad : _        -> Just bad

        slurpJumpDestsOfBlock (BasicBlock _ instrs)
                = unionManyUniqSets
                $ map (mkUniqSet . jumpDestsOfInstr)
                        [ i | LiveInstr i _ <- instrs]


-- | If we've compute liveness info for this code already we have to reverse
--   the SCCs in each top to get them back to the right order so we can do it again.
reverseBlocksInTops :: LiveCmmDecl statics instr -> LiveCmmDecl statics instr
reverseBlocksInTops top
 = case top of
        CmmData{}                       -> top
        CmmProc info lbl live sccs      -> CmmProc info lbl live (reverse sccs)


-- | Computing liveness
--
--  On entry, the SCCs must be in "reverse" order: later blocks may transfer
--  control to earlier ones only, else `panic`.
--
--  The SCCs returned are in the *opposite* order, which is exactly what we
--  want for the next pass.
--
computeLiveness
        :: (Outputable instr, Instruction instr)
        => Platform
        -> [SCC (LiveBasicBlock instr)]
        -> ([SCC (LiveBasicBlock instr)],       -- instructions annotated with list of registers
                                                -- which are "dead after this instruction".
               BlockMap RegSet)                 -- blocks annontated with set of live registers
                                                -- on entry to the block.

computeLiveness platform sccs
 = case checkIsReverseDependent sccs of
        Nothing         -> livenessSCCs platform emptyBlockMap [] sccs
        Just bad        -> pprPanic "RegAlloc.Liveness.computeLivenss"
                                (vcat   [ text "SCCs aren't in reverse dependent order"
                                        , text "bad blockId" <+> ppr bad
                                        , ppr sccs])

livenessSCCs
       :: Instruction instr
       => Platform
       -> BlockMap RegSet
       -> [SCC (LiveBasicBlock instr)]          -- accum
       -> [SCC (LiveBasicBlock instr)]
       -> ( [SCC (LiveBasicBlock instr)]
          , BlockMap RegSet)

livenessSCCs _ blockmap done []
        = (done, blockmap)

livenessSCCs platform blockmap done (AcyclicSCC block : sccs)
 = let  (blockmap', block')     = livenessBlock platform blockmap block
   in   livenessSCCs platform blockmap' (AcyclicSCC block' : done) sccs

livenessSCCs platform blockmap done
        (CyclicSCC blocks : sccs) =
        livenessSCCs platform blockmap' (CyclicSCC blocks':done) sccs
 where      (blockmap', blocks')
                = iterateUntilUnchanged linearLiveness equalBlockMaps
                                      blockmap blocks

            iterateUntilUnchanged
                :: (a -> b -> (a,c)) -> (a -> a -> Bool)
                -> a -> b
                -> (a,c)

            iterateUntilUnchanged f eq a b
                = head $
                  concatMap tail $
                  groupBy (\(a1, _) (a2, _) -> eq a1 a2) $
                  iterate (\(a, _) -> f a b) $
                  (a, panic "RegLiveness.livenessSCCs")


            linearLiveness
                :: Instruction instr
                => BlockMap RegSet -> [LiveBasicBlock instr]
                -> (BlockMap RegSet, [LiveBasicBlock instr])

            linearLiveness = mapAccumL (livenessBlock platform)

                -- probably the least efficient way to compare two
                -- BlockMaps for equality.
            equalBlockMaps a b
                = a' == b'
              where a' = map f $ mapToList a
                    b' = map f $ mapToList b
                    f (key,elt) = (key, uniqSetToList elt)



-- | Annotate a basic block with register liveness information.
--
livenessBlock
        :: Instruction instr
        => Platform
        -> BlockMap RegSet
        -> LiveBasicBlock instr
        -> (BlockMap RegSet, LiveBasicBlock instr)

livenessBlock platform blockmap (BasicBlock block_id instrs)
 = let
        (regsLiveOnEntry, instrs1)
            = livenessBack platform emptyUniqSet blockmap [] (reverse instrs)
        blockmap'       = mapInsert block_id regsLiveOnEntry blockmap

        instrs2         = livenessForward platform regsLiveOnEntry instrs1

        output          = BasicBlock block_id instrs2

   in   ( blockmap', output)

-- | Calculate liveness going forwards,
--   filling in when regs are born

livenessForward
        :: Instruction instr
        => Platform
        -> RegSet                       -- regs live on this instr
        -> [LiveInstr instr] -> [LiveInstr instr]

livenessForward _        _           []  = []
livenessForward platform rsLiveEntry (li@(LiveInstr instr mLive) : lis)
        | Nothing               <- mLive
        = li : livenessForward platform rsLiveEntry lis

        | Just live     <- mLive
        , RU _ written  <- regUsageOfInstr platform instr
        = let
                -- Regs that are written to but weren't live on entry to this instruction
                --      are recorded as being born here.
                rsBorn          = mkUniqSet
                                $ filter (\r -> not $ elementOfUniqSet r rsLiveEntry) written

                rsLiveNext      = (rsLiveEntry `unionUniqSets` rsBorn)
                                        `minusUniqSet` (liveDieRead live)
                                        `minusUniqSet` (liveDieWrite live)

        in LiveInstr instr (Just live { liveBorn = rsBorn })
                : livenessForward platform rsLiveNext lis

livenessForward _ _ _             = panic "RegLiveness.livenessForward: no match"


-- | Calculate liveness going backwards,
--   filling in when regs die, and what regs are live across each instruction

livenessBack
        :: Instruction instr
        => Platform
        -> RegSet                       -- regs live on this instr
        -> BlockMap RegSet              -- regs live on entry to other BBs
        -> [LiveInstr instr]            -- instructions (accum)
        -> [LiveInstr instr]            -- instructions
        -> (RegSet, [LiveInstr instr])

livenessBack _        liveregs _        done []  = (liveregs, done)

livenessBack platform liveregs blockmap acc (instr : instrs)
 = let  (liveregs', instr')     = liveness1 platform liveregs blockmap instr
   in   livenessBack platform liveregs' blockmap (instr' : acc) instrs


-- don't bother tagging comments or deltas with liveness
liveness1
        :: Instruction instr
        => Platform
        -> RegSet
        -> BlockMap RegSet
        -> LiveInstr instr
        -> (RegSet, LiveInstr instr)

liveness1 _ liveregs _ (LiveInstr instr _)
        | isMetaInstr instr
        = (liveregs, LiveInstr instr Nothing)

liveness1 platform liveregs blockmap (LiveInstr instr _)

        | not_a_branch
        = (liveregs1, LiveInstr instr
                        (Just $ Liveness
                        { liveBorn      = emptyUniqSet
                        , liveDieRead   = mkUniqSet r_dying
                        , liveDieWrite  = mkUniqSet w_dying }))

        | otherwise
        = (liveregs_br, LiveInstr instr
                        (Just $ Liveness
                        { liveBorn      = emptyUniqSet
                        , liveDieRead   = mkUniqSet r_dying_br
                        , liveDieWrite  = mkUniqSet w_dying }))

        where
            !(RU read written) = regUsageOfInstr platform instr

            -- registers that were written here are dead going backwards.
            -- registers that were read here are live going backwards.
            liveregs1   = (liveregs `delListFromUniqSet` written)
                                    `addListToUniqSet` read

            -- registers that are not live beyond this point, are recorded
            --  as dying here.
            r_dying     = [ reg | reg <- read, reg `notElem` written,
                              not (elementOfUniqSet reg liveregs) ]

            w_dying     = [ reg | reg <- written,
                             not (elementOfUniqSet reg liveregs) ]

            -- union in the live regs from all the jump destinations of this
            -- instruction.
            targets      = jumpDestsOfInstr instr -- where we go from here
            not_a_branch = null targets

            targetLiveRegs target
                  = case mapLookup target blockmap of
                                Just ra -> ra
                                Nothing -> emptyRegMap

            live_from_branch = unionManyUniqSets (map targetLiveRegs targets)

            liveregs_br = liveregs1 `unionUniqSets` live_from_branch

            -- registers that are live only in the branch targets should
            -- be listed as dying here.
            live_branch_only = live_from_branch `minusUniqSet` liveregs
            r_dying_br  = uniqSetToList (mkUniqSet r_dying `unionUniqSets`
                                        live_branch_only)



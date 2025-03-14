{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- The register liveness determinator
--
-- (c) The University of Glasgow 2004-2013
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.Reg.Liveness (
        RegMap, emptyRegMap,
        BlockMap,
        LiveCmmDecl,
        InstrSR   (..),
        LiveInstr (..),
        Liveness (..),
        LiveInfo (..),
        LiveBasicBlock,

        mapBlockTop,    mapBlockTopM,   mapSCCM,
        mapGenBlockTop, mapGenBlockTopM,
        mapLiveCmmDecl, pprLiveCmmDecl,
        stripLive,
        stripLiveBlock,
        slurpConflicts,
        slurpReloadCoalesce,
        eraseDeltasLive,
        patchEraseLive,
        patchRegsLiveInstr,
        reverseBlocksInTops,
        regLiveness,
        cmmTopLiveness
  ) where
import GHC.Prelude

import GHC.Platform.Reg
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.CmmToAsm.Reg.Target

import GHC.Data.Graph.Directed
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Types.Unique (Uniquable(..))
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSM
import GHC.Data.Bag
import GHC.Utils.Monad.State.Strict

import Data.List (mapAccumL, sortOn)
import Data.Maybe
import Data.IntSet              (IntSet)
import GHC.Utils.Misc

-----------------------------------------------------------------------------

-- | Map from some kind of register to a.
--
-- While we give the type for keys as Reg which is the common case
-- sometimes we end up using VirtualReq or naked Uniques.
-- See Note [UniqFM and the register allocator]
type RegMap a = UniqFM Reg a

emptyRegMap :: RegMap a
emptyRegMap = emptyUFM

type BlockMap a = LabelMap a

type SlotMap a = UniqFM Slot a

type Slot = Int

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
        = Instr  !instr

        -- | spill this reg to a stack slot
        | SPILL  !RegWithFormat !Int

        -- | reload this reg from a stack slot
        | RELOAD !Int !RegWithFormat

        deriving (Functor)

instance Instruction instr => Instruction (InstrSR instr) where
        regUsageOfInstr platform i
         = case i of
                Instr  instr  -> regUsageOfInstr platform instr
                SPILL  reg _  -> RU [reg] []
                RELOAD _ reg  -> RU [] [reg]

        patchRegsOfInstr platform i f
         = case i of
                Instr instr     -> Instr (patchRegsOfInstr platform instr f)
                SPILL  reg slot -> SPILL (updReg f reg) slot
                RELOAD slot reg -> RELOAD slot (updReg f reg)
          where
            updReg g (RegWithFormat reg fmt) = RegWithFormat (g reg) fmt

        isJumpishInstr :: Instruction instr => InstrSR instr -> Bool
        isJumpishInstr i
         = case i of
                Instr instr     -> isJumpishInstr instr
                _               -> False

        canFallthroughTo i bid
         = case i of
                Instr instr     -> canFallthroughTo instr bid
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

        mkRegRegMoveInstr platform fmt r1 r2
            = Instr (mkRegRegMoveInstr platform fmt r1 r2)

        takeRegRegMoveInstr platform i
         = case i of
                Instr instr     -> takeRegRegMoveInstr platform instr
                _               -> Nothing

        mkJumpInstr target      = map Instr (mkJumpInstr target)

        mkStackAllocInstr platform amount =
             Instr <$> mkStackAllocInstr platform amount

        mkStackDeallocInstr platform amount =
             Instr <$> mkStackDeallocInstr platform amount

        pprInstr platform i = ppr (fmap (pprInstr platform) i)

        mkComment               = fmap Instr . mkComment


-- | An instruction with liveness information.
data LiveInstr instr
        = LiveInstr (InstrSR instr) (Maybe Liveness)
        deriving (Functor)

-- | Liveness information.
--   The regs which die are ones which are no longer live in the *next* instruction
--   in this sequence.
--   (NB. if the instruction is a jump, these registers might still be live
--   at the jump target(s) - you have to check the liveness at the destination
--   block to find out).

data Liveness
        = Liveness
        { liveBorn      :: UniqSet RegWithFormat      -- ^ registers born in this instruction (written to for first time).
        , liveDieRead   :: UniqSet RegWithFormat      -- ^ registers that died because they were read for the last time.
        , liveDieWrite  :: UniqSet RegWithFormat}     -- ^ registers that died because they were clobbered by something.


-- | Stash regs live on entry to each basic block in the info part of the cmm code.
data LiveInfo
        = LiveInfo
                (LabelMap RawCmmStatics)  -- cmm info table static stuff
                [BlockId]                 -- entry points (first one is the
                                          -- entry point for the proc).
                (BlockMap (UniqSet RegWithFormat))         -- argument locals live on entry to this block
                (BlockMap IntSet)         -- stack slots live on entry to this block


-- | A basic block with liveness information.
type LiveBasicBlock instr
        = GenBasicBlock (LiveInstr instr)


instance Outputable instr
      => Outputable (InstrSR instr) where

        ppr (Instr realInstr)
           = ppr realInstr

        ppr (SPILL (RegWithFormat reg _fmt) slot)
           = hcat [
                text "\tSPILL",
                char ' ',
                ppr reg,
                comma,
                text "SLOT" <> parens (int slot)]

        ppr (RELOAD slot (RegWithFormat reg _fmt))
           = hcat [
                text "\tRELOAD",
                char ' ',
                text "SLOT" <> parens (int slot),
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
                        [ pprRegs (text "# born:    ") (liveBorn live)
                        , pprRegs (text "# r_dying: ") (liveDieRead live)
                        , pprRegs (text "# w_dying: ") (liveDieWrite live) ]
                    $+$ space)

         where  pprRegs :: SDoc -> UniqSet RegWithFormat -> SDoc
                pprRegs name regs
                 | isEmptyUniqSet regs  = empty
                 | otherwise            = name <>
                     (pprUFM (getUniqSet regs) (hcat . punctuate space . map ppr))

instance OutputableP env instr => OutputableP env (LiveInstr instr) where
   pdoc env i = ppr (fmap (pdoc env) i)

instance OutputableP Platform LiveInfo where
    pdoc env (LiveInfo mb_static entryIds liveVRegsOnEntry liveSlotsOnEntry)
        =  (pdoc env mb_static)
        $$ text "# entryIds         = " <> ppr entryIds
        $$ text "# liveVRegsOnEntry = " <> ppr liveVRegsOnEntry
        $$ text "# liveSlotsOnEntry = " <> ppr liveSlotsOnEntry


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
        => Platform
        -> LiveCmmDecl statics instr
        -> (Bag (UniqSet RegWithFormat), Bag (Reg, Reg))

slurpConflicts platform live
        = slurpCmm (emptyBag, emptyBag) live

 where  slurpCmm   rs  CmmData{}                = rs
        slurpCmm   rs (CmmProc info _ _ sccs)
                = foldl' (slurpSCC info) rs sccs

        slurpSCC  info rs (AcyclicSCC b)
                = slurpBlock info rs b

        slurpSCC  info rs (CyclicSCC bs)
                = foldl'  (slurpBlock info) rs bs

        slurpBlock info rs (BasicBlock blockId instrs)
                | LiveInfo _ _ blockLive _        <- info
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

          in    case takeRegRegMoveInstr platform instr of
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
                   -> State (UniqFM BlockId [UniqFM Slot Reg]) [Bag (Reg, Reg)]
        slurpCompM blocks
         = do   -- run the analysis once to record the mapping across jumps.
                mapM_   (slurpBlock False) blocks

                -- run it a second time while using the information from the last pass.
                --      We /could/ run this many more times to deal with graphical control
                --      flow and propagating info across multiple jumps, but it's probably
                --      not worth the trouble.
                mapM    (slurpBlock True) blocks

        slurpBlock :: Bool -> LiveBasicBlock instr
                   -> State (UniqFM BlockId [UniqFM Slot Reg]) (Bag (Reg, Reg))
        slurpBlock propagate (BasicBlock blockId instrs)
         = do   -- grab the slot map for entry to this block
                slotMap         <- if propagate
                                        then getSlotMap blockId
                                        else return emptyUFM

                (_, mMoves)     <- mapAccumLM slurpLI slotMap instrs
                return $ listToBag $ catMaybes mMoves

        slurpLI :: SlotMap Reg                           -- current slotMap
                -> LiveInstr instr
                -> State (UniqFM BlockId [SlotMap Reg])  -- blockId -> [slot -> reg]
                                                        --      for tracking slotMaps across jumps

                         ( SlotMap Reg           -- new slotMap
                         , Maybe (Reg, Reg))            -- maybe a new coalesce edge

        slurpLI slotMap li

                -- remember what reg was stored into the slot
                | LiveInstr (SPILL (RegWithFormat reg _fmt) slot) _  <- li
                , slotMap'                                       <- addToUFM slotMap slot reg
                = return (slotMap', Nothing)

                -- add an edge between the this reg and the last one stored into the slot
                | LiveInstr (RELOAD slot (RegWithFormat reg _fmt)) _ <- li
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

        mergeSlotMaps :: SlotMap Reg -> SlotMap Reg -> SlotMap Reg
        mergeSlotMaps map1 map2
                -- toList sadly means we have to use the _Directly style
                -- functions.
                -- TODO: We shouldn't need to go through a list here.
                = listToUFM_Directly
                $ [ (k, r1)
                  | (k, r1) <- nonDetUFMToList map1
                  -- This is non-deterministic but we do not
                  -- currently support deterministic code-generation.
                  -- See Note [Unique Determinism and code generation]
                  , case lookupUFM_Directly map2 k of
                          Nothing -> False
                          Just r2 -> r1 == r2 ]


-- | Strip away liveness information, yielding NatCmmDecl
stripLive
        :: (OutputableP Platform statics, Instruction instr)
        => NCGConfig
        -> LiveCmmDecl statics instr
        -> NatCmmDecl statics instr

stripLive config live
        = stripCmm live

 where  stripCmm :: (OutputableP Platform statics, Instruction instr)
                 => LiveCmmDecl statics instr -> NatCmmDecl statics instr
        stripCmm (CmmData sec ds)       = CmmData sec ds
        stripCmm (CmmProc (LiveInfo info (first_id:_) _ _) label live sccs)
         = let  final_blocks    = flattenSCCs sccs

                -- make sure the block that was first in the input list
                --      stays at the front of the output. This is the entry point
                --      of the proc, and it needs to come first.
                final_blocks' = sortOn ((/= first_id) . blockId) final_blocks

           in   CmmProc info label live $ ListGraph $
                map (stripLiveBlock config) final_blocks'

        -- If the proc has blocks but we don't know what the first one was, then we're dead.
        stripCmm proc
                 = pprPanic "RegAlloc.Liveness.stripLive: no first_id on proc" (pprLiveCmmDecl (ncgPlatform config) proc)


-- | Pretty-print a `LiveCmmDecl`
pprLiveCmmDecl :: (OutputableP Platform statics, Instruction instr) => Platform -> LiveCmmDecl statics instr -> SDoc
pprLiveCmmDecl platform d = pdoc platform (mapLiveCmmDecl (pprInstr platform) d)


-- | Map over instruction type in `LiveCmmDecl`
mapLiveCmmDecl
   :: (instr -> b)
   -> LiveCmmDecl statics instr
   -> LiveCmmDecl statics b
mapLiveCmmDecl f proc = fmap (fmap (fmap (fmap (fmap f)))) proc

-- | Strip away liveness information from a basic block,
--   and make real spill instructions out of SPILL, RELOAD pseudos along the way.

stripLiveBlock
        :: Instruction instr
        => NCGConfig
        -> LiveBasicBlock instr
        -> NatBasicBlock instr

stripLiveBlock config (BasicBlock i lis)
 =      BasicBlock i instrs'

 where  (instrs', _)
                = runState (spillNat [] lis) 0

        -- spillNat :: [instr] -> [LiveInstr instr] -> State Int [instr]
        spillNat :: Instruction instr => [instr] -> [LiveInstr instr] -> State Int [instr]
        spillNat acc []
         =      return (reverse acc)

        -- The SPILL/RELOAD cases do not appear to be exercised by our codegens
        --
        spillNat acc (LiveInstr (SPILL reg slot) _ : instrs)
         = do   delta   <- get
                spillNat (mkSpillInstr config reg delta slot ++ acc) instrs

        spillNat acc (LiveInstr (RELOAD slot reg) _ : instrs)
         = do   delta   <- get
                spillNat (mkLoadInstr config reg delta slot ++ acc) instrs

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
        :: (Instruction instr, HasDebugCallStack)
        => Platform
        -> (Reg -> Reg)
        -> LiveCmmDecl statics instr -> LiveCmmDecl statics instr

patchEraseLive platform patchF cmm
        = patchCmm cmm
 where
        patchCmm cmm@CmmData{}  = cmm

        patchCmm (CmmProc info label live sccs)
         | LiveInfo static id blockMap mLiveSlots <- info
         = let
                  -- See Note [Unique Determinism and code generation]
                blockMap'       = mapMap (mapRegFormatSet patchF) blockMap

                info'           = LiveInfo static id blockMap' mLiveSlots
           in   CmmProc info' label live $ map patchSCC sccs

        patchSCC (AcyclicSCC b)  = AcyclicSCC (patchBlock b)
        patchSCC (CyclicSCC  bs) = CyclicSCC  (map patchBlock bs)

        patchBlock (BasicBlock id lis)
                = BasicBlock id $ patchInstrs lis

        patchInstrs []          = []
        patchInstrs (li : lis)

                | LiveInstr i (Just live)       <- li'
                , Just (r1, r2) <- takeRegRegMoveInstr platform i
                , eatMe r1 r2 live
                = patchInstrs lis

                | otherwise
                = li' : patchInstrs lis

                where   li'     = patchRegsLiveInstr platform patchF li

        eatMe   r1 r2 live
                -- source and destination regs are the same
                | r1 == r2      = True

                -- destination reg is never used
                | elemUniqSet_Directly (getUnique r2) (liveBorn live)
                , elemUniqSet_Directly (getUnique r2) (liveDieRead live) || elemUniqSet_Directly (getUnique r2) (liveDieWrite live)
                = True

                | otherwise     = False


-- | Patch registers in this LiveInstr, including the liveness information.
--
patchRegsLiveInstr
        :: (Instruction instr, HasDebugCallStack)
        => Platform
        -> (Reg -> Reg)
        -> LiveInstr instr -> LiveInstr instr

patchRegsLiveInstr platform patchF li
 = case li of
        LiveInstr instr Nothing
         -> LiveInstr (patchRegsOfInstr platform instr patchF) Nothing

        LiveInstr instr (Just live)
         -> LiveInstr
                (patchRegsOfInstr platform instr patchF)
                (Just live
                        { -- WARNING: have to go via lists here because patchF changes the uniq in the Reg
                          liveBorn      = mapRegFormatSet patchF $ liveBorn live
                        , liveDieRead   = mapRegFormatSet patchF $ liveDieRead live
                        , liveDieWrite  = mapRegFormatSet patchF $ liveDieWrite live })
                          -- See Note [Unique Determinism and code generation]

--------------------------------------------------------------------------------
-- | Convert a NatCmmDecl to a LiveCmmDecl, with liveness information

cmmTopLiveness
        :: Instruction instr
        => Maybe CFG
        -> Platform
        -> NatCmmDecl statics instr
        -> UniqDSM (LiveCmmDecl statics instr)
cmmTopLiveness cfg platform cmm
        = regLiveness platform $ natCmmTopToLive cfg cmm

natCmmTopToLive
        :: Instruction instr
        => Maybe CFG -> NatCmmDecl statics instr
        -> LiveCmmDecl statics instr

natCmmTopToLive _ (CmmData i d)
        = CmmData i d

natCmmTopToLive _ (CmmProc info lbl live (ListGraph []))
        = CmmProc (LiveInfo info [] mapEmpty mapEmpty) lbl live []

natCmmTopToLive mCfg proc@(CmmProc info lbl live (ListGraph blocks@(first : _)))
        = CmmProc (LiveInfo info' (first_id : entry_ids) mapEmpty mapEmpty)
                lbl live sccsLive
   where
        first_id        = blockId first
        all_entry_ids   = entryBlocks proc
        sccs            = sccBlocks blocks all_entry_ids mCfg
        sccsLive        = map (fmap (\(BasicBlock l instrs) ->
                                       BasicBlock l (map (\i -> LiveInstr (Instr i) Nothing) instrs)))
                        $ sccs

        entry_ids       = filter (reachable_node) .
                          filter (/= first_id) $ all_entry_ids
        info'           = mapFilterWithKey (\node _ -> reachable_node node) info
        reachable_node
          | Just cfg <- mCfg
          = hasNode cfg
          | otherwise
          = const True

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
        :: forall instr . Instruction instr
        => [NatBasicBlock instr]
        -> [BlockId]
        -> Maybe CFG
        -> [SCC (NatBasicBlock instr)]

sccBlocks blocks entries mcfg = map (fmap node_payload) sccs
  where
        nodes :: [ Node BlockId (NatBasicBlock instr) ]
        nodes = [ DigraphNode block id (getOutEdges instrs)
                | block@(BasicBlock id instrs) <- blocks ]

        g1 = graphFromEdgedVerticesUniq nodes

        reachable :: LabelSet
        reachable
            | Just cfg <- mcfg
            -- Our CFG only contains reachable nodes by construction at this point.
            = setFromList $ getCfgNodes cfg
            | otherwise
            = setFromList $ [ node_key node | node <- reachablesG g1 roots ]

        g2 = graphFromEdgedVerticesUniq [ node | node <- nodes
                                               , node_key node
                                                  `setMember` reachable ]

        sccs = stronglyConnCompG g2

        getOutEdges :: Instruction instr => [instr] -> [BlockId]
        getOutEdges instrs = concatMap jumpDestsOfInstr instrs

        -- This is truly ugly, but I don't see a good alternative.
        -- Digraph just has the wrong API.  We want to identify nodes
        -- by their keys (BlockId), but Digraph requires the whole
        -- node: (NatBasicBlock, BlockId, [BlockId]).  This takes
        -- advantage of the fact that Digraph only looks at the key,
        -- even though it asks for the whole triple.
        roots = [DigraphNode (panic "sccBlocks") b (panic "sccBlocks")
                | b <- entries ]

--------------------------------------------------------------------------------
-- Annotate code with register liveness information
--

regLiveness
        :: Instruction instr
        => Platform
        -> LiveCmmDecl statics instr
        -> UniqDSM (LiveCmmDecl statics instr)

regLiveness _ (CmmData i d)
        = return $ CmmData i d

regLiveness _ (CmmProc info lbl live [])
        | LiveInfo static mFirst _ _    <- info
        = return $ CmmProc
                        (LiveInfo static mFirst mapEmpty mapEmpty)
                        lbl live []

regLiveness platform (CmmProc info lbl live sccs)
        | LiveInfo static mFirst _ liveSlotsOnEntry     <- info
        = let   (ann_sccs, block_live)  = computeLiveness platform sccs

          in    return $ CmmProc (LiveInfo static mFirst block_live liveSlotsOnEntry)
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
           in   case nonDetEltsUniqSet badDests of
                 -- See Note [Unique Determinism and code generation]
                 []             -> go blocksSeen' sccs
                 bad : _        -> Just bad

        go blocksSeen (CyclicSCC blocks : sccs)
         = let  dests           = unionManyUniqSets $ map slurpJumpDestsOfBlock blocks
                blocksSeen'     = unionUniqSets blocksSeen $ mkUniqSet $ map blockId blocks
                badDests        = dests `minusUniqSet` blocksSeen'
           in   case nonDetEltsUniqSet badDests of
                 -- See Note [Unique Determinism and code generation]
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
        :: Instruction instr
        => Platform
        -> [SCC (LiveBasicBlock instr)]
        -> ([SCC (LiveBasicBlock instr)],       -- instructions annotated with list of registers
                                                -- which are "dead after this instruction".
               BlockMap (UniqSet RegWithFormat))                 -- blocks annotated with set of live registers
                                                -- on entry to the block.

computeLiveness platform sccs
 = case checkIsReverseDependent sccs of
        Nothing         -> livenessSCCs platform mapEmpty [] sccs
        Just bad        -> let sccs' = fmap (fmap (fmap (fmap (pprInstr platform)))) sccs
                           in pprPanic "RegAlloc.Liveness.computeLiveness"
                                (vcat   [ text "SCCs aren't in reverse dependent order"
                                        , text "bad blockId" <+> ppr bad
                                        , ppr sccs'])

livenessSCCs
       :: Instruction instr
       => Platform
       -> BlockMap (UniqSet RegWithFormat)
       -> [SCC (LiveBasicBlock instr)]          -- accum
       -> [SCC (LiveBasicBlock instr)]
       -> ( [SCC (LiveBasicBlock instr)]
          , BlockMap (UniqSet RegWithFormat))

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

            iterateUntilUnchanged f eq aa b = go aa
              where
                go a = if eq a a' then ac else go a'
                  where
                    ac@(a', _) = f a b

            linearLiveness
                :: Instruction instr
                => BlockMap (UniqSet RegWithFormat) -> [LiveBasicBlock instr]
                -> (BlockMap (UniqSet RegWithFormat), [LiveBasicBlock instr])

            linearLiveness = mapAccumL (livenessBlock platform)

                -- probably the least efficient way to compare two
                -- BlockMaps for equality.
            equalBlockMaps a b
                = a' == b'
              where a' = mapToList a
                    b' = mapToList b
                    -- See Note [Unique Determinism and code generation]


-- | Annotate a basic block with register liveness information.
--
livenessBlock
        :: Instruction instr
        => Platform
        -> BlockMap (UniqSet RegWithFormat)
        -> LiveBasicBlock instr
        -> (BlockMap (UniqSet RegWithFormat), LiveBasicBlock instr)

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
        -> UniqSet RegWithFormat -- regs live on this instr
        -> [LiveInstr instr] -> [LiveInstr instr]

livenessForward _        _           []  = []
livenessForward platform rsLiveEntry (li@(LiveInstr instr mLive) : lis)
        | Just live <- mLive
        = let
                RU _ written  = regUsageOfInstr platform instr
                -- Regs that are written to but weren't live on entry to this instruction
                --      are recorded as being born here.
                rsBorn          = mkUniqSet
                                $ filter (\ r -> not $ elemUniqSet_Directly (getUnique r) rsLiveEntry)
                                $ written

                rsLiveNext      = (rsLiveEntry `unionUniqSets` rsBorn)
                                        `minusUniqSet` (liveDieRead live)
                                        `minusUniqSet` (liveDieWrite live)

        in LiveInstr instr (Just live { liveBorn = rsBorn })
                : livenessForward platform rsLiveNext lis

        | otherwise
        = li : livenessForward platform rsLiveEntry lis


-- | Calculate liveness going backwards,
--   filling in when regs die, and what regs are live across each instruction

livenessBack
        :: Instruction instr
        => Platform
        -> UniqSet RegWithFormat            -- regs live on this instr
        -> BlockMap (UniqSet RegWithFormat) -- regs live on entry to other BBs
        -> [LiveInstr instr]            -- instructions (accum)
        -> [LiveInstr instr]            -- instructions
        -> (UniqSet RegWithFormat, [LiveInstr instr])

livenessBack _        liveregs _        done []  = (liveregs, done)

livenessBack platform liveregs blockmap acc (instr : instrs)
 = let  !(!liveregs', instr')     = liveness1 platform liveregs blockmap instr
   in   livenessBack platform liveregs' blockmap (instr' : acc) instrs


-- don't bother tagging comments or deltas with liveness
liveness1
        :: Instruction instr
        => Platform
        -> UniqSet RegWithFormat
        -> BlockMap (UniqSet RegWithFormat)
        -> LiveInstr instr
        -> (UniqSet RegWithFormat, LiveInstr instr)

liveness1 _ liveregs _ (LiveInstr instr _)
        | isMetaInstr instr
        = (liveregs, LiveInstr instr Nothing)

liveness1 platform liveregs blockmap (LiveInstr instr _)

        | not_a_branch
        = (liveregs1, LiveInstr instr
                        (Just $ Liveness
                        { liveBorn      = emptyUniqSet
                        , liveDieRead   = r_dying
                        , liveDieWrite  = w_dying }))

        | otherwise
        = (liveregs_br, LiveInstr instr
                        (Just $ Liveness
                        { liveBorn      = emptyUniqSet
                        , liveDieRead   = r_dying_br
                        , liveDieWrite  = w_dying }))

        where
            !(RU read written) = regUsageOfInstr platform instr

            -- registers that were written here are dead going backwards.
            -- registers that were read here are live going backwards.
            liveregs1   = (liveregs `delListFromUniqSet` written)
                                    `addListToUniqSet` read

            -- registers that are not live beyond this point, are recorded
            --  as dying here.
            r_dying     = mkUniqSet
                          [ reg
                          | reg@(RegWithFormat r _) <- read
                          , not $ any (\ w -> getUnique w == getUnique r) written
                          , not (elementOfUniqSet reg liveregs) ]

            w_dying     = mkUniqSet
                          [ reg
                          | reg <- written
                          , not (elementOfUniqSet reg liveregs) ]

            -- union in the live regs from all the jump destinations of this
            -- instruction.
            targets      = jumpDestsOfInstr instr -- where we go from here
            not_a_branch = null targets

            targetLiveRegs target
                  = case mapLookup target blockmap of
                                Just ra -> ra
                                Nothing -> emptyUniqSet

            live_from_branch = unionManyUniqSets (map targetLiveRegs targets)

            liveregs_br = liveregs1 `unionUniqSets` live_from_branch

            -- registers that are live only in the branch targets should
            -- be listed as dying here.
            live_branch_only = live_from_branch `minusUniqSet` liveregs
            r_dying_br  = (r_dying `unionUniqSets` live_branch_only)
                          -- See Note [Unique Determinism and code generation]

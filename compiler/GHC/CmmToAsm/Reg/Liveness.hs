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
        regLivenessBorn,
        cmmTopLiveness,

        module GHC.CmmToAsm.Reg.Regs
  ) where
import GHC.Prelude

import GHC.Platform.Reg
import GHC.CmmToAsm.Instr
import qualified GHC.CmmToAsm.X86.Instr as X86 (Instr)
import GHC.CmmToAsm.X86 ()  -- instance Instruction X86.Instr
import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.CmmToAsm.Reg.Regs

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm

import GHC.Data.Graph.Directed
import GHC.Data.OrdList
import GHC.Data.Maybe (expectJust)
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Types.Unique (Uniquable(..))
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSM
import Data.Functor.Classes (liftEq)
import GHC.Data.Bag
import GHC.Utils.Monad.StrictState

import Data.List (sortOn)
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
        { liveBorn      :: Regs      -- ^ registers born in this instruction (written to for first time);
                                     --   empty until 'regLivenessBorn' fills it
                                     --   in, an unfilled one being
                                     --   indistinguishable from one with no
                                     --   register born.
        , liveDieRead   :: Regs      -- ^ registers that died because they were read for the last time.
                                     --   At a branch, also those live into its
                                     --   targets only.
        , liveDieWrite  :: Regs      -- ^ registers that died because they were clobbered by something.
        , liveUsage     :: RegUsage  -- ^ the registers the instruction reads and writes, as
                                     --   'regUsageOfInstr' gives them, kept so that later passes
                                     --   need not compute them again.
        , liveAcross    :: Regs      -- ^ where the instruction writes an allocatable real register, the
                                     --   registers live after it (empty elsewhere), so that the
                                     --   linear allocator can find the virtual registers live across
                                     --   such a write; the graph allocator's patching renames it but
                                     --   does not compute it again.
        }


-- | Stash regs live on entry to each basic block in the info part of the cmm code.
data LiveInfo
        = LiveInfo
                (LabelMap RawCmmStatics)  -- cmm info table static stuff
                [BlockId]                 -- entry points (first one is the
                                          -- entry point for the proc).
                (BlockMap Regs)       -- argument locals live on entry to this block
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

         where  pprRegs :: SDoc -> Regs -> SDoc
                pprRegs name ( Regs regs )
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
        -> (Bag Regs, Bag (Reg, Reg))

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
                rsLiveAcross    = rsLiveEntry `minusRegs` (liveDieRead live)

                -- regs live on entry to the next instruction.
                --      be careful of orphans, make sure to delete dying regs _after_ unioning
                --      in the ones that are born here.
                rsLiveNext      = (rsLiveAcross `unionRegsMaxFmt`  (liveBorn     live))
                                                `minusCoveredRegs` (liveDieWrite live)

                -- orphan vregs are the ones that die in the same instruction they are born in.
                --      these are likely to be results that are never used, but we still
                --      need to assign a hreg to them..
                rsOrphans       = intersectRegsMaxFmt
                                        (liveBorn live)
                                        (unionRegsMaxFmt (liveDieWrite live) (liveDieRead live))

                rsConflicts     = unionRegsMaxFmt rsLiveNext rsOrphans

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
                  -- Each entry is kept or dropped on its own and the list
                  -- becomes a map again, so its order does not matter
                  -- (Note [Unique Determinism and code generation])
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
                = runState (spillNat nilOL lis) 0

        spillNat :: Instruction instr => OrdList instr -> [LiveInstr instr] -> State Int [instr]
        spillNat acc []
         =      return (fromOL acc)

        spillNat acc (LiveInstr (SPILL reg slot) _ : instrs)
         = do   delta   <- get
                spillNat (acc `appOL` toOL (mkSpillInstr config reg delta slot)) instrs

        spillNat acc (LiveInstr (RELOAD slot reg) _ : instrs)
         = do   delta   <- get
                spillNat (acc `appOL` toOL (mkLoadInstr config reg delta slot)) instrs

        spillNat acc (LiveInstr (Instr instr) _ : instrs)
         | Just i <- takeDeltaInstr instr
         = do   put i
                spillNat acc instrs
         | otherwise
         =      spillNat (acc `snocOL` instr) instrs


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
                blockMap'       = mapMap (mapRegs patchF) blockMap

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
                | r2 `elemRegs` liveBorn live
                , r2 `elemRegs` liveDieRead live || r2 `elemRegs` liveDieWrite live
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
         -> let instr' = patchRegsOfInstr platform instr patchF
            in  LiveInstr instr'
                (Just live
                        { -- WARNING: have to go via lists here because patchF changes the uniq in the Reg
                          liveBorn      = mapRegs patchF $ liveBorn live
                        , liveDieRead   = mapRegs patchF $ liveDieRead live
                        , liveDieWrite  = mapRegs patchF $ liveDieWrite live
                          -- computed again: an instruction's usage can
                          -- depend on whether its registers are equal
                          -- (Note [What x86's regUsageOfInstr reports] in
                          -- GHC.CmmToAsm.X86.Instr)
                        , liveUsage     = regUsageOfInstr platform instr'
                        , liveAcross    = mapRegs patchF $ liveAcross live })
                          -- See Note [Unique Determinism and code generation]

--------------------------------------------------------------------------------
-- | Convert a NatCmmDecl to a LiveCmmDecl, with liveness information

cmmTopLiveness
        :: Instruction instr
        => Maybe CFG
        -> Platform
        -> NatCmmDecl statics instr
        -> UniqDSM (LiveCmmDecl statics instr)
{-# NOINLINE [0] cmmTopLiveness #-}
{-# SPECIALISE cmmTopLiveness
      :: Maybe CFG -> Platform -> NatCmmDecl statics X86.Instr
      -> UniqDSM (LiveCmmDecl statics X86.Instr) #-}
      -- See Note [Specialising the linear allocator] in
      -- "GHC.CmmToAsm.Reg.Linear"
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

        reachable :: BlockId -> Bool
        reachable
            | Just cfg <- mcfg
            -- Our CFG only contains reachable nodes by construction at this point.
            = (`mapMember` cfg)
            | otherwise
            = (`setMember` setFromList (map node_key (reachablesG g1 roots)))

        g2 = graphFromEdgedVerticesUniq [ node | node <- nodes
                                               , reachable (node_key node) ]

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
        = let   (ann_sccs, block_live)  = livenessSCCs platform mapEmpty [] sccs

          in    return $ CmmProc (LiveInfo static mFirst block_live liveSlotsOnEntry)
                           lbl live ann_sccs


-- | Fill in when registers are born ('liveBorn').  Only the graph allocator
--   and the liveness dump use it, and it costs a forward walk over every
--   instruction, so it is a pass of its own, which the linear allocator skips.
regLivenessBorn
        :: LiveCmmDecl statics instr
        -> LiveCmmDecl statics instr

regLivenessBorn decl@(CmmData {})
        = decl

regLivenessBorn (CmmProc info@(LiveInfo _ _ block_live _) lbl live sccs)
        = CmmProc info lbl live (map (fmap born) sccs)
        where   born (BasicBlock block_id instrs)
                 = BasicBlock block_id
                 $ livenessForward
                        (mapFindWithDefault noRegs block_id block_live) instrs


-- | If we've compute liveness info for this code already we have to reverse
--   the SCCs in each top to get them back to the right order so we can do it again.
reverseBlocksInTops :: LiveCmmDecl statics instr -> LiveCmmDecl statics instr
reverseBlocksInTops top
 = case top of
        CmmData{}                       -> top
        CmmProc info lbl live sccs      -> CmmProc info lbl live (reverse sccs)


-- | The block map with every block of a cyclic SCC in it, no register live
--   into it, as before the SCC's first walk: a jump target missing from the
--   map is then one the SCCs are out of order for, or one the CFG lacks
--   (liveness1).
seedCyclic :: BlockMap Regs -> [LiveBasicBlock instr] -> BlockMap Regs
seedCyclic = foldl' (\m b -> mapInsert (blockId b) noRegs m)

-- | Computing liveness
--
--  On entry, the SCCs must be in "reverse" order: later blocks may transfer
--  control to earlier ones only, else `panic`. Every block must end in jumps
--  that name all its successors ('jumpDestsOfInstr'), none falling through:
--  a block's walk adds the registers live into each target at its jump, so
--  a register live into a block it fell into would count as dead at its end.
--
--  The SCCs returned are in the *opposite* order, which is exactly what we
--  want for the next pass.
--
livenessSCCs
       :: Instruction instr
       => Platform
       -> BlockMap Regs
       -> [SCC (LiveBasicBlock instr)]          -- accum
       -> [SCC (LiveBasicBlock instr)]
       -> ( [SCC (LiveBasicBlock instr)]
          , BlockMap Regs)

livenessSCCs _ !blockmap done []
        = (done, blockmap)

livenessSCCs platform !blockmap done (AcyclicSCC block : sccs)
 = let  !(!blockmap', block')   = livenessBlock platform blockmap block
   in   livenessSCCs platform blockmap' (AcyclicSCC block' : done) sccs

livenessSCCs platform !blockmap done
        (CyclicSCC blocks : sccs) =
        livenessSCCs platform blockmap' (CyclicSCC blocks':done) sccs
 where      !(!blockmap', blocks') = livenessCyclic platform blockmap blocks


-- | Annotate the blocks of a cyclic SCC with register liveness information,
--   walking a block again only when the registers live on entry to one of
--   its successors in the SCC, or their formats, have changed since its last
--   walk, until none have.  The blocks come back in their order.
livenessCyclic
        :: Instruction instr
        => Platform
        -> BlockMap Regs
        -> [LiveBasicBlock instr]
        -> (BlockMap Regs, [LiveBasicBlock instr])

livenessCyclic platform blockmap0 blocks
 = go (seedCyclic blockmap0 blocks) mapEmpty (setFromList ids) ids []
 where
        ids     = map blockId blocks
        byId    = mapFromList (zip ids blocks)
        -- the blocks of the SCC jumping to each of its blocks
        preds   = mapFromListWith (++)
                    [ (dest, [blockId b])
                    | b@(BasicBlock _ instrs) <- blocks
                    , LiveInstr instr _ <- instrs
                    , dest <- jumpDestsOfInstr instr
                    , dest `mapMember` byId ]
        get m i = expectJust (mapLookup i m)
        -- the registers live on entry changed, or their formats ('Regs''
        -- Eq compares only the registers)
        changed a b = not (liftEq sameFormat (regsMap a) (regsMap b))
        sameFormat r q = regWithFormat_format r == regWithFormat_format q
        regsMap = ufmToIntMap . getUniqSet . getRegs

        -- the blocks to walk: front ++ reverse back, the set of them queued
        go !bm !out _       []          []   = (bm, map (get out) ids)
        go !bm !out !queued []          back = go bm out queued (reverse back) []
        go !bm !out !queued (i : front) back
         = let  (bm', block')   = livenessBlock platform bm (get byId i)
                queued1         = setDelete i queued
                push (q, bk) p
                  | p `setMember` q = (q, bk)
                  | otherwise       = (setInsert p q, p : bk)
                (queued', back')
                  | changed (get bm i) (get bm' i)
                  = foldl' push (queued1, back) (mapFindWithDefault [] i preds)
                  | otherwise
                  = (queued1, back)
           in   go bm' (mapInsert i block' out) queued' front back'



-- | Annotate a basic block with register liveness information.
--
livenessBlock
        :: Instruction instr
        => Platform
        -> BlockMap Regs
        -> LiveBasicBlock instr
        -> (BlockMap Regs, LiveBasicBlock instr)

livenessBlock platform blockmap (BasicBlock block_id instrs)
 = let  !(!regsLiveOnEntry, instrs')
            = livenessBack platform noRegs blockmap [] (reverse instrs)
        !blockmap' = mapInsert block_id regsLiveOnEntry blockmap
   in   (blockmap', BasicBlock block_id instrs')

-- | Calculate liveness going forwards,
--   filling in when regs are born

livenessForward
        :: Regs -- regs live on this instr
        -> [LiveInstr instr] -> [LiveInstr instr]

livenessForward _           []  = []
livenessForward rsLiveEntry (li@(LiveInstr instr mLive) : lis)
        | Just live <- mLive
        = let
                RU _ rsWritten  = liveUsage live
                -- Regs that are written to but weren't live on entry to this instruction
                --      are recorded as being born here.
                rsBorn          = mkRegsMaxFmt
                                    [ reg
                                    | reg@( RegWithFormat r _ ) <- rsWritten
                                    , not $ r `elemRegs` rsLiveEntry
                                    ]

                   -- See Note [Register formats in liveness analysis]
                rsLiveNext      = (rsLiveEntry `addRegsMaxFmt` rsWritten)
                                        `minusRegs` (liveDieRead live)  -- (FmtFwd1)
                                        `minusRegs` (liveDieWrite live) -- (FmtFwd2)

        in LiveInstr instr (Just live { liveBorn = rsBorn })
                : livenessForward rsLiveNext lis

        | otherwise
        = li : livenessForward rsLiveEntry lis


-- | Calculate liveness going backwards,
--   filling in when regs die, and what regs are live across each instruction

livenessBack
        :: Instruction instr
        => Platform
        -> Regs           -- ^ regs live on this instr
        -> BlockMap Regs  -- ^ regs live on entry to other BBs
        -> [LiveInstr instr]  -- ^ instructions (accum)
        -> [LiveInstr instr]  -- ^ instructions
        -> (Regs, [LiveInstr instr])

livenessBack _        liveregs _        done []  = (liveregs, done)

livenessBack platform liveregs blockmap acc (instr : instrs)
 = let  !(!liveregs', instr')     = liveness1 platform liveregs blockmap instr
   in   livenessBack platform liveregs' blockmap (instr' : acc) instrs

-- don't bother tagging comments or deltas with liveness
liveness1
        :: Instruction instr
        => Platform
        -> Regs
        -> BlockMap Regs
        -> LiveInstr instr
        -> (Regs, LiveInstr instr)

liveness1 _ liveregs _ (LiveInstr instr _)
        | isMetaInstr instr
        = (liveregs, LiveInstr instr Nothing)

liveness1 platform liveregs blockmap (LiveInstr instr _)

        | not_a_branch
        = (liveregs1, LiveInstr instr (Just live))

        | otherwise
        = r_dying_br `seq` (liveregs_br, LiveInstr instr
                        (Just live { liveDieRead = r_dying_br
                                   , liveAcross  = across_br }))

        where
            live = Liveness
                { liveBorn      = noRegs
                , liveDieRead   = r_dying
                , liveDieWrite  = w_dying
                , liveUsage     = usage
                , liveAcross    = across }
            !usage@(RU read written) = regUsageOfInstr platform instr

            -- the registers live after this instruction, where it writes a
            -- real register
            writesReal  = any (isRealReg . regWithFormat_reg) written
            !across     | writesReal = liveregs
                        | otherwise  = noRegs
            across_br   | writesReal = liveregs `unionRegsMaxFmt` live_from_branch
                        | otherwise  = noRegs

            -- registers that were written here are dead going backwards.
            -- registers that were read here are live going backwards.
            -- As for the formats, see Note [Register formats in liveness analysis]
            liveregs1   = (liveregs `minusCoveredRegs` mkRegsMaxFmt written) -- (FmtBwd2)
                                    `addRegsMaxFmt` read                     -- (FmtBwd1)

            -- registers that are not live beyond this point are recorded
            -- as dying here.
            !r_dying    = mkRegsMaxFmt
                          [ reg
                          | reg@(RegWithFormat r _) <- read
                          , not $ any (\ w -> getUnique w == getUnique r) written
                          , not $ r `elemRegs` liveregs ]

            !w_dying    = mkRegsMaxFmt
                          [ reg
                          | reg@(RegWithFormat r _) <- written
                          , not $ r `elemRegs` liveregs ]

            -- union in the live regs from all the jump destinations of this
            -- instruction.
            targets      = jumpDestsOfInstr instr -- where we go from here
            not_a_branch = null targets

            -- the map has every jump target: a block of an SCC walked before,
            -- or one of a cyclic SCC's (seedCyclic); a target missing means
            -- the SCCs are out of order, or, where the CFG gave the reachable
            -- blocks (sccBlocks), that it lacks a block a reachable one jumps
            -- to; the liveness would be wrong
            targetLiveRegs target
                  = case mapLookup target blockmap of
                                Just ra -> ra
                                Nothing -> pprPanic "RegAlloc.Liveness.liveness1"
                                             (text "SCCs aren't in reverse dependent order,"
                                              <+> text "or the CFG lacks the block of"
                                              <+> text "a jump to" <+> ppr target)

            -- registers that are live only in the branch targets should
            -- be listed as dying here.
            live_from_branch = unionManyRegsMaxFmt (map targetLiveRegs targets)
            liveregs_br = liveregs1 `unionRegsMaxFmt` live_from_branch
            live_branch_only = live_from_branch `minusRegs` liveregs
            r_dying_br  = r_dying `unionRegsMaxFmt` live_branch_only
              -- NB: we treat registers live in branches similar to any other
              -- registers read by the instruction, so the logic here matches
              -- the logic in the definition of 'r_dying' above.

{- Note [Register formats in liveness analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We keep track of which format each virtual register is live at, and make use
of this information during liveness analysis.

First, we do backwards liveness analysis:

  (FmtBwd1) Take the larger format when computing registers live going backwards.

    Suppose for example that we have:

      <previous instructions>
      movps  %v0 %v1
      movupd %v0 %v2

    Here we read %v0 both at format F64 and F64x2, so we must consider it live
    at format F64x2, going backwards, in the previous instructions.
    Not doing so caused #26411.

  (FmtBwd2) Only consider fully clobbered registers to be dead going backwards.

    Consider for example the liveness of %v0 going backwards in the following
    instruction block:

      movlhps %v5 %v0  -- write the upper F64 of %v0
      movupd  %v1 %v2  -- some unrelated instruction
      movsd   %v3 %v0  -- write the lower F64 of %v0
      movupd  %v0 %v4  -- read %v0 at format F64x2

    We must not consider %v0 to be dead going backwards from 'movsd %v3 %v0'.
    If we do, that means we think %v0 is dead during 'movupd %v1 %v2', and thus
    that we can assign both %v0 and %v2 to the same real register. However, this
    would be catastrophic, as 'movupd %v1 %v2' would then clobber the data
    written to '%v0' in 'movlhps %v5 %v0'.

    Wrinkle [Don't allow scalar partial writes]

      We don't allow partial writes within scalar registers, for many reasons:

        - partial writes can cause partial register stalls, which can have
          disastrous performance implications (as seen in #20405)
        - partial writes makes register allocation more difficult, as they can
          require preserving the contents of a register across many instructions,
          as in:

            mulw %v0             -- 32-bit write to %rax
            <many instructions>
            mulb %v1             -- 16-bit partial write to %rax

          The current register allocator is not equipped for spilling real
          registers (only virtual registers), which means that e.g. on i386 we
          end up with only 2 allocatable real GP registers for <many instructions>,
          which is insufficient for instructions that require 3 registers.

      We could allow this to be customised depending on the architecture, but
      currently we simply never allow scalar partial writes.

The forwards analysis is a bit simpler:

  (FmtFwd1) Remove without considering format when dead going forwards.

    If a register is no longer read after an instruction, then it is dead
    going forwards. The format doesn't matter.

  (FmtFwd2) Consider all writes as making a register dead going forwards.

    If we write to the lower 64 bits of a 128 bit register, we don't currently
    have a way to say "the lower 64 bits are dead but the top 64 bits are still live".
    We would need a notion of partial register, similar to 'VirtualRegHi' for
    the top 32 bits of a I32x2 virtual register.

    As a result, the current approach is to consider the entire register to
    be dead. This might cause us to unnecessarily spill/reload an entire vector
    register to avoid its lower bits getting clobbered even though later
    instructions might only care about its upper bits.

The backwards analysis of a cyclic SCC iterates to a fixpoint:

  (FmtFix) The walk order of a cyclic SCC does not matter.

    The fixpoint livenessCyclic finds does not depend on the order its blocks
    are walked in, because (FmtBwd1) and (FmtBwd2) are monotone: live sets and
    their formats only grow ('unionRegsMaxFmt', 'addRegsMaxFmt'), and a larger
    format is killed by no more writes ('minusCoveredRegs'). A change that let a
    format shrink would make the result depend on that order. Larger means later
    in Format's Ord, which ranks a vector by its number of elements before their
    format, so VecFormat 16 FmtInt8 counts as larger than VecFormat 4 FmtDouble,
    though narrower: a register read at both would be kept at the narrower.
-}

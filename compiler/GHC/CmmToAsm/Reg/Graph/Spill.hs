-- | When there aren't enough registers to hold all the vregs we have to spill
--   some of those vregs to slots on the stack. This module is used modify the
--   code to use those slots.
module GHC.CmmToAsm.Reg.Graph.Spill (
        regSpill,
        SpillStats(..),
        accSpillSL
) where

import GHC.Prelude

import GHC.CmmToAsm.Format ( RegWithFormat(..) )
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Reg.Utils
import GHC.CmmToAsm.Instr
import GHC.Platform.Reg
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label


import GHC.Utils.Monad
import GHC.Utils.Monad.State.Strict
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Types.Unique.DSM
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform

import Data.Function ( on )
import Data.List (intersectBy, nubBy)
import Data.Maybe
import Data.IntSet              (IntSet)
import qualified Data.IntSet    as IntSet


-- | Spill all these virtual regs to stack slots.
--
--   Bumps the number of required stack slots if required.
--
--
--   TODO: See if we can split some of the live ranges instead of just globally
--         spilling the virtual reg. This might make the spill cleaner's job easier.
--
--   TODO: On CISCy x86 and x86_64 we don't necessarily have to add a mov instruction
--         when making spills. If an instr is using a spilled virtual we may be able to
--         address the spill slot directly.
--
regSpill
        :: Instruction instr
        => Platform
        -> [LiveCmmDecl statics instr]  -- ^ the code
        -> UniqSet Int                  -- ^ available stack slots
        -> Int                          -- ^ current number of spill slots.
        -> UniqSet VirtualReg           -- ^ the regs to spill
        -> UniqDSM
            ([LiveCmmDecl statics instr]
                 -- code with SPILL and RELOAD meta instructions added.
            , UniqSet Int               -- left over slots
            , Int                       -- slot count in use now.
            , SpillStats )              -- stats about what happened during spilling

regSpill platform code slotsFree slotCount regs

        -- Not enough slots to spill these regs.
        | sizeUniqSet slotsFree < sizeUniqSet regs
        = -- pprTrace "Bumping slot count:" (ppr slotCount <> text " -> " <> ppr (slotCount+512)) $
          let slotsFree' = (addListToUniqSet slotsFree [slotCount+1 .. slotCount+512])
          in regSpill platform code slotsFree' (slotCount+512) regs

        | otherwise
        = do
                -- Allocate a slot for each of the spilled regs.
                let slots       = take (sizeUniqSet regs) $ nonDetEltsUniqSet slotsFree
                let
                    regSlotMap  = toRegMap -- Cast keys from VirtualReg to Reg
                                           -- See Note [UniqFM and the register allocator]
                                $ listToUFM
                                $ zip (nonDetEltsUniqSet regs) slots :: UniqFM Reg Int
                    -- This is non-deterministic but we do not
                    -- currently support deterministic code-generation.
                    -- See Note [Unique Determinism and code generation]

                -- Grab the unique supply from the monad.
                UDSM $ \us ->

                  -- Run the spiller on all the blocks.
                  let (code', state')     =
                          runState (mapM (regSpill_top platform regSlotMap) code)
                                   (initSpillS us)

                   in DUniqResult
                        ( code'
                        , minusUniqSet slotsFree (mkUniqSet slots)
                        , slotCount
                        , makeSpillStats state')
                        ( stateUS state' )



-- | Spill some registers to stack slots in a top-level thing.
regSpill_top
        :: Instruction instr
        => Platform
        -> RegMap Int
                -- ^ map of vregs to slots they're being spilled to.
        -> LiveCmmDecl statics instr
                -- ^ the top level thing.
        -> SpillM (LiveCmmDecl statics instr)

regSpill_top platform regSlotMap cmm
 = case cmm of
        CmmData{}
         -> return cmm

        CmmProc info label live sccs
         |  LiveInfo static firstId liveVRegsOnEntry liveSlotsOnEntry <- info
         -> do
                -- The liveVRegsOnEntry contains the set of vregs that are live
                -- on entry to each basic block. If we spill one of those vregs
                -- we remove it from that set and add the corresponding slot
                -- number to the liveSlotsOnEntry set. The spill cleaner needs
                -- this information to erase unneeded spill and reload instructions
                -- after we've done a successful allocation.
                let liveSlotsOnEntry' :: BlockMap IntSet
                    liveSlotsOnEntry'
                        = mapFoldlWithKey patchLiveSlot
                                          liveSlotsOnEntry liveVRegsOnEntry

                let info'
                        = LiveInfo static firstId
                                liveVRegsOnEntry
                                liveSlotsOnEntry'

                -- Apply the spiller to all the basic blocks in the CmmProc.
                sccs'   <- mapM (mapSCCM (regSpill_block platform regSlotMap)) sccs

                return  $ CmmProc info' label live sccs'

 where  -- Given a BlockId and the set of registers live in it,
        -- if registers in this block are being spilled to stack slots,
        -- then record the fact that these slots are now live in those blocks
        -- in the given slotmap.
        patchLiveSlot
                :: BlockMap IntSet -> BlockId -> UniqSet RegWithFormat-> BlockMap IntSet

        patchLiveSlot slotMap blockId regsLive
         = let
                -- Slots that are already recorded as being live.
                curSlotsLive    = fromMaybe IntSet.empty
                                $ mapLookup blockId slotMap

                moreSlotsLive   = IntSet.fromList
                                $ mapMaybe (lookupUFM regSlotMap . regWithFormat_reg)
                                $ nonDetEltsUniqSet regsLive
                    -- See Note [Unique Determinism and code generation]

                slotMap'
                 = mapInsert blockId (IntSet.union curSlotsLive moreSlotsLive)
                             slotMap

           in   slotMap'


-- | Spill some registers to stack slots in a basic block.
regSpill_block
        :: Instruction instr
        => Platform
        -> UniqFM Reg Int   -- ^ map of vregs to slots they're being spilled to.
        -> LiveBasicBlock instr
        -> SpillM (LiveBasicBlock instr)

regSpill_block platform regSlotMap (BasicBlock i instrs)
 = do   instrss'        <- mapM (regSpill_instr platform regSlotMap) instrs
        return  $ BasicBlock i (concat instrss')


-- | Spill some registers to stack slots in a single instruction.
--   If the instruction uses registers that need to be spilled, then it is
--   prefixed (or postfixed) with the appropriate RELOAD or SPILL meta
--   instructions.
regSpill_instr
        :: Instruction instr
        => Platform
        -> UniqFM Reg Int -- ^ map of vregs to slots they're being spilled to.
        -> LiveInstr instr
        -> SpillM [LiveInstr instr]
regSpill_instr _ _ li@(LiveInstr _ Nothing) = return [li]
regSpill_instr platform regSlotMap (LiveInstr instr (Just _)) = do
  -- work out which regs are read and written in this instr
  let RU rlRead rlWritten = regUsageOfInstr platform instr

  -- sometimes a register is listed as being read more than once,
  --      nub this so we don't end up inserting two lots of spill code.
  let rsRead_             = nubBy ((==) `on` getUnique) rlRead
      rsWritten_          = nubBy ((==) `on` getUnique) rlWritten

  -- if a reg is modified, it appears in both lists, want to undo this..
  let rsModify            = intersectBy ((==) `on` getUnique) rsRead_ rsWritten_
      modified            = mkUniqSet rsModify
      rsRead              = filter (\ r -> not $ elementOfUniqSet r modified) rsRead_
      rsWritten           = filter (\ r -> not $ elementOfUniqSet r modified) rsWritten_


  -- work out if any of the regs being used are currently being spilled.
  let rsSpillRead         = filter (\r -> elemUFM (regWithFormat_reg r) regSlotMap) rsRead
  let rsSpillWritten      = filter (\r -> elemUFM (regWithFormat_reg r) regSlotMap) rsWritten
  let rsSpillModify       = filter (\r -> elemUFM (regWithFormat_reg r) regSlotMap) rsModify

  -- rewrite the instr and work out spill code.
  (instr1, prepost1)      <- mapAccumLM (spillRead   platform regSlotMap) instr  rsSpillRead
  (instr2, prepost2)      <- mapAccumLM (spillWrite  platform regSlotMap) instr1 rsSpillWritten
  (instr3, prepost3)      <- mapAccumLM (spillModify platform regSlotMap) instr2 rsSpillModify

  let (mPrefixes, mPostfixes) = unzip (prepost1 ++ prepost2 ++ prepost3)
  let prefixes                = concat mPrefixes
  let postfixes               = concat mPostfixes

  -- final code
  let instrs' =  prefixes
              ++ [LiveInstr instr3 Nothing]
              ++ postfixes

  return instrs'


-- | Add a RELOAD met a instruction to load a value for an instruction that
--   writes to a vreg that is being spilled.
spillRead
        :: Instruction instr
        => Platform
        -> UniqFM Reg Int
        -> instr
        -> RegWithFormat
        -> SpillM (instr, ([LiveInstr instr'], [LiveInstr instr']))

spillRead platform regSlotMap instr (RegWithFormat reg fmt)
 | Just slot     <- lookupUFM regSlotMap reg
 = do    (instr', nReg)  <- patchInstr platform reg instr

         modify $ \s -> s
                { stateSpillSL  = addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 0, 1) }

         return  ( instr'
                 , ( [LiveInstr (RELOAD slot (RegWithFormat nReg fmt)) Nothing]
                 , []) )

 | otherwise     = panic "RegSpill.spillRead: no slot defined for spilled reg"


-- | Add a SPILL meta instruction to store a value for an instruction that
--   writes to a vreg that is being spilled.
spillWrite
        :: Instruction instr
        => Platform
        -> UniqFM Reg Int
        -> instr
        -> RegWithFormat
        -> SpillM (instr, ([LiveInstr instr'], [LiveInstr instr']))

spillWrite platform regSlotMap instr (RegWithFormat reg fmt)
 | Just slot     <- lookupUFM regSlotMap reg
 = do    (instr', nReg)  <- patchInstr platform reg instr

         modify $ \s -> s
                { stateSpillSL  = addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 1, 0) }

         return  ( instr'
                 , ( []
                   , [LiveInstr (SPILL (RegWithFormat nReg fmt) slot) Nothing]))

 | otherwise     = panic "RegSpill.spillWrite: no slot defined for spilled reg"


-- | Add both RELOAD and SPILL meta instructions for an instruction that
--   both reads and writes to a vreg that is being spilled.
spillModify
        :: Instruction instr
        => Platform
        -> UniqFM Reg Int
        -> instr
        -> RegWithFormat
        -> SpillM (instr, ([LiveInstr instr'], [LiveInstr instr']))

spillModify platform regSlotMap instr (RegWithFormat reg fmt)
 | Just slot     <- lookupUFM regSlotMap reg
 = do    (instr', nReg)  <- patchInstr platform reg instr

         modify $ \s -> s
                { stateSpillSL  = addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 1, 1) }

         return  ( instr'
                 , ( [LiveInstr (RELOAD slot (RegWithFormat nReg fmt)) Nothing]
                   , [LiveInstr (SPILL (RegWithFormat nReg fmt) slot) Nothing]))

 | otherwise     = panic "RegSpill.spillModify: no slot defined for spilled reg"


-- | Rewrite uses of this virtual reg in an instr to use a different
--   virtual reg.
patchInstr
        :: Instruction instr
        => Platform -> Reg -> instr -> SpillM (instr, Reg)

patchInstr platform reg instr
 = do   nUnique         <- newUnique

        -- The register we're rewriting is supposed to be virtual.
        -- If it's not then something has gone horribly wrong.
        let nReg
             = case reg of
                RegVirtual vr
                 -> RegVirtual (renameVirtualReg nUnique vr)

                RegReal{}
                 -> panic "RegAlloc.Graph.Spill.patchIntr: not patching real reg"

        let instr'      = patchReg1 platform reg nReg instr
        return          (instr', nReg)


patchReg1
        :: Instruction instr
        => Platform -> Reg -> Reg -> instr -> instr

patchReg1 platform old new instr
 = let  patchF r
                | r == old      = new
                | otherwise     = r
   in   patchRegsOfInstr platform instr patchF


-- Spiller monad --------------------------------------------------------------
-- | State monad for the spill code generator.
type SpillM = State SpillS

-- | Spill code generator state.
data SpillS
        = SpillS
        { -- | Unique supply for generating fresh vregs.
          stateUS       :: DUniqSupply

          -- | Spilled vreg vs the number of times it was loaded, stored.
        , stateSpillSL  :: UniqFM Reg (Reg, Int, Int) }

instance MonadGetUnique SpillM where
  getUniqueM = do
    us <- gets stateUS
    case takeUniqueFromDSupply us of
     (uniq, us')
      -> do modify $ \s -> s { stateUS = us' }
            return uniq


-- | Create a new spiller state.
initSpillS :: DUniqSupply -> SpillS
initSpillS uniqueSupply
        = SpillS
        { stateUS       = uniqueSupply
        , stateSpillSL  = emptyUFM }


-- | Allocate a new unique in the spiller monad.
newUnique :: SpillM Unique
newUnique = getUniqueM


-- | Add a spill/reload count to a stats record for a register.
accSpillSL :: (Reg, Int, Int) -> (Reg, Int, Int) -> (Reg, Int, Int)
accSpillSL (r1, s1, l1) (_, s2, l2)
        = (r1, s1 + s2, l1 + l2)


-- Spiller stats --------------------------------------------------------------
-- | Spiller statistics.
--   Tells us what registers were spilled.
data SpillStats
        = SpillStats
        { spillStoreLoad        :: UniqFM Reg (Reg, Int, Int) }


-- | Extract spiller statistics from the spiller state.
makeSpillStats :: SpillS -> SpillStats
makeSpillStats s
        = SpillStats
        { spillStoreLoad        = stateSpillSL s }


instance Outputable SpillStats where
 ppr stats
        = pprUFM (spillStoreLoad stats)
                 (vcat . map (\(r, s, l) -> ppr r <+> int s <+> int l))

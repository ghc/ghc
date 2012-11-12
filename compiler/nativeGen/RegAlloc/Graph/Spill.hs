
-- | When there aren't enough registers to hold all the vregs we have to spill
--   some of those vregs to slots on the stack. This module is used modify the
--   code to use those slots.
module RegAlloc.Graph.Spill (
        regSpill,
        SpillStats(..),
        accSpillSL
) where
import RegAlloc.Liveness
import Instruction
import Reg
import Cmm hiding (RegSet)
import BlockId

import State
import Unique
import UniqFM
import UniqSet
import UniqSupply
import Outputable
import Platform

import Data.List
import Data.Maybe
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Spill all these virtual regs to stack slots.
--
--   TODO: See if we can split some of the live ranges instead of just globally
--         spilling the virtual reg. This might make the spill cleaner's job easier.
--
--   TODO: On CISCy x86 and x86_64 we don't nessesarally have to add a mov instruction
--         when making spills. If an instr is using a spilled virtual we may be able to
--         address the spill slot directly.
--
regSpill
        :: Instruction instr
        => Platform
        -> [LiveCmmDecl statics instr]  -- ^ the code
        -> UniqSet Int                  -- ^ available stack slots
        -> UniqSet VirtualReg           -- ^ the regs to spill
        -> UniqSM
            ([LiveCmmDecl statics instr] 
                 -- code with SPILL and RELOAD meta instructions added.
            , UniqSet Int               -- left over slots
            , SpillStats )              -- stats about what happened during spilling

regSpill platform code slotsFree regs

        -- Not enough slots to spill these regs.
        | sizeUniqSet slotsFree < sizeUniqSet regs
        = pprPanic "regSpill: out of spill slots!"
                (  text "   regs to spill = " <> ppr (sizeUniqSet regs)
                $$ text "   slots left    = " <> ppr (sizeUniqSet slotsFree))

        | otherwise
        = do
                -- Allocate a slot for each of the spilled regs.
                let slots       = take (sizeUniqSet regs) $ uniqSetToList slotsFree
                let regSlotMap  = listToUFM
                                $ zip (uniqSetToList regs) slots

                -- Grab the unique supply from the monad.
                us      <- getUs

                -- Run the spiller on all the blocks.
                let (code', state')     =
                        runState (mapM (regSpill_top platform regSlotMap) code)
                                 (initSpillS us)

                return  ( code'
                        , minusUniqSet slotsFree (mkUniqSet slots)
                        , makeSpillStats state')


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
         |  LiveInfo static firstId mLiveVRegsOnEntry liveSlotsOnEntry <- info
         -> do
                -- We should only passed Cmms with the liveness maps filled in,
                -- but we'll create empty ones if they're not there just in case.
                let liveVRegsOnEntry    = fromMaybe mapEmpty mLiveVRegsOnEntry

                -- The liveVRegsOnEntry contains the set of vregs that are live
                -- on entry to each basic block. If we spill one of those vregs
                -- we remove it from that set and add the corresponding slot
                -- number to the liveSlotsOnEntry set. The spill cleaner needs
                -- this information to erase unneeded spill and reload instructions
                -- after we've done a successful allocation.
                let liveSlotsOnEntry' :: Map BlockId (Set Int)
                    liveSlotsOnEntry'
                        = mapFoldWithKey patchLiveSlot 
                                         liveSlotsOnEntry liveVRegsOnEntry

                let info'
                        = LiveInfo static firstId
                                (Just liveVRegsOnEntry)
                                liveSlotsOnEntry'

                -- Apply the spiller to all the basic blocks in the CmmProc.
                sccs'   <- mapM (mapSCCM (regSpill_block platform regSlotMap)) sccs

                return  $ CmmProc info' label live sccs'

 where  -- Given a BlockId and the set of registers live in it,
        -- if registers in this block are being spilled to stack slots,
        -- then record the fact that these slots are now live in those blocks
        -- in the given slotmap.
        patchLiveSlot 
                :: BlockId -> RegSet 
                -> Map BlockId (Set Int) -> Map BlockId (Set Int)

        patchLiveSlot blockId regsLive slotMap
         = let  
                -- Slots that are already recorded as being live.
                curSlotsLive    = fromMaybe Set.empty
                                $ Map.lookup blockId slotMap

                moreSlotsLive   = Set.fromList
                                $ catMaybes
                                $ map (lookupUFM regSlotMap)
                                $ uniqSetToList regsLive

                slotMap'
                 = Map.insert blockId (Set.union curSlotsLive moreSlotsLive) 
                              slotMap

           in   slotMap'


-- | Spill some registers to stack slots in a basic block.
regSpill_block
        :: Instruction instr
        => Platform
        -> UniqFM Int   -- ^ map of vregs to slots they're being spilled to.
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
        -> UniqFM Int -- ^ map of vregs to slots they're being spilled to.
        -> LiveInstr instr
        -> SpillM [LiveInstr instr]

regSpill_instr _ _ li@(LiveInstr _ Nothing)
 = do   return [li]

regSpill_instr platform regSlotMap
        (LiveInstr instr (Just _))
 = do
        -- work out which regs are read and written in this instr
        let RU rlRead rlWritten = regUsageOfInstr platform instr

        -- sometimes a register is listed as being read more than once,
        --      nub this so we don't end up inserting two lots of spill code.
        let rsRead_             = nub rlRead
        let rsWritten_          = nub rlWritten

        -- if a reg is modified, it appears in both lists, want to undo this..
        let rsRead              = rsRead_    \\ rsWritten_
        let rsWritten           = rsWritten_ \\ rsRead_
        let rsModify            = intersect rsRead_ rsWritten_

        -- work out if any of the regs being used are currently being spilled.
        let rsSpillRead         = filter (\r -> elemUFM r regSlotMap) rsRead
        let rsSpillWritten      = filter (\r -> elemUFM r regSlotMap) rsWritten
        let rsSpillModify       = filter (\r -> elemUFM r regSlotMap) rsModify

        -- rewrite the instr and work out spill code.
        (instr1, prepost1)      <- mapAccumLM (spillRead   regSlotMap) instr  rsSpillRead
        (instr2, prepost2)      <- mapAccumLM (spillWrite  regSlotMap) instr1 rsSpillWritten
        (instr3, prepost3)      <- mapAccumLM (spillModify regSlotMap) instr2 rsSpillModify

        let (mPrefixes, mPostfixes)     = unzip (prepost1 ++ prepost2 ++ prepost3)
        let prefixes                    = concat mPrefixes
        let postfixes                   = concat mPostfixes

        -- final code
        let instrs'     =  prefixes
                        ++ [LiveInstr instr3 Nothing]
                        ++ postfixes

        return $ instrs'


-- | Add a RELOAD met a instruction to load a value for an instruction that
--   writes to a vreg that is being spilled.
spillRead
        :: Instruction instr
        => UniqFM Int
        -> instr
        -> Reg
        -> SpillM (instr, ([LiveInstr instr'], [LiveInstr instr']))

spillRead regSlotMap instr reg
 | Just slot     <- lookupUFM regSlotMap reg
 = do    (instr', nReg)  <- patchInstr reg instr

         modify $ \s -> s
                { stateSpillSL  = addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 0, 1) }

         return  ( instr'
                 , ( [LiveInstr (RELOAD slot nReg) Nothing]
                 , []) )

 | otherwise     = panic "RegSpill.spillRead: no slot defined for spilled reg"


-- | Add a SPILL meta instruction to store a value for an instruction that
--   writes to a vreg that is being spilled.
spillWrite
        :: Instruction instr
        => UniqFM Int
        -> instr
        -> Reg
        -> SpillM (instr, ([LiveInstr instr'], [LiveInstr instr']))

spillWrite regSlotMap instr reg
 | Just slot     <- lookupUFM regSlotMap reg
 = do    (instr', nReg)  <- patchInstr reg instr

         modify $ \s -> s
                { stateSpillSL  = addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 1, 0) }

         return  ( instr'
                 , ( []
                   , [LiveInstr (SPILL nReg slot) Nothing]))

 | otherwise     = panic "RegSpill.spillWrite: no slot defined for spilled reg"


-- | Add both RELOAD and SPILL meta instructions for an instruction that
--   both reads and writes to a vreg that is being spilled.
spillModify
        :: Instruction instr
        => UniqFM Int
        -> instr
        -> Reg
        -> SpillM (instr, ([LiveInstr instr'], [LiveInstr instr']))

spillModify regSlotMap instr reg
 | Just slot     <- lookupUFM regSlotMap reg
 = do    (instr', nReg)  <- patchInstr reg instr

         modify $ \s -> s
                { stateSpillSL  = addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 1, 1) }

         return  ( instr'
                 , ( [LiveInstr (RELOAD slot nReg) Nothing]
                   , [LiveInstr (SPILL nReg slot) Nothing]))

 | otherwise     = panic "RegSpill.spillModify: no slot defined for spilled reg"


-- | Rewrite uses of this virtual reg in an instr to use a different
--   virtual reg.
patchInstr
        :: Instruction instr
        => Reg -> instr -> SpillM (instr, Reg)

patchInstr reg instr
 = do   nUnique         <- newUnique

        -- The register we're rewriting is suppoed to be virtual.
        -- If it's not then something has gone horribly wrong.
        let nReg
             = case reg of
                RegVirtual vr   
                 -> RegVirtual (renameVirtualReg nUnique vr)

                RegReal{}       
                 -> panic "RegAlloc.Graph.Spill.patchIntr: not patching real reg"

        let instr'      = patchReg1 reg nReg instr
        return          (instr', nReg)


patchReg1
        :: Instruction instr
        => Reg -> Reg -> instr -> instr

patchReg1 old new instr
 = let  patchF r
                | r == old      = new
                | otherwise     = r
   in   patchRegsOfInstr instr patchF


-- Spiller monad --------------------------------------------------------------
-- | State monad for the spill code generator.
type SpillM a
        = State SpillS a

-- | Spill code generator state.
data SpillS
        = SpillS
        { -- | Unique supply for generating fresh vregs.
          stateUS       :: UniqSupply

          -- | Spilled vreg vs the number of times it was loaded, stored.
        , stateSpillSL  :: UniqFM (Reg, Int, Int) }


-- | Create a new spiller state.
initSpillS :: UniqSupply -> SpillS
initSpillS uniqueSupply
        = SpillS
        { stateUS       = uniqueSupply
        , stateSpillSL  = emptyUFM }


-- | Allocate a new unique in the spiller monad.
newUnique :: SpillM Unique
newUnique
 = do   us      <- gets stateUS
        case takeUniqFromSupply us of
         (uniq, us')
          -> do modify $ \s -> s { stateUS = us' }
                return uniq


-- | Add a spill/reload count to a stats record for a register.
accSpillSL :: (Reg, Int, Int) -> (Reg, Int, Int) -> (Reg, Int, Int)
accSpillSL (r1, s1, l1) (_, s2, l2)
        = (r1, s1 + s2, l1 + l2)


-- Spiller stats --------------------------------------------------------------
-- | Spiller statistics.
--   Tells us what registers were spilled.
data SpillStats
        = SpillStats
        { spillStoreLoad        :: UniqFM (Reg, Int, Int) }


-- | Extract spiller statistics from the spiller state.
makeSpillStats :: SpillS -> SpillStats
makeSpillStats s
        = SpillStats
        { spillStoreLoad        = stateSpillSL s }


instance Outputable SpillStats where
 ppr stats
        = (vcat $ map (\(r, s, l) -> ppr r <+> int s <+> int l)
                        $ eltsUFM (spillStoreLoad stats))


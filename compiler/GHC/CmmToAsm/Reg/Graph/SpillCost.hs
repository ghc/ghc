{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.CmmToAsm.Reg.Graph.SpillCost (
        SpillCostRecord,
        plusSpillCostRecord,
        pprSpillCostRecord,

        SpillCostInfo,
        zeroSpillCostInfo,
        plusSpillCostInfo,

        slurpSpillCostInfo,
        chooseSpill,

        lifeMapFromSpillCostInfo
) where
import GHC.Prelude

import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Instr
import GHC.Platform.Reg.Class
import GHC.Platform.Reg

import GHC.Data.Graph.Base

import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Data.Graph.Directed          (flattenSCCs)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Utils.Monad.State.Strict
import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.Format
import GHC.Utils.Misc

import Data.List        (nub)
import Data.Maybe
import Control.Monad (join)


-- | Records the expected cost to spill some register.
type SpillCostRecord
 =      ( VirtualReg    -- register name
        , Int           -- number of writes to this reg
        , Int           -- number of reads from this reg
        , Int)          -- number of instrs this reg was live on entry to


-- | Map of `SpillCostRecord`
type SpillCostInfo
        = UniqFM VirtualReg SpillCostRecord

type SpillCostState = State SpillCostInfo ()

-- | An empty map of spill costs.
zeroSpillCostInfo :: SpillCostInfo
zeroSpillCostInfo       = emptyUFM


-- | Add two spill cost infos.
plusSpillCostInfo :: SpillCostInfo -> SpillCostInfo -> SpillCostInfo
plusSpillCostInfo sc1 sc2
        = plusUFM_C plusSpillCostRecord sc1 sc2


-- | Add two spill cost records.
plusSpillCostRecord :: SpillCostRecord -> SpillCostRecord -> SpillCostRecord
plusSpillCostRecord (r1, a1, b1, c1) (r2, a2, b2, c2)
        | r1 == r2      = (r1, a1 + a2, b1 + b2, c1 + c2)
        | otherwise     = error "RegSpillCost.plusRegInt: regs don't match"


-- | Slurp out information used for determining spill costs.
--
--   For each vreg, the number of times it was written to, read from,
--   and the number of instructions it was live on entry to (lifetime)
--
slurpSpillCostInfo :: forall instr statics. Instruction instr
                   => Platform
                   -> Maybe CFG
                   -> LiveCmmDecl statics instr
                   -> SpillCostInfo

slurpSpillCostInfo platform cfg cmm
        = execState (countCmm cmm) zeroSpillCostInfo
 where
        countCmm CmmData{}              = return ()
        countCmm (CmmProc info _ _ sccs)
                = mapM_ (countBlock info freqMap)
                $ flattenSCCs sccs
            where
                LiveInfo _ entries _ _ = info
                freqMap = (fst . mkGlobalWeights (head entries)) <$> cfg

        -- Lookup the regs that are live on entry to this block in
        --      the info table from the CmmProc.
        countBlock info freqMap (BasicBlock blockId instrs)
                | LiveInfo _ _ blockLive _ <- info
                , Just rsLiveEntry  <- mapLookup blockId blockLive
                , rsLiveEntry_virt  <- takeVirtualRegs rsLiveEntry
                = countLIs (ceiling $ blockFreq freqMap blockId) rsLiveEntry_virt instrs

                | otherwise
                = error "RegAlloc.SpillCost.slurpSpillCostInfo: bad block"


        countLIs :: Int -> UniqSet VirtualReg -> [LiveInstr instr] -> SpillCostState
        countLIs _      _      []
                = return ()

        -- Skip over comment and delta pseudo instrs.
        countLIs scale rsLive (LiveInstr instr Nothing : lis)
                | isMetaInstr instr
                = countLIs scale rsLive lis

                | otherwise
                = pprPanic "RegSpillCost.slurpSpillCostInfo"
                $ text "no liveness information on instruction " <> pprInstr platform instr

        countLIs scale rsLiveEntry (LiveInstr instr (Just live) : lis)
         = do
                -- Increment the lifetime counts for regs live on entry to this instr.
                mapM_ incLifetime $ nonDetEltsUniqSet rsLiveEntry
                    -- This is non-deterministic but we do not
                    -- currently support deterministic code-generation.
                    -- See Note [Unique Determinism and code generation]

                -- Increment counts for what regs were read/written from.
                let (RU read written)   = regUsageOfInstr platform instr
                mapM_ (incUses scale) $ nub $ mapMaybe (takeVirtualReg . regWithFormat_reg) read
                mapM_ (incDefs scale) $ nub $ mapMaybe (takeVirtualReg . regWithFormat_reg) written

                -- Compute liveness for entry to next instruction.
                let liveDieRead_virt    = takeVirtualRegs (liveDieRead  live)
                let liveDieWrite_virt   = takeVirtualRegs (liveDieWrite live)
                let liveBorn_virt       = takeVirtualRegs (liveBorn     live)

                let rsLiveAcross
                        = rsLiveEntry `minusUniqSet` liveDieRead_virt

                let rsLiveNext
                        = (rsLiveAcross `unionUniqSets` liveBorn_virt)
                                        `minusUniqSet`  liveDieWrite_virt

                countLIs scale rsLiveNext lis

        incDefs     count reg = modify $ \s -> addToUFM_C plusSpillCostRecord s reg (reg, count, 0, 0)
        incUses     count reg = modify $ \s -> addToUFM_C plusSpillCostRecord s reg (reg, 0, count, 0)
        incLifetime       reg = modify $ \s -> addToUFM_C plusSpillCostRecord s reg (reg, 0, 0, 1)

        blockFreq :: Maybe (LabelMap Double) -> Label -> Double
        blockFreq freqs bid
          | Just freq <- join (mapLookup bid <$> freqs)
          = max 1.0 (10000 * freq)
          | otherwise
          = 1.0 -- Only if no cfg given

-- | Choose a node to spill from this graph
chooseSpill
        :: SpillCostInfo
        -> Graph VirtualReg RegClass RealReg
        -> VirtualReg

chooseSpill info graph
 = let  cost    = spillCost_length info graph
        node    = minimumBy (\n1 n2 -> compare (cost $ nodeId n1) (cost $ nodeId n2))
                $ expectNonEmpty $ nonDetEltsUFM $ graphMap graph
                -- See Note [Unique Determinism and code generation]

   in   nodeId node


-------------------------------------------------------------------------------
-- | Chaitins spill cost function is:
--
--   cost =     sum         loadCost * freq (u)  +    sum        storeCost * freq (d)
--          u <- uses (v)                         d <- defs (v)
--
--   There are no loops in our code at the moment, so we can set the freq's to 1.
--
--  If we don't have live range splitting then Chaitins function performs badly
--  if we have lots of nested live ranges and very few registers.
--
--               v1 v2 v3
--      def v1   .
--      use v1   .
--      def v2   .  .
--      def v3   .  .  .
--      use v1   .  .  .
--      use v3   .  .  .
--      use v2   .  .
--      use v1   .
--
--           defs uses degree   cost
--      v1:  1     3     3      1.5
--      v2:  1     2     3      1.0
--      v3:  1     1     3      0.666
--
--   v3 has the lowest cost, but if we only have 2 hardregs and we insert
--   spill code for v3 then this isn't going to improve the colorability of
--   the graph.
--
--  When compiling SHA1, which as very long basic blocks and some vregs
--  with very long live ranges the allocator seems to try and spill from
--  the inside out and eventually run out of stack slots.
--
--  Without live range splitting, its's better to spill from the outside
--  in so set the cost of very long live ranges to zero
--

-- spillCost_chaitin
--         :: SpillCostInfo
--         -> Graph VirtualReg RegClass RealReg
--         -> VirtualReg
--         -> Float

-- spillCost_chaitin info graph reg
--         -- Spilling a live range that only lives for 1 instruction
--         -- isn't going to help us at all - and we definitely want to avoid
--         -- trying to re-spill previously inserted spill code.
--         | lifetime <= 1         = 1/0

--         -- It's unlikely that we'll find a reg for a live range this long
--         -- better to spill it straight up and not risk trying to keep it around
--         -- and have to go through the build/color cycle again.

--         -- To facility this we scale down the spill cost of long ranges.
--         -- This makes sure long ranges are still spilled first.
--         -- But this way spill cost remains relevant for long live
--         -- ranges.
--         | lifetime >= 128
--         = (spillCost / conflicts) / 10.0


--         -- Otherwise revert to chaitin's regular cost function.
--         | otherwise = (spillCost / conflicts)
--         where
--             !spillCost = fromIntegral (uses + defs) :: Float
--             conflicts = fromIntegral (nodeDegree classOfVirtualReg graph reg)
--             (_, defs, uses, lifetime)
--                 = fromMaybe (reg, 0, 0, 0) $ lookupUFM info reg


-- Just spill the longest live range.
spillCost_length
        :: SpillCostInfo
        -> Graph VirtualReg RegClass RealReg
        -> VirtualReg
        -> Float

spillCost_length info _ reg
        | lifetime <= 1         = 1/0
        | otherwise             = 1 / fromIntegral lifetime
        where (_, _, _, lifetime)
                = fromMaybe (reg, 0, 0, 0)
                $ lookupUFM info reg


-- | Extract a map of register lifetimes from a `SpillCostInfo`.
lifeMapFromSpillCostInfo :: SpillCostInfo -> UniqFM VirtualReg (VirtualReg, Int)
lifeMapFromSpillCostInfo info
        = listToUFM
        $ map (\(r, _, _, life) -> (r, (r, life)))
        $ nonDetEltsUFM info
        -- See Note [Unique Determinism and code generation]


-- | Determine the degree (number of neighbors) of this node which
--   have the same class.
nodeDegree
        :: (VirtualReg -> RegClass)
        -> Graph VirtualReg RegClass RealReg
        -> VirtualReg
        -> Int

nodeDegree classOfVirtualReg graph reg
        | Just node     <- lookupUFM (graphMap graph) reg

        , virtConflicts
           <- length
           $ filter (\r -> classOfVirtualReg r == classOfVirtualReg reg)
           $ nonDetEltsUniqSet
           -- See Note [Unique Determinism and code generation]
           $ nodeConflicts node

        = virtConflicts + sizeUniqSet (nodeExclusions node)

        | otherwise
        = 0


-- | Show a spill cost record, including the degree from the graph
--   and final calculated spill cost.
pprSpillCostRecord
        :: (VirtualReg -> RegClass)
        -> (Reg -> SDoc)
        -> Graph VirtualReg RegClass RealReg
        -> SpillCostRecord
        -> SDoc

pprSpillCostRecord regClass pprReg graph (reg, uses, defs, life)
        =  hsep
        [ pprReg (RegVirtual reg)
        , ppr uses
        , ppr defs
        , ppr life
        , ppr $ nodeDegree regClass graph reg
        , text $ show $ (fromIntegral (uses + defs)
                       / fromIntegral (nodeDegree regClass graph reg) :: Float) ]


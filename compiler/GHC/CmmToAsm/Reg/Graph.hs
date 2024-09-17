{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Graph coloring register allocator.
module GHC.CmmToAsm.Reg.Graph (
        regAlloc
) where
import GHC.Prelude

import qualified GHC.Data.Graph.Color as Color
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Reg.Graph.Spill
import GHC.CmmToAsm.Reg.Graph.SpillClean
import GHC.CmmToAsm.Reg.Graph.SpillCost
import GHC.CmmToAsm.Reg.Graph.Stats
import GHC.CmmToAsm.Reg.Graph.TrivColorable
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.Platform.Reg.Class
import GHC.Platform.Reg

import GHC.Data.Bag
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Types.Unique.DSM
import GHC.Utils.Misc (seqList, HasDebugCallStack)
import GHC.CmmToAsm.CFG

import Data.Maybe
import Control.Monad


-- | The maximum number of build\/spill cycles we'll allow.
--
--   It should only take 3 or 4 cycles for the allocator to converge.
--   If it takes any longer than this it's probably in an infinite loop,
--   so it's better just to bail out and report a bug.
maxSpinCount    :: Int
maxSpinCount    = 10


-- | The top level of the graph coloring register allocator.
regAlloc
        :: (OutputableP Platform statics, Instruction instr)
        => NCGConfig
        -> UniqFM RegClass (UniqSet RealReg)     -- ^ registers we can use for allocation
        -> UniqSet Int                  -- ^ set of available spill slots.
        -> Int                          -- ^ current number of spill slots
        -> [LiveCmmDecl statics instr]  -- ^ code annotated with liveness information.
        -> Maybe CFG                    -- ^ CFG of basic blocks if available
        -> UniqDSM ( [NatCmmDecl statics instr]
                   , Maybe Int, [RegAllocStats statics instr] )
           -- ^ code with registers allocated, additional stacks required
           -- and stats for each stage of allocation

regAlloc config regsFree slotsFree slotsCount code cfg
 = do
        let platform = ncgPlatform config
            triv = trivColorable platform
                        (targetVirtualRegSqueeze platform)
                        (targetRealRegSqueeze platform)

        (code_final, debug_codeGraphs, slotsCount', _)
                <- regAlloc_spin config 0
                        triv
                        regsFree slotsFree slotsCount [] code cfg

        let needStack
                | slotsCount == slotsCount'
                = Nothing
                | otherwise
                = Just slotsCount'

        return  ( code_final
                , needStack
                , reverse debug_codeGraphs )


-- | Perform solver iterations for the graph coloring allocator.
--
--   We extract a register conflict graph from the provided cmm code,
--   and try to colour it. If that works then we use the solution rewrite
--   the code with real hregs. If coloring doesn't work we add spill code
--   and try to colour it again. After `maxSpinCount` iterations we give up.
--
regAlloc_spin
        :: forall instr statics.
           (Instruction instr,
            OutputableP Platform statics,
            HasDebugCallStack)
        => NCGConfig
        -> Int  -- ^ Number of solver iterations we've already performed.
        -> Color.Triv VirtualReg RegClass RealReg
                -- ^ Function for calculating whether a register is trivially
                --   colourable.
        -> UniqFM RegClass (UniqSet RealReg)      -- ^ Free registers that we can allocate.
        -> UniqSet Int                   -- ^ Free stack slots that we can use.
        -> Int                           -- ^ Number of spill slots in use
        -> [RegAllocStats statics instr] -- ^ Current regalloc stats to add to.
        -> [LiveCmmDecl statics instr]   -- ^ Liveness annotated code to allocate.
        -> Maybe CFG
        -> UniqDSM ( [NatCmmDecl statics instr]
                  , [RegAllocStats statics instr]
                  , Int                  -- Slots in use
                  , Color.Graph VirtualReg RegClass RealReg)

regAlloc_spin config spinCount triv regsFree slotsFree slotsCount debug_codeGraphs code cfg
 = do
        let platform = ncgPlatform config

        -- If any of these dump flags are turned on we want to hang on to
        -- intermediate structures in the allocator - otherwise tell the
        -- allocator to ditch them early so we don't end up creating space leaks.
        let dump = or
                [ ncgDumpRegAllocStages config
                , ncgDumpAsmStats       config
                , ncgDumpAsmConflicts   config
                ]

        -- Check that we're not running off down the garden path.
        when (spinCount > maxSpinCount)
         $ pprPanic "regAlloc_spin: max build/spill cycle count exceeded."
           (  text "It looks like the register allocator is stuck in an infinite loop."
           $$ text "max cycles  = " <> int maxSpinCount
           $$ text "regsFree    = " <> (hcat $ punctuate space $ map ppr
                                             $ nonDetEltsUniqSet $ unionManyUniqSets
                                             $ nonDetEltsUFM regsFree)
              -- This is non-deterministic but we do not
              -- currently support deterministic code-generation.
              -- See Note [Unique Determinism and code generation]
           $$ text "slotsFree   = " <> ppr (sizeUniqSet slotsFree))

        -- Build the register conflict graph from the cmm code.
        (graph  :: Color.Graph VirtualReg RegClass RealReg)
                <- {-# SCC "BuildGraph" #-} buildGraph platform code

        -- VERY IMPORTANT:
        --   We really do want the graph to be fully evaluated _before_ we
        --   start coloring. If we don't do this now then when the call to
        --   Color.colorGraph forces bits of it, the heap will be filled with
        --   half evaluated pieces of graph and zillions of apply thunks.
        seqGraph graph `seq` return ()

        -- Build a map of the cost of spilling each instruction.
        -- This is a lazy binding, so the map will only be computed if we
        -- actually have to spill to the stack.
        let spillCosts  = foldl' plusSpillCostInfo zeroSpillCostInfo
                        $ map (slurpSpillCostInfo platform cfg) code

        -- The function to choose regs to leave uncolored.
        let spill       = chooseSpill spillCosts

        -- Record startup state in our log.
        let stat1
             = if spinCount == 0
                 then   Just $ RegAllocStatsStart
                        { raLiveCmm     = code
                        , raGraph       = graph
                        , raSpillCosts  = spillCosts
                        , raPlatform    = platform
                        }
                 else   Nothing

        -- Try and color the graph.
        let (graph_colored, rsSpill, rmCoalesce)
                = {-# SCC "ColorGraph" #-}
                  Color.colorGraph
                       (ncgRegsIterative config)
                       spinCount
                       regsFree triv spill graph

        -- Rewrite registers in the code that have been coalesced.
        let patchF reg
                | RegVirtual vr <- reg
                = case lookupUFM rmCoalesce vr of
                        Just vr'        -> patchF (RegVirtual vr')
                        Nothing         -> reg

                | otherwise
                = reg

        let (code_coalesced :: [LiveCmmDecl statics instr])
                = map (patchEraseLive platform patchF) code

        -- Check whether we've found a coloring.
        if isEmptyUniqSet rsSpill

         -- Coloring was successful because no registers needed to be spilled.
         then do
                -- if -fasm-lint is turned on then validate the graph.
                -- This checks for bugs in the graph allocator itself.
                let graph_colored_lint  =
                        if ncgAsmLinting config
                                then Color.validateGraph (text "")
                                        True    -- Require all nodes to be colored.
                                        graph_colored
                                else graph_colored

                -- Rewrite the code to use real hregs, using the colored graph.
                let code_patched
                        = map (patchRegsFromGraph platform graph_colored_lint)
                              code_coalesced

                -- Clean out unneeded SPILL/RELOAD meta instructions.
                --   The spill code generator just spills the entire live range
                --   of a vreg, but it might not need to be on the stack for
                --   its entire lifetime.
                let code_spillclean
                        = map (cleanSpills config) code_patched

                -- Strip off liveness information from the allocated code.
                -- Also rewrite SPILL/RELOAD meta instructions into real machine
                -- instructions along the way
                let code_final
                        = map (stripLive config) code_spillclean

                -- Record what happened in this stage for debugging
                let stat
                     =  RegAllocStatsColored
                        { raCode                = code
                        , raGraph               = graph
                        , raGraphColored        = graph_colored_lint
                        , raCoalesced           = rmCoalesce
                        , raCodeCoalesced       = code_coalesced
                        , raPatched             = code_patched
                        , raSpillClean          = code_spillclean
                        , raFinal               = code_final
                        , raSRMs                = foldl' addSRM (0, 0, 0)
                                                $ map (countSRMs platform) code_spillclean
                        , raPlatform    = platform
                     }

                -- Bundle up all the register allocator statistics.
                --   .. but make sure to drop them on the floor if they're not
                --      needed, otherwise we'll get a space leak.
                let statList =
                        if dump then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
                                else []

                -- Ensure all the statistics are evaluated, to avoid space leaks.
                seqList statList (return ())

                return  ( code_final
                        , statList
                        , slotsCount
                        , graph_colored_lint)

         -- Coloring was unsuccessful. We need to spill some register to the
         -- stack, make a new graph, and try to color it again.
         else do
                -- if -fasm-lint is turned on then validate the graph
                let graph_colored_lint  =
                        if ncgAsmLinting config
                                then Color.validateGraph (text "")
                                        False   -- don't require nodes to be colored
                                        graph_colored
                                else graph_colored

                -- Spill uncolored regs to the stack.
                (code_spilled, slotsFree', slotsCount', spillStats)
                        <- regSpill platform code_coalesced slotsFree slotsCount rsSpill

                -- Recalculate liveness information.
                -- NOTE: we have to reverse the SCCs here to get them back into
                --       the reverse-dependency order required by computeLiveness.
                --       If they're not in the correct order that function will panic.
                code_relive     <- mapM (regLiveness platform . reverseBlocksInTops)
                                        code_spilled

                -- Record what happened in this stage for debugging.
                let stat        =
                        RegAllocStatsSpill
                        { raCode        = code
                        , raGraph       = graph_colored_lint
                        , raCoalesced   = rmCoalesce
                        , raSpillStats  = spillStats
                        , raSpillCosts  = spillCosts
                        , raSpilled     = code_spilled
                        , raPlatform    = platform }

                -- Bundle up all the register allocator statistics.
                --   .. but make sure to drop them on the floor if they're not
                --      needed, otherwise we'll get a space leak.
                let statList =
                        if dump
                                then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
                                else []

                -- Ensure all the statistics are evaluated, to avoid space leaks.
                seqList statList (return ())

                regAlloc_spin config (spinCount + 1) triv regsFree slotsFree'
                              slotsCount' statList code_relive cfg


-- | Build a graph from the liveness and coalesce information in this code.
buildGraph
        :: Instruction instr
        => Platform
        -> [LiveCmmDecl statics instr]
        -> UniqDSM (Color.Graph VirtualReg RegClass RealReg)

buildGraph platform code
 = do
        -- Slurp out the conflicts and reg->reg moves from this code.
        let (conflictList, moveList) =
                unzip $ map (slurpConflicts platform) code

        -- Slurp out the spill/reload coalesces.
        let moveList2           = map slurpReloadCoalesce code

        -- Add the reg-reg conflicts to the graph.
        let conflictBag         = unionManyBags conflictList
        let graph_conflict
                = foldr (graphAddConflictSet platform) Color.initGraph conflictBag

        -- Add the coalescences edges to the graph.
        let moveBag
                = unionBags (unionManyBags moveList2)
                            (unionManyBags moveList)

        let graph_coalesce
                = foldr (graphAddCoalesce platform) graph_conflict moveBag

        return  graph_coalesce


-- | Add some conflict edges to the graph.
--   Conflicts between virtual and real regs are recorded as exclusions.
graphAddConflictSet
        :: Platform
        -> UniqSet RegWithFormat
        -> Color.Graph VirtualReg RegClass RealReg
        -> Color.Graph VirtualReg RegClass RealReg

graphAddConflictSet platform regs graph
 = let  arch = platformArch platform
        virtuals = takeVirtualRegs regs
        reals    = takeRealRegs regs

        graph1  = Color.addConflicts virtuals (classOfVirtualReg arch) graph
          -- NB: we could add "arch" as argument to functions such as "addConflicts"
          -- and "addExclusion" if it turns out that the partial application
          -- "classOfVirtualReg arch" affects performance.

        graph2  = foldr (\(r1, r2) -> Color.addExclusion r1 (classOfVirtualReg arch) r2)
                        graph1
                        [ (vr, rr)
                        | vr <- nonDetEltsUniqSet virtuals
                        , rr <- nonDetEltsUniqSet reals ]
                          -- See Note [Unique Determinism and code generation]

   in   graph2


-- | Add some coalescence edges to the graph
--   Coalescences between virtual and real regs are recorded as preferences.
graphAddCoalesce
        :: Platform
        -> (Reg, Reg)
        -> Color.Graph VirtualReg RegClass RealReg
        -> Color.Graph VirtualReg RegClass RealReg

graphAddCoalesce platform (r1, r2) graph
        | RegReal rr            <- r1
        , RegVirtual vr         <- r2
        = Color.addPreference (vr, classOfVirtualReg arch vr) rr graph

        | RegReal rr            <- r2
        , RegVirtual vr         <- r1
        = Color.addPreference (vr, classOfVirtualReg arch vr) rr graph

        | RegVirtual vr1        <- r1
        , RegVirtual vr2        <- r2
        = Color.addCoalesce
                (vr1, classOfVirtualReg arch vr1)
                (vr2, classOfVirtualReg arch vr2)
                graph

        -- We can't coalesce two real regs, but there could well be existing
        --      hreg,hreg moves in the input code. We'll just ignore these
        --      for coalescing purposes.
        | RegReal _             <- r1
        , RegReal _             <- r2
        = graph
        where
          arch = platformArch platform


-- | Patch registers in code using the reg -> reg mapping in this graph.
patchRegsFromGraph
        :: (OutputableP Platform statics, Instruction instr)
        => Platform -> Color.Graph VirtualReg RegClass RealReg
        -> LiveCmmDecl statics instr -> LiveCmmDecl statics instr

patchRegsFromGraph platform graph code
 = patchEraseLive platform patchF code
 where
        -- Function to lookup the hardreg for a virtual reg from the graph.
        patchF reg
                -- leave real regs alone.
                | RegReal{}     <- reg
                = reg

                -- this virtual has a regular node in the graph.
                | RegVirtual vr <- reg
                , Just node     <- Color.lookupNode graph vr
                = case Color.nodeColor node of
                        Just color      -> RegReal    color
                        Nothing         -> RegVirtual vr

                -- no node in the graph for this virtual, bad news.
                | otherwise
                = pprPanic "patchRegsFromGraph: register mapping failed."
                        (  text "There is no node in the graph for register "
                                <> ppr reg
                        $$ pprLiveCmmDecl platform code
                        $$ Color.dotGraph
                                (\_ -> text "white")
                                (trivColorable platform
                                        (targetVirtualRegSqueeze platform)
                                        (targetRealRegSqueeze platform))
                                graph)


-----
-- for when laziness just isn't what you wanted...
--  We need to deepSeq the whole graph before trying to colour it to avoid
--  space leaks.
seqGraph :: Color.Graph VirtualReg RegClass RealReg -> ()
seqGraph graph          = seqNodes (nonDetEltsUFM (Color.graphMap graph))
   -- See Note [Unique Determinism and code generation]

seqNodes :: [Color.Node VirtualReg RegClass RealReg] -> ()
seqNodes ns
 = case ns of
        []              -> ()
        (n : ns)        -> seqNode n `seq` seqNodes ns

seqNode :: Color.Node VirtualReg RegClass RealReg -> ()
seqNode node
        =     seqVirtualReg     (Color.nodeId node)
        `seq` seqRegClass       (Color.nodeClass node)
        `seq` seqMaybeRealReg   (Color.nodeColor node)
        `seq` (seqVirtualRegList (nonDetEltsUniqSet (Color.nodeConflicts node)))
        `seq` (seqRealRegList    (nonDetEltsUniqSet (Color.nodeExclusions node)))
        `seq` (seqRealRegList (Color.nodePreference node))
        `seq` (seqVirtualRegList (nonDetEltsUniqSet (Color.nodeCoalesce node)))
              -- It's OK to use nonDetEltsUniqSet for seq

seqVirtualReg :: VirtualReg -> ()
seqVirtualReg reg = reg `seq` ()

seqRealReg :: RealReg -> ()
seqRealReg reg = reg `seq` ()

seqRegClass :: RegClass -> ()
seqRegClass c = c `seq` ()

seqMaybeRealReg :: Maybe RealReg -> ()
seqMaybeRealReg mr
 = case mr of
        Nothing         -> ()
        Just r          -> seqRealReg r

seqVirtualRegList :: [VirtualReg] -> ()
seqVirtualRegList rs
 = case rs of
        []              -> ()
        (r : rs)        -> seqVirtualReg r `seq` seqVirtualRegList rs

seqRealRegList :: [RealReg] -> ()
seqRealRegList rs
 = case rs of
        []              -> ()
        (r : rs)        -> seqRealReg r `seq` seqRealRegList rs

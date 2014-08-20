{-# LANGUAGE ScopedTypeVariables #-}

-- | Graph coloring register allocator.
module RegAlloc.Graph.Main (
        regAlloc
) where
import qualified GraphColor as Color
import RegAlloc.Liveness
import RegAlloc.Graph.Spill
import RegAlloc.Graph.SpillClean
import RegAlloc.Graph.SpillCost
import RegAlloc.Graph.Stats
import RegAlloc.Graph.TrivColorable
import Instruction
import TargetReg
import RegClass
import Reg

import UniqSupply
import UniqSet
import UniqFM
import Bag
import Outputable
import Platform
import DynFlags

import Data.List
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
        :: (Outputable statics, Outputable instr, Instruction instr)
        => DynFlags
        -> UniqFM (UniqSet RealReg)     -- ^ registers we can use for allocation
        -> UniqSet Int                  -- ^ set of available spill slots.
        -> [LiveCmmDecl statics instr]  -- ^ code annotated with liveness information.
        -> UniqSM ( [NatCmmDecl statics instr], [RegAllocStats statics instr] )
           -- ^ code with registers allocated and stats for each stage of
           -- allocation

regAlloc dflags regsFree slotsFree code
 = do
        -- TODO: the regClass function is currently hard coded to the default
        --       target architecture. Would prefer to determine this from dflags.
        --       There are other uses of targetRegClass later in this module.
        let platform = targetPlatform dflags
            triv = trivColorable platform
                        (targetVirtualRegSqueeze platform)
                        (targetRealRegSqueeze platform)

        (code_final, debug_codeGraphs, _)
                <- regAlloc_spin dflags 0
                        triv
                        regsFree slotsFree [] code

        return  ( code_final
                , reverse debug_codeGraphs )


-- | Perform solver iterations for the graph coloring allocator.
--
--   We extract a register confict graph from the provided cmm code,
--   and try to colour it. If that works then we use the solution rewrite 
--   the code with real hregs. If coloring doesn't work we add spill code
--   and try to colour it again. After `maxSpinCount` iterations we give up.
--
regAlloc_spin 
        :: (Instruction instr,
            Outputable instr,
            Outputable statics)
        => DynFlags
        -> Int  -- ^ Number of solver iterations we've already performed.
        -> Color.Triv VirtualReg RegClass RealReg
                -- ^ Function for calculating whether a register is trivially
                --   colourable.
        -> UniqFM (UniqSet RealReg)      -- ^ Free registers that we can allocate.
        -> UniqSet Int                   -- ^ Free stack slots that we can use.
        -> [RegAllocStats statics instr] -- ^ Current regalloc stats to add to.
        -> [LiveCmmDecl statics instr]   -- ^ Liveness annotated code to allocate.
        -> UniqSM ( [NatCmmDecl statics instr]
                  , [RegAllocStats statics instr]
                  , Color.Graph VirtualReg RegClass RealReg)

regAlloc_spin dflags spinCount triv regsFree slotsFree debug_codeGraphs code
 = do
        let platform = targetPlatform dflags

        -- If any of these dump flags are turned on we want to hang on to
        -- intermediate structures in the allocator - otherwise tell the
        -- allocator to ditch them early so we don't end up creating space leaks.
        let dump = or
                [ dopt Opt_D_dump_asm_regalloc_stages dflags
                , dopt Opt_D_dump_asm_stats dflags
                , dopt Opt_D_dump_asm_conflicts dflags ]

        -- Check that we're not running off down the garden path.
        when (spinCount > maxSpinCount)
         $ pprPanic "regAlloc_spin: max build/spill cycle count exceeded."
           (  text "It looks like the register allocator is stuck in an infinite loop."
           $$ text "max cycles  = " <> int maxSpinCount
           $$ text "regsFree    = " <> (hcat $ punctuate space $ map ppr
                                             $ uniqSetToList $ unionManyUniqSets 
                                             $ eltsUFM regsFree)
           $$ text "slotsFree   = " <> ppr (sizeUniqSet slotsFree))

        -- Build the register conflict graph from the cmm code.
        (graph  :: Color.Graph VirtualReg RegClass RealReg)
                <- {-# SCC "BuildGraph" #-} buildGraph code

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
                        $ map (slurpSpillCostInfo platform) code

        -- The function to choose regs to leave uncolored.
        let spill       = chooseSpill spillCosts

        -- Record startup state in our log.
        let stat1       
             = if spinCount == 0
                 then   Just $ RegAllocStatsStart
                        { raLiveCmm     = code
                        , raGraph       = graph
                        , raSpillCosts  = spillCosts }
                 else   Nothing

        -- Try and color the graph.
        let (graph_colored, rsSpill, rmCoalesce)
                = {-# SCC "ColorGraph" #-}
                  Color.colorGraph
                       (gopt Opt_RegsIterative dflags)
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

        let code_coalesced
                = map (patchEraseLive patchF) code

        -- Check whether we've found a coloring.
        if isEmptyUniqSet rsSpill

         -- Coloring was successful because no registers needed to be spilled.
         then do
                -- if -fasm-lint is turned on then validate the graph.
                -- This checks for bugs in the graph allocator itself.
                let graph_colored_lint  =
                        if gopt Opt_DoAsmLinting dflags
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
                        = map (cleanSpills platform) code_patched

                -- Strip off liveness information from the allocated code.
                -- Also rewrite SPILL/RELOAD meta instructions into real machine
                -- instructions along the way
                let code_final
                        = map (stripLive dflags) code_spillclean

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
                                                $ map countSRMs code_spillclean }

                -- Bundle up all the register allocator statistics.
                --   .. but make sure to drop them on the floor if they're not 
                --      needed, otherwise we'll get a space leak.
                let statList =
                        if dump then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
                                else []

                -- Ensure all the statistics are evaluated, to avoid space leaks.
                seqList statList `seq` return ()

                return  ( code_final
                        , statList
                        , graph_colored_lint)

         -- Coloring was unsuccessful. We need to spill some register to the
         -- stack, make a new graph, and try to color it again.
         else do
                -- if -fasm-lint is turned on then validate the graph
                let graph_colored_lint  =
                        if gopt Opt_DoAsmLinting dflags
                                then Color.validateGraph (text "")
                                        False   -- don't require nodes to be colored
                                        graph_colored
                                else graph_colored

                -- Spill uncolored regs to the stack.
                (code_spilled, slotsFree', spillStats)
                        <- regSpill platform code_coalesced slotsFree rsSpill

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
                        , raSpilled     = code_spilled }

                -- Bundle up all the register allocator statistics.
                --   .. but make sure to drop them on the floor if they're not 
                --      needed, otherwise we'll get a space leak.
                let statList =
                        if dump
                                then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
                                else []

                -- Ensure all the statistics are evaluated, to avoid space leaks.
                seqList statList `seq` return ()

                regAlloc_spin dflags (spinCount + 1) triv regsFree slotsFree'
                        statList
                        code_relive


-- | Build a graph from the liveness and coalesce information in this code.
buildGraph
        :: Instruction instr
        => [LiveCmmDecl statics instr]
        -> UniqSM (Color.Graph VirtualReg RegClass RealReg)

buildGraph code
 = do
        -- Slurp out the conflicts and reg->reg moves from this code.
        let (conflictList, moveList) =
                unzip $ map slurpConflicts code

        -- Slurp out the spill/reload coalesces.
        let moveList2           = map slurpReloadCoalesce code

        -- Add the reg-reg conflicts to the graph.
        let conflictBag         = unionManyBags conflictList
        let graph_conflict      
                = foldrBag graphAddConflictSet Color.initGraph conflictBag

        -- Add the coalescences edges to the graph.
        let moveBag
                = unionBags (unionManyBags moveList2)
                            (unionManyBags moveList)

        let graph_coalesce
                = foldrBag graphAddCoalesce graph_conflict moveBag

        return  graph_coalesce


-- | Add some conflict edges to the graph.
--   Conflicts between virtual and real regs are recorded as exclusions.
graphAddConflictSet
        :: UniqSet Reg
        -> Color.Graph VirtualReg RegClass RealReg
        -> Color.Graph VirtualReg RegClass RealReg

graphAddConflictSet set graph
 = let  virtuals        = mkUniqSet
                        [ vr | RegVirtual vr <- uniqSetToList set ]

        graph1  = Color.addConflicts virtuals classOfVirtualReg graph

        graph2  = foldr (\(r1, r2) -> Color.addExclusion r1 classOfVirtualReg r2)
                        graph1
                        [ (vr, rr)
                                | RegVirtual vr <- uniqSetToList set
                                , RegReal    rr <- uniqSetToList set]

   in   graph2


-- | Add some coalesence edges to the graph
--   Coalesences between virtual and real regs are recorded as preferences.
graphAddCoalesce
        :: (Reg, Reg)
        -> Color.Graph VirtualReg RegClass RealReg
        -> Color.Graph VirtualReg RegClass RealReg

graphAddCoalesce (r1, r2) graph
        | RegReal rr            <- r1
        , RegVirtual vr         <- r2
        = Color.addPreference (vr, classOfVirtualReg vr) rr graph

        | RegReal rr            <- r2
        , RegVirtual vr         <- r1
        = Color.addPreference (vr, classOfVirtualReg vr) rr graph

        | RegVirtual vr1        <- r1
        , RegVirtual vr2        <- r2
        = Color.addCoalesce
                (vr1, classOfVirtualReg vr1)
                (vr2, classOfVirtualReg vr2)
                graph

        -- We can't coalesce two real regs, but there could well be existing
        --      hreg,hreg moves in the input code. We'll just ignore these
        --      for coalescing purposes.
        | RegReal _             <- r1
        , RegReal _             <- r2
        = graph

graphAddCoalesce _ _
        = panic "graphAddCoalesce: bogus"


-- | Patch registers in code using the reg -> reg mapping in this graph.
patchRegsFromGraph
        :: (Outputable statics, Outputable instr, Instruction instr)
        => Platform -> Color.Graph VirtualReg RegClass RealReg
        -> LiveCmmDecl statics instr -> LiveCmmDecl statics instr

patchRegsFromGraph platform graph code
 = patchEraseLive patchF code
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
                        $$ ppr code
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
seqGraph graph          = seqNodes (eltsUFM (Color.graphMap graph))

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
        `seq` (seqVirtualRegList (uniqSetToList (Color.nodeConflicts node)))
        `seq` (seqRealRegList    (uniqSetToList (Color.nodeExclusions node)))
        `seq` (seqRealRegList (Color.nodePreference node))
        `seq` (seqVirtualRegList (uniqSetToList (Color.nodeCoalesce node)))

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

seqList :: [a] -> ()
seqList ls
 = case ls of
        []              -> ()
        (r : rs)        -> r `seq` seqList rs



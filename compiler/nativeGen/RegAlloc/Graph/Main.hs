{-# OPTIONS -fno-warn-missing-signatures #-}
-- | Graph coloring register allocator.
--
-- TODO: The colors in graphviz graphs for x86_64 and ppc could be nicer.
--

module RegAlloc.Graph.Main (
        regAlloc
)

where

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
--      We should only need 3 or 4 cycles tops.
--      If we run for any longer than this we're probably in an infinite loop,
--      It's probably better just to bail out and report a bug at this stage.
maxSpinCount    :: Int
maxSpinCount    = 10


-- | The top level of the graph coloring register allocator.
regAlloc
        :: (Outputable statics, Outputable instr, Instruction instr)
        => DynFlags
        -> UniqFM (UniqSet RealReg)     -- ^ the registers we can use for allocation
        -> UniqSet Int                  -- ^ the set of available spill slots.
        -> [LiveCmmDecl statics instr]  -- ^ code annotated with liveness information.
        -> UniqSM ( [NatCmmDecl statics instr], [RegAllocStats statics instr] )
           -- ^ code with registers allocated and stats for each stage of
           -- allocation

regAlloc dflags regsFree slotsFree code
 = do
        -- TODO: the regClass function is currently hard coded to the default target
        --       architecture. Would prefer to determine this from dflags.
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

regAlloc_spin :: (Instruction instr,
                  Outputable instr,
                  Outputable statics)
              => DynFlags
              -> Int
              -> Color.Triv VirtualReg RegClass RealReg
              -> UniqFM (UniqSet RealReg)
              -> UniqSet Int
              -> [RegAllocStats statics instr]
              -> [LiveCmmDecl statics instr]
              -> UniqSM ([NatCmmDecl statics instr],
                         [RegAllocStats statics instr],
                         Color.Graph VirtualReg RegClass RealReg)
regAlloc_spin dflags spinCount triv regsFree slotsFree debug_codeGraphs code
 = do
        let platform = targetPlatform dflags
        -- if any of these dump flags are turned on we want to hang on to
        --      intermediate structures in the allocator - otherwise tell the
        --      allocator to ditch them early so we don't end up creating space leaks.
        let dump = or
                [ dopt Opt_D_dump_asm_regalloc_stages dflags
                , dopt Opt_D_dump_asm_stats dflags
                , dopt Opt_D_dump_asm_conflicts dflags ]

        -- check that we're not running off down the garden path.
        when (spinCount > maxSpinCount)
         $ pprPanic "regAlloc_spin: max build/spill cycle count exceeded."
                (  text "It looks like the register allocator is stuck in an infinite loop."
                $$ text "max cycles  = " <> int maxSpinCount
                $$ text "regsFree    = " <> (hcat       $ punctuate space $ map ppr
                                                $ uniqSetToList $ unionManyUniqSets $ eltsUFM regsFree)
                $$ text "slotsFree   = " <> ppr (sizeUniqSet slotsFree))

        -- build a conflict graph from the code.
        (graph  :: Color.Graph VirtualReg RegClass RealReg)
                <- {-# SCC "BuildGraph" #-} buildGraph code

        -- VERY IMPORTANT:
        --      We really do want the graph to be fully evaluated _before_ we start coloring.
        --      If we don't do this now then when the call to Color.colorGraph forces bits of it,
        --      the heap will be filled with half evaluated pieces of graph and zillions of apply thunks.
        --
        seqGraph graph `seq` return ()


        -- build a map of the cost of spilling each instruction
        --      this will only actually be computed if we have to spill something.
        let spillCosts  = foldl' plusSpillCostInfo zeroSpillCostInfo
                        $ map slurpSpillCostInfo code

        -- the function to choose regs to leave uncolored
        let spill       = chooseSpill spillCosts

        -- record startup state
        let stat1       =
                if spinCount == 0
                 then   Just $ RegAllocStatsStart
                        { raLiveCmm     = code
                        , raGraph       = graph
                        , raSpillCosts  = spillCosts }
                 else   Nothing

        -- try and color the graph
        let (graph_colored, rsSpill, rmCoalesce)
                        = {-# SCC "ColorGraph" #-}
                           Color.colorGraph
                                (dopt Opt_RegsIterative dflags)
                                spinCount
                                regsFree triv spill graph

        -- rewrite regs in the code that have been coalesced
        let patchF reg
                | RegVirtual vr <- reg
                = case lookupUFM rmCoalesce vr of
                        Just vr'        -> patchF (RegVirtual vr')
                        Nothing         -> reg

                | otherwise
                = reg

        let code_coalesced
                        = map (patchEraseLive patchF) code


        -- see if we've found a coloring
        if isEmptyUniqSet rsSpill
         then do
                -- if -fasm-lint is turned on then validate the graph
                let graph_colored_lint  =
                        if dopt Opt_DoAsmLinting dflags
                                then Color.validateGraph (text "")
                                        True    -- require all nodes to be colored
                                        graph_colored
                                else graph_colored

                -- patch the registers using the info in the graph
                let code_patched        = map (patchRegsFromGraph platform graph_colored_lint) code_coalesced

                -- clean out unneeded SPILL/RELOADs
                let code_spillclean     = map (cleanSpills platform) code_patched

                -- strip off liveness information,
                --      and rewrite SPILL/RELOAD pseudos into real instructions along the way
                let code_final          = map (stripLive platform) code_spillclean

                -- record what happened in this stage for debugging
                let stat                =
                        RegAllocStatsColored
                        { raCode                = code
                        , raGraph               = graph
                        , raGraphColored        = graph_colored_lint
                        , raCoalesced           = rmCoalesce
                        , raCodeCoalesced       = code_coalesced
                        , raPatched             = code_patched
                        , raSpillClean          = code_spillclean
                        , raFinal               = code_final
                        , raSRMs                = foldl' addSRM (0, 0, 0) $ map countSRMs code_spillclean }


                let statList =
                        if dump then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
                                else []

                -- space leak avoidance
                seqList statList `seq` return ()

                return  ( code_final
                        , statList
                        , graph_colored_lint)

         -- we couldn't find a coloring, time to spill something
         else do
                -- if -fasm-lint is turned on then validate the graph
                let graph_colored_lint  =
                        if dopt Opt_DoAsmLinting dflags
                                then Color.validateGraph (text "")
                                        False   -- don't require nodes to be colored
                                        graph_colored
                                else graph_colored

                -- spill the uncolored regs
                (code_spilled, slotsFree', spillStats)
                        <- regSpill code_coalesced slotsFree rsSpill

                -- recalculate liveness
                -- NOTE: we have to reverse the SCCs here to get them back into the reverse-dependency
                --       order required by computeLiveness. If they're not in the correct order
                --       that function will panic.
                code_relive     <- mapM (regLiveness . reverseBlocksInTops) code_spilled

                -- record what happened in this stage for debugging
                let stat        =
                        RegAllocStatsSpill
                        { raCode        = code
                        , raGraph       = graph_colored_lint
                        , raCoalesced   = rmCoalesce
                        , raSpillStats  = spillStats
                        , raSpillCosts  = spillCosts
                        , raSpilled     = code_spilled }

                let statList =
                        if dump
                                then [stat] ++ maybeToList stat1 ++ debug_codeGraphs
                                else []

                -- space leak avoidance
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
        -- Slurp out the conflicts and reg->reg moves from this code
        let (conflictList, moveList) =
                unzip $ map slurpConflicts code

        -- Slurp out the spill/reload coalesces
        let moveList2           = map slurpReloadCoalesce code

        -- Add the reg-reg conflicts to the graph
        let conflictBag         = unionManyBags conflictList
        let graph_conflict      = foldrBag graphAddConflictSet Color.initGraph conflictBag

        -- Add the coalescences edges to the graph.
        let moveBag             = unionBags (unionManyBags moveList2) (unionManyBags moveList)
        let graph_coalesce      = foldrBag graphAddCoalesce graph_conflict moveBag

        return  graph_coalesce


-- | Add some conflict edges to the graph.
--      Conflicts between virtual and real regs are recorded as exclusions.
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
--      Coalesences between virtual and real regs are recorded as preferences.
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
 = let
        -- a function to lookup the hardreg for a virtual reg from the graph.
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
                        (  text "There is no node in the graph for register " <> ppr reg
                        $$ ppr code
                        $$ Color.dotGraph
                                (\_ -> text "white")
                                (trivColorable platform
                                        (targetVirtualRegSqueeze platform)
                                        (targetRealRegSqueeze platform))
                                graph)

   in   patchEraseLive patchF code


-----
-- for when laziness just isn't what you wanted...
--
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



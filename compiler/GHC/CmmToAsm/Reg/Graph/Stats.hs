
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Carries interesting info for debugging / profiling of the
--   graph coloring register allocator.
module GHC.CmmToAsm.Reg.Graph.Stats (
        RegAllocStats (..),

        pprStats,
        pprStatsSpills,
        pprStatsLifetimes,
        pprStatsConflict,
        pprStatsLifeConflict,

        countSRMs, addSRM
) where

import GHC.Prelude

import qualified GHC.Data.Graph.Color as Color
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Reg.Graph.Spill
import GHC.CmmToAsm.Reg.Graph.SpillCost
import GHC.CmmToAsm.Reg.Graph.TrivColorable
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Types

import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Reg.Class

import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Utils.Outputable
import GHC.Utils.Monad.State.Strict

-- | Holds interesting statistics from the register allocator.
data RegAllocStats statics instr

        -- Information about the initial conflict graph.
        = RegAllocStatsStart
        { -- | Initial code, with liveness.
          raLiveCmm     :: [LiveCmmDecl statics instr]

          -- | The initial, uncolored graph.
        , raGraph       :: Color.Graph VirtualReg RegClass RealReg

          -- | Information to help choose which regs to spill.
        , raSpillCosts  :: SpillCostInfo

          -- | Target platform
        , raPlatform    :: !Platform
        }


        -- Information about an intermediate graph.
        -- This is one that we couldn't color, so had to insert spill code
        -- instruction stream.
        | RegAllocStatsSpill
        { -- | Code we tried to allocate registers for.
          raCode        :: [LiveCmmDecl statics instr]

          -- | Partially colored graph.
        , raGraph       :: Color.Graph VirtualReg RegClass RealReg

          -- | The regs that were coalesced.
        , raCoalesced   :: UniqFM VirtualReg VirtualReg

          -- | Spiller stats.
        , raSpillStats  :: SpillStats

          -- | Number of instructions each reg lives for.
        , raSpillCosts  :: SpillCostInfo

          -- | Code with spill instructions added.
        , raSpilled     :: [LiveCmmDecl statics instr]

          -- | Target platform
        , raPlatform    :: !Platform
        }


        -- a successful coloring
        | RegAllocStatsColored
        { -- | Code we tried to allocate registers for.
          raCode          :: [LiveCmmDecl statics instr]

          -- | Uncolored graph.
        , raGraph         :: Color.Graph VirtualReg RegClass RealReg

          -- | Coalesced and colored graph.
        , raGraphColored  :: Color.Graph VirtualReg RegClass RealReg

          -- | Regs that were coalesced.
        , raCoalesced     :: UniqFM VirtualReg VirtualReg

          -- | Code with coalescings applied.
        , raCodeCoalesced :: [LiveCmmDecl statics instr]

          -- | Code with vregs replaced by hregs.
        , raPatched       :: [LiveCmmDecl statics instr]

          -- | Code with unneeded spill\/reloads cleaned out.
        , raSpillClean    :: [LiveCmmDecl statics instr]

          -- | Final code.
        , raFinal         :: [NatCmmDecl statics instr]

          -- | Spill\/reload\/reg-reg moves present in this code.
        , raSRMs          :: (Int, Int, Int)

          -- | Target platform
        , raPlatform    :: !Platform
        }
        deriving (Functor)


instance (OutputableP Platform statics, OutputableP Platform instr)
       => Outputable (RegAllocStats statics instr) where

 ppr (s@RegAllocStatsStart{})
    =      text "#  Start"
        $$ text "#  Native code with liveness information."
        $$ pdoc (raPlatform s) (raLiveCmm s)
        $$ text ""
        $$ text "#  Initial register conflict graph."
        $$ Color.dotGraph
                (targetRegDotColor (raPlatform s))
                (trivColorable (raPlatform s)
                        (targetVirtualRegSqueeze (raPlatform s))
                        (targetRealRegSqueeze (raPlatform s)))
                (raGraph s)


 ppr (s@RegAllocStatsSpill{}) =
           text "#  Spill"

        $$ text "#  Code with liveness information."
        $$ pdoc (raPlatform s) (raCode s)
        $$ text ""

        $$ (if (not $ isNullUFM $ raCoalesced s)
                then    text "#  Registers coalesced."
                        $$ pprUFMWithKeys (raCoalesced s) (vcat . map ppr)
                        $$ text ""
                else empty)

        $$ text "#  Spills inserted."
        $$ ppr (raSpillStats s)
        $$ text ""

        $$ text "#  Code with spills inserted."
        $$ pdoc (raPlatform s) (raSpilled s)


 ppr (s@RegAllocStatsColored { raSRMs = (spills, reloads, moves) })
    =      text "#  Colored"

        $$ text "#  Code with liveness information."
        $$ pdoc (raPlatform s) (raCode s)
        $$ text ""

        $$ text "#  Register conflict graph (colored)."
        $$ Color.dotGraph
                (targetRegDotColor (raPlatform s))
                (trivColorable (raPlatform s)
                        (targetVirtualRegSqueeze (raPlatform s))
                        (targetRealRegSqueeze (raPlatform s)))
                (raGraphColored s)
        $$ text ""

        $$ (if (not $ isNullUFM $ raCoalesced s)
                then    text "#  Registers coalesced."
                        $$ pprUFMWithKeys (raCoalesced s) (vcat . map ppr)
                        $$ text ""
                else empty)

        $$ text "#  Native code after coalescings applied."
        $$ pdoc (raPlatform s) (raCodeCoalesced s)
        $$ text ""

        $$ text "#  Native code after register allocation."
        $$ pdoc (raPlatform s) (raPatched s)
        $$ text ""

        $$ text "#  Clean out unneeded spill/reloads."
        $$ pdoc (raPlatform s) (raSpillClean s)
        $$ text ""

        $$ text "#  Final code, after rewriting spill/rewrite pseudo instrs."
        $$ pdoc (raPlatform s) (raFinal s)
        $$ text ""
        $$  text "#  Score:"
        $$ (text "#          spills  inserted: " <> int spills)
        $$ (text "#          reloads inserted: " <> int reloads)
        $$ (text "#   reg-reg moves remaining: " <> int moves)
        $$ text ""


-- | Do all the different analysis on this list of RegAllocStats
pprStats
        :: [RegAllocStats statics instr]
        -> Color.Graph VirtualReg RegClass RealReg
        -> SDoc

pprStats stats graph
 = let  outSpills       = pprStatsSpills    stats
        outLife         = pprStatsLifetimes stats
        outConflict     = pprStatsConflict  stats
        outScatter      = pprStatsLifeConflict stats graph

  in    vcat [outSpills, outLife, outConflict, outScatter]


-- | Dump a table of how many spill loads \/ stores were inserted for each vreg.
pprStatsSpills
        :: [RegAllocStats statics instr] -> SDoc

pprStatsSpills stats
 = let
        finals  = [srms | RegAllocStatsColored{ raSRMs = srms } <- stats]

        -- sum up how many stores\/loads\/reg-reg-moves were left in the code
        total   = foldl' addSRM (0, 0, 0) finals

    in  (  text "-- spills-added-total"
        $$ text "--    (stores, loads, reg_reg_moves_remaining)"
        $$ ppr total
        $$ text "")


-- | Dump a table of how long vregs tend to live for in the initial code.
pprStatsLifetimes
        :: [RegAllocStats statics instr] -> SDoc

pprStatsLifetimes stats
 = let  info            = foldl' plusSpillCostInfo zeroSpillCostInfo
                          [ sc | RegAllocStatsStart{ raSpillCosts = sc } <- stats ]

        lifeBins        = binLifetimeCount $ lifeMapFromSpillCostInfo info

   in   (  text "-- vreg-population-lifetimes"
        $$ text "--   (instruction_count, number_of_vregs_that_lived_that_long)"
        $$ pprUFM lifeBins (vcat . map ppr)
        $$ text "\n")


binLifetimeCount :: UniqFM VirtualReg (VirtualReg, Int) -> UniqFM Int (Int, Int)
binLifetimeCount fm
 = let  lifes   = map (\l -> (l, (l, 1)))
                $ map snd
                $ nonDetEltsUFM fm
                -- See Note [Unique Determinism and code generation]

   in   addListToUFM_C
                (\(l1, c1) (_, c2) -> (l1, c1 + c2))
                emptyUFM
                lifes


-- | Dump a table of how many conflicts vregs tend to have in the initial code.
pprStatsConflict
        :: [RegAllocStats statics instr] -> SDoc

pprStatsConflict stats
 = let  confMap = foldl' (plusUFM_C (\(c1, n1) (_, n2) -> (c1, n1 + n2)))
                        emptyUFM
                $ map Color.slurpNodeConflictCount
                        [ raGraph s | s@RegAllocStatsStart{} <- stats ]

   in   (  text "-- vreg-conflicts"
        $$ text "--   (conflict_count, number_of_vregs_that_had_that_many_conflicts)"
        $$ pprUFM confMap (vcat . map ppr)
        $$ text "\n")


-- | For every vreg, dump how many conflicts it has, and its lifetime.
--      Good for making a scatter plot.
pprStatsLifeConflict
        :: [RegAllocStats statics instr]
        -> Color.Graph VirtualReg RegClass RealReg -- ^ global register conflict graph
        -> SDoc

pprStatsLifeConflict stats graph
 = let  lifeMap = lifeMapFromSpillCostInfo
                $ foldl' plusSpillCostInfo zeroSpillCostInfo
                $ [ sc | RegAllocStatsStart{ raSpillCosts = sc } <- stats ]

        scatter = map   (\r ->  let lifetime  = case lookupUFM lifeMap r of
                                                      Just (_, l) -> l
                                                      Nothing     -> 0
                                    Just node = Color.lookupNode graph r
                                in parens $ hcat $ punctuate (text ", ")
                                        [ doubleQuotes $ ppr $ Color.nodeId node
                                        , ppr $ sizeUniqSet (Color.nodeConflicts node)
                                        , ppr $ lifetime ])
                $ map Color.nodeId
                $ nonDetEltsUFM
                -- See Note [Unique Determinism and code generation]
                $ Color.graphMap graph

   in   (  text "-- vreg-conflict-lifetime"
        $$ text "--   (vreg, vreg_conflicts, vreg_lifetime)"
        $$ (vcat scatter)
        $$ text "\n")


-- | Count spill/reload/reg-reg moves.
--      Lets us see how well the register allocator has done.
countSRMs
        :: Instruction instr
        => Platform
        -> LiveCmmDecl statics instr -> (Int, Int, Int)

countSRMs platform cmm
        = execState (mapBlockTopM (countSRM_block platform) cmm) (0, 0, 0)


countSRM_block
        :: Instruction instr
        => Platform
        -> GenBasicBlock (LiveInstr instr)
        -> State (Int, Int, Int) (GenBasicBlock (LiveInstr instr))

countSRM_block platform (BasicBlock i instrs)
 = do   instrs' <- mapM (countSRM_instr platform) instrs
        return  $ BasicBlock i instrs'


countSRM_instr
        :: Instruction instr
        => Platform -> LiveInstr instr -> State (Int, Int, Int) (LiveInstr instr)

countSRM_instr platform li
        | LiveInstr SPILL{} _    <- li
        = do    modify  $ \(s, r, m)    -> (s + 1, r, m)
                return li

        | LiveInstr RELOAD{} _  <- li
        = do    modify  $ \(s, r, m)    -> (s, r + 1, m)
                return li

        | LiveInstr instr _     <- li
        , Just _        <- takeRegRegMoveInstr platform instr
        = do    modify  $ \(s, r, m)    -> (s, r, m + 1)
                return li

        | otherwise
        =       return li


-- sigh..
addSRM :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addSRM (s1, r1, m1) (s2, r2, m2)
 = let  !s = s1 + s2
        !r = r1 + r2
        !m = m1 + m2
   in   (s, r, m)


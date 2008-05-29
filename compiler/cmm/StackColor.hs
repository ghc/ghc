
module StackColor where

import BlockId
import StackPlacements
import qualified GraphColor as Color
import CmmExpr
import CmmSpillReload
import DFMonad
import qualified GraphOps
import MachOp
import ZipCfg
import ZipCfgCmmRep
import ZipDataflow

import Maybes
import Panic
import UniqSet

import Data.List

type M = ExtendWithSpills Middle

fold_edge_facts_b ::
  LastNode l => (DualLive -> a -> a) -> BackwardTransfers m l DualLive -> LGraph m l
                                     -> (BlockId -> DualLive) -> a -> a
fold_edge_facts_b f comp graph env z =
    foldl fold_block_facts z (postorder_dfs graph)
  where
    fold_block_facts z b =              
      let (h, l) = goto_end (ZipCfg.unzip b) 
          last_in _ LastExit = fact_bot dualLiveLattice
          last_in env (LastOther l) = bt_last_in comp env l
      in head_fold h (last_in env l) z
    head_fold (ZHead h m) out z = head_fold h (bt_middle_in comp out m) (f out z)
    head_fold (ZFirst id) out z = f (bt_first_in comp out id) (f out z)

foldConflicts :: (RegSet -> a -> a) -> a -> LGraph M Last -> FuelMonad a
foldConflicts f z g =
  do env <- dualLiveness emptyBlockSet $ graphOfLGraph g
     let lookup id = lookupBlockEnv env id `orElse` fact_bot dualLiveLattice
         f' dual z = f (on_stack dual) z
     return $ fold_edge_facts_b f' (dualLiveTransfers emptyBlockSet) g lookup z
  --let env = runDFA dualLiveLattice (run_b_anal dualLiveness g >> getAllFacts)
  --    lookup id = lookupBlockEnv env id `orElse` fact_bot dualLiveLattice
  --    f' dual z = f (on_stack dual) z
  --in  fold_edge_facts_b f' dualLiveness g lookup z


type IGraph = Color.Graph LocalReg SlotClass StackPlacement
type ClassCount = [(SlotClass, Int)]

buildIGraphAndCounts :: LGraph M Last -> FuelMonad (IGraph, ClassCount)
buildIGraphAndCounts g = igraph_and_counts
    where igraph_and_counts = foldConflicts add (Color.initGraph, zero) g
          zero = map (\c -> (c, 0)) allSlotClasses
          add live (igraph, counts) = (graphAddConflictSet live igraph,
                                       addSimulCounts (classCounts live) counts)
          addSimulCounts =
            zipWith (\(c, n) (c', n') -> if c == c' then (c, max n n')
                                         else panic "slot classes out of order")
          classCounts regs = foldUniqSet addReg zero regs
          addReg reg counts =
              let cls = slotClass reg in
              map (\(c, n) -> (c, if c == cls then n + 1 else n)) counts
                           

-- | Add some conflict edges to the graph.
--	Conflicts between virtual and real regs are recorded as exclusions.
--

graphAddConflictSet :: RegSet -> IGraph -> IGraph
graphAddConflictSet set graph = GraphOps.addConflicts set slotClass graph

slotClass :: LocalReg -> SlotClass
slotClass (LocalReg _ machRep _) = 
    case machRep of -- the horror, the horror
      I8   -> SlotClass32
      I16  -> SlotClass32
      I32  -> SlotClass32
      I64  -> SlotClass64
      I128 -> SlotClass128
      F32  -> SlotClass32
      F64  -> SlotClass64
      F80  -> SlotClass64

{-
colorMe :: (IGraph, ClassCount) -> (IGraph, UniqSet LocalReg)
colorMe (igraph, counts) = Color.colorGraph starter_colors triv spill_max_degree igraph
    where starter_colors = allocate [] counts allStackSlots
          allocate prev [] colors = insert prev colors
          allocate prev ((c, n) : counts) colors =
              let go prev 0 colors = allocate prev counts colors
                  go prev n colors = let (p, colors') = getStackSlot c colors in
                                     go (p:prev) (n-1) colors'
              in  go prev n colors
          insert :: [StackPlacement] -> SlotSet -> SlotSet
          insert [] colors = colors
          insert (p:ps) colors = insert ps (extendSlotSet colors p)
          triv :: Color.Triv LocalReg SlotClass StackPlacement
          triv = trivColorable (mkSizeOf counts)

spill_max_degree :: IGraph -> LocalReg
spill_max_degree igraph = Color.nodeId node
    where node	= maximumBy (\n1 n2 -> compare 
 				(sizeUniqSet $ Color.nodeConflicts n1) 
				(sizeUniqSet $ Color.nodeConflicts n2)) $
 		  eltsUFM $ Color.graphMap igraph


type Worst = SlotClass -> (Int, Int, Int) -> Int

trivColorable :: (SlotClass -> Int) -> 
                 SlotClass -> UniqSet LocalReg -> UniqSet StackPlacement -> Bool
trivColorable sizeOf classN conflicts exclusions = squeeze < sizeOf classN
  where	squeeze = worst classN counts
        counts   = if isEmptyUniqSet exclusions then foldUniqSet acc zero conflicts
                   else panic "exclusions in stack slots?!"
        zero = (0, 0, 0)
	acc r (word, dbl, quad)	=
            case slotClass r of
              SlotClass32  -> (word+1, dbl, quad)
              SlotClass64  -> (word, dbl+1, quad)
              SlotClass128 -> (word, dbl, quad+1)
        worst SlotClass128 (_, _, q) = q
        worst SlotClass64  (_, d, q) = d + 2 * q
        worst SlotClass32  (w, d, q) = w + 2 * d + 4 * q
-}

-- | number of placements available is from class and all larger classes
mkSizeOf :: ClassCount -> (SlotClass -> Int)
mkSizeOf counts = sizeOf
    where sizeOf SlotClass32  = n32
          sizeOf SlotClass64  = n64
          sizeOf SlotClass128 = n128
          n128 = (lookup SlotClass128 counts `orElse` 0)
          n64  = (lookup SlotClass64  counts `orElse` 0) + 2 * n128
          n32  = (lookup SlotClass32  counts `orElse` 0) + 2 * n32

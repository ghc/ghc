%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[AsmRegAlloc]{Register allocator}

\begin{code}
module AsmRegAlloc ( runRegAllocate ) where	

#include "HsVersions.h"

import MachCode		( InstrBlock )
import MachMisc		( Instr(..) )
import PprMach		( pprInstr )	-- Just for debugging
import MachRegs
import RegAllocInfo

import FiniteMap	( FiniteMap, emptyFM, 
			  lookupFM, eltsFM, addToFM_C, addToFM,
			  listToFM, fmToList )
import OrdList		( fromOL )
import Outputable
import Unique		( mkPseudoUnique3 )
import CLabel		( CLabel, pprCLabel )
import FastTypes

import List		( mapAccumL, nub, sort )
import Array		( Array, array, (!), bounds )
\end{code}

This is the generic register allocator.  It does allocation for all
architectures.  Details for specific architectures are given in
RegAllocInfo.lhs.  In practice the allocator needs to know next to
nothing about an architecture to do its job:

* It needs to be given a list of the registers it can allocate to.

* It needs to be able to find out which registers each insn reads and
  writes.

* It needs be able to change registers in instructions into other
  registers.

* It needs to be able to find out where execution could go after an
  in instruction.

* It needs to be able to discover sets of registers which can be
  used to attempt spilling.

First we try something extremely simple.  If that fails, we have to do
things the hard way.

\begin{code}
runRegAllocate
    :: [Reg]
    -> ([Instr] -> [[Reg]])
    -> InstrBlock
    -> [Instr]

runRegAllocate regs find_reserve_regs instrs
  = --trace ("runRegAllocate: " ++ show regs) (
    case simpleAlloc of
       Just simple -> --trace "SIMPLE" 
                      simple
       Nothing     -> --trace "GENERAL"
                      (tryGeneral reserves)
    --)
  where
    tryGeneral [] 
       = pprPanic "nativeGen: spilling failed.  Workaround: compile with -fvia-C.\n"
            ( (text "reserves = " <> ppr reserves)
              $$
              (text "code = ")
              $$
              (vcat (map (docToSDoc.pprInstr) flatInstrs))
            )
    tryGeneral (resv:resvs)
       = case generalAlloc resv of
            Just success -> success
            Nothing      -> tryGeneral resvs

    reserves           = find_reserve_regs flatInstrs
    flatInstrs         = fromOL instrs
    simpleAlloc        = doSimpleAlloc regs flatInstrs
    generalAlloc resvd = doGeneralAlloc regs resvd flatInstrs
\end{code}

Rather than invoke the heavyweight machinery in @doGeneralAlloc@ for
each and every code block, we first try using this simple, fast and
utterly braindead allocator.  In practice it handles about 60\% of the
code blocks really fast, even with only 3 integer registers available.
Since we can always give up and fall back to @doGeneralAlloc@,
@doSimpleAlloc@ is geared to handling the common case as fast as
possible.  It will succeed only if:

* The code mentions registers only of integer class, not floating
  class.

* The code doesn't mention any real registers, so we don't have to
  think about dodging and weaving to work around fixed register uses.

* The code mentions at most N virtual registers, where N is the number
  of real registers for allocation.

If those conditions are satisfied, we simply trundle along the code, 
doling out a real register every time we see mention of a new virtual
register.  We either succeed at this, or give up when one of the above
three conditions is no longer satisfied.

\begin{code}
doSimpleAlloc :: [Reg] -> [Instr] -> Maybe [Instr]
doSimpleAlloc available_real_regs instrs
   = let available_iregs 
            = filter ((== RcInteger).regClass) available_real_regs

         trundle :: [( {-Virtual-}Reg, {-Real-}Reg )]
                    -> [ {-Real-}Reg ]
                    -> [Instr]
                    -> [Instr]
                    -> Maybe [Instr]
         trundle vreg_map uncommitted_rregs ris_done []
            = Just (reverse ris_done)
         trundle vreg_map uncommitted_rregs ris_done (i:is)
            = case regUsage i of
                 RU rds wrs

                    -- Mentions no regs?  Move on quickly
                    |  null rds_l && null wrs_l
                    -> trundle vreg_map uncommitted_rregs (i:ris_done) is

                    -- A case we can't be bothered to handle?
                    |  any isFloatingOrReal rds_l || any isFloatingOrReal wrs_l
                    -> Nothing

                    -- Update the rreg commitments, and map the insn
                    |  otherwise
                    -> case upd_commitment (wrs_l++rds_l) 
                                           vreg_map uncommitted_rregs of
                          Nothing -- out of rregs; give up
                             -> Nothing
                          Just (vreg_map2, uncommitted_rregs2)
                             -> let i2 = patchRegs i (subst_reg vreg_map2)
                                in  trundle vreg_map2 uncommitted_rregs2 
                                            (i2:ris_done) is
                       where
                          isFloatingOrReal reg
                             = isRealReg reg || regClass reg == RcFloat
                                             || regClass reg == RcDouble

                          rds_l = regSetToList rds
                          wrs_l = regSetToList wrs

                          upd_commitment [] vr_map uncomm
                             = Just (vr_map, uncomm)
                          upd_commitment (reg:regs) vr_map uncomm
                             | isRealReg reg 
                             = upd_commitment regs vr_map uncomm
                             | reg `elem` (map fst vr_map)
                             = upd_commitment regs vr_map uncomm
                             | null uncomm
                             = Nothing
                             | otherwise
                             = upd_commitment regs ((reg, head uncomm):vr_map) 
                                                   (tail uncomm)

                          subst_reg vreg_map r
                             -- If it's a RealReg, it must be STG-specific one 
                             -- (Hp,Sp,BaseReg,etc), since regUsage filters them out,
                             -- so isFloatingOrReal would not have objected to it.
                             | isRealReg r 
                             = r
                             | otherwise 
                             = case [rr | (vr,rr) <- vreg_map, vr == r] of
                                  [rr2] -> rr2
                                  other -> pprPanic 
                                              "doSimpleAlloc: unmapped VirtualReg"
                                              (ppr r)
     in
         trundle [] available_iregs [] instrs
\end{code}

From here onwards is the general register allocator and spiller.  For
each flow edge (possible transition between instructions), we compute
which virtual and real registers are live on that edge.  Then the
mapping is inverted, to give a mapping from register (virtual+real) to
sets of flow edges on which the register is live.  Finally, we can use
those sets to decide whether a virtual reg v can be assigned to a real
reg r, by checking that v's live-edge-set does not intersect with r's
current live-edge-set.  Having made that assignment, we then augment
r's current live-edge-set (its current commitment, you could say) with
v's live-edge-set.

doGeneralAlloc takes reserve_regs as the regs to use as spill
temporaries.  First it tries to allocate using all regs except
reserve_regs.  If that fails, it inserts spill code and tries again to
allocate regs, but this time with the spill temporaries available.
Even this might not work if there are insufficient spill temporaries:
in the worst case on x86, we'd need 3 of them, for insns like addl
(%reg1,%reg2,4) %reg3, since this insn uses all 3 regs as input.

\begin{code}
doGeneralAlloc 
    :: [Reg]            -- all allocatable regs
    -> [Reg]            -- the reserve regs
    -> [Instr]          -- instrs in
    -> Maybe [Instr]    -- instrs out

doGeneralAlloc all_regs reserve_regs instrs
   -- succeeded without spilling
   | prespill_ok
   = Just prespill_insns

   -- failed, and no spill regs avail, so pointless to attempt spilling 
   | null reserve_regs  = Nothing
   -- success after spilling
   | postspill_ok       = maybetrace (spillMsg True) (Just postspill_insns)
   -- still not enough reserves after spilling; we have to give up
   | otherwise          = maybetrace (spillMsg False) Nothing
     where
         prespill_regs 
            = filter (`notElem` reserve_regs) all_regs
         (prespill_ok, prespill_insns)
            = allocUsingTheseRegs instrs prespill_regs
         instrs_with_spill_code
            = insertSpillCode prespill_insns
         (postspill_ok, postspill_insns)
            = allocUsingTheseRegs instrs_with_spill_code all_regs

         spillMsg success
            = "nativeGen: spilling " 
              ++ (if success then "succeeded" else "failed   ")
              ++ " using " 
              ++ showSDoc (hsep (map ppr reserve_regs))

#        ifdef NCG_DEBUG
         maybetrace msg x = trace msg x
#        else
         maybetrace msg x = x
#        endif
\end{code}

Here we patch instructions that reference ``registers'' which are
really in memory somewhere (the mapping is under the control of the
machine-specific code generator).  We place the appropriate load
sequences before any instructions that use memory registers as
sources, and we place the appropriate spill sequences after any
instructions that use memory registers as destinations.  The offending
instructions are rewritten with new dynamic registers, so generalAlloc
has to run register allocation again after all of this is said and
done.

On some architectures (x86, currently), we do without a frame-pointer,
and instead spill relative to the stack pointer (%esp on x86).
Because the stack pointer may move, the patcher needs to keep track of
the current stack pointer "delta".  That's easy, because all it needs
to do is spot the DELTA bogus-insns which will have been inserted by
the relevant insn selector precisely so as to notify the spiller of
stack-pointer movement.  The delta is passed to loadReg and spillReg,
since they generate the actual spill code.  We expect the final delta
to be the same as the starting one (zero), reflecting the fact that
changes to the stack pointer should not extend beyond a basic block.

Finally, there is the issue of mapping an arbitrary set of unallocated
VirtualRegs into a contiguous sequence of spill slots.  The failed
allocation will have left the code peppered with references to
VirtualRegs, each of which contains a unique.  So we make an env which
maps these VirtualRegs to integers, starting from zero, and pass that
env through to loadReg and spillReg.  There, they are used to look up
spill slot numbers for the uniques.

\begin{code}
insertSpillCode :: [Instr] -> [Instr]
insertSpillCode insns
   = let uniques_in_insns
            = map getVRegUnique 
                  (regSetToList 
                     (foldl unionRegSets emptyRegSet 
                            (map vregs_in_insn insns)))
         vregs_in_insn i
            = case regUsage i of
                 RU rds wrs -> filterRegSet isVirtualReg 
                                             (rds `unionRegSets` wrs)
         vreg_to_slot_map :: FiniteMap VRegUnique Int
         vreg_to_slot_map
            = listToFM (zip uniques_in_insns [0..])

         ((final_stack_delta, final_ctr), insnss) 
            = mapAccumL (patchInstr vreg_to_slot_map) (0,0) insns
     in
         if   final_stack_delta == 0
         then concat insnss
         else pprPanic "patchMem: non-zero final delta" 
                       (int final_stack_delta)


-- patchInstr has as a running state two Ints, one the current stack delta,
-- needed to figure out offsets to stack slots on archs where we spill relative
-- to the stack pointer, as opposed to the frame pointer.  The other is a 
-- counter, used to manufacture new temporary register names.

patchInstr :: FiniteMap VRegUnique Int -> (Int,Int) -> Instr -> ((Int,Int), [Instr])
patchInstr vreg_to_slot_map (delta,ctr) instr

 | null memSrcs && null memDsts 
 = ((delta',ctr), [instr])

 | otherwise
 = ((delta',ctr'), loadSrcs ++ [instr'] ++ spillDsts)
   where
        delta' = case instr of DELTA d -> d ; _ -> delta

        (RU srcs dsts) = regUsage instr

        -- The instr being patched may mention several vregs -- those which
        -- could not be assigned real registers.  For each such vreg, we 
        -- invent a new vreg, used only around this instruction and nowhere
        -- else.  These new vregs replace the unallocatable vregs; they are
        -- loaded from the spill area, the instruction is done with them,
        -- and results if any are then written back to the spill area.
        vregs_in_instr 
           = nub (filter isVirtualReg 
                         (regSetToList srcs ++ regSetToList dsts))
        n_vregs_in_instr
           = length vregs_in_instr
        ctr' 
           = ctr + n_vregs_in_instr
        vreg_env
           = zip vregs_in_instr [ctr, ctr+1 ..]

        mkTmpReg vreg
           | isVirtualReg vreg
           = case [vi | (vreg', vi) <- vreg_env, vreg' == vreg] of
                [i] -> case regClass vreg of
                          RcInteger -> VirtualRegI (pseudoVReg i)
                          RcFloat   -> VirtualRegF (pseudoVReg i)
                          RcDouble  -> VirtualRegD (pseudoVReg i)
                _   -> pprPanic "patchInstr: unmapped VReg" (ppr vreg)
           | otherwise
           = vreg

        pseudoVReg i = VRegUniqueLo (mkPseudoUnique3 i)

	memSrcs   = filter isVirtualReg (regSetToList srcs)
	memDsts   = filter isVirtualReg (regSetToList dsts)

	loadSrcs  = map load  memSrcs
	spillDsts = map spill memDsts

	load mem  = loadReg  vreg_to_slot_map delta  mem (mkTmpReg mem)
	spill mem = spillReg vreg_to_slot_map delta' (mkTmpReg mem) mem

	instr'    = patchRegs instr mkTmpReg
\end{code}

allocUsingTheseRegs is the register allocator proper.  It attempts
to allocate dynamic regs to real regs, given a list of real regs
which it may use.  If it fails due to lack of real regs, the returned
instructions use what real regs there are, but will retain uses of
dynamic regs for which a real reg could not be found.  It is these
leftover dynamic reg references which insertSpillCode will later
assign to spill slots.

Some implementation notes.
~~~~~~~~~~~~~~~~~~~~~~~~~~
Instructions are numbered sequentially, starting at zero.

A flow edge (FE) is a pair of insn numbers (MkFE Int Int) denoting
a possible flow of control from the first insn to the second.

The input to the register allocator is a list of instructions, which
mention Regs.  A Reg can be a RealReg -- a real machine reg -- or a
VirtualReg, which carries a unique.  After allocation, all the 
VirtualReg references will have been converted into RealRegs, and
possible some spill code will have been inserted.

The heart of the register allocator works in four phases.

1.  (find_flow_edges) Calculate all the FEs for the code list.
    Return them not as a [FE], but implicitly, as a pair of 
    Array Int [Int], being the successor and predecessor maps
    for instructions.

2.  (calc_liveness) Returns a FiniteMap FE RegSet.  For each 
    FE, indicates the set of registers live on that FE.  Note
    that the set includes both RealRegs and VirtualRegs.  The
    former appear because the code could mention fixed register
    usages, and we need to take them into account from the start.

3.  (calc_live_range_sets) Invert the above mapping, giving a 
    FiniteMap Reg FeSet, indicating, for each virtual and real
    reg mentioned in the code, which FEs it is live on.

4.  (calc_vreg_to_rreg_mapping) For virtual reg, try and find
    an allocatable real register for it.  Each real register has
    a "current commitment", indicating the set of FEs it is 
    currently live on.  A virtual reg v can be assigned to 
    real reg r iff v's live-fe-set does not intersect with r's
    current commitment fe-set.  If the assignment is made,
    v's live-fe-set is union'd into r's current commitment fe-set.
    There is also the minor restriction that v and r must be of
    the same register class (integer or floating).

    Once this mapping is established, we simply apply it to the
    input insns, and that's it.

    If no suitable real register can be found, the vreg is mapped
    to itself, and we deem allocation to have failed.  The partially
    allocated code is returned.  The higher echelons of the allocator
    (doGeneralAlloc and runRegAlloc) then cooperate to insert spill
    code and re-run allocation, until a successful allocation is found.
\begin{code}

allocUsingTheseRegs :: [Instr] -> [Reg] -> (Bool, [Instr])
allocUsingTheseRegs instrs available_real_regs
   = let (all_vregs_mapped, v_to_r_mapping)
            = calc_vreg_to_rreg_mapping instrs available_real_regs
         new_insns
            = map (flip patchRegs sr) instrs
         sr reg
            | isRealReg reg
            = reg
            | otherwise
            = case lookupFM v_to_r_mapping reg of
                 Just r  -> r
                 Nothing -> pprPanic "allocateUsingTheseRegs: unmapped vreg: " 
                                     (ppr reg)
     in
         --trace ("allocUsingTheseRegs: " ++ show available_real_regs) (
         (all_vregs_mapped, new_insns)
         --)


-- the heart of the matter.  
calc_vreg_to_rreg_mapping :: [Instr] -> [Reg] -> (Bool, FiniteMap Reg Reg)
calc_vreg_to_rreg_mapping insns available_real_regs
   = let 
         lr_sets  :: FiniteMap Reg FeSet
         lr_sets = calc_live_range_sets insns

         -- lr_sets maps: vregs mentioned in insns to sets of live FEs
         -- and also:     rregs mentioned in insns to sets of live FEs
         -- We need to extract the rreg mapping, and use it as the
         -- initial real-register-commitment.  Also, add to the initial
         -- commitment, empty commitments for any real regs not
         -- mentioned in it.

         -- which real regs do we want to keep track of in the running
         -- commitment mapping?  Precisely the available_real_regs.  
         -- We don't care about real regs mentioned by insns which are
         -- not in this list, since we're not allocating to them.
         initial_rr_commitment :: FiniteMap Reg FeSet
         initial_rr_commitment
            = listToFM [(rreg,
                         case lookupFM lr_sets rreg of
                            Nothing            -> emptyFeSet
                            Just fixed_use_fes -> fixed_use_fes
                        )
                        | rreg <- available_real_regs]

         -- These are the vregs for which we actually have to (try to) 
         -- assign a real register. (ie, the whole reason we're here at all :)
         vreg_liveness_list :: [(Reg, FeSet)]
         vreg_liveness_list = filter (not.isRealReg.fst) 
                                     (fmToList lr_sets)

         -- A loop, which attempts to assign each vreg to a rreg.
         loop rr_commitment v_to_r_map [] 
            = v_to_r_map
         loop rr_commitment v_to_r_map ((vreg,vreg_live_fes):not_yet_done)
            = let
                  -- find a real reg which is not live for any of vreg_live_fes
                  cand_reals
                     = [rreg 
                           | (rreg,rreg_live_FEs) <- fmToList rr_commitment,
                              regClass vreg == regClass rreg,
                              isEmptyFeSet (intersectionFeSets rreg_live_FEs 
                                                               vreg_live_fes)
                       ]
              in
                 case cand_reals of
                    [] -> -- bummer.  No register is available.  Just go on to
                          -- the next vreg, mapping the vreg to itself.
                          loop rr_commitment (addToFM v_to_r_map vreg vreg)
                               not_yet_done
                    (r:_) 
                       -> -- Hurrah!  Found a free reg of the right class.
                          -- Now we need to update the RR commitment.
                          loop rr_commitment2 (addToFM v_to_r_map vreg r)
                               not_yet_done
                          where
                             rr_commitment2
                                = addToFM_C unionFeSets rr_commitment r 
                                            vreg_live_fes

         -- the final vreg to rreg mapping
         vreg_assignment
            = loop initial_rr_commitment emptyFM vreg_liveness_list
         -- did we succeed in mapping everyone to a real reg?
         allocation_succeeded
            = all isRealReg (eltsFM vreg_assignment)
     in
         (allocation_succeeded, vreg_assignment)



-- calculate liveness, then produce the live range info
-- as a mapping of VRegs to the set of FEs on which they are live.
-- The difficult part is inverting the mapping of Reg -> FeSet
-- to produce a mapping FE -> RegSet.

calc_live_range_sets :: [Instr] -> FiniteMap Reg FeSet
calc_live_range_sets insns
   = let 
         -- this is the "original" (old) mapping
         lis :: FiniteMap FE RegSet
         lis = calc_liveness insns

         -- establish the totality of reg names mentioned by the
         -- insns, by scanning over the insns.
         all_mentioned_regs :: RegSet
         all_mentioned_regs 
            = foldl unionRegSets emptyRegSet
                    (map (\i -> case regUsage i of
                                   RU rds wrs -> unionRegSets rds wrs)
                         insns)

         -- Initial inverted mapping, from Reg to sets of FEs
         initial_imap :: FiniteMap Reg FeSet
         initial_imap
            = listToFM [(reg, emptyFeSet) 
                        | reg <- regSetToList all_mentioned_regs]

         -- Update the new map with one element of the old map
         upd_imap :: FiniteMap Reg FeSet -> (FE, RegSet)
                     -> FiniteMap Reg FeSet
         upd_imap imap (fe, regset)
             = foldl upd_1_imap imap (regSetToList regset)
               where
                  upd_1_imap curr reg
                     = addToFM_C unionFeSets curr reg (unitFeSet fe)

         -- the complete inverse mapping
         final_imap :: FiniteMap Reg FeSet
         final_imap
             = foldl upd_imap initial_imap (fmToList lis)
     in
         final_imap



-- Given the insns, calculate the FEs, and then doing fixpointing to
-- figure out the set of live regs (virtual regs AND real regs) live
-- on each FE.

calc_liveness :: [Instr] -> FiniteMap FE RegSet
calc_liveness insns
   = let (pred_map, succ_map)
            = find_flow_edges insns

         -- We use the convention that if the current approximation
         -- doesn't give a mapping for some FE, that FE maps to the
         -- empty set.
         initial_approx, fixpoint :: FiniteMap FE RegSet
         initial_approx
            = mk_initial_approx 0 insns succ_map emptyFM
         fixpoint 
            = fix_set initial_approx 1
              -- If you want to live dangerously, and promise that the code
              -- doesn't contain any loops (ie, there are no back edges in
              -- the flow graph), you should be able to get away with this:
              -- = upd_liveness_info pred_map succ_map insn_array initial_approx
              -- But since I'm paranoid, and since it hardly makes any difference
              -- to the compiler run-time (about 0.1%), I prefer to do the
              -- the full fixpointing game.

         insn_array
            = let n = length insns 
              in  array (0, n-1) (zip [0..] insns)
              
         sameSets []     []       = True
         sameSets (c:cs) (n:ns)   = eqRegSets c n && sameSets cs ns
         sameSets _      _        = False

         fix_set curr_approx iter_number
            = let next_approx
                     = upd_liveness_info pred_map succ_map insn_array curr_approx
                  curr_sets
                     = eltsFM curr_approx
                  next_sets
                     = eltsFM next_approx
                  same
                     = sameSets curr_sets next_sets
                  final_approx
                     = if same then curr_approx 
                               else fix_set next_approx (iter_number+1)
              in
                  --trace (let qqq (fe, regset) 
                  --             = show fe ++ "  " ++ show (regSetToList regset)
                  --       in
                  --          "\n::iteration " ++ show iter_number ++ "\n" 
                  --          ++ (unlines . map qqq . fmToList) 
                  --                               next_approx ++"\n"
                  --      )
                  final_approx
     in
         fixpoint


-- Create a correct initial approximation.  For each instruction that
-- writes a register, we deem that the register is live on the 
-- flow edges leaving the instruction.  Subsequent iterations of
-- the liveness AbI augment this based purely on reads of regs, not
-- writes.  We need to start off with at least this minimal write-
-- based information in order that writes to vregs which are never
-- used have non-empty live ranges.  If we don't do that, we eventually
-- wind up assigning such vregs to any old real reg, since they don't
-- apparently conflict -- you can't conflict with an empty live range.
-- This kludge is unfortunate, but we need to do it to cover not only
-- writes to vregs which are never used, but also to deal correctly
-- with the fact that calls to C will trash the callee saves registers.

mk_initial_approx :: Int -> [Instr] -> Array Int [Int]
                     -> FiniteMap FE RegSet
                     -> FiniteMap FE RegSet
mk_initial_approx ino [] succ_map ia_so_far 
   = ia_so_far
mk_initial_approx ino (i:is) succ_map ia_so_far
   = let wrs 
            = case regUsage i of RU rrr www -> www
         new_fes 
            = [case iUnbox ino of      { inoh ->
               case iUnbox ino_succ of { ino_succh ->
               MkFE inoh ino_succh 
               }}
                  | ino_succ <- succ_map ! ino]

         loop [] ia = ia
         loop (fe:fes) ia
            = loop fes (addToFM_C unionRegSets ia fe wrs)

         next_ia
            = loop new_fes ia_so_far
     in
         mk_initial_approx (ino+1) is succ_map next_ia
 

-- Do one step in the liveness info calculation (AbI :).  Given the
-- prior approximation (which tells you a subset of live VRegs+RRegs 
-- for each flow edge), calculate new information for all FEs.
-- Rather than do this by iterating over FEs, it's easier to iterate
-- over insns, and update their incoming FEs.

upd_liveness_info :: Array Int [Int]         -- instruction pred map
                     -> Array Int [Int]      -- instruction succ map
                     -> Array Int Instr      -- array of instructions
                     -> FiniteMap FE RegSet  -- previous approx
                     -> FiniteMap FE RegSet  -- improved approx

upd_liveness_info pred_map succ_map insn_array prev_approx
   = do_insns hi prev_approx
     where
        (lo, hi) = bounds insn_array

        enquireMapFE :: FiniteMap FE RegSet -> FE 
                        -> RegSet
        enquireMapFE fm fe
           = case lookupFM fm fe of
                Just set -> set
                Nothing  -> emptyRegSet

        -- Work backwards, from the highest numbered insn to the lowest.
        -- This is a heuristic which causes faster convergence to the
        -- fixed point.  In particular, for straight-line code with no
        -- branches at all, arrives at the fixpoint in one iteration.
        do_insns ino approx
           | ino < lo
           = approx
           | otherwise
           = let fes_to_futures
                    = [case iUnbox ino of        { inoh ->
                       case iUnbox future_ino of { future_inoh ->
                       MkFE inoh future_inoh
                       }}
                          | future_ino <- succ_map ! ino]
                 future_lives
                    = map (enquireMapFE approx) fes_to_futures
                 future_live
                    = foldr unionRegSets emptyRegSet future_lives

                 fes_from_histories
                    = [case iUnbox history_ino of { history_inoh ->
                       case iUnbox ino of         { inoh ->
                       MkFE history_inoh inoh
                       }}
                          | history_ino <- pred_map ! ino]
                 new_approx
                    = foldl update_one_history approx fes_from_histories
                 
                 insn
                    = insn_array ! ino
                 history_independent_component
                    = case regUsage insn of
                         RU rds wrs
                            -> unionRegSets rds
                                  (minusRegSets future_live wrs)

                 update_one_history :: FiniteMap FE RegSet
                                       -> FE
                                       -> FiniteMap FE RegSet
                 update_one_history approx0 fe
                      = addToFM_C unionRegSets approx0 fe 
                                  history_independent_component

                 rest_done
                    = do_insns (ino-1) new_approx
             in
                 rest_done
                 


-- Extract the flow edges from a list of insns.  Express the information 
-- as two mappings, from insn number to insn numbers of predecessors,
-- and from insn number to insn numbers of successors.  (Since that's
-- what we need to know when computing live ranges later).  Instructions
-- are numbered starting at zero.  This function is long and complex 
-- in order to be efficient; it could equally well be shorter and slower.

find_flow_edges :: [Instr] -> (Array Int [Int],
                               Array Int [Int])
find_flow_edges insns
   = let 
         -- First phase: make a temp env which maps labels
         -- to insn numbers, so the second pass can know the insn
         -- numbers for jump targets.

         label_env :: FiniteMap CLabel Int

         mk_label_env n env [] = env
         mk_label_env n env ((LABEL clbl):is)
            = mk_label_env (n+1) (addToFM env clbl n) is
         mk_label_env n env (i:is)
            = mk_label_env (n+1) env is
   
         label_env = mk_label_env 0 emptyFM insns

         find_label :: CLabel -> Int
         find_label jmptarget
            = case lookupFM label_env jmptarget of
                 Just ino -> ino
                 Nothing  -> pprPanic "find_flow_edges: unmapped label" 
                                      (pprCLabel jmptarget)

         -- Second phase: traverse the insns, and make up the successor map.

         least_ino, greatest_ino :: Int
         least_ino    = 0
         greatest_ino = length insns - 1

         mk_succ_map :: Int -> [(Int, [Int])] -> [Instr] -> [(Int, [Int])]

         mk_succ_map i_num rsucc_map [] 
            = reverse rsucc_map

         mk_succ_map i_num rsucc_map (i:is)
            = let i_num_1 = i_num + 1
              in
              case insnFuture i of

                 NoFuture
                    -> -- A non-local jump.  We can regard this insn as a terminal
                       -- insn in the graph, so we don't add any edges.
                       mk_succ_map i_num_1 ((i_num,[]):rsucc_map) is

                 Next 
                    |  null is -- this is the last insn, and it doesn't go anywhere
                               -- (a meaningless scenario); handle it anyway
                    -> mk_succ_map i_num_1 ((i_num,[]):rsucc_map) is

                    |  otherwise -- flows to next insn; add fe i_num -> i_num+1
                    -> mk_succ_map i_num_1 ((i_num, [i_num_1]): rsucc_map)
                                           is

                 Branch lab -- jmps to lab; add fe i_num -> i_target
                    -> let i_target = find_label lab
                       in 
                       mk_succ_map i_num_1 ((i_num, [i_target]): rsucc_map) is

                 NextOrBranch lab
                    |  null is   -- jmps to label, or falls through, and this is
                                 -- the last insn (a meaningless scenario); 
                                 -- flag an error
                    -> error "find_flow_edges: NextOrBranch is last"

                    |  otherwise -- add fes i_num -> i_num+1  
                                 --     and i_num -> i_target
                    -> let i_target = find_label lab
                       in
                       mk_succ_map i_num_1 ((i_num, [i_num_1, i_target]):rsucc_map)
                                           is
                 MultiFuture labels
                    -> -- A jump, whose targets are listed explicitly.  
                       -- (Generated from table-based switch translations).
                       -- Add fes  i_num -> x  for each x in labels
                       let is_target = nub (map find_label labels)
                       in
                       mk_succ_map i_num_1 ((i_num, is_target):rsucc_map) is

         -- Third phase: invert the successor map to get the predecessor
         -- map, using an algorithm which is quadratic in the worst case,
         -- but runs in almost-linear time, because of the nature of our
         -- inputs: most insns have a single successor, the next insn.

         invert :: [(Int, [Int])] -> [(Int, [Int])]
         invert fmap
            = let inverted_pairs
                     = concatMap ( \ (a, bs) -> [(b,a) | b <- bs] ) fmap
                  sorted_inverted_pairs
                     = isort inverted_pairs
         
                  grp :: Int -> [Int] -> [(Int,Int)] -> [(Int,[Int])]
                  grp k vs [] = [(k, vs)]
                  grp k vs ((kk,vv):rest)
                     | k == kk   = grp k (vv:vs) rest
                     | otherwise = (k,vs) : grp kk [vv] rest

                  grp_start []             = []
                  grp_start ((kk,vv):rest) = grp kk [vv] rest

                  grouped
                     = grp_start sorted_inverted_pairs

                  -- make sure that the reverse mapping maps all inos
                  add_empties ino []
                     | ino > greatest_ino  = []
                     | otherwise           = (ino,[]): add_empties (ino+1) []
                  add_empties ino ((k,vs):rest)
                     | ino <  k   = (ino,[]): add_empties (ino+1) ((k,vs):rest)
                     | ino == k   = (k,vs) : add_empties (ino+1) rest

                  -- This is nearly linear provided that the fsts of the 
                  -- list are nearly in order -- a critical assumption 
                  -- for efficiency.
                  isort :: [(Int,Int)] -> [(Int,Int)]
                  isort []     = []
                  isort (x:xs) = insert x (isort xs)

                  insert :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
                  insert y []     = [y]
                  insert y (z:zs)
                     -- specifically, this first test should almost always
                     -- be True in order for the near-linearity to happen
                     | fst y <= fst z  = y:z:zs 
                     | otherwise       = z: insert y zs
              in
                 add_empties least_ino grouped

         -- Finally ...

         succ_list
            = mk_succ_map 0 [] insns
         succ_map
            = array (least_ino, greatest_ino) succ_list
         pred_list
            = invert succ_list
         pred_map
            = array (least_ino, greatest_ino) pred_list
     in
         (pred_map, succ_map)


-- That's all, folks!  From here on is just some dull supporting stuff.

-- A data type for flow edges
data FE 
   = MkFE FastInt FastInt deriving (Eq, Ord)

-- deriving Show on types with unboxed fields doesn't work
instance Show FE where
    showsPrec _ (MkFE s d) 
       = showString "MkFE" . shows (iBox s) . shows ' ' . shows (iBox d)

-- Blargh.  Use ghc stuff soon!  Or: perhaps that's not such a good
-- idea.  Most of these sets are either empty or very small, and it
-- might be that the overheads of the FiniteMap based set implementation
-- is a net loss.  The same might be true of RegSets.

newtype FeSet = MkFeSet [FE]

feSetFromList xs 
   = MkFeSet (nukeDups (sort xs))
     where nukeDups :: [FE] -> [FE]
           nukeDups []  = []
           nukeDups [x] = [x]
           nukeDups (x:y:xys)
              = if x == y then nukeDups (y:xys)
                          else x : nukeDups (y:xys)

feSetToList (MkFeSet xs)            = xs
isEmptyFeSet (MkFeSet xs)           = null xs
emptyFeSet                          = MkFeSet []
eqFeSet (MkFeSet xs1) (MkFeSet xs2) = xs1 == xs2
unitFeSet x                         = MkFeSet [x]

elemFeSet x (MkFeSet xs) 
   = f xs
     where
        f []     = False
        f (y:ys) | x == y    = True
                 | x < y     = False
                 | otherwise = f ys

unionFeSets (MkFeSet xs1) (MkFeSet xs2)
   = MkFeSet (f xs1 xs2)
     where
        f [] bs = bs
        f as [] = as
        f (a:as) (b:bs)
           | a < b      = a : f as (b:bs)
           | a > b      = b : f (a:as) bs
           | otherwise  = a : f as bs

minusFeSets (MkFeSet xs1) (MkFeSet xs2)
   = MkFeSet (f xs1 xs2)
     where
        f [] bs = []
        f as [] = as
        f (a:as) (b:bs)
           | a < b      = a : f as (b:bs)
           | a > b      = f (a:as) bs
           | otherwise  = f as bs

intersectionFeSets (MkFeSet xs1) (MkFeSet xs2)
   = MkFeSet (f xs1 xs2)
     where
        f [] bs = []
        f as [] = []
        f (a:as) (b:bs)
           | a < b      = f as (b:bs)
           | a > b      = f (a:as) bs
           | otherwise  = a : f as bs

\end{code}

-- | Compute prefered real reg locations for vregs.
module GHC.CmmToAsm.Reg.RegHints (
        RegHints (..),
        ClobberCost,
        computeRegHints
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Regs (freeReg)
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Reg.Target (targetClassOfRealReg)
import GHC.CmmToAsm.Reg.Liveness ( BlockMap, LiveBasicBlock
                                 , LiveInstr(..), Liveness(..) )
import GHC.CmmToAsm.Reg.Regs
import GHC.Cmm (GenBasicBlock(..))
import GHC.Cmm.Dataflow.Label
import GHC.Data.Graph.Directed
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM

import Data.Maybe
import qualified Data.IntMap.Strict as IM

{- Note [Register hints for the linear allocator]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By doing one pass over the output of liveness before we do real register allocation
we can compute allocation hints that greatly improve the allocators performance.

We compute two things during this traversal:

  * a clobber score: inside loops, an instruction that clobbers a real reg
    raises the cost to allocate any vreg into this register.
    This biases register allocation towards callee saved registers where on
    calls and against fixed output registers for instructions that have them.

  * preferences: a vreg moves into a real register and
    dies at the move prefers to be allocated into that register in the first place.
    This biases vregs that will move to e.g. R1 to satisfy the calling convention to
    be allocated into the real reg of R1 from the start.

'chooseFreeReg' in GHC.CmmToAsm.Reg.Linear does the actual register selection among
the registers that are *free*.
-}

-- | What it costs a virtual register to live in a particular real register:
--   one point per clobber of that register the vreg is live across. Higher = worse
type ClobberCost = Int

-- | Register allocation hints for one procedure.
--   See Note [Register hints for the linear allocator].
data RegHints
        = NoRegHints

        | RegHints
                { rh_pref_reg :: !(UniqFM VirtualReg RealReg)
                        -- ^ the real register this vreg would like to live in.
                        -- Could be a ranking/set but we just pick one.
                , rh_clobber_cost :: !(UniqFM VirtualReg (IM.IntMap ClobberCost))
                        -- ^ what it costs this vreg to live in a real register,
                        --   keyed by RegNo. Higher is worse.
                }

-- | Compute register hints for one procedure from its liveness annotations.
--
-- See Note [Register hints for the linear allocator]
computeRegHints
        :: Instruction instr
        => Platform
        -> BlockMap Regs                        -- ^ regs live on entry to each block
        -> [SCC (LiveBasicBlock instr)]
        -> RegHints

computeRegHints platform block_live sccs
        | isNullUFM prefs, isNullUFM scores     = NoRegHints
        | otherwise                             = RegHints prefs scores
  where
        (cold, hot, scores) = foldl' hintSCC (emptyUFM, emptyUFM, emptyUFM) sccs

        -- plusUFM is right biased, so a preference seen in a loop wins over one
        -- seen in straight line code.
        prefs   = plusUFM cold hot

        arch        = platformArch platform
        isFree      = freeReg platform
        classOfReal = targetClassOfRealReg platform

        -- Only blocks in a loop carry weight, and only they are walked for the
        -- clobber score. Every block contributes preferences.
        hintSCC acc (AcyclicSCC blk)    = hintBlock False acc blk
        hintSCC acc (CyclicSCC blks)    = foldl' (hintBlock True) acc blks

        -- Perf optimization: We check if any instruction clobbers in the block
        -- and only if so we run the clobber penalty analysis via goBoth.
        -- Otherwise we only update (potentially) preferences.
        hintBlock inLoop acc (BasicBlock bid instrs)
                | inLoop, any instrHasPin instrs
                = goBoth acc (fromMaybe noRegs (mapLookup bid block_live)) instrs
                | otherwise
                = goPrefs inLoop acc instrs

        -- No clobber in this block: preferences only, no live set needed.
        goPrefs _ !acc [] = acc
        goPrefs inLoop !acc (LiveInstr instr mb_lv : is)
                | Just lv <- mb_lv
                = goPrefs inLoop (addPrefOf inLoop acc instr lv) is
                | otherwise
                = goPrefs inLoop acc is

        goBoth !acc _ [] = acc
        goBoth !acc live (LiveInstr _ Nothing : is)
                = goBoth acc live is
        goBoth !acc live (LiveInstr instr (Just lv) : is)
                = goBoth acc'' live' is
          where
            -- Exactly saveClobberedTemps' save set: in a register coming in, and
            -- not dying here.
            !across = live `minusRegs` liveDieRead lv
            !live'  = (across `unionRegsMaxFmt` liveBorn lv) `minusRegs` liveDieWrite lv

            !acc' = addPrefOf True acc instr lv

            -- The real registers *born* here are the ones this instruction starts
            -- clobbering. A real register written while it is already live was
            -- pinned by whatever defined it (the codegen's `mov ...,%rax` in
            -- front of a MUL2, say) and was charged there; charging it again
            -- would count one clobber region twice.
            !acc''
              | pins@(_:_) <- pinsOf (liveBorn lv)
              , (cp, hp, sc) <- acc'
              = ( cp, hp
                , foldl' (bumpVReg pins) sc
                        (nonDetEltsUniqSet (takeVirtualRegs (getRegs across))) )
                        -- Summing into a per-vreg map, so the result does not
                        -- depend on the enumeration order.
              | otherwise
              = acc'

        -- A vreg moved into an allocatable real register, dying at the move,
        -- would rather live in that register: then the move goes away.
        addPrefOf inLoop (cp, hp, sc) instr lv
                | Just (RegVirtual v, RegReal rr@(RealRegSingle n))
                        <- takeRegRegMoveInstr platform instr
                , isFree n
                , isJust (lookupReg (RegVirtual v) (liveDieRead lv))
                = if inLoop then (cp, addPref hp v rr, sc)
                            else (addPref cp v rr, hp, sc)
                | otherwise
                = (cp, hp, sc)

        instrHasPin (LiveInstr _ (Just lv))
                = nonDetStrictFoldUniqSet
                        (\ rwf b -> b || isPinReg rwf) False (getRegs (liveBorn lv))
        instrHasPin _
                = False

        isPinReg (RegWithFormat (RegReal (RealRegSingle n)) _) = isFree n
        isPinReg _                                             = False

        -- First one in program order wins, as for findPrefRealReg.
        -- We could try building a set/list here instead but it rarely matters.
        addPref m v r = addToUFM_C (\old _ -> old) m v r

        -- The allocatable real registers written here, deduplicated, with their
        -- class. Filtering by freeReg is not optional: Sp/Hp adjustments write
        -- real registers on nearly every heap allocation.
        pinsOf born = nonDetStrictFoldUniqSet add [] (getRegs born)
          where
            add (RegWithFormat (RegReal rr@(RealRegSingle n)) _) acc
                | isFree n
                = (classOfReal rr, n) : acc
            add _ acc = acc
            -- The set has no duplicates, and the result is only ever used as a
            -- summand, so the enumeration order does not reach the output.

        -- A pinned register only costs vregs of its own class.
        bumpVReg pins !m v = foldl' step m pins
          where
            !cls = classOfVirtualReg arch v
            step !m' (c, n)
                | c == cls  = alterUFM (Just . IM.insertWith (+) n 1 . fromMaybe IM.empty) m' v
                | otherwise = m'

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
When the linear allocator needs a register for a virtual register and one is
free it takes the register the vreg was first assigned to if that is still free
(see findPrefRealReg and #18208), and otherwise the *first* free register of the
class. On x86-64 that hands out rax, rcx and rdx first -- exactly the registers
that mulq, divq and a variable shift pin -- so whichever values happen to be
defined first take them, and every later fixed-register instruction has to move
those values out of the way and back again, once per loop iteration (#27742).

'computeRegHints' therefore records two things per procedure, out of the
liveness annotations that are there anyway:

  * a clobber score: inside loops, an instruction that starts clobbering an
    allocatable real register charges one point to every vreg of the same
    register class that is live across it -- exactly the set
    'saveClobberedTemps' would have to save;

  * a preference: a vreg that is moved into an allocatable real register and
    dies at the move would rather live in that register, so that the move goes
    away.

'chooseFreeReg' in GHC.CmmToAsm.Reg.Linear breaks the tie among the registers
that are *free* with them. Nothing else consumes them: how many values are live,
whether a spill happens and which register is spilled are all unchanged, so the
hints cannot introduce a spill.
-}

-- | What it costs a virtual register to live in a particular real register:
--   one point per clobber of that register the vreg is live across.
--
--   Higher is worse. 'chooseFreeReg' picks the free register of least cost,
--   so a zero cost means "nothing clobbers this register under the vreg".
type ClobberCost = Int

-- | Register allocation hints for one procedure.
--   See Note [Register hints for the linear allocator].
data RegHints
        -- | No hints: every consumer falls back to its unhinted rule.
        = NoRegHints

        | RegHints
                { rh_pref_reg :: !(UniqFM VirtualReg RealReg)
                        -- ^ the real register this vreg would like to live in
                , rh_clobber_cost :: !(UniqFM VirtualReg (IM.IntMap ClobberCost))
                        -- ^ what it costs this vreg to live in a real register,
                        --   keyed by RegNo. Higher is worse.
                }

-- | Compute register hints for one procedure from its liveness annotations.
--
-- Two things are recorded, see Note [Register hints for the linear allocator]:
--
--  * a clobber score: inside loops, an instruction that starts clobbering an
--    allocatable real register charges one point to every vreg of the same
--    register class that is live across it (which is exactly the set
--    'saveClobberedTemps' would have to save);
--
--  * a preference: a vreg that is moved into an allocatable real register and
--    dies there would rather live in that register, so that the move goes away.
--
-- This runs over the *final* annotations. It cannot be folded into the liveness
-- pass itself: 'livenessSCCs' iterates 'livenessBlock' to a fixpoint over every
-- CyclicSCC, so anything accumulated in there would be counted once per
-- iteration, with a data dependent iteration count, for exactly the blocks that
-- carry weight.
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

        -- Maintaining the live set costs two set rebuilds per instruction, so
        -- only do it for blocks that actually contain a clobber. Most loop
        -- blocks contain none, and then only the (much cheaper) preference scan
        -- runs.
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

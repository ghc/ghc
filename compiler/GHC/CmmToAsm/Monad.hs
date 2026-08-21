{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
--
-- The native code generator's monad.
--
-- -----------------------------------------------------------------------------

module GHC.CmmToAsm.Monad (
        NcgImpl(..),
        NatM_State(..), mkNatM_State,

        NatM, -- instance Monad
        initNat,
        addImportNat,
        addNodeBetweenNat,
        addImmediateSuccessorNat,
        getCurrentBlock,
        setCurrentBlock,
        currentBlock,
        continueInNewBlock,
        addDiamondFlow,
        addCondBlock,
        addColdSelfLoop,
        increaseEdgeWeight,
        getUniqueNat,
        setDeltaNat,
        getConfig,
        getPlatform,
        getDeltaNat,
        getThisModuleNat,
        getBlockIdNat,
        getNewLabelNat,
        getNewRegNat,
        getPicBaseMaybeNat,
        getPicBaseNat,
        getCfgWeights,
        getFileId,
        getDebugBlock,

        DwarfFiles,

        -- * 64-bit registers on 32-bit architectures
        Reg64(..), RegCode64(..),
        getNewReg64, localReg64
)

where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Reg
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.CLabel           ( CLabel )
import GHC.Cmm.DebugBlock
import GHC.Cmm.Expr             (LocalReg (..), isWord64)

import GHC.Data.FastString      ( FastString )
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSM
import GHC.Types.Unique         ( Unique )
import GHC.Unit.Module

import GHC.Utils.Outputable (SDoc, HDoc, ppr)
import GHC.Utils.Panic      (panic, pprPanic)
import GHC.Utils.Monad.State.Strict (State (..), runState, state)
import GHC.Utils.Misc
import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.CFG.Weight
import GHC.Data.Unboxed (MaybeUB (..))

-- | A Native Code Generator implementation is parametrised over
-- * The type of static data (typically related to 'CmmStatics')
-- * The type of instructions
-- * The type of jump destinations
data NcgImpl statics instr jumpDest = NcgImpl {
    ncgConfig                 :: !NCGConfig,
    cmmTopCodeGen             :: RawCmmDecl -> NatM [NatCmmDecl statics instr],
    generateJumpTableForInstr :: instr -> Maybe (NatCmmDecl statics instr),
    -- | Given a jump destination, if it refers to a block, return the block id of the destination.
    getJumpDestBlockId        :: jumpDest -> Maybe BlockId,
    -- | Does this jump always jump to a single destination and is shortcutable?
    --
    -- We use this to determine whether the given instruction is a shortcutable
    -- jump to some destination - See Note [supporting shortcutting]
    -- Note that if we return a destination here we *most* support the relevant shortcutting in
    -- shortcutStatics for jump tables and shortcutJump for the instructions itself.
    canShortcut               :: instr -> Maybe jumpDest,
    -- | Replace references to blockIds with other destinations - used to update jump tables.
    shortcutStatics           :: (BlockId -> Maybe jumpDest) -> statics -> statics,
    -- | Change the jump destination(s) of an instruction.
    --
    -- Rewrites the destination of a jump instruction to another
    -- destination, if the given function returns a new jump destination for
    -- the 'BlockId' of the original destination.
    --
    -- For instance, for a mapping @block_a -> dest_b@ and a instruction @goto block_a@ we would
    -- rewrite the instruction to @goto dest_b@
    shortcutJump              :: (BlockId -> Maybe jumpDest) -> instr -> instr,
    -- | 'Module' is only for printing internal labels. See Note [Internal proc
    -- labels] in CLabel.
    pprNatCmmDeclS            :: NatCmmDecl statics instr -> SDoc,
    pprNatCmmDeclH            :: NatCmmDecl statics instr -> HDoc,
        -- see Note [pprNatCmmDeclS and pprNatCmmDeclH]
    maxSpillSlots             :: Int,
    allocatableRegs           :: [RealReg],
    ncgAllocMoreStack         :: Int -> NatCmmDecl statics instr
                              -> UniqDSM (NatCmmDecl statics instr, [(BlockId,BlockId)]),
    -- ^ The list of block ids records the redirected jumps to allow us to update
    -- the CFG.
    ncgMakeFarBranches        :: Platform -> LabelMap RawCmmStatics -> [NatBasicBlock instr]
                              -> UniqDSM [NatBasicBlock instr],
    extractUnwindPoints       :: [instr] -> [UnwindPoint],
    -- ^ given the instruction sequence of a block, produce a list of
    -- the block's 'UnwindPoint's
    -- See Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock"
    -- and Note [Unwinding information in the NCG] in this module.
    invertCondBranches        :: Maybe CFG -> LabelMap RawCmmStatics -> [NatBasicBlock instr]
                              -> [NatBasicBlock instr]
    -- ^ Turn the sequence of @jcc l1; jmp l2@ into @jncc l2; \<block_l1>@
    -- when possible.
    }

{- Note [supporting shortcutting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the concept of shortcutting see Note [What is shortcutting].

In order to support shortcutting across multiple backends uniformly we
use canShortcut, shortcutStatics and shortcutJump.

canShortcut tells us if the backend support shortcutting of a instruction
and if so what destination we should retarget instruction to instead.

shortcutStatics exists to allow us to update jump destinations in jump tables.

shortcutJump updates the instructions itself.

A backend can opt out of those by always returning Nothing for canShortcut
and implementing shortcutStatics/shortcutJump as \_ x -> x

-}

{- Note [pprNatCmmDeclS and pprNatCmmDeclH]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each NcgImpl provides two implementations of its CmmDecl printer, pprNatCmmDeclS
and pprNatCmmDeclH, which are specialized to SDoc and HDoc, respectively
(see Note [SDoc versus HDoc] in GHC.Utils.Outputable). These are both internally
implemented as a single, polymorphic function, but they need to be stored using
monomorphic types to ensure the specialized versions are used, which is
essential for performance (see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable).

One might wonder why we bother with pprNatCmmDeclS and SDoc at all, since we
have a perfectly serviceable HDoc-based implementation that is more efficient.
However, it turns out we benefit from keeping both, for two (related) reasons:

  1. Although we absolutely want to take care to use pprNatCmmDeclH for actual
     code generation (the improved performance there is why we have HDoc at
     all!), we also sometimes print assembly for debug dumps, when requested via
     -ddump-asm. In this case, it’s more convenient to produce an SDoc, which
     can be concatenated with other SDocs for consistency with the general-
     purpose dump file infrastructure.

  2. Some debug information is sometimes useful to include in -ddump-asm that is
     neither necessary nor useful in normal code generation, and it turns out to
     be tricky to format neatly using the one-line-at-a-time model of HLine/HDoc.

Therefore, we provide both pprNatCmmDeclS and pprNatCmmDeclH, and we sometimes
include additional information in the SDoc variant using dualDoc
(see Note [dualLine and dualDoc] in GHC.Utils.Outputable). However, it is
absolutely *critical* that pprNatCmmDeclS is not actually used unless -ddump-asm
is provided, as that would rather defeat the whole point. (Fortunately, the
difference in allocations between the two implementations is so vast that such a
mistake would readily show up in performance tests). -}

data NatM_State
        = NatM_State {
                natm_us          :: DUniqSupply,
                natm_delta       :: Int, -- ^ Stack offset for unwinding information
                natm_imports     :: [(CLabel)],
                natm_pic         :: Maybe Reg,
                natm_config      :: NCGConfig,
                natm_fileid      :: DwarfFiles,
                natm_debug_map   :: LabelMap DebugBlock,
                natm_cfg         :: CFG,
        -- ^ Having a CFG with additional information is essential for some
        -- operations. However we can't reconstruct all information once we
        -- generated instructions. So instead we update the CFG as we go.
                natm_cur_block   :: !(MaybeUB BlockId)
        -- ^ Keep track of the current block during code generation for
        -- CFG updates. Only used by backends using the CFG for code layout.
        }

type DwarfFiles = UniqFM FastString (FastString, Int)

newtype NatM a = NatM' (State NatM_State a)
  deriving stock (Functor)
  deriving (Applicative, Monad) via State NatM_State

pattern NatM :: (NatM_State -> (a, NatM_State)) -> NatM a
pattern NatM f <- NatM' (runState -> f)
  where NatM f  = NatM' (state f)
{-# COMPLETE NatM #-}

unNat :: NatM a -> NatM_State -> (a, NatM_State)
unNat (NatM a) = a

mkNatM_State :: DUniqSupply -> Int -> NCGConfig ->
                DwarfFiles -> LabelMap DebugBlock -> CFG -> NatM_State
mkNatM_State us delta config
        = \dwf dbg cfg ->
                NatM_State
                        { natm_us = us
                        , natm_delta = delta
                        , natm_imports = []
                        , natm_pic = Nothing
                        , natm_config = config
                        , natm_fileid = dwf
                        , natm_debug_map = dbg
                        , natm_cfg = cfg
                        , natm_cur_block = NothingUB
                        }

initNat :: NatM_State -> NatM a -> (a, NatM_State)
initNat = flip unNat

instance MonadGetUnique NatM where
  getUniqueM = NatM $ \st ->
      case takeUniqueFromDSupply (natm_us st) of
        (uniq, us') -> (uniq, st {natm_us = us'})

getUniqueNat :: NatM Unique
getUniqueNat = getUniqueM

getDeltaNat :: NatM Int
getDeltaNat = NatM $ \ st -> (natm_delta st, st)

-- | Get CFG edge weights
getCfgWeights :: NatM Weights
getCfgWeights = NatM $ \ st -> (ncgCfgWeights (natm_config st), st)

setDeltaNat :: Int -> NatM ()
setDeltaNat delta = NatM $ \ st -> ((), st {natm_delta = delta})

getThisModuleNat :: NatM Module
getThisModuleNat = NatM $ \ st -> (ncgThisModule $ natm_config st, st)

instance HasModule NatM where
  getModule = getThisModuleNat

addImportNat :: CLabel -> NatM ()
addImportNat imp
        = NatM $ \ st -> ((), st {natm_imports = imp : natm_imports st})

updateCfgNat :: (CFG -> CFG) -> NatM ()
updateCfgNat f
        = NatM $ \ st -> let !cfg' = f (natm_cfg st)
                         in ((), st { natm_cfg = cfg'})

setCurrentBlock :: BlockId -> NatM ()
setCurrentBlock bid = NatM $ \ st -> ((), st { natm_cur_block = JustUB bid })

getCurrentBlock :: NatM (Maybe BlockId)
getCurrentBlock = NatM $ \ st ->
  let !cbid = case natm_cur_block st of
        JustUB bid -> Just bid
        NothingUB  -> Nothing
  in
  ( cbid, st )

{- Note [Updating the CFG during CodeGen]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the CMM level life is simple:
Blocks consist of a sequence of statements.
Control flow exists only between blocks.

We are not so lucky for CodeGen. There we may introduce
intra-block control flow. For example we might turn a
simple ´MO_Ctz width arg` into branching code like this:

       arg_block
        ╱     ╲
    left       right
        ╲     ╱
          cont

We update the CFG to account for this. (See also Note [CFG based code layout]).
To do so we generally:
* Keep track of the current block in the NatM state.
* Compute code for all dependencies (arg in this case)
* Then generate the instructions for the MachOp at hand.
  + If that involves branching control flow we call update the CFG
    by calling one of continueInNewBlock, addCondBlock or addDiamondFlow,
    which will adjust the CFG
    and update the current block.
* We then return our generated instructions, and the parent expression
  can use the update CFG/currentBlock to generate it's own code.
-}

-- | The block instructions are currently being generated for.
--
-- Panics if the current block isn't being tracked, which is a code generator
-- bug: any backend using the CFG based operations below must set the current
-- block for each basic block it starts to generate code for.
currentBlock :: HasDebugCallStack => NatM BlockId
currentBlock = NatM $ \ st ->
  case natm_cur_block st of
    JustUB bid -> (bid, st)
    NothingUB  -> panic "currentBlock: current block not tracked"

-- | Continue/extend the current block under a new label.
--
-- >   Before:  cur -> S       After:  cur -> cont -> S
--
-- All cur->S edges get rewritten to cont->S.
-- @cont@ becomes the current block.
-- Returns @cur@ (the old current block).
--
-- Use for example for self loops. See also Note [Updating the CFG during CodeGen]
continueInNewBlock :: HasDebugCallStack => BlockId -> NatM BlockId
continueInNewBlock cont = do
    cur <- currentBlock
    addImmediateSuccessorNat cur cont
    setCurrentBlock cont
    return cur

-- | Register diamond shaped control flow.
--
-- >     Before:            After:
-- >
-- >       cur                cur
-- >        │                ╱   ╲
-- >        │          likely     unlikely
-- >        │                ╲   ╱
-- >        │                 cont
-- >        ▼                  │
-- >        S                  ▼
-- >                           S
--
-- * All cur->S edges get rewritten to cont->S.
-- * @cont becomes the current block.
--
-- See also Note [Updating the CFG during CodeGen]
addDiamondFlow :: HasDebugCallStack
               => BlockId -- ^ the arm we expect to be taken
               -> BlockId -- ^ the arm we expect not to be taken
               -> BlockId -- ^ the block both arms converge on
               -> NatM ()
addDiamondFlow likely unlikely cont = do
    weights <- getCfgWeights
    cur <- continueInNewBlock cont
    -- Both arms end in an unconditional jump to cont. Control never passes
    -- from cur to cont directly, so we drop the edge continueInNewBlock added.
    updateCfgNat ( addWeightEdge cur likely    (fromIntegral $ likelyCondWeight weights)
                 . addWeightEdge cur unlikely  (fromIntegral $ unlikelyCondWeight weights)
                 . addWeightEdge likely   cont (fromIntegral $ uncondWeight weights)
                 . addWeightEdge unlikely cont (fromIntegral $ uncondWeight weights)
                 . delEdge cur cont )

-- | Register a conditional block that converges again on the same path.
--
-- >     Before:            After:
-- >
-- >       cur                cur ────╮
-- >        │                  │      │
-- >        │                  │  cond_block
-- >        │                  │      │
-- >        │                 cont ◀─╯
-- >        ▼                  │
-- >        S                  ▼
-- >                           S
--
-- Takes a bool @is_likely@ that indicates if the new block is the likely code
-- path or not.
--
-- @cont@ takes over the successors of the current block and becomes the
-- current block.
--
-- See also Note [Updating the CFG during CodeGen]
addCondBlock :: HasDebugCallStack
             => BlockId -- ^ the new code block
             -> Bool    -- ^ Is the newly given block the likely code path?
             -> BlockId -- ^ the block control flow converges on
             -> NatM ()
addCondBlock cond_block is_likely cont = do
    weights <- getCfgWeights
    cur <- continueInNewBlock cont
    let likely   = fromIntegral (likelyCondWeight weights)
        unlikely = fromIntegral (unlikelyCondWeight weights)
        (w_cond, w_skip) | is_likely = (likely, unlikely)
                         | otherwise = (unlikely, likely)
    -- This overwrites the cur -> cont edge added by continueInNewBlock, which
    -- is no longer an unconditional jump now that cond_block can be taken.
    updateCfgNat ( addWeightEdge cur cond_block w_cond
                 . addWeightEdge cur cont       w_skip
                 . addWeightEdge cond_block cont (fromIntegral $ uncondWeight weights) )

-- | Register a self loop on the given block, e.g. the retry loop of a
-- cmpxchg based sequence.
--
-- >   bid ──╮
-- >    ▲    │
-- >    ╰────╯
--
-- The edge gets a weight of zero, which keeps it irrelevant for layout:
-- 'optimizeCFG' deliberately does not apply its back edge bonus to zero weight
-- edges, so @bid@ is not treated as the head of a hot loop.
--
-- See also Note [Updating the CFG during CodeGen]
-- See also Note [Introducing cfg edges inside basic blocks] for some wrinkles around
-- self loops in particular.
addColdSelfLoop :: BlockId -> NatM ()
addColdSelfLoop bid = updateCfgNat (addWeightEdge bid bid 0)

-- | Allows us to bias layout towards a specific edge.
increaseEdgeWeight :: HasDebugCallStack => BlockId -> EdgeWeight -> NatM ()
increaseEdgeWeight target bonus = do
    cur <- currentBlock
    updateCfgNat (\cfg -> adjustEdgeWeight cfg (+ bonus) cur target)

-- | Record that we added a block between `from` and `old`.
addNodeBetweenNat :: BlockId -> BlockId -> BlockId -> NatM ()
addNodeBetweenNat from between to
 = do   weights <- getCfgWeights
        let jmpWeight = fromIntegral (uncondWeight weights)
        updateCfgNat (updateCfg jmpWeight from between to)
  where
    -- When transforming A -> B to A -> A' -> B
    -- A -> A' keeps the old edge info while
    -- A' -> B gets the info for an unconditional
    -- jump.
    updateCfg weight from between old m
        | Just info <- getEdgeInfo from old m
        = addEdge from between info .
          addWeightEdge between old weight .
          delEdge from old $ m
        | otherwise
        = pprPanic "Failed to update cfg: Untracked edge" (ppr (from,to))


-- | Place `succ` after `block` and change any edges
--   block -> X to `succ` -> X
addImmediateSuccessorNat :: BlockId -> BlockId -> NatM ()
addImmediateSuccessorNat block succ = do
   weights <- getCfgWeights
   updateCfgNat (addImmediateSuccessor weights block succ)

getBlockIdNat :: NatM BlockId
getBlockIdNat
 = mkBlockId <$> getUniqueNat

getNewLabelNat :: NatM CLabel
getNewLabelNat
 = blockLbl <$> getBlockIdNat


getNewRegNat :: Format -> NatM Reg
getNewRegNat rep
 = do u <- getUniqueNat
      platform <- getPlatform
      return (RegVirtual $ targetMkVirtualReg platform u rep)


-- | Two 32-bit regs used as a single virtual 64-bit register
data Reg64 = Reg64
  !Reg -- ^ Higher part
  !Reg -- ^ Lower part

-- | Two 32-bit regs used as a single virtual 64-bit register
-- and the code to set them appropriately
data RegCode64 code = RegCode64
  code -- ^ Code to initialize the registers
  !Reg -- ^ Higher part
  !Reg -- ^ Lower part

-- | Return a virtual 64-bit register
getNewReg64 :: NatM Reg64
getNewReg64 = do
  let rep = II32
  u <- getUniqueNat
  platform <- getPlatform
  let vLo = targetMkVirtualReg platform u rep
  let lo  = RegVirtual $ targetMkVirtualReg platform u rep
  let hi  = RegVirtual $ getHiVirtualRegFromLo vLo
  return $ Reg64 hi lo

-- | Convert a 64-bit LocalReg into two virtual 32-bit regs.
--
-- Used to handle 64-bit "registers" on 32-bit architectures
localReg64 :: HasDebugCallStack => LocalReg -> Reg64
localReg64 (LocalReg vu ty)
  | isWord64 ty = let lo = RegVirtual (VirtualRegI vu)
                      hi = getHiVRegFromLo lo
                  in Reg64 hi lo
  | otherwise   = pprPanic "localReg64" (ppr ty)


getPicBaseMaybeNat :: NatM (Maybe Reg)
getPicBaseMaybeNat
        = NatM (\state -> (natm_pic state, state))


getPicBaseNat :: Format -> NatM Reg
getPicBaseNat rep
 = do   mbPicBase <- getPicBaseMaybeNat
        case mbPicBase of
                Just picBase -> return picBase
                Nothing
                 -> do
                        reg <- getNewRegNat rep
                        NatM (\state -> (reg, state { natm_pic = Just reg }))

-- | Get native code generator configuration
getConfig :: NatM NCGConfig
getConfig = NatM $ \st -> (natm_config st, st)

-- | Get target platform from native code generator configuration
getPlatform :: NatM Platform
getPlatform = ncgPlatform <$> getConfig

getFileId :: FastString -> NatM Int
getFileId f = NatM $ \st ->
  case lookupUFM (natm_fileid st) f of
    Just (_,n) -> (n, st)
    Nothing    -> let n = 1 + sizeUFM (natm_fileid st)
                      fids = addToUFM (natm_fileid st) f (f,n)
                  in n `seq` fids `seq` (n, st { natm_fileid = fids  })

getDebugBlock :: Label -> NatM (Maybe DebugBlock)
getDebugBlock l = NatM $ \st -> (mapLookup l (natm_debug_map st), st)

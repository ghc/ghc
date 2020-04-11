{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

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
        initConfig,
        addImportNat,
        addNodeBetweenNat,
        addImmediateSuccessorNat,
        updateCfgNat,
        getUniqueNat,
        mapAccumLNat,
        setDeltaNat,
        getConfig,
        getPlatform,
        getDeltaNat,
        getThisModuleNat,
        getBlockIdNat,
        getNewLabelNat,
        getNewRegNat,
        getNewRegPairNat,
        getPicBaseMaybeNat,
        getPicBaseNat,
        getDynFlags,
        getModLoc,
        getFileId,
        getDebugBlock,

        DwarfFiles
)

where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Reg
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.CLabel           ( CLabel )
import GHC.Cmm.DebugBlock
import GHC.Data.FastString      ( FastString )
import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply
import GHC.Types.Unique         ( Unique )
import GHC.Driver.Session
import GHC.Unit.Module

import Control.Monad    ( ap )

import GHC.CmmToAsm.Instr
import GHC.Utils.Outputable (SDoc, pprPanic, ppr)
import GHC.Cmm (RawCmmDecl, RawCmmStatics)
import GHC.CmmToAsm.CFG

data NcgImpl statics instr jumpDest = NcgImpl {
    ncgConfig                 :: !NCGConfig,
    cmmTopCodeGen             :: RawCmmDecl -> NatM [NatCmmDecl statics instr],
    generateJumpTableForInstr :: instr -> Maybe (NatCmmDecl statics instr),
    getJumpDestBlockId        :: jumpDest -> Maybe BlockId,
    canShortcut               :: instr -> Maybe jumpDest,
    shortcutStatics           :: (BlockId -> Maybe jumpDest) -> statics -> statics,
    shortcutJump              :: (BlockId -> Maybe jumpDest) -> instr -> instr,
    pprNatCmmDecl             :: NatCmmDecl statics instr -> SDoc,
    maxSpillSlots             :: Int,
    allocatableRegs           :: [RealReg],
    ncgExpandTop              :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr],
    ncgAllocMoreStack         :: Int -> NatCmmDecl statics instr
                              -> UniqSM (NatCmmDecl statics instr, [(BlockId,BlockId)]),
    -- ^ The list of block ids records the redirected jumps to allow us to update
    -- the CFG.
    ncgMakeFarBranches        :: LabelMap RawCmmStatics -> [NatBasicBlock instr] -> [NatBasicBlock instr],
    extractUnwindPoints       :: [instr] -> [UnwindPoint],
    -- ^ given the instruction sequence of a block, produce a list of
    -- the block's 'UnwindPoint's
    -- See Note [What is this unwinding business?] in Debug
    -- and Note [Unwinding information in the NCG] in this module.
    invertCondBranches        :: Maybe CFG -> LabelMap RawCmmStatics -> [NatBasicBlock instr]
                              -> [NatBasicBlock instr]
    -- ^ Turn the sequence of `jcc l1; jmp l2` into `jncc l2; <block_l1>`
    -- when possible.
    }

data NatM_State
        = NatM_State {
                natm_us          :: UniqSupply,
                natm_delta       :: Int,
                natm_imports     :: [(CLabel)],
                natm_pic         :: Maybe Reg,
                natm_dflags      :: DynFlags,
                natm_config      :: NCGConfig,
                natm_this_module :: Module,
                natm_modloc      :: ModLocation,
                natm_fileid      :: DwarfFiles,
                natm_debug_map   :: LabelMap DebugBlock,
                natm_cfg         :: CFG
        -- ^ Having a CFG with additional information is essential for some
        -- operations. However we can't reconstruct all information once we
        -- generated instructions. So instead we update the CFG as we go.
        }

type DwarfFiles = UniqFM (FastString, Int)

newtype NatM result = NatM (NatM_State -> (result, NatM_State))
    deriving (Functor)

unNat :: NatM a -> NatM_State -> (a, NatM_State)
unNat (NatM a) = a

mkNatM_State :: UniqSupply -> Int -> DynFlags -> Module -> ModLocation ->
                DwarfFiles -> LabelMap DebugBlock -> CFG -> NatM_State
mkNatM_State us delta dflags this_mod
        = \loc dwf dbg cfg ->
                NatM_State
                        { natm_us = us
                        , natm_delta = delta
                        , natm_imports = []
                        , natm_pic = Nothing
                        , natm_dflags = dflags
                        , natm_config = initConfig dflags
                        , natm_this_module = this_mod
                        , natm_modloc = loc
                        , natm_fileid = dwf
                        , natm_debug_map = dbg
                        , natm_cfg = cfg
                        }

-- | Initialize the native code generator configuration from the DynFlags
initConfig :: DynFlags -> NCGConfig
initConfig dflags = NCGConfig
   { ncgPlatform              = targetPlatform dflags
   , ncgUnitId                = thisPackage dflags
   , ncgProcAlignment         = cmmProcAlignment dflags
   , ncgDebugLevel            = debugLevel dflags
   , ncgExternalDynamicRefs   = gopt Opt_ExternalDynamicRefs dflags
   , ncgPIC                   = positionIndependent dflags
   , ncgInlineThresholdMemcpy = fromIntegral $ maxInlineMemcpyInsns dflags
   , ncgInlineThresholdMemset = fromIntegral $ maxInlineMemsetInsns dflags
   , ncgSplitSections         = gopt Opt_SplitSections dflags
   , ncgSpillPreallocSize     = rESERVED_C_STACK_BYTES dflags
   , ncgRegsIterative         = gopt Opt_RegsIterative dflags
   , ncgAsmLinting            = gopt Opt_DoAsmLinting dflags

     -- With -O1 and greater, the cmmSink pass does constant-folding, so
     -- we don't need to do it again in the native code generator.
   , ncgDoConstantFolding     = optLevel dflags < 1

   , ncgDumpRegAllocStages    = dopt Opt_D_dump_asm_regalloc_stages dflags
   , ncgDumpAsmStats          = dopt Opt_D_dump_asm_stats dflags
   , ncgDumpAsmConflicts      = dopt Opt_D_dump_asm_conflicts dflags
   , ncgBmiVersion            = case platformArch (targetPlatform dflags) of
                                 ArchX86_64 -> bmiVersion dflags
                                 ArchX86    -> bmiVersion dflags
                                 _          -> Nothing

     -- We Assume  SSE1 and SSE2 operations are available on both
     -- x86 and x86_64. Historically we didn't default to SSE2 and
     -- SSE1 on x86, which results in defacto nondeterminism for how
     -- rounding behaves in the associated x87 floating point instructions
     -- because variations in the spill/fpu stack placement of arguments for
     -- operations would change the precision and final result of what
     -- would otherwise be the same expressions with respect to single or
     -- double precision IEEE floating point computations.
   , ncgSseVersion =
      let v | sseVersion dflags < Just SSE2 = Just SSE2
            | otherwise                     = sseVersion dflags
      in case platformArch (targetPlatform dflags) of
            ArchX86_64 -> v
            ArchX86    -> v
            _          -> Nothing
   }


initNat :: NatM_State -> NatM a -> (a, NatM_State)
initNat init_st m
        = case unNat m init_st of { (r,st) -> (r,st) }

instance Applicative NatM where
      pure = returnNat
      (<*>) = ap

instance Monad NatM where
  (>>=) = thenNat

instance MonadUnique NatM where
  getUniqueSupplyM = NatM $ \st ->
      case splitUniqSupply (natm_us st) of
          (us1, us2) -> (us1, st {natm_us = us2})

  getUniqueM = NatM $ \st ->
      case takeUniqFromSupply (natm_us st) of
          (uniq, us') -> (uniq, st {natm_us = us'})

thenNat :: NatM a -> (a -> NatM b) -> NatM b
thenNat expr cont
        = NatM $ \st -> case unNat expr st of
                        (result, st') -> unNat (cont result) st'

returnNat :: a -> NatM a
returnNat result
        = NatM $ \st ->  (result, st)

mapAccumLNat :: (acc -> x -> NatM (acc, y))
                -> acc
                -> [x]
                -> NatM (acc, [y])

mapAccumLNat _ b []
  = return (b, [])
mapAccumLNat f b (x:xs)
  = do (b__2, x__2)  <- f b x
       (b__3, xs__2) <- mapAccumLNat f b__2 xs
       return (b__3, x__2:xs__2)

getUniqueNat :: NatM Unique
getUniqueNat = NatM $ \ st ->
    case takeUniqFromSupply $ natm_us st of
    (uniq, us') -> (uniq, st {natm_us = us'})

instance HasDynFlags NatM where
    getDynFlags = NatM $ \ st -> (natm_dflags st, st)


getDeltaNat :: NatM Int
getDeltaNat = NatM $ \ st -> (natm_delta st, st)


setDeltaNat :: Int -> NatM ()
setDeltaNat delta = NatM $ \ st -> ((), st {natm_delta = delta})


getThisModuleNat :: NatM Module
getThisModuleNat = NatM $ \ st -> (natm_this_module st, st)


addImportNat :: CLabel -> NatM ()
addImportNat imp
        = NatM $ \ st -> ((), st {natm_imports = imp : natm_imports st})

updateCfgNat :: (CFG -> CFG) -> NatM ()
updateCfgNat f
        = NatM $ \ st -> let !cfg' = f (natm_cfg st)
                         in ((), st { natm_cfg = cfg'})

-- | Record that we added a block between `from` and `old`.
addNodeBetweenNat :: BlockId -> BlockId -> BlockId -> NatM ()
addNodeBetweenNat from between to
 = do   df <- getDynFlags
        let jmpWeight = fromIntegral . uncondWeight .
                        cfgWeightInfo $ df
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
   dflags <- getDynFlags
   updateCfgNat (addImmediateSuccessor dflags block succ)

getBlockIdNat :: NatM BlockId
getBlockIdNat
 = do   u <- getUniqueNat
        return (mkBlockId u)


getNewLabelNat :: NatM CLabel
getNewLabelNat
 = blockLbl <$> getBlockIdNat


getNewRegNat :: Format -> NatM Reg
getNewRegNat rep
 = do u <- getUniqueNat
      platform <- getPlatform
      return (RegVirtual $ targetMkVirtualReg platform u rep)


getNewRegPairNat :: Format -> NatM (Reg,Reg)
getNewRegPairNat rep
 = do u <- getUniqueNat
      platform <- getPlatform
      let vLo = targetMkVirtualReg platform u rep
      let lo  = RegVirtual $ targetMkVirtualReg platform u rep
      let hi  = RegVirtual $ getHiVirtualRegFromLo vLo
      return (lo, hi)


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

getModLoc :: NatM ModLocation
getModLoc
        = NatM $ \ st -> (natm_modloc st, st)

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

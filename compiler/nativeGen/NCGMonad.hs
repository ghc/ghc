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

module NCGMonad (
        NcgImpl(..),
        NatM_State(..), mkNatM_State,

        NatM, -- instance Monad
        initNat,
        addImportNat,
        addNodeBetweenNat,
        addImmediateSuccessorNat,
        updateCfgNat,
        getUniqueNat, getCfgNat,
        mapAccumLNat,
        setDeltaNat,
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

import GhcPrelude

import Reg
import Format
import TargetReg

import BlockId
import Hoopl.Collections
import Hoopl.Label
import CLabel           ( CLabel )
import Debug
import FastString       ( FastString )
import UniqFM
import UniqSupply
import Unique           ( Unique )
import DynFlags
import Module

import Control.Monad    ( liftM, ap )

import Instruction
import Outputable (SDoc, pprPanic, ppr)
import Cmm (RawCmmDecl, CmmStatics)
import CFG
import Util

data NcgImpl statics instr jumpDest = NcgImpl {
    cmmTopCodeGen             :: RawCmmDecl -> NatM [NatCmmDecl statics instr],
    generateJumpTableForInstr :: instr -> Maybe (NatCmmDecl statics instr),
    getJumpDestBlockId        :: jumpDest -> Maybe BlockId,
    canShortcut               :: instr -> Maybe jumpDest,
    shortcutStatics           :: (BlockId -> Maybe jumpDest) -> statics -> statics,
    shortcutJump              :: (BlockId -> Maybe jumpDest) -> instr -> instr,
    pprNatCmmDecl             :: NatCmmDecl statics instr -> SDoc,
    maxSpillSlots             :: Int,
    allocatableRegs           :: [RealReg],
    ncg_x86fp_kludge          :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr],
    ncgExpandTop              :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr],
    ncgAllocMoreStack         :: Int -> NatCmmDecl statics instr
                              -> UniqSM (NatCmmDecl statics instr, [(BlockId,BlockId)]),
    -- ^ The list of block ids records the redirected jumps to allow us to update
    -- the CFG.
    ncgMakeFarBranches        :: LabelMap CmmStatics -> [NatBasicBlock instr] -> [NatBasicBlock instr],
    extractUnwindPoints       :: [instr] -> [UnwindPoint],
    -- ^ given the instruction sequence of a block, produce a list of
    -- the block's 'UnwindPoint's
    -- See Note [What is this unwinding business?] in Debug
    -- and Note [Unwinding information in the NCG] in this module.
    invertCondBranches        :: Maybe CFG -> LabelMap CmmStatics -> [NatBasicBlock instr]
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
                        , natm_this_module = this_mod
                        , natm_modloc = loc
                        , natm_fileid = dwf
                        , natm_debug_map = dbg
                        , natm_cfg = cfg
                        }

initNat :: NatM_State -> NatM a -> (a, NatM_State)
initNat init_st m
        = case unNat m init_st of { (r,st) -> (r,st) }

instance Functor NatM where
      fmap = liftM

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

getCfgNat :: NatM CFG
getCfgNat = NatM $ \ st -> (natm_cfg st, st)

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
        = pprPanic "Faild to update cfg: Untracked edge" (ppr (from,to))


-- | Place `succ` after `block` and change any edges
--   block -> X to `succ` -> X
addImmediateSuccessorNat :: HasDebugCallStack => BlockId -> BlockId -> NatM ()
addImmediateSuccessorNat block succ
        = updateCfgNat (addImmediateSuccessor block succ)

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
      dflags <- getDynFlags
      return (RegVirtual $ targetMkVirtualReg (targetPlatform dflags) u rep)


getNewRegPairNat :: Format -> NatM (Reg,Reg)
getNewRegPairNat rep
 = do u <- getUniqueNat
      dflags <- getDynFlags
      let vLo = targetMkVirtualReg (targetPlatform dflags) u rep
      let lo  = RegVirtual $ targetMkVirtualReg (targetPlatform dflags) u rep
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

getFileId :: FastString -> NatM Int
getFileId f = NatM $ \st ->
  case lookupUFM (natm_fileid st) f of
    Just (_,n) -> (n, st)
    Nothing    -> let n = 1 + sizeUFM (natm_fileid st)
                      fids = addToUFM (natm_fileid st) f (f,n)
                  in n `seq` fids `seq` (n, st { natm_fileid = fids  })

getDebugBlock :: Label -> NatM (Maybe DebugBlock)
getDebugBlock l = NatM $ \st -> (mapLookup l (natm_debug_map st), st)

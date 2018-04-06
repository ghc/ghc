{-# LANGUAGE CPP #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
--
-- The native code generator's monad.
--
-- -----------------------------------------------------------------------------

module NCGMonad (
        NatM_State(..), mkNatM_State,

        NatM, -- instance Monad
        initNat,
        addImportNat,
        getUniqueNat,
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
                natm_debug_map   :: LabelMap DebugBlock
        }

type DwarfFiles = UniqFM (FastString, Int)

newtype NatM result = NatM (NatM_State -> (result, NatM_State))

unNat :: NatM a -> NatM_State -> (a, NatM_State)
unNat (NatM a) = a

mkNatM_State :: UniqSupply -> Int -> DynFlags -> Module -> ModLocation ->
                DwarfFiles -> LabelMap DebugBlock -> NatM_State
mkNatM_State us delta dflags this_mod
        = NatM_State us delta [] Nothing dflags this_mod

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

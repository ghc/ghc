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
	initNat, addImportNat, getUniqueNat,
	mapAccumLNat, setDeltaNat, getDeltaNat,
	getBlockIdNat, getNewLabelNat, getNewRegNat, getNewRegPairNat,
 ) where
  
#include "HsVersions.h"

import Cmm		( BlockId(..) )
import CLabel		( CLabel, mkAsmTempLabel )
import MachRegs
import MachOp		( MachRep )
import UniqSupply
import Unique		( Unique )


data NatM_State = NatM_State {
			natm_us      :: UniqSupply,
			natm_delta   :: Int,
			natm_imports :: [(Bool,CLabel)]
		}

newtype NatM result = NatM (NatM_State -> (result, NatM_State))

unNat (NatM a) = a

mkNatM_State :: UniqSupply -> Int -> NatM_State
mkNatM_State us delta = NatM_State us delta []

initNat :: NatM_State -> NatM a -> (a, NatM_State)
initNat init_st m = case unNat m init_st of { (r,st) -> (r,st) }

instance Monad NatM where
  (>>=) = thenNat
  return = returnNat

thenNat :: NatM a -> (a -> NatM b) -> NatM b
thenNat expr cont
  = NatM $ \st -> case unNat expr st of
			(result, st') -> unNat (cont result) st'

returnNat :: a -> NatM a
returnNat result = NatM $ \st ->  (result, st)

mapAccumLNat :: (acc -> x -> NatM (acc, y))
                -> acc
	        -> [x]
	        -> NatM (acc, [y])

mapAccumLNat f b []
  = return (b, [])
mapAccumLNat f b (x:xs)
  = do (b__2, x__2)  <- f b x
       (b__3, xs__2) <- mapAccumLNat f b__2 xs
       return (b__3, x__2:xs__2)

getUniqueNat :: NatM Unique
getUniqueNat = NatM $ \ (NatM_State us delta imports) ->
    case splitUniqSupply us of
         (us1,us2) -> (uniqFromSupply us1, (NatM_State us2 delta imports))

getDeltaNat :: NatM Int
getDeltaNat = NatM $ \ st -> (natm_delta st, st)

setDeltaNat :: Int -> NatM ()
setDeltaNat delta = NatM $ \ (NatM_State us _ imports) ->
   ((), NatM_State us delta imports)

addImportNat :: Bool -> CLabel -> NatM ()
addImportNat is_code imp = NatM $ \ (NatM_State us delta imports) -> 
   ((), NatM_State us delta ((is_code,imp):imports))

getBlockIdNat :: NatM BlockId
getBlockIdNat = do u <- getUniqueNat; return (BlockId u)

getNewLabelNat :: NatM CLabel
getNewLabelNat = do u <- getUniqueNat; return (mkAsmTempLabel u)

getNewRegNat :: MachRep -> NatM Reg
getNewRegNat rep = do u <- getUniqueNat; return (mkVReg u rep)

getNewRegPairNat :: MachRep -> NatM (Reg,Reg)
getNewRegPairNat rep = do 
  u <- getUniqueNat
  let lo = mkVReg u rep; hi = getHiVRegFromLo lo
  return (lo,hi)


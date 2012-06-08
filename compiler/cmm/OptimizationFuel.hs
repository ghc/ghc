{-# LANGUAGE TypeFamilies #-}
-- | Optimisation fuel is used to control the amount of work the optimiser does.
--
-- Every optimisation step consumes a certain amount of fuel and stops when
-- it runs out of fuel.  This can be used e.g. to debug optimiser bugs: Run
-- the optimiser with varying amount of fuel to find out the exact number of
-- steps where a bug is introduced in the output.
module OptimizationFuel
    ( OptimizationFuel, amountOfFuel, tankFilledTo, unlimitedFuel, anyFuelLeft, oneLessFuel
    , OptFuelState, initOptFuelState
    , FuelConsumer, FuelUsingMonad, FuelState
    , fuelGet, fuelSet, lastFuelPass, setFuelPass
    , fuelExhausted, fuelDec1, tryWithFuel
    , runFuelIO, runInfiniteFuelIO, fuelConsumingPass
    , FuelUniqSM
    , liftUniq
    )
where

import Data.IORef
import Control.Monad
import StaticFlags (opt_Fuel)
import UniqSupply
import Panic
import Util

import Compiler.Hoopl
import Compiler.Hoopl.GHC (getFuel, setFuel)

#include "HsVersions.h"


-- We limit the number of transactions executed using a record of flags
-- stored in an HscEnv. The flags store the name of the last optimization
-- pass and the amount of optimization fuel remaining.
data OptFuelState =
  OptFuelState { pass_ref :: IORef String
               , fuel_ref :: IORef OptimizationFuel
               }
initOptFuelState :: IO OptFuelState
initOptFuelState =
  do pass_ref' <- newIORef "unoptimized program"
     fuel_ref' <- newIORef (tankFilledTo opt_Fuel)
     return OptFuelState {pass_ref = pass_ref', fuel_ref = fuel_ref'}

type FuelConsumer a = OptimizationFuel -> (a, OptimizationFuel)

tankFilledTo :: Int -> OptimizationFuel
amountOfFuel :: OptimizationFuel -> Int

anyFuelLeft :: OptimizationFuel -> Bool
oneLessFuel :: OptimizationFuel -> OptimizationFuel
unlimitedFuel :: OptimizationFuel

newtype OptimizationFuel = OptimizationFuel Int
  deriving Show

tankFilledTo = OptimizationFuel
amountOfFuel (OptimizationFuel f) = f

anyFuelLeft (OptimizationFuel f) = f > 0
oneLessFuel (OptimizationFuel f) = ASSERT (f > 0) (OptimizationFuel (f - 1))
unlimitedFuel = OptimizationFuel infiniteFuel

data FuelState = FuelState { fs_fuel :: OptimizationFuel, fs_lastpass :: String }
newtype FuelUniqSM a = FUSM { unFUSM :: FuelState -> UniqSM (a, FuelState) }

fuelConsumingPass :: String -> FuelConsumer a -> FuelUniqSM a
fuelConsumingPass name f = do setFuelPass name
                              fuel <- fuelGet
                              let (a, fuel') = f fuel
                              fuelSet fuel'
                              return a

runFuelIO :: OptFuelState -> FuelUniqSM a -> IO a
runFuelIO fs (FUSM f) =
    do pass <- readIORef (pass_ref fs)
       fuel <- readIORef (fuel_ref fs)
       u    <- mkSplitUniqSupply 'u'
       let (a, FuelState fuel' pass') = initUs_ u $ f (FuelState fuel pass)
       writeIORef (pass_ref fs) pass'
       writeIORef (fuel_ref fs) fuel'
       return a

-- ToDo: Do we need the pass_ref when we are doing infinite fueld
-- transformations?
runInfiniteFuelIO :: OptFuelState -> FuelUniqSM a -> IO a
runInfiniteFuelIO fs (FUSM f) =
    do pass <- readIORef (pass_ref fs)
       u <- mkSplitUniqSupply 'u'
       let (a, FuelState _ pass') = initUs_ u $ f (FuelState unlimitedFuel pass)
       writeIORef (pass_ref fs) pass'
       return a

instance Monad FuelUniqSM where
  FUSM f >>= k = FUSM (\s -> f s >>= \(a, s') -> unFUSM (k a) s')
  return a     = FUSM (\s -> return (a, s))

instance MonadUnique FuelUniqSM where
    getUniqueSupplyM = liftUniq getUniqueSupplyM
    getUniqueM       = liftUniq getUniqueM
    getUniquesM      = liftUniq getUniquesM

liftUniq :: UniqSM x -> FuelUniqSM x
liftUniq x = FUSM (\s -> x >>= (\u -> return (u, s)))

class Monad m => FuelUsingMonad m where
  fuelGet      :: m OptimizationFuel
  fuelSet      :: OptimizationFuel -> m ()
  lastFuelPass :: m String
  setFuelPass  :: String -> m ()

fuelExhausted :: FuelUsingMonad m => m Bool
fuelExhausted = fuelGet >>= return . anyFuelLeft

fuelDec1 :: FuelUsingMonad m => m ()
fuelDec1 = fuelGet >>= fuelSet . oneLessFuel

tryWithFuel :: FuelUsingMonad m => a -> m (Maybe a)
tryWithFuel r = do f <- fuelGet
                   if anyFuelLeft f then fuelSet (oneLessFuel f) >> return (Just r)
                                    else return Nothing

instance FuelUsingMonad FuelUniqSM where
  fuelGet          = extract fs_fuel
  lastFuelPass     = extract fs_lastpass
  fuelSet fuel     = FUSM (\s -> return ((), s { fs_fuel     = fuel }))
  setFuelPass pass = FUSM (\s -> return ((), s { fs_lastpass = pass }))

extract :: (FuelState -> a) -> FuelUniqSM a
extract f = FUSM (\s -> return (f s, s))

instance FuelMonad FuelUniqSM where
  getFuel = liftM amountOfFuel fuelGet
  setFuel = fuelSet . tankFilledTo

-- Don't bother to checkpoint the unique supply; it doesn't matter
instance CheckpointMonad FuelUniqSM where
    type Checkpoint FuelUniqSM = FuelState
    checkpoint = FUSM $ \fuel -> return (fuel, fuel) 
    restart fuel = FUSM $ \_ -> return ((), fuel)


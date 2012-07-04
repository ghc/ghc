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
    , FuelConsumer, FuelState
    , runFuelIO, runInfiniteFuelIO
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

data FuelState = FuelState { fs_fuel :: {-# UNPACK #-} !OptimizationFuel,
                             fs_lastpass :: String }
newtype FuelUniqSM a = FUSM { unFUSM :: UniqSupply -> FuelState -> (# a, UniqSupply, FuelState #) }

runFuelIO :: OptFuelState -> FuelUniqSM a -> IO a
runFuelIO fs (FUSM f) =
    do pass <- readIORef (pass_ref fs)
       fuel <- readIORef (fuel_ref fs)
       u    <- mkSplitUniqSupply 'u'
       case f u (FuelState fuel pass) of
          (# a, _, FuelState fuel' pass' #) -> do
            writeIORef (pass_ref fs) pass'
            writeIORef (fuel_ref fs) fuel'
            return a

-- ToDo: Do we need the pass_ref when we are doing infinite fueld
-- transformations?
runInfiniteFuelIO :: OptFuelState -> FuelUniqSM a -> IO a
runInfiniteFuelIO fs (FUSM f) =
    do pass <- readIORef (pass_ref fs)
       u <- mkSplitUniqSupply 'u'
       case f u (FuelState unlimitedFuel pass) of
          (# a, _, FuelState _fuel pass' #) -> do
            writeIORef (pass_ref fs) pass'
            return a

instance Monad FuelUniqSM where
  FUSM f >>= k = FUSM (\u s -> case f u s of (# a, u', s' #) ->
                                                unFUSM (k a) u' s')
  return a     = FUSM (\u s -> (# a, u, s #))

instance MonadUnique FuelUniqSM where
    getUniqueSupplyM =
       FUSM $ \us f -> case splitUniqSupply us of
                         (us1,us2) -> (# us1, us2, f #)

    getUniqueM =
       FUSM $ \us f -> case splitUniqSupply us of
                         (us1,us2) -> (# uniqFromSupply us1, us2, f #)

    getUniquesM =
       FUSM $ \us f -> case splitUniqSupply us of
                         (us1,us2) -> (# uniqsFromSupply us1, us2, f #)


liftUniq :: UniqSM x -> FuelUniqSM x
liftUniq x = FUSM (\u s -> case initUs u x of (a,u') -> (# a, u', s #))

class Monad m => FuelUsingMonad m where
  fuelGet      :: m OptimizationFuel
  fuelSet      :: OptimizationFuel -> m ()
  lastFuelPass :: m String
  setFuelPass  :: String -> m ()

instance FuelUsingMonad FuelUniqSM where
  fuelGet          = extract fs_fuel
  lastFuelPass     = extract fs_lastpass
  fuelSet fuel     = FUSM (\u s -> (# (), u, s { fs_fuel     = fuel } #))
  setFuelPass pass = FUSM (\u s -> (# (), u, s { fs_lastpass = pass } #))

extract :: (FuelState -> a) -> FuelUniqSM a
extract f = FUSM (\u s -> (# f s, u, s #))

instance FuelMonad FuelUniqSM where
  getFuel = liftM amountOfFuel fuelGet
  setFuel = fuelSet . tankFilledTo

-- Don't bother to checkpoint the unique supply; it doesn't matter
instance CheckpointMonad FuelUniqSM where
    type Checkpoint FuelUniqSM = FuelState
    checkpoint = FUSM $ \u fuel -> (# fuel, u, fuel #)
    restart fuel = FUSM $ \u _ -> (# (), u, fuel #)


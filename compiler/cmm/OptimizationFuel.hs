module OptimizationFuel
    ( OptimizationFuel, canRewriteWithFuel, maybeRewriteWithFuel, oneLessFuel
    , tankFilledTo, diffFuel
    , FuelConsumer
    , FuelUsingMonad, FuelState
    , lastFuelPass, fuelExhausted, fuelRemaining, fuelDecrement
    , lastFuelPassInState, fuelExhaustedInState, fuelRemainingInState
    , fuelDecrementState
    , runFuel, runFuelIO, runFuelWithLastPass, fuelConsumingPass
    , FuelMonad(..)
    )
where

--import GHC.Exts (State#)
import Panic

import Data.IORef

#include "HsVersions.h"

type FuelConsumer a = OptimizationFuel -> (a, OptimizationFuel)

canRewriteWithFuel :: OptimizationFuel -> Bool
oneLessFuel :: OptimizationFuel -> OptimizationFuel
maybeRewriteWithFuel :: OptimizationFuel -> Maybe a -> Maybe a
diffFuel :: OptimizationFuel -> OptimizationFuel -> Int
   -- to measure consumption during compilation
tankFilledTo :: Int -> OptimizationFuel

#ifdef DEBUG
newtype OptimizationFuel = OptimizationFuel Int
  deriving Show

tankFilledTo = OptimizationFuel
canRewriteWithFuel (OptimizationFuel f) = f > 0
maybeRewriteWithFuel fuel ma = if canRewriteWithFuel fuel then ma else Nothing
oneLessFuel (OptimizationFuel f) = ASSERT (f > 0) (OptimizationFuel (f - 1))
diffFuel (OptimizationFuel f) (OptimizationFuel f') = f - f'
#else
-- type OptimizationFuel = State# () -- would like this, but it won't work
data OptimizationFuel = OptimizationFuel
  deriving Show
tankFilledTo _ = undefined -- should be impossible to evaluate
  -- realWorld# might come in handy, too...
canRewriteWithFuel OptimizationFuel = True
maybeRewriteWithFuel _ ma = ma
oneLessFuel f = f
diffFuel _ _ = 0
#endif

data FuelState = FuelState { fs_fuellimit :: OptimizationFuel, fs_lastpass :: String }
newtype FuelMonad a = FuelMonad (FuelState -> (a, FuelState))

fuelConsumingPass :: String -> FuelConsumer a -> FuelMonad a
fuelConsumingPass name f = do fuel <- fuelRemaining
                              let (a, fuel') = f fuel
                              fuelDecrement name fuel fuel'
                              return a

runFuel             :: FuelMonad a -> FuelConsumer a
runFuelWithLastPass :: FuelMonad a -> FuelConsumer (a, String)

runFuelIO :: IORef String -> IORef OptimizationFuel -> FuelMonad a -> IO a
runFuelIO pass_ref fuel_ref (FuelMonad f) =
    do { pass <- readIORef pass_ref
       ; fuel <- readIORef fuel_ref
       ; let (a, FuelState fuel' pass') = f (FuelState fuel pass)
       ; writeIORef pass_ref pass'
       ; writeIORef fuel_ref fuel'
       ; return a
       }

initialFuelState :: OptimizationFuel -> FuelState
initialFuelState fuel = FuelState fuel "unoptimized program"

runFuel             (FuelMonad f) fuel = let (a, s) = f $ initialFuelState fuel
                                         in (a, fs_fuellimit s)
runFuelWithLastPass (FuelMonad f) fuel = let (a, s) = f $ initialFuelState fuel
                                         in ((a, fs_lastpass s), fs_fuellimit s)

lastFuelPassInState :: FuelState -> String
lastFuelPassInState = fs_lastpass

fuelExhaustedInState :: FuelState -> Bool
fuelExhaustedInState = canRewriteWithFuel . fs_fuellimit

fuelRemainingInState :: FuelState -> OptimizationFuel
fuelRemainingInState = fs_fuellimit

fuelDecrementState
    :: String -> OptimizationFuel -> OptimizationFuel -> FuelState -> FuelState
fuelDecrementState new_optimizer old new s =
    FuelState { fs_fuellimit = lim, fs_lastpass = optimizer }
  where lim = if diffFuel old (fs_fuellimit s) == 0 then new
              else panic $
                   concat ["lost track of ", new_optimizer, "'s transactions"]
        optimizer = if diffFuel old new > 0 then new_optimizer else fs_lastpass s

class Monad m => FuelUsingMonad m where
  fuelRemaining :: m OptimizationFuel
  fuelDecrement :: String -> OptimizationFuel -> OptimizationFuel -> m ()
  fuelExhausted :: m Bool
  lastFuelPass  :: m String
  

instance Monad FuelMonad where
  FuelMonad f >>= k = FuelMonad (\s -> let (a, s') = f s
                                           FuelMonad f' = k a
                                       in  f' s')
  return a = FuelMonad (\s -> (a, s))

instance FuelUsingMonad FuelMonad where
  fuelRemaining = extract fuelRemainingInState
  lastFuelPass  = extract lastFuelPassInState
  fuelExhausted = extract fuelExhaustedInState
  fuelDecrement p f f' = FuelMonad (\s -> ((), fuelDecrementState p f f' s))

extract :: (FuelState -> a) -> FuelMonad a
extract f = FuelMonad (\s -> (f s, s))

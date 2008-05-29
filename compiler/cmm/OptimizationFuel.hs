module OptimizationFuel
    ( OptimizationFuel ,  canRewriteWithFuel, maybeRewriteWithFuel, oneLessFuel
    , OptFuelState, initOptFuelState --, setTotalFuel
    , tankFilledTo, diffFuel
    , FuelConsumer
    , FuelUsingMonad, FuelState
    , lastFuelPass, fuelExhausted, fuelRemaining, fuelDecrement, fuelDec1
    , runFuelIO, fuelConsumingPass
    , FuelMonad
    , liftUniq
    , lGraphOfGraph -- needs to be able to create a unique ID...
    )
where

import BlockId
import ZipCfg
--import GHC.Exts (State#)
import Panic
import Data.IORef
import Monad
import StaticFlags (opt_Fuel)
import UniqSupply

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
newtype FuelMonad a = FuelMonad (FuelState -> UniqSM (a, FuelState))

fuelConsumingPass :: String -> FuelConsumer a -> FuelMonad a
fuelConsumingPass name f = do fuel <- fuelRemaining
                              let (a, fuel') = f fuel
                              fuelDecrement name fuel fuel'
                              return a

runFuelIO :: OptFuelState -> FuelMonad a -> IO a
runFuelIO fs (FuelMonad f) =
    do pass <- readIORef (pass_ref fs)
       fuel <- readIORef (fuel_ref fs)
       u    <- mkSplitUniqSupply 'u'
       let (a, FuelState fuel' pass') = initUs_ u $ f (FuelState fuel pass)
       writeIORef (pass_ref fs) pass'
       writeIORef (fuel_ref fs) fuel'
       return a

instance Monad FuelMonad where
  FuelMonad f >>= k = FuelMonad (\s -> do (a, s') <- f s
                                          let FuelMonad f' = k a in (f' s'))
  return a = FuelMonad (\s -> return (a, s))

instance MonadUnique FuelMonad where
    getUniqueSupplyM = liftUniq getUniqueSupplyM
    getUniqueM       = liftUniq getUniqueM
    getUniquesM      = liftUniq getUniquesM
liftUniq :: UniqSM x -> FuelMonad x
liftUniq x = FuelMonad (\s -> x >>= (\u -> return (u, s)))

class Monad m => FuelUsingMonad m where
  fuelRemaining :: m OptimizationFuel
  fuelDecrement :: String -> OptimizationFuel -> OptimizationFuel -> m ()
  fuelDec1      :: m ()
  fuelExhausted :: m Bool
  lastFuelPass  :: m String

instance FuelUsingMonad FuelMonad where
  fuelRemaining = extract fs_fuellimit
  lastFuelPass  = extract fs_lastpass
  fuelExhausted = extract $ not . canRewriteWithFuel . fs_fuellimit
  fuelDecrement p f f' = FuelMonad (\s -> return ((), fuelDecrementState p f f' s))
  fuelDec1      = FuelMonad f 
     where f s = if canRewriteWithFuel (fs_fuellimit s) then
                    return ((), s { fs_fuellimit = oneLessFuel (fs_fuellimit s) })
                 else panic "Tried to use exhausted fuel supply"

extract :: (FuelState -> a) -> FuelMonad a
extract f = FuelMonad (\s -> return (f s, s))

fuelDecrementState
    :: String -> OptimizationFuel -> OptimizationFuel -> FuelState -> FuelState
fuelDecrementState new_optimizer old new s =
    FuelState { fs_fuellimit = lim, fs_lastpass = optimizer }
  where lim = if diffFuel old (fs_fuellimit s) == 0 then new
              else panic $
                   concat ["lost track of ", new_optimizer, "'s transactions"]
        optimizer = if diffFuel old new > 0 then new_optimizer else fs_lastpass s

-- lGraphOfGraph is here because we need uniques to implement it.
lGraphOfGraph :: Graph m l -> FuelMonad (LGraph m l)
lGraphOfGraph (Graph tail blocks) =
  do entry <- liftM BlockId $ getUniqueM
     return $ LGraph entry (insertBlock (Block entry tail) blocks)

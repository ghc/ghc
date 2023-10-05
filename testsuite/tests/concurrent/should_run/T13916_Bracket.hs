{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Module      : Bracket
Description : Handling multiple environments with bracket-like apis
Maintainer  : robertkennedy@clearwateranalytics.com
Stability   : stable

This module is meant for ie Sql or mongo connections, where you may wish for some number of easy to grab
environments. In particular, this assumes your connection has some initialization/release functions

This module creates bugs with any optimizations enabled. The bugs do not occur if the program is in the same
module.
-}
module T13916_Bracket (
    -- * Data Types
    Spawner(..), Limit(..), Cache,
    -- * Usage
    withEnvCache, withEnv
    ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception hiding (handle)
import Control.Monad
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- * Data Types
-- | Tells the program how many environments it is allowed to spawn.
-- A `Lax` limit will spawn extra connections if the `Cache` is empty,
-- while a `Hard` limit will not spawn any more than the given number of connections simultaneously.
--
-- @since 0.3.7
data Limit = Hard {getLimit :: {-# unpack #-} !Int}

data Spawner env = Spawner
    { maker  :: IO env
    , killer :: env -> IO ()
    , isDead :: env -> IO Bool
    }

type VCache env = Vector (TMVar env)
data Cache env = Unlimited { spawner :: Spawner env
                           , vcache :: !(VCache env)
                           }
               | Limited   { spawner :: Spawner env
                           , vcache :: !(VCache env)
                           , envsem :: TSem
                           }

-- ** Initialization
withEnvCache :: Limit -> Spawner env -> (Cache env -> IO a) -> IO a
withEnvCache limit spawner = bracket starter releaseCache
    where starter = case limit of
            Hard n -> Limited spawner <$> initializeEmptyCache n <*> atomically (newTSem n)

-- ** Using a single value
withEnv :: Cache env -> (env -> IO a) -> IO a
withEnv cache = case cache of
    Unlimited{..} -> withEnvUnlimited spawner vcache
    Limited{..}   -> withEnvLimited   spawner vcache envsem

-- *** Unlimited
-- | Takes an env and returns it on completion of the function.
-- If all envs are already taken or closed, this will spin up a new env.
-- When the function finishes, this will attempt to put the env into the cache. If it cannot,
-- it will kill the env. Note this can lead to many concurrent connections.
--
-- @since 0.3.5
withEnvUnlimited :: Spawner env -> VCache env -> (env -> IO a) -> IO a
withEnvUnlimited Spawner{..} cache = bracket taker putter
  where
    taker = do
        mpipe <- atomically $ tryTakeEnv cache
        case mpipe of
            Nothing  -> maker
            Just env -> isDead env >>= \b -> if not b then return env else killer env >> maker

    putter env = do
        accepted <- atomically $ tryPutEnv cache env
        unless accepted $ killer env

-- *** Limited
-- | Takes an env and returns it on completion of the function.
-- If all envs are already taken, this will wait. This should have a constant number of environments
--
-- @since 0.3.6
withEnvLimited :: Spawner env -> VCache env -> TSem -> (env -> IO a) -> IO a
withEnvLimited spawner vcache envsem = bracket taker putter
  where
    taker = limitMakeEnv spawner vcache envsem
    putter env = atomically $ putEnv vcache env

limitMakeEnv :: Spawner env -> VCache env -> TSem -> IO env
limitMakeEnv Spawner{..} vcache envsem = go
  where
    go = do
        eenvpermission <- atomically $ ( Left  <$> takeEnv  vcache )
                              `orElse` ( Right <$> waitTSem envsem )
        case eenvpermission of
            Right () -> maker
            Left env -> do
                -- Given our env, we check if it's dead. If it's not, we are done and return it.
                -- If it is dead, we release it, signal that a new env can be created, and then recurse
                isdead <- isDead env
                if not isdead then return env
                    else do
                         killer env
                         atomically $ signalTSem envsem
                         go

-- * Low level
initializeEmptyCache :: Int -> IO (VCache env)
initializeEmptyCache n | n < 1     = return mempty
                       | otherwise = Vector.replicateM n newEmptyTMVarIO

takeEnv :: VCache env -> STM env
takeEnv = Vector.foldl folding retry
    where folding m stmenv = m `orElse` takeTMVar stmenv

tryTakeEnv :: VCache env -> STM (Maybe env)
tryTakeEnv cache = (Just <$> takeEnv cache) `orElse` pure Nothing

putEnv :: VCache env -> env -> STM ()
putEnv cache env = Vector.foldl folding retry cache
    where folding m stmenv = m `orElse` putTMVar stmenv env

tryPutEnv :: VCache env -> env -> STM Bool
tryPutEnv cache env = (putEnv cache env *> return True) `orElse` pure False

releaseCache :: Cache env -> IO ()
releaseCache cache = Vector.mapM_ qkRelease (vcache cache)
    where qkRelease tenv = atomically (tryTakeTMVar tenv)
                       >>= maybe (return ()) (killer $ spawner cache)

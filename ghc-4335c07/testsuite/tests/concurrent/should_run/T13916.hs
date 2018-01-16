module Main where

import Data.IORef
import System.IO.Unsafe
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import System.IO
import System.Directory
import System.FilePath
import T13916_Bracket

type Thing = MVar Bool

main :: IO ()
main = do
    withEnvCache limit spawner $ \cache ->
        forConcurrently_ [1..1000 :: Int] $ \n -> withEnv cache (\handle -> put handle n)
    where
        limit :: Limit
        limit = Hard 1

        put handle n = return ()

spawner :: Spawner Thing
spawner = Spawner
    { maker  = mkhandle
    , killer = \thing -> takeMVar thing >> putMVar thing True
    , isDead = \thing -> readMVar thing
    }

mkhandle :: IO Thing
mkhandle = newMVar False

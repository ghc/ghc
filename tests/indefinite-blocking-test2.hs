{-# LANGUAGE ScopedTypeVariables #-}

import LwConc.RunQueue
import LwConc.MVar
import System.Environment
import Control.Exception

task n = do
  print $ "Running" ++ show n
  return ()

parse (a:b:_) = (rInt a, rInt b)
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  -- Initialize
  args <- getArgs
  newSched
  let (n, maxTick) = parse args
  -- Define zombie task
  let zombie = do {
    mv <- newEmptyMVar;
    let mask :: IO () -> IO (Either BlockedIndefinitelyOnConcDS ()) = try
    in mask $ takeMVar mv;
    print "Zombie: caught exception"
  }
  -- Define loop
  let loop tick 0 = return ()
      loop tick n = do {
        -- create Zombie
        z <- forkIO zombie;
        nextTick <- (if tick == maxTick
                        then do {
                              print "Main yield";
                              yield;
                              return 0
                            }
                       else return (tick+1));
        loop nextTick $ n-1
      }
  -- invoke loop
  loop 0 n
  yield
  print "Main done"

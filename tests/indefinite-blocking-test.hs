import LwConc.RunQueue
import LwConc.MVar
import System.Environment

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
    takeMVar mv;
    print "Zombie: should not see this"
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

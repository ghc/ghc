import LwConc.RunQueue
import LwConc.Substrate
import System.Environment

task n = do
  print $ "Running" ++ show n
  return ()

loop tick (0, maxTick) = return ()
loop tick (n, maxTick) = do
  forkIO $ task n
  tick <- if tick == maxTick
             then do {
              print "Main yield";
              yield;
              return 0
             }
             else return (tick+1)
  loop tick (n-1, maxTick)


parse (a:b:_) = (rInt a, rInt b)
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  newSched
  n <- getNumCapabilities
  spawnScheds $ n-1
  loop 0 $ parse args
  yield
  print "Main Done"

spawnScheds 0 = return ()
spawnScheds n = do
  newCapability
  spawnScheds $ n-1

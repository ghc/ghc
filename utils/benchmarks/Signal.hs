import Control.Concurrent
import System.Event.Signal
import System.Posix.Signals

handler sk = do
  print (skSignal sk)

main = do
  mgr <- new
  blockAllSignals
  registerHandler_ mgr sigINT handler
  putStrLn "INT handler installed"
  forkIO $ do
    threadDelay 1000000
    registerHandler mgr sigUSR1 handler
    registerHandler mgr sigUSR2 handler
    putStrLn "USR1 and USR2 handlers installed"
  loop mgr

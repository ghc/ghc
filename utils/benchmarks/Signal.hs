{-# LANGUAGE FlexibleContexts #-}

import Control.Concurrent
import System.Posix.Signals (Handler(Catch), sigINT,
                              sigUSR1, sigUSR2,
                              blockSignals,
                              fullSignalSet, installHandler)
import GHC.Event (loop, EventManager)
import qualified GHC.Event.Manager as EM (new, emState)
import GHC.IORef (readIORef)

handler :: EventManager -> Handler
handler em = do
  Catch (readIORef (EM.emState em) >>= print)

main :: IO ()
main = do
  mgr <- EM.new
  blockSignals fullSignalSet
  _ <- installHandler sigINT (handler mgr) (Just fullSignalSet)
  putStrLn "INT handler installed"
  _ <- forkIO $ do
         threadDelay 1000000
         _ <- installHandler sigUSR1 (handler mgr) (Just fullSignalSet)
         _ <- installHandler sigUSR2 (handler mgr) (Just fullSignalSet)
         putStrLn "USR1 and USR2 handlers installed"
  loop mgr

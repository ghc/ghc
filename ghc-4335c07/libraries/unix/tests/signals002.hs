import System.Posix
import Control.Concurrent

-- !!! test blockSignals, raiseSignal, unblockSignals, getPendingSignals

main = do
  blockSignals ( userDefinedSignal1 `addSignal` emptySignalSet )
  raiseSignal userDefinedSignal1
  set <- getPendingSignals
  print (userDefinedSignal1 `inSignalSet` set)
  m <- newEmptyMVar 
  installHandler userDefinedSignal1 
	(Catch (putStrLn "hello" >> putMVar m ())) Nothing
  awaitSignal (Just emptySignalSet)
  takeMVar m

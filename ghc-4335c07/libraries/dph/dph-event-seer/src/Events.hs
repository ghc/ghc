module Events where
import GHC.RTS.Events.Analysis

runMachine :: Machine s e -> [e] -> s
runMachine machine = getState . validate machine
 where
  getState (Left (s,_)) = s
  getState (Right s)    = s

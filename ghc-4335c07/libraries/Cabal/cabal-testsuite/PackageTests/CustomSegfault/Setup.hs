import System.Posix.Signals

main = putStrLn "Quitting..." >> raiseSignal sigSEGV

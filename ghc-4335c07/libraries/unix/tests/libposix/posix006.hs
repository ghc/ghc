
import System.Posix.Time
import System.Posix.Unistd
import System.Posix.Signals

main = do start <- epochTime
          blockSignals reservedSignals -- see #4504
          sleep 1
          finish <- epochTime
          let slept = finish - start
          if slept >= 1 && slept <= 2
              then putStrLn "OK"
              else do putStr "Started: "
                      print start
                      putStr "Finished: "
                      print finish
                      putStr "Slept: "
                      print slept

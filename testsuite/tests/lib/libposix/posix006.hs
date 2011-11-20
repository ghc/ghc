
import System.Posix.Time
import System.Posix.Unistd

main = do start <- epochTime
          sleep 5
          finish <- epochTime
          let slept = finish - start
          if slept >= 5 && slept <= 6
              then putStrLn "OK"
              else do putStr "Started: "
                      print start
                      putStr "Finished: "
                      print finish
                      putStr "Slept: "
                      print slept

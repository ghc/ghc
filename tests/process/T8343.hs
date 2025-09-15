import System.Process
import System.Timeout

main = timeout 1000000 $ do -- The outer timeout shouldn't trigger
  timeout 10000 $ print =<< readProcess "sleep" ["7200"] ""
  putStrLn "Good!"
  timeout 10000 $ print =<< readProcessWithExitCode "sleep" ["7200"] ""
  putStrLn "Good!"

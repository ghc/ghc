-- Regression test for #26169: stat_endGC summed the CPU time of every GC
-- thread, idle ones included, whose fields it had never set. With -qn2, 6
-- of the 8 GC threads are idle. Check that the GC CPU time stays within
-- the process CPU time.
import Control.Concurrent (setNumCapabilities)
import Control.Monad (unless)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.Exit (exitFailure)
import System.Mem (performGC)

main :: IO ()
main = do
  setNumCapabilities 8
  performGC
  s <- getRTSStats
  let gc  = gc_cpu_ns s
      cpu = cpu_ns s
  unless (gc >= 0 && gc <= cpu) $ do
    putStrLn ("gc_cpu_ns = " ++ show gc ++ " but cpu_ns = " ++ show cpu)
    exitFailure

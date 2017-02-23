import Control.Exception
import GHC.Compact
import qualified Data.Map as Map
import Data.Time.Clock
import Text.Printf
import System.Environment
import System.Mem
import Control.DeepSeq

-- Benchmark compact against compactWithSharing. e.g.
--   ./compact_bench 1000000

main = do
  [n] <- map read <$> getArgs
  let m = Map.fromList [(x,[x*1000..x*1000+10]) | x <- [1..(n::Integer)]]
  evaluate (force m)
  timeIt "compact" $ compact m >>= compactSize >>= print
  timeIt "compactWithSharing" $ compactWithSharing m >>= compactSize >>= print

timeIt :: String -> IO a -> IO a
timeIt str io = do
  performMajorGC
  t0 <- getCurrentTime
  a <- io
  t1 <- getCurrentTime
  printf "%s: %.2f\n" str (realToFrac (t1 `diffUTCTime` t0) :: Double)
  return a

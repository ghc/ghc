import Data.Word
import Debug.Trace

main = do
  traceEventIO "testing"
  traceEventIO "%s" -- see #3874
  traceEventIO $ replicate (maxSize + 1) 'A'
  putStrLn $ traceEvent "one" "two"
  putStrLn $ traceEventWith (show . length) "three"

maxSize :: Int
maxSize = fromIntegral (maxBound :: Word16)

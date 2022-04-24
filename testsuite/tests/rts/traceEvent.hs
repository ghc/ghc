import Data.Word
import Debug.Trace

main = do
  traceEventIO "testing"
  traceEventIO "%s" -- see #3874
  traceEventIO $ replicate (maxSize + 1) 'A'
  putStrLn $ traceEvent "one" "two"

maxSize :: Int
maxSize = fromIntegral (maxBound :: Word16)

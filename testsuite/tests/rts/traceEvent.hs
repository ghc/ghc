import Data.Word
import Debug.Trace

main = do
  traceEventIO "testing"
  traceEventIO "%s" -- see #3874
  traceEventIO $ replicate (maxSize + 1) 'A'

maxSize :: Int
maxSize = fromIntegral (maxBound :: Word16)

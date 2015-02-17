import Debug.Trace

main = do
  traceEventIO "testing"
  traceEventIO "%s" -- see #3874

import GHC.Exts

main = do
  traceEvent "testing"
  traceEvent "%s" -- see #3874

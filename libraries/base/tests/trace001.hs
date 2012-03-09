import System.IO
import Debug.Trace

main = do
   hPutStr stderr
	(trace (trace (trace (trace (trace (trace (trace
	  "one" "fish") "two") "fish") "red") "fish") "blue") "fish")
   hPutStr stdout
	(trace (trace (trace (trace (trace (trace (trace
	  "ONE" "FISH") "TWO") "FISH") "RED") "FISH") "BLUE") "FISH")

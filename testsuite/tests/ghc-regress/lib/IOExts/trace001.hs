import IO
import IOExts

main = do
   hPutStr stderr
	(trace (trace (trace (trace (trace (trace (trace
	  "one" "fish") "two") "fish") "red") "fish") "blue") "fish")
   hPutStr stdout
	(trace (trace (trace (trace (trace (trace (trace
	  "one" "fish") "two") "fish") "red") "fish") "blue") "fish")

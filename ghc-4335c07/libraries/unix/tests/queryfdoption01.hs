import System.Posix.IO
import System.IO

showNBR = do
	v <- System.Posix.IO.queryFdOption 0 System.Posix.IO.NonBlockingRead
	putStr $ "NonBlockingRead = " ++ (show v) ++ "\n"

main = do
	showNBR
	System.Posix.IO.setFdOption 0 System.Posix.IO.NonBlockingRead True
	showNBR

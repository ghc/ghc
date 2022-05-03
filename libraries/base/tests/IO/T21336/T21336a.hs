import GHC.Weak
import System.IO
import System.Mem

main :: IO ()
main = do
    f <- openFile "/dev/full" WriteMode
    hPutStr f "hello"
    -- Ensure that the Handle's finalizer is run
    performMajorGC


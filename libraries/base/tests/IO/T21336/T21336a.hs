import System.IO
import System.Mem
import FinalizerExceptionHandler

main :: IO ()
main = do
    setFinalizerExceptionHandler printToStderrFinalizerExceptionHandler
    f <- openFile "/dev/full" WriteMode
    hPutStr f "hello"
    -- Ensure that the Handle's finalizer is run
    performMajorGC


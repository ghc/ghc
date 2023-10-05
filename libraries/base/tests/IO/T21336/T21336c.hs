import System.IO
import System.Mem
import FinalizerExceptionHandler

main :: IO ()
main = do
  setFinalizerExceptionHandler printToStderrFinalizerExceptionHandler
  hPutStr stdout "hello"


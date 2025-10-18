import GHC.Internal.Control.Exception
import GHC.Internal.Exception.Backtrace

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  throwIO $ ErrorCall "Throw error"

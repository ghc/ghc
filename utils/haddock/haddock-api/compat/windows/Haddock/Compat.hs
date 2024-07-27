module Haddock.Compat
  ( getProcessID
  , setEncoding
  ) where

import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
import System.IO (hSetEncoding, stdout, stderr)
import System.Win32.Process (ProcessId)
import qualified System.Win32.Process as Windows

-- | Avoid internal error: <stderr>: hPutChar: invalid argument (invalid character)' non UTF-8 Windows
setEncoding :: IO ()
setEncoding = do
  hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
  hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure

getProcessID :: IO Int
getProcessID = fromIntegral @ProcessId @Int <$> Windows.getCurrentProcessId

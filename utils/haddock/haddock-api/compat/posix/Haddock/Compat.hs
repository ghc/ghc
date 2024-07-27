module Haddock.Compat
  ( getProcessID
  , setEncoding
  ) where

import System.Posix.Types (ProcessID)
import qualified System.Posix.Process as Posix

-- | Windows-only failsafe, not applicable on POSIX plateforms
setEncoding :: IO ()
setEncoding = pure ()

getProcessID :: IO Int
getProcessID = fromIntegral @ProcessID @Int <$> Posix.getProcessID

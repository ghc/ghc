module Distribution.Haddock (
  readInterfaceFile,
  H.InterfaceFile(..)
) where

import Haddock.Exception
import qualified Haddock.InterfaceFile as H

import Control.Exception
import Control.Monad

readInterfaceFile :: FilePath -> IO (Either String H.InterfaceFile)
readInterfaceFile f = 
  liftM Right (H.readInterfaceFile f)
  `catchDyn` 
  (\(e::HaddockException) -> return $ Left $ show e)

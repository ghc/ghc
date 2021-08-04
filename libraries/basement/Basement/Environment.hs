module Basement.Environment
    ( getArgs
    , lookupEnv
    ) where

import           Basement.Compat.Base
import           Basement.UTF8.Base (String)
import qualified System.Environment as Sys (getArgs, lookupEnv)

-- | Returns a list of the program's command line arguments (not including the program name).
getArgs :: IO [String]
getArgs = fmap fromList <$> Sys.getArgs

-- | Lookup variable in the environment
lookupEnv :: String -> IO (Maybe String)
lookupEnv s = fmap fromList <$> Sys.lookupEnv (toList s)

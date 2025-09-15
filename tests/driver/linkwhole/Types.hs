module Types
  ( SOHandles(..)
  , SOHandleExport
  ) where

import Foreign

data SOHandles = SOHandles
  { someData :: String
  , someFn :: Int -> IO ()
  }

type SOHandleExport = IO (StablePtr SOHandles)

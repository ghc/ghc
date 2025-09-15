module Main where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.IORef

main :: IO ()
main = do
  ref <- newIORef ()
  replicateM_ 1000 $ withIORef ref $ runStateT (pure ())

withIORef :: IORef a -> (a -> IO (b, a)) -> IO b
withIORef ref f =
  readIORef ref >>= f >>= \(b, a) -> writeIORef ref a >> pure b

module Main (main) where

import Control.Exception.Base
import qualified Control.Monad.State.Strict as S

n :: Int
n = 10000000

times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}

main = do

  putStrLn "S"
  evaluate $ S.runState (times n $ (S.modify (+1))) 0


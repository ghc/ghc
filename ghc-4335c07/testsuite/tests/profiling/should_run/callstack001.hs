module Main where

import GHC.Stack
import Prelude hiding (mapM)

f :: Int -> IO Int
f x = do currentCallStack >>= print; return (x+1)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = go xs
  where go []     = return []
        go (x:xs) = do
          x' <- f x
          xs' <- mapM f xs
          return (x':xs')

main = mapM f [42,42]


module Main where

import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Data.Array.ST
import Data.Int
import Debug.Trace

data Refs s = Refs
    { memory    :: STArray s Int8 Int8
    , pc        :: STRef s Int8
    }

main :: IO ()
main = do
    print $ runST m
  where
    m = do
      m <- newArray_ (0,30)
      p <- newSTRef 0
      let r = Refs m p
      writeArray m 0 0x4
      v <- readSTRef p
      modifySTRef p (+1)
--      trace ("v: " ++ show v) $ return ()
      op <- readArray m v
      case {- trace ("v: " ++ show v) $ -} op of
          0x4 -> modifySTRef p (+100) -- should run this
          n   -> error ("should never match this: " ++ show n)

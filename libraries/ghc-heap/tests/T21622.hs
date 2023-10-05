{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE MagicHash #-}

import Control.Monad
import GHC.Exts.Heap
import System.Mem
import GHC.Exts
import GHC.Weak
import Data.Foldable

main :: IO ()
main = do
    let x = 42 :: Integer
    Weak w <- mkWeak x () Nothing
    weaks <- collectWeaks w
    performMajorGC
    print (x, fmap length weaks)

collectWeaks :: Weak# v -> IO [Closure]
collectWeaks = \w -> getClosureData w >>= go []
  where
    go :: [Closure] -> Closure -> IO [Closure]
    go acc w@(WeakClosure {weakLink})
      | Just next <- weakLink = getBoxedClosureData next >>= go (w:acc)
      | otherwise = return (w:acc)


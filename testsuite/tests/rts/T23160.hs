{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

-- Regression test for multiple threads competing for evacuating a Weak#
-- Based on the reproducer by Ben Gamari in #23160


import GHC.Exts
import GHC.Weak
import GHC.IO (IO(..))
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Mem
import System.Mem.Weak

data MBA = MBA (MutableByteArray# RealWorld)

newPinnedMBA :: Int -> IO MBA
newPinnedMBA (I# n) = IO $ \s ->
  case newPinnedByteArray# n s of
    (# s', mba #) -> (# s', MBA mba #)

mkMBAWeak :: MBA -> a -> IO (Weak a)
mkMBAWeak (MBA mba) v = IO $
    \s -> case mkWeakNoFinalizer# mba v s of
            (# s', w #) -> (# s', Weak w #)

forced :: String -> IO Int
forced a = evaluate (length a)

worker :: IO ()
worker = forever $ do
    xs <- mapM f [0..200000::Int]
    mapM_ (\w -> deRefWeak w >>= mapM_ forced) (take 1000 xs)
  where
    f n = do
      mba <- newPinnedMBA 10
      v <- forced (show n)
      mkMBAWeak mba (show (n + v))

main :: IO ()
main = do
  forkIO $ forever $ do
    threadDelay (100*1000)
    performMajorGC

  replicateM_ 10 (forkIO worker)
  threadDelay (25*1000*1000)
  putStrLn "OK"

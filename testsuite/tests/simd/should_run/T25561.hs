{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.IO

main :: IO ()
main = do
  ma@(IOUArray (STUArray l _ _ mba)) <- newListArray (0, 10) ([0..10] :: [Float])
  IO $ \s -> (# writeFloatArrayAsFloatX4# mba 1# (broadcastFloatX4# 3.0#) s, () #)
  print =<< getElems ma

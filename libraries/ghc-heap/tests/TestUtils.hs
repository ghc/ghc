{-# LANGUAGE MagicHash #-}
module TestUtils where

import Foreign (Ptr)
import GHC.Exts (Addr#)
import GHC.Ptr (Ptr(Ptr))

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = return ()

unpackAddr# :: Ptr () -> Addr#
unpackAddr# (Ptr addr) = addr

{-# LANGUAGE MagicHash #-}
module TestUtils where

import GHC.Exts.Heap (getClosureData, LiftedClosure, Box, GenClosure)
import Foreign (Ptr)
import GHC.Exts (Ptr, Addr#, unsafeCoerce#)
import GHC.Ptr (Ptr(Ptr))

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = return ()

createClosure :: Ptr () -> IO (GenClosure Box)
createClosure tsoPtr = do
    let addr = unpackAddr# tsoPtr
    getClosureData ((unsafeCoerce# addr) :: LiftedClosure)

unpackAddr# :: Ptr () -> Addr#
unpackAddr# (Ptr addr) = addr

{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeOperators #-}
module NativeSourceLengthEmpty where

import Data.IORef
import PackedVecLibrary

safeHead :: Vec a (1 + n) -> a
safeHead (VCons x _) = x

empty :: Int
empty = safeHead (sourceVector @Int [])

fromRef :: IORef [Int] -> IO Int
fromRef ref = do
    xs <- readIORef ref
    pure (safeHead (sourceVector xs))

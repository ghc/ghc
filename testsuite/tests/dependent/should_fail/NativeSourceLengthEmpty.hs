{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications #-}
module NativeSourceLengthEmpty where

import Data.IORef
import PackedVecLibrary

safeHead :: Vec a ('S n) -> a
safeHead (VCons x _) = x

empty :: Int
empty = safeHead (sourceVector @Int [])

fromRef :: IORef [Int] -> IO Int
fromRef ref = do
    xs <- readIORef ref
    pure (safeHead (sourceVector xs))

{-# LANGUAGE RankNTypes, TypeApplications #-}

module VtaCoerce where

import Data.Coerce (coerce)

newtype Age = Age Int

convert :: Int -> Age
convert = coerce @Int @Age

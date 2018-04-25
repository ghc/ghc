{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RolesIArray where

import Data.Word
import Data.Array.IArray
import Data.Array.Unboxed

newtype N = MkN Word64
  deriving (IArray UArray)